args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
pred_type <- tolower(as.character(args[3]))

draws.max <- 1000
draws.load.chunk <- 100
draws.eval.chunk <- 10

# n.threads <- 4
# region <- "amz"
# draws.max <- 8
# draws.load.chunk <- 4
# draws.eval.chunk <- 2

setDTthreads(n.threads)
set_cpu_count(n.threads)


path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")
path.cf <- paste0(path.base, "models/egp_cf/", region, "/")
path.mar <- paste0(path.base, "models/marginal/", region, "/")
if(!dir.exists(path.mar))
  dir.create(path.mar, recursive = TRUE)
file.cf <- paste0(path.cf, region, ".covid.", pred_type, ".rds")
file.mar <- paste0(path.mar, region, ".covid.", pred_type, ".rds")
path.arrow <- paste0(path.pred, region, "/fac/")

cf <- readRDS(file.cf)
pred.ds <- open_dataset(path.arrow, format = "arrow")

draw.chunks.load <- chunk_seq(1, draws.max, draws.load.chunk)
group.chunks <- chunk_seq(1, nrow(cf$groups), 100)

id.var <- "id"

eval.mar.i <- list()

for(i in seq_along(draw.chunks.load$from)) {

  a <- Sys.time()

  paste0("Loading predictions ", draw.chunks.load$from[i],
         " to ", draw.chunks.load$to[i],
         " …") |>
  message()

  pred.draw <-
    pred.ds |>
    filter(.draw >= draw.chunks.load$from[i] & .draw <= draw.chunks.load$to[i]) |>
    select(.draw, any_of(id.var), forestloss) |>
    collect()
  
  draw.chunks.eval <-
    chunk_seq(draw.chunks.load$from[i],
              draw.chunks.load$to[i],
              draws.eval.chunk)

  eval.mar.j <- list()

  b <- Sys.time()
  print(b-a)

  for(j in seq_along(draw.chunks.eval$from)) {

    paste0("Evaluating draws ", draw.chunks.eval$from[j],
           " to ", draw.chunks.eval$to[j],
           " …") |>
    message()

    pred.draw.j <- pred.draw[.draw %in% draw.chunks.eval$from[j]:draw.chunks.eval$to[j]]

    message("  Evaluating marginals …")

    a <- Sys.time()

    eval.fac <-
      egp_evaluate_factual(predictions = pred.draw.j,
                           cf.def = cf,
                           name = "factual",
                           pred.var = "forestloss",
                           draw.chunk = NULL,
                           agg.size = 1e6,
                           parallel = n.threads,
                           progress = TRUE)
    
    silence <- gc()

    eval.cf.l <- list()

    for(k in seq_along(group.chunks$from)) {

      if(length(group.chunks$from) > 1) {
        paste0("  Groups ", group.chunks$from[k],
           " to ", group.chunks$to[k],
           " …") |>
        message()
      }

      eval.cf.l[[k]] <-
        egp_evaluate_counterfactual(predictions = pred.draw.j,
                                    cf.def = cf,
                                    name = "counterfactual",
                                    group.eval = group.chunks$from[k]:group.chunks$to[k],
                                    pred.var = "forestloss",
                                    draw.chunk = NULL,
                                    agg.size = 1e6,
                                    parallel = n.threads,
                                    progress = TRUE)

    }

    eval.cf <- rbindlist(eval.cf.l)

    rm(eval.cf.l)
    gc()

    eval.mar.j[[j]] <-
      egp_marginal(factual = eval.fac,
                   counterfactual = eval.cf,
                   marginal.name = "marginal")

    rm(eval.fac, eval.cf, pred.draw.j)
    gc()

    b <- Sys.time()
    print(b-a)

  }

  eval.mar.i[[i]] <- rbindlist(eval.mar.j)

  rm(eval.mar.j, pred.draw)
  gc()

}

eval.mar <- rbindlist(eval.mar.i)

rm(eval.mar.i)
gc()

paste0("Saving marginal as ", file.mar, " …") |>
message()

saveRDS(eval.mar, file.mar)
