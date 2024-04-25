args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))


draws.max <- 1000
draws.load.chunk <- 100
draws.eval.chunk <- 100

# n.threads <- 1
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
path.mar <- paste0(path.base, "models/marginal/", region, "/")
if(!dir.exists(path.mar))
  dir.create(path.mar, recursive = TRUE)

file.data.fac <- paste0(path.data.proc, region, ".data.proc.rds")
file.data.cf <- paste0(path.data.proc, region, ".data.cf1.rds")
path.arrow.fac <- paste0(path.pred, region, "/fac/")
path.arrow.cf <- paste0(path.pred, region, "/cf1/")
file.mar <- paste0(path.mar, region, ".adm.rds")


data.fac <-
  readRDS(file.data.fac) |>
  _[year >= 2020]
data.fac[, type := factor("factual", levels = c("factual", "counterfactual"))]
data.cf <- readRDS(file.data.cf)
data.cf[, type := factor("counterfactual", levels = c("factual", "counterfactual"))]

pred.ds.fac <- open_dataset(path.arrow.fac, format = "arrow")
pred.ds.cf <- open_dataset(path.arrow.cf, format = "arrow")

draw.chunks.load <- chunk_seq(1, draws.max, draws.load.chunk)

group.by.fac <- list("type",
                     "adm0",
                     "year",
                     c("year", "adm0"))

groups.fac.l <- list()
for(i in seq_along(group.by.fac)){
  groups.fac.l[[i]] <-
    .ids_by_group(data.fac,
                  id.var = "id",
                  group.vars = group.by.fac[[i]],
                  add.label = FALSE)
}
groups.fac <- rbindlist(groups.fac.l, fill = TRUE)
groups.fac[, group.id := 1:nrow(groups.fac)]
setorder(groups.fac, group.id)
setcolorder(groups.fac, c("group.id", unique(unlist(group.by.fac))))
groups.fac[, type := rep(na.omit(unique(type)), .N)]



group.by.cf <- list("type", "adm0")

groups.cf.l <- list()
for(i in seq_along(group.by.cf)){
  groups.cf.l[[i]] <-
    .ids_by_group(data.cf,
                  id.var = "cf.id",
                  group.vars = group.by.cf[[i]],
                  add.label = FALSE)
}
groups.cf <- rbindlist(groups.cf.l, fill = TRUE)
groups.cf[, group.id := 1:nrow(groups.cf)]
setorder(groups.cf, group.id)
setcolorder(groups.cf, c("group.id", unique(unlist(group.by.cf))))
groups.cf[, type := rep(na.omit(unique(type)), .N)]




eval.agg.fac.i <- list()
eval.agg.cf.i <- list()

for(i in seq_along(draw.chunks.load$from)) {

  a <- Sys.time()

  paste0("Loading predictions ", draw.chunks.load$from[i],
         " to ", draw.chunks.load$to[i],
         " …") |>
  message()

  pred.draw.fac <-
    pred.ds.fac |>
    filter(.draw >= draw.chunks.load$from[i] & .draw <= draw.chunks.load$to[i]) |>
    select(.draw, id, forestloss) |>
    collect() |>
    merge(data.fac[, .(id, year, adm0)], by = "id", all.x = FALSE)

  pred.draw.cf <-
    pred.ds.cf |>
    filter(.draw >= draw.chunks.load$from[i] & .draw <= draw.chunks.load$to[i]) |>
    select(.draw, cf.id, forestloss) |>
    collect() |>
    merge(data.cf[, .(cf.id, adm0)], by = "cf.id")

  draw.chunks.eval <-
    chunk_seq(draw.chunks.load$from[i],
              draw.chunks.load$to[i],
              draws.eval.chunk)

  eval.agg.fac.j <- list()
  eval.agg.cf.j <- list()

  b <- Sys.time()
  print(b-a)

  for(j in seq_along(draw.chunks.eval$from)) {

    a <- Sys.time()

    paste0("Evaluating draws ", draw.chunks.eval$from[j],
           " to ", draw.chunks.eval$to[j],
           " …") |>
    message()

    pred.draw.fac.j <- pred.draw.fac[.draw %in% draw.chunks.eval$from[j]:draw.chunks.eval$to[j]]
    pred.draw.cf.j <- pred.draw.cf[.draw %in% draw.chunks.eval$from[j]:draw.chunks.eval$to[j]]

    message("  Aggregating predictions for factual …")

    pred.draw.fac.agg.j <-
      .aggregate_variables(pred.draw.fac.j,
                         ids = groups.fac$id,
                         id.var = "id",
                         pred.var = "forestloss",
                         agg.name = "forestloss",
                         agg.size = 1e6,
                         parallel = n.threads)

    eval.agg.fac.j[[j]] <-
      merge(pred.draw.fac.agg.j, groups.fac[, -"id"],
            all.x = TRUE, all.y = FALSE, by = "group.id")
    
    message("  Aggregating predictions for counterfactual …")

    pred.draw.cf.agg.j <-
      .aggregate_variables(pred.draw.cf.j,
                         ids = groups.cf$cf.id,
                         id.var = "cf.id",
                         pred.var = "forestloss",
                         agg.name = "forestloss",
                         agg.size = 1e6,
                         parallel = n.threads)

    eval.agg.cf.j[[j]] <-
      merge(pred.draw.cf.agg.j, groups.cf[, -"cf.id"],
            all.x = TRUE, all.y = FALSE, by = "group.id")

  }

  eval.agg.fac.i[[i]] <- rbindlist(eval.agg.fac.j)
  eval.agg.cf.i[[i]] <- rbindlist(eval.agg.cf.j)

  b <- Sys.time()
  print(b-a)

  rm(eval.agg.fac.j, eval.agg.cf.j, pred.draw.fac, pred.draw.cf)
  gc()
}

eval.agg.fac <- rbindlist(eval.agg.fac.i)
setorder(eval.agg.fac, group.id, .draw)
# eval.agg.fac[is.na(adm0), adm0 := "AMZ"]
eval.agg.cf <- rbindlist(eval.agg.cf.i)
setorder(eval.agg.cf, group.id, .draw)
# eval.agg.c[is.na(adm0), adm0 := "AMZ"]

eval.mar <-
  merge(eval.agg.fac[, .(.draw, adm0, year, factual = forestloss)],
        eval.agg.cf[, .(.draw, adm0, counterfactual = forestloss)],
        by = c("adm0", ".draw"))
setcolorder(eval.mar, c("adm0", "year", ".draw")) 
eval.mar[, marginal := factual - counterfactual]


rm(eval.agg.fac, eval.agg.fac.i, eval.agg.cf, eval.agg.cf.i)
gc()

paste0("Saving marginal as ", file.mar, " …") |>
message()

saveRDS(eval.mar, file.mar)
