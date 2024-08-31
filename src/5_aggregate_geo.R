args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(arrow)
library(dplyr)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
resp_type <- tolower(as.character(args[3]))
pred_type <- as.character(args[4])


draws.max <- 1000
draws.load.chunk <- 100
draws.eval.chunk <- 10

# n.threads <- 1
# region <- "amz"
# resp_type <- "dis"
# pred_type <- "fac"
# draws.max <- 8
# draws.load.chunk <- 4
# draws.eval.chunk <- 2

setDTthreads(n.threads)
set_cpu_count(n.threads)


path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
if(!dir.exists(path.agg))
  dir.create(path.agg, recursive = TRUE)

if(pred_type == "fac") {
  file.data <- paste0(path.data.proc, region, ".data.proc.rds")
} else {
  file.data <- paste0(path.data.proc, region, ".data.", pred_type, ".rds")
}
path.arrow <- paste0(path.pred, region, "/", resp_type, "/", pred_type, "/")

file.agg <- paste0(path.agg, region, ".", resp_type, ".geo.", pred_type, ".rds")

if(pred_type == "fac") {
  id.var <- "id"
  data <-
    readRDS(file.data) |>
    _[year >= 2020]
} else {
  id.var <- "cf.id"
  data <- readRDS(file.data)
  if(pred_type %in% c("cf1", "cf4")) {
    data[, year := year.fac]
  }
}


merge.cols <- c(id.var, "year", "hex")
data <- data[, ..merge.cols]
data[, type := factor(pred_type)]


group.sel <- c("group.id", "type", "year", "hex")
group.by <- list("hex", c("year", "hex"))


groups.l <- list()
for(i in seq_along(group.by)){
  groups.l[[i]] <-
    .ids_by_group(data,
                  id.var = id.var,
                  group.vars = group.by[[i]],
                  add.label = FALSE)
}
groups <- rbindlist(groups.l, fill = TRUE)


groups[, group.id := 1:nrow(groups)]
setorder(groups, group.id)
setcolorder(groups, c("group.id", unique(unlist(group.by))))
groups[, type := rep(as.factor(pred_type), .N)]


pred.ds <- open_dataset(path.arrow, format = "arrow")

draw.chunks.load <- chunk_seq(1, draws.max, draws.load.chunk)

resp.var <-
  switch(resp_type,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

select.var <- c(".draw", id.var, resp.var)

eval.agg.i <- list()

for(i in seq_along(draw.chunks.load$from)) {

  a <- Sys.time()

  paste0("Loading predictions ", draw.chunks.load$from[i],
         " to ", draw.chunks.load$to[i],
         " …") |>
  message()

  pred.draw <-
    pred.ds |>
    filter(.draw >= draw.chunks.load$from[i] & .draw <= draw.chunks.load$to[i]) |>
    select(all_of(select.var)) |>
    collect() |>
    merge(data[, ..merge.cols],
          by = id.var, all.x = FALSE)

  pred.draw[, resp.col := as.numeric(resp.col), env = list(resp.col = resp.var)]

  draw.chunks.eval <-
    chunk_seq(draw.chunks.load$from[i],
              draw.chunks.load$to[i],
              draws.eval.chunk)

  eval.agg.j <- list()

  b <- Sys.time()
  print(b-a)

  for(j in seq_along(draw.chunks.eval$from)) {

    a <- Sys.time()

    paste0("Evaluating draws ", draw.chunks.eval$from[j],
           " to ", draw.chunks.eval$to[j],
           " …") |>
    message()

    pred.draw.j <- pred.draw[.draw %in% draw.chunks.eval$from[j]:draw.chunks.eval$to[j]]

    message("  Aggregating predictions …")

    pred.draw.agg.j <-
      .aggregate_variables(pred.draw.j,
                         ids = groups[[id.var]],
                         id.var = id.var,
                         pred.var = resp.var,
                         agg.name = resp.var,
                         agg.size = 1e6,
                         parallel = n.threads)

    eval.agg.j[[j]] <-
      merge(pred.draw.agg.j, groups[, ..group.sel],
            all.x = TRUE, all.y = FALSE, by = "group.id")

    b <- Sys.time()
    print(b-a)
  }

  eval.agg.i[[i]] <- rbindlist(eval.agg.j)


  rm(eval.agg.j, pred.draw)
  gc()
}

eval.agg <- rbindlist(eval.agg.i)
setcolorder(eval.agg, group.sel)
setorder(eval.agg, .draw, group.id)

print(eval.agg)

paste0("Saving aggregated predictions as ", file.agg, " …") |>
message()

saveRDS(eval.agg, file.agg)
