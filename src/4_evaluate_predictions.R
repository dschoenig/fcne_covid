args <- commandArgs(trailingOnly = TRUE)
library(mgcv)
library(data.table)
library(posterior)
library(stringi)
library(arrow)

source("utilities.R")

path.base <- "../"
path.gam <- paste0(path.base, "models/gam/")
path.data.proc <- paste0(path.base, "data/processed/")
path.pred <- paste0(path.base, "models/gam/pred/")

region <- tolower(as.character(args[1]))
model_resp <- tolower(as.character(args[2]))
pred_type <- tolower(as.character(args[3]))
task_id <- as.integer(args[4])
task_count <- as.integer(args[5])

# region <- "amz"
# pred_type <- "cf1"
# model_resp <- "def"
# task_id <- 1
# task_count <- 1000

file.model <- paste0(path.gam, region, ".m1.", model_resp, ".rds")
file.post <- paste0(path.gam, region, ".m1.", model_resp, ".post.rds")


var.resp <-
  switch(model_resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

vars.pred <-
  c("year",
    "it_type", "pa_type", "overlap",
    "ed_east", "ed_north", "adm0",
    "som_x", "som_y")


if(pred_type == "fac") {
  file.data <- paste0(path.data.proc, region, ".data.proc.rds")
  id.var <- "id"
  vars.pred <- c(id.var, vars.pred)
}
if(pred_type %in% paste0("cf", 1:4)) {
  file.data <- paste0(path.data.proc, region, ".data.", pred_type, ".rds")
  id.var <- "cf.id"
  vars.pred <- c("cf.type", id.var, vars.pred)
}


path.out <- paste0(path.pred, region, "/", model_resp, "/", pred_type, "/")
if(!dir.exists(path.out))
  dir.create(path.out, recursive = TRUE)
file.out <-
  paste0(path.out, region, ".", stri_pad_left(task_id, 3, 0) , ".arrow")




## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

data.proc <- readRDS(file.data)
data.pred <- data.proc[, ..vars.pred]
rm(data.proc)

data.pred[,year := factor(year)]

y.seq <- as.character(2017:2022)
for(i in seq_along(y.seq)) {
  y.it <- paste0("it_type_", y.seq[i])
  y.pa <- paste0("pa_type_", y.seq[i])
  y.ov <- paste0("overlap_", y.seq[i])
  it.lev <- levels(data.pred$it_type)
  pa.lev <- levels(data.pred$pa_type)
  ov.lev <- levels(data.pred$overlap)
  data.pred[,
            `:=`(
                 it.col = factor(fifelse(year == y.seq[i], as.character(it_type), "none"),
                                 levels = it.lev, ordered = TRUE),
                 pa.col = factor(fifelse(year == y.seq[i], as.character(pa_type), "none"),
                                 levels = pa.lev, ordered = TRUE),
                 ov.col = factor(fifelse(year == y.seq[i], as.character(overlap), "none"),
                                 levels = ov.lev, ordered = TRUE)),
              env = list(it.col = y.it, pa.col = y.pa, ov.col = y.ov)]
}



# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(data.pred), ceiling(nrow(data.pred) / task_count))

# Subset data
data.pred <- data.pred[row.chunks$from[task_id]:row.chunks$to[task_id],]
silence <- gc()

message(paste0("Generating predictions for model `", region, ".m1`, counterfactual `", pred_type,
        "`, using draws from the posterior distribution.\n"))
message(paste0("Processing rows ", row.chunks$from[task_id],
        " to ", row.chunks$to[task_id],
        " (chunk ", task_id, " / ", task_count, "):\n"))



# Load model, posterior draws

mod <- readRDS(file.model)
post <- readRDS(file.post)


# Evaluate posterior, calculate linear predictor
a <- Sys.time()

pred <-
  egp_posterior_predict(model = mod,
                        posterior = post,
                        data = data.pred,
                        id.var = id.var,
                        type = "response",
                        epred = FALSE,
                        pred.name = var.resp,
                        predict.chunk = 500,
                        post.chunk = 200,
                        progress = TRUE
                        )

b <- Sys.time()
b-a

silence <- gc()


# Prepare export

pred[, resp.col := as.logical(resp.col), env = list(resp.col = var.resp)]
setcolorder(pred, c(".draw", id.var, var.resp))

setorderv(pred, c(".draw", id.var))

message(paste0("Writing output to `", file.out, "` …"))
write_feather(pred, file.out, version = 2, chunk_size = 1e7, compression = "uncompressed")
