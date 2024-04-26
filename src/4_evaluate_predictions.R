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
pred_type <- tolower(as.character(args[2]))
task_id <- as.integer(args[3])
task_count <- as.integer(args[4])

# region <- "amz"
# pred_type <- "fac"
# task_id <- 1
# task_count <- 1000

file.gam <- paste0(path.gam, region, ".m1.rds")
file.post <- paste0(path.gam, region,  ".m1.post.rds")

col.sel <-
  c("year", "for_type", "it_type", "pa_type", "overlap",
    "pandemic", "mort", "ed_east", "ed_north",
    "som_x", "som_y")

if(pred_type == "fac") {
  file.data <- paste0(path.data.proc, region, ".data.proc.rds")
  col.sel <- c("id", col.sel)
  id.var <- "id"
}
if(pred_type %in% paste0("cf", 1:4)) {
  file.data <- paste0(path.data.proc, region, ".data.", pred_type, ".rds")
  col.sel <- c("cf.type", "cf.id", col.sel)
  id.var <- "cf.id"
}


path.out <- paste0(path.pred, region, "/", pred_type, "/")
if(!dir.exists(path.out))
  dir.create(path.out, recursive = TRUE)
file.out <-
  paste0(path.out, region, ".", stri_pad_left(task_id, 3, 0) , ".arrow")

## EVALUATE LINEAR PREDICTOR BASED ON DRAWS FROM MODEL POSTERIOR ###############

# Load model, posterior draws, and data
data <-
  readRDS(file.data) |>
  _[, ..col.sel]
gam <- readRDS(file.gam)
post <- readRDS(file.post)



# Construct chunk overview
row.chunks <- chunk_seq(1, nrow(data), ceiling(nrow(data) / task_count))

# Subset data
data.pred <- data[row.chunks$from[task_id]:row.chunks$to[task_id],]
rm(data)
silence <- gc()


message(paste0("Generating predictions for model `", region, ".m1`, counterfactual `", pred_type,
        "`, using draws from the posterior distribution.\n"))
message(paste0("Processing rows ", row.chunks$from[task_id],
        " to ", row.chunks$to[task_id],
        " (chunk ", task_id, " / ", task_count, "):\n"))


# Evaluate posterior, calculate linear predictor
a <- Sys.time()

pred <-
  egp_posterior_predict(model = gam,
                        posterior = post,
                        data = data.pred,
                        id.var = id.var,
                        type = "response",
                        epred = FALSE,
                        pred.name = "forestloss",
                        predict.chunk = 500,
                        post.chunk = 200,
                        progress = TRUE
                        )

b <- Sys.time()
b-a

silence <- gc()


# Prepare export

pred[, forestloss := as.logical(forestloss)]
pred[, .draw.chunk := factor(ceiling(.draw/100), levels = as.character(1:10))]
setcolorder(pred, c(".draw.chunk", ".draw", id.var, "forestloss"))

setorderv(pred, c(".draw.chunk", ".draw", id.var))

message(paste0("Writing output to `", file.out, "` …"))
write_feather(pred, file.out, version = 2, chunk_size = 1e7, compression = "uncompressed")
