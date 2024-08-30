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
model_id <- as.integer(args[2])
model_resp <- tolower(as.character(args[3]))
pred_type <- "fac"

# region <- "amz"
# model_id <- 1
# model_resp <- "dis"
# pred_type <- "fac"


file.model <- paste0(path.gam, region, ".test.m", model_id, ".", model_resp, ".rds")
file.post <- paste0(path.gam, region, ".test.m",  model_id, ".", model_resp, ".post.rds")
file.out <- paste0(path.gam, region, ".test.m",  model_id, ".", model_resp, ".pt.rds")

var.resp <-
  switch(model_resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

vars.pred <-
  c("id", "year",
    "disturbance", "deforestation", "degradation",
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




# Load model, posterior draws

mod <- readRDS(file.model)
post <- readRDS(file.post)

data.pred[, fit.mod := fitted(mod)]


set.seed(1234)
idx.sam <- sample(data.pred$id, 1e5)
data.pred.sam <- data.pred[id %in% idx.sam]



# Evaluate posterior, calculate linear predictor
a <- Sys.time()

pred <-
  egp_posterior_predict(model = mod,
                        posterior = post,
                        data = data.pred.sam,
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

pred.mean <- pred[, .(fit.post = mean(resp.col)), by = id.var, env = list(resp.col = var.resp)]
data.test <- merge(data.pred.sam, pred.mean)

saveRDS(data.test, file.out)

data.test[order(year),
          .(mean.obs = mean(disturbance),
            mean.mod = mean(fit.mod),
            mean.post = mean(fit.post)),
          by = .(year)]
data.test[order(adm0),
          .(mean.obs = mean(disturbance),
            mean.mod = mean(fit.mod),
            mean.post = mean(fit.post)),
          by = .(adm0)]
