args <- commandArgs(trailingOnly = TRUE)

library(mgcv)
library(mvnfast)
library(posterior)

source("utilities.R")

region <- tolower(as.character(args[1]))
model.resp <- tolower(as.character(args[2]))
model.id <- as.integer(args[3])
n.threads <- ifelse(length(args) < 3, 1, as.integer(args[4]))

# region <- "amz"
# model.resp <- "dis"
# model.id <- 1
# n.threads <- 4

path.gam <- "../models/gam/"

seed <- 18980605


## SIMULATION FROM MULTIVARIATE NORMAL APPROXIMATION OF MODEL POSTERIOR ########

file.model <- paste0(path.gam, region, ".test.m", model.id, ".", model.resp, ".rds")
file.post <- paste0(path.gam, region, ".test.m",  model.id, ".", model.resp, ".post.rds")

paste0("Simulating 1000 draws from the approximate posterior distribution\n",
       "of model ", file.model, " …") |>
message()

# Load model
model <- readRDS(file.model)


# Posterior draws
set.seed(seed)
post <- 
  egp_posterior_draw(model,
                     n = 1000,
                     unconditional = TRUE,
                     package = "mvnfast",
                     parallel = 4)

saveRDS(post, file.post)

