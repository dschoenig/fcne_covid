args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)
library(kohonen)
library(mvnfast)
source("utilities.R")

## Paths
path.data.proc <- "../data/processed/"
path.som <- "../models/som/"
path.gam <- "../models/gam/"


model.reg <- tolower(as.character(args[1]))
model.id <- as.integer(args[2])
model.resp <- tolower(as.character(args[3]))
if(length(args) < 4) {
  n.threads <- c(2,1)
} else {
  n.threads <- as.integer(args[4])
}



# model.reg <- "amz"
# model.id <- 1
# model.resp <- "dis"
# n.threads <- c(2,1)
# # n.threads <- 4


if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
model.name <- paste0(model.reg, ".test.m", model.id, ".", model.resp)

k.reg <- list(amz = c(loc.bl = 1000,
                      t.bl = 6,
                      loc.itpa = 150,
                      loc.ov = 25,
                      som = 250))
# Increase number of maximum knots 5-fold (default: 2000)
max.knots.reg <- list(amz = c(loc.bl = 2e4,
                              t.bl = NULL,
                              loc.itpa = 2e4,
                              loc.ov = 2e4,
                              som = 2500))
# max.knots.reg <- list(cam = c(k.reg$cam[1:3] * 10, som = 10000),
#                       amz = c(k.reg$amz[1:3] * 10, som = 10000))

# Fitting parameters
conv.eps <- 1e-7 # Default is 1e-7
max.discrete.bins <- 1e5 # Default for bivariate smooths is effectively 1e4

## FIT MODELS ##################################################################

form.resp <-
  switch(model.resp,
         "def" = deforestation ~ .,
         "deg" = degradation ~ .,
         "dis" = disturbance ~ .)
var.resp <-
  switch(model.resp,
         "def" = "deforestation",
         "deg" = "degradation",
         "dis" = "disturbance")

vars.mod <-
  c(var.resp,
    "year", "pandemic",
    "it_type", "pa_type", "overlap",
    "ed_east", "ed_north", "adm0",
    "som_x", "som_y")

data.proc <- readRDS(file.data.proc)
data.mod <- data.proc[, ..vars.mod]
rm(data.proc)

data.mod[,year := factor(year)]


# Set up dummy variables

y.seq <- as.character(2017:2022)
for(i in seq_along(y.seq)) {
  y.it <- paste0("it_type_", y.seq[i])
  y.pa <- paste0("pa_type_", y.seq[i])
  y.ov <- paste0("overlap_", y.seq[i])
  it.lev <- levels(data.mod$it_type)
  pa.lev <- levels(data.mod$pa_type)
  ov.lev <- levels(data.mod$overlap)
  data.mod[,
           `:=`(
                it.col = factor(fifelse(year == y.seq[i], as.character(it_type), "none"),
                                levels = it.lev, ordered = TRUE),
                pa.col = factor(fifelse(year == y.seq[i], as.character(pa_type), "none"),
                                levels = pa.lev, ordered = TRUE),
                ov.col = factor(fifelse(year == y.seq[i], as.character(overlap), "none"),
                                levels = ov.lev, ordered = TRUE)),
             env = list(it.col = y.it, pa.col = y.pa, ov.col = y.ov)]
}


k.def <- k.reg[[model.reg]]
max.knots.def <- max.knots.reg[[model.reg]]

# # FOR TESTING ONLY:
# k.def = k.def / 10
# max.knots.def = max.knots.def / 10
# set.seed(1234)
# data.mod <- data.mod[sample(1:nrow(data.mod), 1e5)]
# max.discrete.bins <- 1e4

k.def <- as.list(k.def)
max.knots.def <- as.list(max.knots.def)

print(k.def)
print(max.knots.def)

message(paste0("Fitting model `", model.name, "` …"))

a <- Sys.time()

if(model.id == 1) {
  # No SOM variation
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 2) {
  # SOM by year
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 3) {
  # Like 2, with overlapping areas
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2017, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2018, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2019, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2020, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2021, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2022, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 4) {
  # Like 2, with adm0 terms
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            # Tenure effects, discontinuous variation between countries
            s(year, adm0, bs = "re") +
            s(year, adm0, it_type, bs = "re") +
            s(year, adm0, pa_type, bs = "re") +
            s(year, adm0, it_type, pa_type, bs = "re") +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}



if(model.id == 5) {
  # Full model
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2017, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2018, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2019, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2020, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2021, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = overlap_2022, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            # Tenure effects, discontinuous variation between countries
            s(year, adm0, bs = "re") +
            s(year, adm0, it_type, bs = "re") +
            s(year, adm0, pa_type, bs = "re") +
            s(year, adm0, it_type, pa_type, bs = "re") +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 6) {
  # Like 2 but with ordered year
  data.mod[, year := factor(year, ordered = TRUE)]
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              k = k.def$loc.bl*2,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              k = k.def$som*2,
              xt = list(max.knots = max.knots.def$som)) +
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 7) {
  # Like 2 but with ordered year, SOM only by pandemic
  data.mod[, year := factor(year, ordered = TRUE)]
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              k = k.def$loc.bl*2,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2017, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2018, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2019, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2020, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2021, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = it_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = pa_type_2022, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            # Covariates
            s(som_x, som_y, bs = 'gp',
              k = k.def$som*2,
              xt = list(max.knots = max.knots.def$som)) +
            s(som_x, som_y, bs = 'gp',
              by = year, k = k.def$som,
              xt = list(max.knots = max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


if(model.id == 8) {
  data.mod[, year := as.numeric(as.character(year))]
  # Oldie model
  mod.form <-
    formula( ~
            # Tenure effects, continuous variation over geographic location
            te(ed_east, ed_north, year,
               d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
               xt = list(list(max.knots = max.knots.def$loc.bl),
                         list(max.knots = max.knots.def$t.bl))) +
            te(ed_east, ed_north, year, by = it_type,
               d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
               xt = list(list(max.knots = max.knots.def$loc.itpa),
                         list(max.knots = max.knots.def$t.bl))) +
            te(ed_east, ed_north, year, by = pa_type,
               d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
               xt = list(list(max.knots = max.knots.def$loc.itpa),
                         list(max.knots = max.knots.def$t.bl))) +
            s(som_x, som_y,
              k = k.def$som*2,
              xt = list(max.knots.def$som)) + 
            s(som_x, som_y, by = pandemic,
              k = k.def$som,
              xt = list(max.knots.def$som))) |>
      update(as.formula(paste0(var.resp, "~.")))
  model <-
    bam(mod.form,
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps))
}


model
summary(model, re.test = FALSE)
k.check(model)
AIC(model)

print("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))

data.mod[, fitted := fitted(model)]
data.mod[order(year), .(mean(as.numeric(disturbance)), mean(fitted)), by = .(year)]
data.mod[, .(mean(as.numeric(disturbance)), mean(fitted)), by = .(adm0)]
data.mod[, .(mean(as.numeric(disturbance)), mean(fitted)), by = .(year, adm0)]

