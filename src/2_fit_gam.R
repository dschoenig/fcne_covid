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
# n.threads <- c(2,1)


if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
model.name <- paste0(model.reg, ".m", model.id, ".", model.resp)

k.reg <- list(amz = c(loc.bl = 1000,
                      loc.itpa = 150,
                      loc.ov = 25,
                      mort = 25,
                      som = 250))
# Increase number of maximum knots 5-fold (default: 2000)
max.knots.reg <- list(amz = c(loc.bl = 2e4,
                              loc.itpa = 2e4,
                              loc.ov = 2e4,
                              mort = NULL,
                              som = 2e4))
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
    "it_type", "pa_type", "overlap",
    "ed_east", "ed_north", "adm0",
    "som_x", "som_y",
    "elevation", "slope", "sx",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")

data.proc <- readRDS(file.data.proc)
data.mod <- data.proc[, ..vars.mod]
setDT(data.mod)
rm(data.proc)

data.mod[,year := factor(year)]


# Set up dummy variables


dict.dummy <-
  data.table(it_type = c("none",
                         "recognized", "not_recognized",
                         "none", "none",
                         "recognized", "recognized", 
                         "not_recognized", "not_recognized"), 
             pa_type = c("none",
                         "none", "none",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use"),
             it_rec = c(0, 1, 0, 0, 0, 1, 1, 0, 0),
             it_nrec = c(0, 0, 1, 0, 0, 0, 0, 1, 1),
             pa_ind = c(0, 0, 0, 1, 0, 1, 0, 1, 0),
             pa_dir = c(0, 0, 0, 0, 1, 0, 1, 0, 1),
             ov_rec_ind = c(0, 0, 0, 0, 0, 1, 0, 0, 0), 
             ov_rec_dir = c(0, 0, 0, 0, 0, 0, 1, 0, 0), 
             ov_nrec_ind = c(0, 0, 0, 0, 0, 0, 0, 1, 0), 
             ov_nrec_dir = c(0, 0, 0, 0, 0, 0, 0, 0, 1))

dict.dummy[,
           `:=`(it_type = factor(it_type,
                                 levels = levels(data.mod$it_type),
                                 ordered = TRUE),
                pa_type = factor(pa_type,
                                 levels = levels(data.mod$pa_type),
                                 ordered = TRUE))]

d.vars <- c("it_rec", "it_nrec",
            "pa_dir", "pa_ind",
           "ov_rec_ind", "ov_rec_dir",
           "ov_nrec_ind", "ov_nrec_dir")

y.seq <- as.character(2017:2022)

y.dict.dummy.l <- list()

for(i in seq_along(y.seq)) {
  d.vars.new <- paste0("y", y.seq[i], ".", d.vars)
  y.dict.dummy.l[[i]] <- copy(dict.dummy)
  y.dict.dummy.l[[i]][, year := y.seq[i]]
  setnames(y.dict.dummy.l[[i]], d.vars.old, d.vars.new)
}

y.dict.dummy <- rbindlist(y.dict.dummy.l, fill = TRUE)
d.vars.all <- names(y.dict.dummy[, -c("year", "it_type", "pa_type")])

y.dict.dummy[,
             (d.vars.all) :=
             lapply(.SD, \(x) ifelse(is.na(x), 0, x)),
             .SDcols = d.vars.all]
y.dict.dummy[, year := factor(year)]

data.mod <-
  merge(data.mod, y.dict.dummy,
        by = c("year", "it_type", "pa_type"),
        sort = FALSE)


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
  # One-year model
  mod.form <-
    formula(~
            # Tenure effects, continuous variation over geographic location
            s(ed_east, ed_north, bs = 'gp',
              by = year, k = k.def$loc.bl,
              xt = list(max.knots = max.knots.def$loc.bl)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2017.ov_nrec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2018.ov_nrec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2019.ov_nrec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2020.ov_nrec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2021.ov_nrec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.it_rec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.it_nrec, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.pa_ind, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.pa_dir, k = k.def$loc.itpa,
              xt = list(max.knots = max.knots.def$loc.itpa)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.ov_rec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.ov_rec_dir, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.ov_nrec_ind, k = k.def$loc.ov,
              xt = list(max.knots = max.knots.def$loc.ov)) +
            s(ed_east, ed_north, bs = 'gp',
              by = y2022.ov_nrec_dir, k = k.def$loc.ov,
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

model
summary(model, re.test = FALSE)
k.check(model)
AIC(model)

print("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))

