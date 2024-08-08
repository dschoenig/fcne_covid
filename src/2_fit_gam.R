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
if(length(args) < 3) {
  n.threads <- c(2,1)
} else {
  n.threads <- as.integer(args[3])
}

# model.reg <- "amz"
# model.id <- 1
# n.threads <- c(2,1)


if(!dir.exists(path.gam))
  dir.create(path.gam, recursive = TRUE)

file.data.proc <- paste0(path.data.proc, model.reg, ".data.proc.rds")
model.name <- paste0(model.reg, ".m", model.id)

k.reg <- list(amz = c(t.bl = 6,
                      loc.bl = 500,
                      loc.foro = 100,
                      loc.itpa = 100,
                      loc.ov = 50,
                      mort = 25,
                      som.np = 500,
                      som.p = 500))
# Increase number of maximum knots 5-fold (default: 2000)
max.knots.reg <- list(amz = c(t.bl = NULL,
                              loc.bl = 2e4,
                              loc.foro = 2e4,
                              loc.itpa = 2e4,
                              loc.ov = 2e4,
                              mort = NULL,
                              som.np = 1e4,
                              som.p = 1e4))
# max.knots.reg <- list(cam = c(k.reg$cam[1:3] * 10, som = 10000),
#                       amz = c(k.reg$amz[1:3] * 10, som = 10000))

# Fitting parameters
conv.eps <- 1e-7 # Default is 1e-7
max.discrete.bins <- 1e5 # Default for bivariate smooths is effectively 1e4

## FIT MODELS ##################################################################

data.mod <-
  readRDS(file.data.proc) |>
  _[,
    .(id,
      forestloss,
      year,
      for_type,
      it_type,
      pa_type,
      overlap,
      pandemic,
      mort, mort.id, mortlag1, mortlag1.id,
      ed_east, ed_north,
      som_x, som_y)]
# rm(data.proc)

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
# Main model
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.foro),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = it_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = pa_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = overlap,
           d = c(2,1), k = c(k.def$loc.ov, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.ov),
                     list(max.knots = max.knots.def$t.bl))) +
        s(som_x, som_y,
          k = k.def$som.np,
          xt = list(max.knots.def$som.np)) + 
        s(som_x, som_y, by = pandemic,
          k = k.def$som.p,
          xt = list(max.knots.def$som.p)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 2) {
# Necessary to model overlapping areas explicitely?
# Does not converge
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.foro),
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
          k = k.def$som.np,
          xt = list(max.knots.def$som.np)) + 
        s(som_x, som_y, by = pandemic,
          k = k.def$som.p,
          xt = list(max.knots.def$som.p)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}


if(model.id == 3) {
# Like 1, but no mortality effect
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.foro),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = it_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = pa_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = overlap,
           d = c(2,1), k = c(k.def$loc.ov, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.ov),
                     list(max.knots = max.knots.def$t.bl))) +
        s(mort,
          by = mort.id,
          k = k.def$mort,
          xt = list(max.knots = max.knots.def$mort)) +
        s(mortlag1,
          by = mortlag1.id,
          k = k.def$mort,
          xt = list(max.knots = max.knots.def$mort)) +
        s(som_x, som_y,
          k = k.def$som.np,
          xt = list(max.knots.def$som.np)) + 
        s(som_x, som_y, by = pandemic,
          k = k.def$som.p,
          xt = list(max.knots.def$som.p)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}


if(model.id == 4) {
# Like 1, but no pandemic effect on covariates
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.foro),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = it_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = pa_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = overlap,
           d = c(2,1), k = c(k.def$loc.ov, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.ov),
                     list(max.knots = max.knots.def$t.bl))) +
        s(som_x, som_y,
          k = k.def$som.np,
          xt = list(max.knots.def$som.np)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}



if(model.id == 5) {
# Like 1, but with gp, tp smooths
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           bs = c("gp", "tp"),
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           bs = c("gp", "tp"),
           xt = list(list(max.knots = max.knots.def$loc.foro),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = it_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           bs = c("gp", "tp"),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = pa_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           bs = c("gp", "tp"),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = overlap,
           d = c(2,1), k = c(k.def$loc.ov, k.def$t.bl),
           bs = c("gp", "tp"),
           xt = list(list(max.knots = max.knots.def$loc.ov),
                     list(max.knots = max.knots.def$t.bl))) +
        s(som_x, som_y,
          k = k.def$som.np,
          bs = "gp",
          xt = list(max.knots.def$som.np)) + 
        s(som_x, som_y, by = pandemic,
          k = k.def$som.p,
          bs = "gp",
          xt = list(max.knots.def$som.p)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

if(model.id == 6) {
# Like 1, but with gp smooths
  model <-
    bam(forestloss ~
        te(ed_east, ed_north, year,
           bs = c("gp", "gp"),
           d = c(2,1), k = c(k.def$loc.bl, k.def$t.bl),
           xt = list(list(max.knots = max.knots.def$loc.bl),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = for_type,
           d = c(2,1), k = c(k.def$loc.foro, k.def$t.bl),
           bs = c("gp", "gp"),
           xt = list(list(max.knots = max.knots.def$loc.foro),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = it_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           bs = c("gp", "gp"),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = pa_type,
           d = c(2,1), k = c(k.def$loc.itpa, k.def$t.bl),
           bs = c("gp", "gp"),
           xt = list(list(max.knots = max.knots.def$loc.itpa),
                     list(max.knots = max.knots.def$t.bl))) +
        te(ed_east, ed_north, year, by = overlap,
           d = c(2,1), k = c(k.def$loc.ov, k.def$t.bl),
           bs = c("gp", "gp"),
           xt = list(list(max.knots = max.knots.def$loc.ov),
                     list(max.knots = max.knots.def$t.bl))) +
        s(som_x, som_y,
          k = k.def$som.np,
          bs = "gp",
          xt = list(max.knots.def$som.np)) + 
        s(som_x, som_y, by = pandemic,
          k = k.def$som.p,
          bs = "gp",
          xt = list(max.knots.def$som.p)),
        family = binomial(link = "cloglog"),
        data = data.mod,
        select = TRUE,
        discrete = max.discrete.bins,
        nthreads = n.threads,
        control = gam.control(trace = TRUE, epsilon = conv.eps)
        )
}

model
summary(model, re.test = FALSE)
k.check(model)
AIC(model)

print("Saving fitted model …")
saveRDS(model, paste0(path.gam, model.name, ".rds"))

