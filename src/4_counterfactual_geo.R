args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
pred_type <- tolower(as.character(args[3]))
area_type <- tolower(as.character(args[4]))

# n.threads <- 4
# region <- "amz"
# pred_type <- "fac"
# area_type <- "it"


setDTthreads(n.threads)

map.res <- 2.5e4

path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/egp_cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

if(pred_type == "fac") {
  id.var <- "id"
  file.data <- paste0(path.data.proc, region, ".data.proc.rds")
} else {
  id.var <- "cf.id"
  file.data <- paste0(path.data.proc, region, ".data.", pred_type, ".rds")
}

file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".geo.", pred_type, ".", area_type, ".rds")


if(pred_type == "fac") {
  id.var <- "id"
  data <-
    readRDS(file.data) |>
    _[year >= 2020]
} else {
  id.var <- "cf.id"
  data <- readRDS(file.data)
  if(pred_type %in% c("cf1")) {
    data[, year := year.fac]
  }
}

var.sel <- c(id.var, "year", "for_type",
             "it_type", "pa_type",
             "som_bmu",
             "ed_east", "ed_north",
             "ea_east", "ea_north")

data.cf <- data[, ..var.sel]
rm(data)
silence <- gc()
data.cf[, year := factor(as.character(year))]


data.cf <- readRDS(file.data)[, ..var.sel]

som.fit <- readRDS(file.som)


map.anchor <- c(ea_east = floor(min(data.cf$ea_east / map.res)) * map.res,
                ea_north = floor(min(data.cf$ea_north / map.res)) * map.res)

data.cf <-
  bin_cols(data.cf,
           columns = c("ea_east", "ea_north"), bin.res = rep(map.res, 2),
           bin.min = map.anchor, append = TRUE)


comp.by <- c("for_type", "year")
if(area_type == "it") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none", id.col, env = list(id.col = id.var)]  
}
if(area_type == "pa") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[pa_type != "none", id.col, env = list(id.col = id.var)]  
}
if(area_type == "itpa") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none" | pa_type != "none", id.col, env = list(id.col = id.var)]  
}

group.by <- list(c("ea_east.bin", "ea_north.bin"))


paste0("No. of data: ", nrow(data)) |>
message()

paste0("No. of factual observations: ", length(fac.ids)) |>
message()

paste0("No. of counterfactual observations: ", length(cf.ids)) |>
message()

paste0("Compared by: ", paste(comp.by, collapse = ", ")) |>
message()

message("Grouped by: ")
print(group.by)

paste0("Counterfactual will be saved as ", file.out) |>
message()

message("Defining counterfactual …")

source("utilities.R")
a <- Sys.time()
cf.def <-
  egp_define_counterfactual(data = data.cf,
                            som = som.fit,
                            cf.ids = cf.ids,
                            fac.ids = fac.ids,
                            compare.by = comp.by,
                            group.by = group.by,
                            som.var = "som_bmu",
                            geo.vars = c("ed_east", "ed_north"),
                            geo.kernel = "matern32",
                            geo.range = NULL,
                            id.var = id.var,
                            group.name = "group.id",
                            unit.name = "cf.unit",
                            assign.name = "assigned",
                            assign.cat = c("counterfactual", "factual"),
                            n.min = 1,
                            nb.strategy = "sequential",
                            deg.max = NULL,
                            agg.size = 1e6,
                            progress = TRUE)
b <- Sys.time()

print(b-a)

print(cf.def)

message("Saving output …")

saveRDS(cf.def, file.out)
gc()
