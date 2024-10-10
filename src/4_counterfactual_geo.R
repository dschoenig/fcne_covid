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

# Establish geographic range for comparisons (using entire study region)
pts.bb <-
  st_multipoint(x = as.matrix(data[, .(ed_east, ed_north)]), dim = "XY") |>
  st_minimum_bounding_circle() |>
  st_bbox()
geo.range <- pts.bb[["xmax"]] - pts.bb[["xmin"]]
rm(pts.bb)
silence <- gc()

var.sel <- c(id.var, "year",
             "it_type", "pa_type",
             "som_bmu", "hex",
             "ed_east", "ed_north",
             "ea_east", "ea_north")

data.cf <- data[, ..var.sel]
rm(data)
silence <- gc()
data.cf[, year := factor(as.character(year))]

som.fit <- readRDS(file.som)


comp.by <- c("year")
if(area_type == "it") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list("hex", c("year", "hex"))
                   # c("ea_east.bin", "ea_north.bin", "it_type"),
                   # c("year", "ea_east.bin", "ea_north.bin", "it_type"))
}
if(area_type == "pa") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[pa_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list("hex", c("year", "hex"))
                   # c("ea_east.bin", "ea_north.bin", "pa_type"),
                   # c("year", "ea_east.bin", "ea_north.bin", "pa_type"))
}
if(area_type == "itpa") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none" | pa_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list("hex", c("year", "hex"))
}


paste0("No. of data: ", nrow(data.cf)) |>
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
