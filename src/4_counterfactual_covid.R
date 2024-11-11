args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
pred_type <- tolower(as.character(args[3]))

n.threads <- 4
region <- "amz"
pred_type <- "mort"

setDTthreads(n.threads)


path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/egp_cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

file.data <- paste0(path.data.proc, region, ".data.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".covid.", pred_type, ".rds")


id.var <- "id"
var.sel <- c(id.var, "year", "adm0",
             "it_type", "pa_type", "pandemic",
             "mort", "mortlag1",
             "som_bmu", "ed_east", "ed_north")

data.cf <- readRDS(file.data)[, ..var.sel]
som.fit <- readRDS(file.som)

# Establish geographic range for comparisons (using entire study region)
pts.bb <-
  st_multipoint(x = as.matrix(data.cf[, .(ed_east, ed_north)]), dim = "XY") |>
  st_minimum_bounding_circle() |>
  st_bbox()
geo.range <- pts.bb[["xmax"]] - pts.bb[["xmin"]]
rm(pts.bb)
silence <- gc()


mort.breaks <- data.cf[, quantile(unique(mort), c(0, 0.2, 0.4, 0.6, 0.8, 1))]
mort.class.lab <- c("quint1", "quint2", "quint3", "quint4", "quint5")
cols.mort <- c("mort", "mortlag1")
cols.mort.class <- paste0(cols.mort, ".class")

data.cf[, 
        (cols.mort.class) :=
          lapply(.SD,
                 \(x) cut(x, labels = mort.class.lab, breaks = mort.breaks,
                          right = TRUE, include.lowest = TRUE)),
        .SDcols = cols.mort]
data.cf[pandemic == "no", (cols.mort.class) := NA]

# data.cf <- data.cf[sample(1:nrow(data.cf), 1e5)]

if(pred_type == "mort") {
  cf.ids <- data.cf[year <= 2019, id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[year >= 2020, id.col, env = list(id.col = id.var)]  
}
if(pred_type == "mortlag1") {
  cf.ids <- data.cf[year <= 2019, id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[year >= 2021, id.col, env = list(id.col = id.var)]  
}

mort.col <- paste0(pred_type, ".class")
comp.by <- c("it_type", "pa_type", "adm0")
group.by <- list(mort.col, c("year", mort.col))


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
