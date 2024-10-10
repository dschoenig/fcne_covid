
args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
mort_type <- tolower(as.character(args[3]))
area_type <- tolower(as.character(args[4]))


# n.threads <- 4
# region <- "amz"
# mort_type <- "mort"
# area_type <- "itpa"


setDTthreads(n.threads)


path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/egp_cf/", region, "/")
if(!dir.exists(path.cf))
  dir.create(path.cf, recursive = TRUE)

id.var <- "id"
file.data <- paste0(path.data.proc, region, ".data.proc.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.out <- paste0(path.cf, region, ".ten_", mort_type, ".", area_type, ".rds")


data <- readRDS(file.data)

# Establish geographic range for comparisons (using entire study region)
pts.bb <-
  st_multipoint(x = as.matrix(data[, .(ed_east, ed_north)]), dim = "XY") |>
  st_minimum_bounding_circle() |>
  st_bbox()
geo.range <- pts.bb[["xmax"]] - pts.bb[["xmin"]]
rm(pts.bb)
silence <- gc()


var.sel <- c(id.var, "year", "adm0",
             "it_type", "pa_type",
             "mort", "mortlag1", "pandemic",
             "som_bmu", "ed_east", "ed_north")

data.cf <- data[year >= 2020, ..var.sel]
rm(data)
silence <- gc()
data.cf[, year := factor(as.character(year))]

som.fit <- readRDS(file.som)

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

mort.col <- paste0(mort_type, ".class")
comp.by <- c("year", mort.col)

if(area_type == "it") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list(
                   c(mort.col, "it_type"),
                   c(mort.col, "year", "it_type"))
}
if(area_type == "pa") {
  cf.ids <- data.cf[pa_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[pa_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list(
                   c(mort.col, "pa_type"),
                   c(mort.col, "year", "pa_type"))
}
if(area_type == "itpa") {
  cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
  fac.ids <- data.cf[it_type != "none" | pa_type != "none", id.col, env = list(id.col = id.var)]  
  group.by <- list(
                   c(mort.col, "it_type", "pa_type"),
                   c(mort.col, "year", "it_type", "pa_type"))
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
