args <- commandArgs(trailingOnly = TRUE)

library(data.table)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
pred_type <- tolower(as.character(args[3]))

# n.threads <- 4
# region <- "amz"
# pred_type <- "cf1"

setDTthreads(n.threads)


path.base <- "../"
path.som <- "../models/som/"
path.data.proc <- paste0(path.base, "data/processed/")
path.cf <- paste0(path.base, "models/cf/", region, "/")
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
file.out <- paste0(path.cf, region, ".ten.", pred_type, ".rds")


if(pred_type == "fac") {
  id.var <- "id"
  data <-
    readRDS(file.data) |>
    _[year >= 2020]
} else {
  id.var <- "cf.id"
  data <- readRDS(file.data)
  if(pred_type %in% c("cf1", "cf4")) {
    data[, year := year.fac]
  }
}

var.sel <- c(id.var, "year",
             "it_type", "pa_type",
             "som_bmu", "ed_east", "ed_north")

data.cf <- data[, ..var.sel]
rm(data)
silence <- gc()
data.cf[, year := factor(as.character(year))]

som.fit <- readRDS(file.som)

cf.ids <- data.cf[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
fac.ids <- data.cf[it_type != "none" | pa_type != "none", id.col, env = list(id.col = id.var)]  
comp.by <- "year"
group.by <- list("it_type",
                 c("year", "it_type"),
                 "pa_type",
                 c("year", "pa_type"))



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

# data.cf2 <- data.cf[sample(1:nrow(data.cf), 1e5)]
# cf.ids <- data.cf2[it_type == "none" & pa_type == "none", id.col, env = list(id.col = id.var)]
# fac.ids <- data.cf2[it_type != "none" | pa_type != "none", id.col, env = list(id.col = id.var)]  
# comp.by <- "year"
# group.by <- list("it_type",
#                  c("year", "it_type"),
#                  "pa_type",
#                  c("year", "pa_type"))

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
