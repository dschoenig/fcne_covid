library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.data <- "../data/"
path.data.raw <- paste0(path.data, "raw/")
path.data.int <- paste0(path.data, "intermediate/")
path.data.proc <- paste0(path.data, "processed/")

region <- "amz"


file.data.raw <- paste0(path.data.raw, "forests/", region, ".1722.vars.csv")

vars <- fread(file.data.raw, 
              na.strings = "",
              key = "id")

vars[, 
     `:=`(
          driver = factor(fcase(is.na(driver), "not_identified",
                                driver == 1, "commodity",
                                driver == 2, "shifting_cultivation",
                                driver == 3, "forestry",
                                driver == 4, "wildfires",
                                driver == 5, "urbanization"),
                          levels = c("not_identified",
                                     "commodity", "shifting_cultivation",
                                     "forestry", "wildfires", "urbanization")))]

vars <- vars[, .(id, driver)]


# Add variable to existing data set

file.data.int <- paste0(path.data.int, region, ".data.int.rds")
file.data.proc <- paste0(path.data.proc, region, ".data.proc.rds")

data.int <- readRDS(file.data.int)
data.proc <- readRDS(file.data.proc)

int.merged <-
  merge(data.int, vars[, .(id, driver)],
        by = "id", all.x = TRUE, all.y = FALSE, sort = FALSE)

proc.merged <-
  merge(data.proc, vars[, .(id, driver)],
        by = "id", all.x = TRUE, all.y = FALSE, sort = FALSE)


vars.sel.int <-
  c("id", "year",
    "pandemic",
    "adm0", "adm1",
    "disturbance", "deforestation", "degradation",
    "tmf_def", "tmf_deg",
    "it", "it_type", "pa", "pa_type", "overlap",
    "it_2017", "it_2018", "it_2019",
    "it_2020", "it_2021", "it_2022",
    "it_type_2017", "it_type_2018", "it_type_2019",
    "it_type_2020", "it_type_2021", "it_type_2022",
    "pa_2017", "pa_2018", "pa_2019",
    "pa_2020", "pa_2021", "pa_2022",
    "pa_type_2017", "pa_type_2018", "pa_type_2019",
    "pa_type_2020", "pa_type_2021", "pa_type_2022",
    "elevation", "slope", "sx",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_roads", "dens_pop", "travel_time",
    "mort", "mort.id", "mortlag1", "mortlag1.id",
    "cabinet", "driver",
    "lon", "lat",
    "ed_east", "ed_north", "ea_east", "ea_north",
    "hex")

vars.sel.proc <-
  c("id", "year",
    "pandemic",
    "adm0", "adm1",
    "disturbance", "deforestation", "degradation",
    "tmf_def", "tmf_deg",
    "it", "it_type", "pa", "pa_type", "overlap",
    "it_2017", "it_2018", "it_2019",
    "it_2020", "it_2021", "it_2022",
    "it_type_2017", "it_type_2018", "it_type_2019",
    "it_type_2020", "it_type_2021", "it_type_2022",
    "pa_2017", "pa_2018", "pa_2019",
    "pa_2020", "pa_2021", "pa_2022",
    "pa_type_2017", "pa_type_2018", "pa_type_2019",
    "pa_type_2020", "pa_type_2021", "pa_type_2022",
    "elevation", "slope", "sx",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_roads", "dens_pop", "travel_time",
    "mort", "mort.id", "mortlag1", "mortlag1.id",
    "cabinet", "driver",
    "lon", "lat",
    "ed_east", "ed_north", "ea_east", "ea_north",
    "hex",
    "som_bmu", "som_x", "som_y")

message("Saving new data files …")

data.int
int.merged[, ..vars.sel.int]

all(data.int$id == int.merged$id)
all(data.int$som_bmu == int.merged$som_bmu)

data.proc
proc.merged[, ..vars.sel.proc]

all(data.proc$id == proc.merged$id)
all(data.proc$som_bmu == proc.merged$som_bmu)

saveRDS(int.merged[, ..vars.sel.int], file.data.int)
saveRDS(proc.merged[, ..vars.sel.proc], file.data.proc)

