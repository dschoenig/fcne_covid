args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(sf)
source("utilities.R")

region <- tolower(as.character(args[1]))
region <- "amz"

## Folders
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.raw <- paste0(path.data, "raw/")
path.fl <- paste0(path.raw, "forests/")
path.cv <- paste0(path.raw, "covid/")
path.int <- paste0(path.data, "intermediate/")
path.proc <- paste0(path.data, "processed/")

if(!dir.exists(path.int)){
  dir.create(path.int, recursive = TRUE)
}
if(!dir.exists(path.proc)){
  dir.create(path.proc, recursive = TRUE)
}


file.data.raw <- paste0(path.fl, region, ".1722.vars.csv")
file.data.df <- paste0(path.fl, region, ".1722.df.csv")
file.stats <- paste0(path.fl, region, ".sumstats_1722.csv")
file.covid.adm1 <- paste0(path.cv, region, ".covid_mort.adm1.gpkg")
file.cabinets <- paste0(path.raw, region, ".cabinets.csv")

file.data.int <- paste0(path.int, region, ".data.int.rds")
file.df.proc <- paste0(path.proc, region, ".df.proc.rds")
file.stats.proc <- paste0(path.proc, region, ".sumstats.proc.rds")


vars <- fread(file.data.raw, na.strings = "", key = "id")


# Handle factor variables (administrative areas, inidgenous territories
# and protected areas)

y.seq <- 2017:2022
it_cols <- paste0("it_", y.seq)
it_type_cols <- paste0("it_type_", y.seq)
pa_cols <- paste0("pa_", y.seq)
pa_type_cols <- paste0("pa_type_", y.seq)
itpa_cols <- c(it_cols, pa_cols)
itpa_type_cols <- c(it_type_cols, pa_type_cols)

vars[, 
     `:=`(
          adm0 = factor(adm0),
          adm1 = factor(adm1),
          driver = factor(fcase(is.na(driver), "not_identified",
                                driver == 1, "commodity",
                                driver == 2, "shifting_cultivation",
                                driver == 3, "forestry",
                                driver == 4, "wildfires",
                                driver == 5, "urbanization"),
                          levels = c("not_identified",
                                     "commodity", "shifting_cultivation",
                                     "forestry", "wildfires", "urbanization")))]
vars[,
     (itpa_cols) := lapply(.SD, \(x) fifelse(x == "t", TRUE, FALSE)),
     .SDcols = itpa_cols]
vars[,
     (it_type_cols) := lapply(.SD,
                              \(x) factor(x,
                                          levels = c("none", "recognized", "not_recognized"),
                                          ordered = TRUE)),
     .SDcols = it_type_cols]
vars[,
     (pa_type_cols) := lapply(.SD,
                              \(x) factor(x,
                                          levels = c("none", "indirect_use", "direct_use"),
                                          ordered = TRUE)),
     .SDcols = pa_type_cols]
vars[,
     (itpa_type_cols) := lapply(.SD,
                                \(x) {
                                  y <- x
                                  y[is.na(x)] <- "none"
                                  return(y)}
                                ),
     .SDcols = itpa_type_cols]



# Remove samples with incomplete covariate information
data.int <- na.omit(vars)
rm(vars)


# Prepare temporal (sub)sample of the spatial observations. The eligible
# population per year consists of undisturbed tropical moist forests
# (that is forests for which no disturbance have been detected prior to
# the sampling year).

set.seed(18470611)

y.seq <- 2017:2022
data.int.sam.l <- list()
idx.sam <- integer()
per.year <- 2e6

for(i in seq_along(y.seq)) {
  y.foc <- y.seq[i]
  data.int.sam.l[[i]] <- 
    data.int[!(id %in% idx.sam) &
              (tmf_deg == 0 | tmf_deg >= y.foc) &
              (tmf_def == 0 | tmf_def >= y.foc)
             ][sample(1:.N, per.year, replace = FALSE)]
  data.int.sam.l[[i]][, year := y.foc]
  idx.sam <- c(idx.sam, data.int.sam.l[[i]][, id])
}

data.int <- rbindlist(data.int.sam.l)
rm(data.int.sam.l)

# Determine deforestation and degradation on an annual basis

data.int[,
         `:=`(deforestation = fifelse(tmf_def == year, TRUE, FALSE),
              degradation = fifelse(tmf_deg == year, TRUE, FALSE),
              disturbance = fifelse(tmf_def == year | tmf_deg == year, TRUE, FALSE))]


# Assign IT and PA status based on sampled year

# data.int[pa_type_2017 != pa_type_2022, ..pa_type_cols]
# data.int[is.na(it_type), it_type := "none"]
# data.int[is.na(pa_type), pa_type := "none"]

for(i in seq_along(y.seq)) {
  it.y <- paste0("it_", y.seq[i])
  it_type.y <- paste0("it_type_", y.seq[i])
  pa.y <- paste0("pa_", y.seq[i])
  pa_type.y <- paste0("pa_type_", y.seq[i])
  data.int[year == y.seq[i],
           `:=`(it = it.col,
                it_type = it_type.col,
                pa = pa.col,
                pa_type = pa_type.col),
           env = list(it.col = it.y,
                      it_type.col = it_type.y,
                      pa.col = pa_type.y,
                      pa_type.col = pa_type.y)]
}

data.int[pa_type != "none" & it_type != "none",
     overlap := paste(it_type, pa_type, sep = ":")]
data.int[is.na(overlap), overlap := "none"]
data.int[, overlap := factor(overlap,
                         levels = c("none",
                                    "recognized:indirect_use",
                                    "recognized:direct_use",
                                    "not_recognized:indirect_use",
                                    "not_recognized:direct_use"),
                         ordered = TRUE)]


# Include mortality data

covid <-
  st_read(file.covid.adm1) |>
  st_drop_geometry() |>
  as.data.table()

adm.nosub <- covid[is.na(adm1), adm0]

# CSSE data does not distinguish between Lima the province and Lima the region
covid.per.15_1 <- covid[adm1 == "PER.16_1"]
covid.per.15_1[,
               `:=`(adm.id = nrow(covid) + 1,
                    adm1 = "PER.15_1",
                    name1 = "Lima Province")]
covid <- rbind(covid, covid.per.15_1)

covid[, `:=`(mortlag1_2020 = 0,
             mortlag1_2021 = mort_2020,
             mortlag1_2022 = mort_2021)]

data.int.nosub <-
  merge(
    melt(covid[adm0 %in% adm.nosub,
               .(adm0, mort_2020, mort_2021, mort_2022)],
         id.vars = "adm0",
         measure.vars = measure(value.name, year = as.integer, sep = "_")),
    melt(covid[adm0 %in% adm.nosub,
               .(adm0, mortlag1_2020, mortlag1_2021, mortlag1_2022)],
         id.vars = "adm0",
         measure.vars = measure(value.name, year = as.integer, sep = "_"))) |>
  merge(data.int[adm0 %in% adm.nosub],
        by = c("adm0", "year"),
        all.y = TRUE)


data.int.sub <-
  merge(
    melt(covid[adm0 %notin% adm.nosub,
               .(adm0, adm1, mort_2020, mort_2021, mort_2022)],
         id.vars = c("adm0", "adm1"),
         measure.vars = measure(value.name, year = as.integer, sep = "_")),
    melt(covid[adm0 %notin% adm.nosub,
               .(adm0, adm1, mortlag1_2020, mortlag1_2021, mortlag1_2022)],
         id.vars = c("adm0", "adm1"),
         measure.vars = measure(value.name, year = as.integer, sep = "_"))) |>
  merge(data.int[adm0 %notin% adm.nosub],
          by = c("adm0", "adm1", "year"),
        all.y = TRUE)


data.int.covid <- rbind(data.int.nosub, data.int.sub)
data.int.covid[is.na(mort), mort := 0]
data.int.covid[is.na(mortlag1), mortlag1 := 0]

# Include political administrations

cabinets <- fread(file.cabinets)

# Truncate to study period and expand
cabinets[is.na(end) | end > 2022, end := 2022]
cabinets[start < 2017, start := 2017]
cabinets <-
  cabinets[, .SD[rep(1, end - start + 1),
                 .(cabinet,
                   adm0,
                   year = seq(start[1], end[1]))],
           by = .I
           ][, -"I"]

data.int.all <-
  merge(data.int.covid, cabinets,
        by = c("adm0", "year"))

data.int.all[,
             `:=`(adm0 = factor(adm0),
                  cabinet = factor(cabinet),
                  pandemic = factor(fifelse(year < 2020, "no", "yes"),
                                    levels = c("no", "yes"),
                                    ordered = TRUE),
                  mort.id = as.numeric(year >= 2020),
                  mortlag1.id = as.numeric(year >= 2021))]

setkey(data.int.all, id)

rm(data.int)
gc()


# Process drought and fire data

# data.int.all <- readRDS(file.data.int)
data.df <-
  fread(file.data.df, na.strings = "", key = "id") |>
  _[data.int.all[, .(id)], on = "id"]


y.seq <- 2017:2022
fire_cols <- paste0("fire_", 2017:2022)
di_cols <- paste0("di12_", 2017:2022)

data.df[,
        (fire_cols) := lapply(.SD, \(x) fifelse(x == "t", TRUE, FALSE)),
        .SDcols = fire_cols]


saveRDS(data.df, file.df.proc)

data.int.all <- merge(data.int.all, data.df)

for(i in seq_along(y.seq)) {
  fire.y <- fire_cols[i]
  di.y <- di_cols[i]
  data.int.all[year == y.seq[i],
               `:=`(fire = fire.col,
                    drought_mod = fifelse(di.col <= -1, TRUE, FALSE),
                    drought_sev = fifelse(di.col <= -1.5, TRUE, FALSE)),
                env = list(fire.col = fire.y,
                           di.col = di.y)]
}




# Prepare export

vars.sel <-
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
    "drought_mod", "drought_sev", "fire",
    "di12_2017", "di12_2018", "di12_2019",
    "di12_2020", "di12_2021", "di12_2022",
    "fire_2017", "fire_2018", "fire_2019",
    "fire_2020", "fire_2021", "fire_2022",
    "elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_roads", "dens_pop", "travel_time",
    "mort", "mort.id", "mortlag1", "mortlag1.id",
    "cabinet", "driver",
    "lon", "lat",
    "ed_east", "ed_north", "ea_east", "ea_north",
    "hex")

data.int.all <- data.int.all[, ..vars.sel]

saveRDS(data.int.all, file.data.int)


# Process data for summary statistics

stats <- fread(file.stats, na.strings = "", key = "id")

# y.seq <- c(2017, 2022)
y.seq <- 2017:2022
it_cols <- paste0("it_", y.seq)
it_type_cols <- paste0("it_type_", y.seq)
pa_cols <- paste0("pa_", y.seq)
pa_type_cols <- paste0("pa_type_", y.seq)
itpa_cols <- c(it_cols, pa_cols)
itpa_type_cols <- c(it_type_cols, pa_type_cols)

stats[, 
     `:=`(adm0 = factor(adm0))
     ]
stats[,
     (itpa_cols) := lapply(.SD, \(x) fifelse(x == "t", TRUE, FALSE)),
     .SDcols = itpa_cols]
stats[,
     (it_type_cols) := lapply(.SD,
                              \(x) factor(x,
                                          levels = c("none", "recognized", "not_recognized"),
                                          ordered = TRUE)),
     .SDcols = it_type_cols]
stats[,
     (pa_type_cols) := lapply(.SD,
                              \(x) factor(x,
                                          levels = c("none", "indirect_use", "direct_use"),
                                          ordered = TRUE)),
     .SDcols = pa_type_cols]
stats[,
     (itpa_type_cols) := lapply(.SD,
                                \(x) {
                                  y <- x
                                  y[is.na(x)] <- "none"
                                  return(y)}
                                ),
     .SDcols = itpa_type_cols]

stats.sel <-
  c("id", "adm0",
    "tmf_annual_1990", "tmf_annual_2016", "tmf_annual_2022",
    "tmf_def", "tmf_deg",
    it_cols, it_type_cols, pa_cols, pa_type_cols,
    "ea_east", "ea_north", "hex")

stats <- stats[, ..stats.sel]

saveRDS(stats, file.stats.proc)

