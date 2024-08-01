library(data.table)
library(sf)

## Folders
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.raw <- paste0(path.data, "raw/")
path.fl <- paste0(path.raw, "forestloss/")
path.cv <- paste0(path.raw, "covid/")
path.int <- paste0(path.data, "intermediate/")
path.proc <- paste0(path.data, "processed/")

if(!dir.exists(path.int)){
  dir.create(path.int, recursive = TRUE)
}
if(!dir.exists(path.proc)){
  dir.create(path.proc, recursive = TRUE)
}

regions <- c("amz")


for(i in 1:length(regions)) {

  file.data.raw <- paste0(path.fl, regions[i], ".1722.vars.csv")
  file.stats <- paste0(path.fl, regions[i], ".sumstats_1722.csv")
  file.covid.adm1 <- paste0(path.cv, regions[i], ".covid_mort.adm1.gpkg")
  file.cabinets <- paste0(path.raw, regions[i], ".cabinets.csv")

  file.data.int <- paste0(path.int, regions[i], ".data.int.rds")
  file.stats.proc <- paste0(path.proc, regions[i], ".sumstats.proc.rds")


  vars <- fread(file.data.raw, na.strings = "", key = "id")

  y.seq <- 2017:2022
  it_cols <- paste0("it_", y.seq)
  it_type_cols <- paste0("it_type_", y.seq)
  pa_cols <- paste0("pa_", y.seq)
  pa_type_cols <- paste0("pa_type_", y.seq)
  itpa_cols <- c(it_cols, pa_cols)
  itpa_type_cols <- c(it_type_cols, pa_type_cols)

  vars[, 
       `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
            primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
            for_type = factor(ifelse(primary_forest == "t",
                                     "primary", "other"),
                              levels = c("primary", "other"),
                              ordered = TRUE),
            # it = ifelse(it == "t", TRUE, FALSE),
            # it_type = factor(it_type,
            #                  levels = c("none", "recognized", "not_recognized"),
            #                  ordered = TRUE),
            # pa = ifelse(pa == "t", TRUE, FALSE),
            # pa_type = factor(pa_type,
            #                  levels = c("none", "indirect_use", "direct_use"),
            #                  ordered = TRUE),
            adm0 = factor(adm0),
            adm1 = factor(adm1)
            )
       ]
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


  # vars[pa_type_2017 != pa_type_2022, ..pa_type_cols]
  # vars[is.na(it_type), it_type := "none"]
  # vars[is.na(pa_type), pa_type := "none"]

  for(i in seq_along(y.seq)) {
    it.y <- paste0("it_", y.seq[i])
    it_type.y <- paste0("it_type_", y.seq[i])
    pa.y <- paste0("pa_", y.seq[i])
    pa_type.y <- paste0("pa_type_", y.seq[i])
    vars[year == y.seq[i],
         `:=`(it = it.col,
              it_type = it_type.col,
              pa = pa.col,
              pa_type = pa_type.col),
         env = list(it.col = it.y,
                    it_type.col = it_type.y,
                    pa.col = pa_type.y,
                    pa_type.col = pa_type.y)]
  }

  vars[pa_type != "none" & it_type != "none",
       overlap := paste(it_type, pa_type, sep = ":")]
  vars[is.na(overlap), overlap := "none"]
  vars[, overlap := factor(overlap,
                           levels = c("none",
                                      "recognized:indirect_use",
                                      "recognized:direct_use",
                                      "not_recognized:indirect_use",
                                      "not_recognized:direct_use"),
                           ordered = TRUE)]




  # Remove samples with incomplete covariate information and
  # progressively choose yearly sample for 2017-2022

  set.seed(18470611)
  data.int <-
    na.omit(vars) |>
    _[, .SD[sample(1:.N, 2e6)], by = "year"] 


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
  setcolorder(data.int.all,
              c("id", "year",
                "pandemic",
                "adm0", "adm1",
                "forestloss", "lossyear",
                "primary_forest", "for_type",
                "it", "it_type", "pa", "pa_type", "overlap",
                "it_2017", "it_2018", "it_2019",
                "it_2020", "it_2021", "it_2022",
                "it_type_2017", "it_type_2018", "it_type_2019",
                "it_type_2020", "it_type_2021", "it_type_2022",
                "pa_2017", "pa_2018", "pa_2019",
                "pa_2020", "pa_2021", "pa_2022",
                "pa_type_2017", "pa_type_2018", "pa_type_2019",
                "pa_type_2020", "pa_type_2021", "pa_type_2022",
                "elevation", "slope", "tri", "sx",
                "dist_set", "dist_roads", "dist_rivers",
                "dens_roads", "dens_pop",
                "mort", "mort.id", "mortlag1", "mortlag1.id",
                "cabinet",
                "lon", "lat",
                "ed_east", "ed_north", "ea_east", "ea_north"))

  saveRDS(data.int.all, file.data.int)

  rm(data.int, vars)


  stats <- fread(file.stats, na.strings = "", key = "id")

  y.seq <- c(2017, 2022)
  it_cols <- paste0("it_", y.seq)
  it_type_cols <- paste0("it_type_", y.seq)
  pa_cols <- paste0("pa_", y.seq)
  pa_type_cols <- paste0("pa_type_", y.seq)
  itpa_cols <- c(it_cols, pa_cols)
  itpa_type_cols <- c(it_type_cols, pa_type_cols)

  stats[, 
       `:=`(primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
            for_type = factor(ifelse(primary_forest == "t",
                                     "primary", "other"),
                              levels = c("primary", "other"),
                              ordered = TRUE),
            adm0 = factor(adm0))
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

  setcolorder(stats,
              c("id", "adm0",
                "dm", "lossyear", "cover",
                "primary_forest", "for_type",
                it_cols, it_type_cols, pa_cols, pa_type_cols,
                "ea_east", "ea_north"))

  saveRDS(stats, file.stats.proc)

}
