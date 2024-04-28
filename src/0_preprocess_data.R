library(data.table)
library(sf)

## Folders
path.base <- "../"
path.data <- paste0(path.base, "data/")
path.raw <- paste0(path.data, "raw/")
path.fl <- paste0(path.raw, "forestloss/")
path.cv <- paste0(path.raw, "covid/")
path.int <- paste0(path.data, "intermediate/")

if(!dir.exists(path.int)){
  dir.create(path.int, recursive = TRUE)
}

regions <- c("amz")


for(i in 1:length(regions)) {

  file.data.raw <- paste0(path.fl, regions[i], ".1722.vars.csv")
  file.covid.adm1 <- paste0(path.cv, regions[i], ".covid_mort.adm1.gpkg")
  file.cabinets <- paste0(path.raw, regions[i], ".cabinets.csv")
  file.data.int <- paste0(path.int, regions[i], ".data.int.rds")

  vars <- fread(file.data.raw, na.strings = "", key = "id")

  vars[, 
       `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
            primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
            for_type = factor(ifelse(primary_forest == "t",
                                     "primary", "other"),
                              levels = c("primary", "other"),
                              ordered = TRUE),
            it = ifelse(it == "t", TRUE, FALSE),
            it_type = factor(it_type,
                             levels = c("none", "recognized", "not_recognized"),
                             ordered = TRUE),
            pa = ifelse(pa == "t", TRUE, FALSE),
            pa_type = factor(pa_type,
                             levels = c("none", "indirect_use", "direct_use"),
                             ordered = TRUE),
            adm0 = factor(adm0),
            adm1 = factor(adm1)
            )
       ]
  vars[is.na(it_type), it_type := "none"]
  vars[is.na(pa_type), pa_type := "none"]
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
               .(adm0, mort_2020,mort_2021, mort_2022)],
         id.vars = "adm0",
         measure.vars = measure(value.name, year = as.integer, sep = "_")),
    melt(covid[adm0 %in% adm.nosub,
               .(adm0, mortlag1_2020,mortlag1_2021, mortlag1_2022)],
         id.vars = "adm0",
         measure.vars = measure(value.name, year = as.integer, sep = "_"))) |>
  merge(data.int[adm0 %in% adm.nosub],
        by = c("adm0", "year"),
        all.y = TRUE)


  data.int.sub <-
  merge(
    melt(covid[adm0 %notin% adm.nosub,
               .(adm0, adm1, mort_2020,mort_2021, mort_2022)],
         id.vars = c("adm0", "adm1"),
         measure.vars = measure(value.name, year = as.integer, sep = "_")),
    melt(covid[adm0 %notin% adm.nosub,
               .(adm0, adm1, mortlag1_2020,mortlag1_2021, mortlag1_2022)],
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
                                      ordered = TRUE))]
  
  setkey(data.int.all, id)
  setcolorder(data.int.all,
              c("id", "year",
                "pandemic",
                "adm0", "adm1",
                "forestloss", "lossyear",
                "primary_forest", "for_type",
                "it", "it_type", "pa", "pa_type", "overlap",
                "tri", "dist_set", "dist_roads", "dist_rivers",
                "dens_roads", "dens_pop",
                "mort", "mortlag1",
                "cabinet",
                "lon", "lat",
                "ed_east", "ed_north", "ea_east", "ea_north"))

  saveRDS(data.int.all, file.data.int)

  rm(data.int, vars)

}
