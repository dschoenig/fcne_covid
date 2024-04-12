library(data.table)

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
  file.data.int <- paste0(path.int, regions[i], ".data.int.rds")

  vars <- fread(file.data.raw, 
                na.strings = "",
                key = "id")

  vars[, 
       `:=`(forestloss = ifelse(forestloss == "t", TRUE, FALSE),
            primary_forest = ifelse(primary_forest == "t", TRUE, FALSE),
            for_type = factor(ifelse(primary_forest == "t",
                                     "primary", "other"),
                              levels = c("other", "primary"),
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
            adm1 = factor(adm0)
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
    _[, .SD[sample(1:.N, 1e6)], by = "year"] 

  setkey(data.int, id)
  setcolorder(data.int,
              c("id", "year",
                "adm0", "adm1",
                "forestloss", "lossyear",
                "primary_forest", "for_type",
                "it", "it_type", "pa", "pa_type", "overlap",
                "tri", "dist_set", "dist_roads", "dist_rivers",
                "dens_roads", "dens_pop",
                "lon", "lat",
                "ed_east", "ed_north", "ea_east", "ea_north"))


  saveRDS(data.int, file.data.int)

  rm(data.int, vars)

}
