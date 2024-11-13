args <- commandArgs(trailingOnly = TRUE)
library(data.table)

source("utilities.R")

path.base <- "../"
path.data.proc <- paste0(path.base, "data/processed/")

region <- tolower(as.character(args[1]))
# region <- "amz"

file.data.proc <- paste0(path.data.proc, region, ".data.proc.rds")
file.data.cf1 <- paste0(path.data.proc, region, ".data.cf1.rds")
file.data.cf2 <- paste0(path.data.proc, region, ".data.cf2.rds")
file.data.cf3 <- paste0(path.data.proc, region, ".data.cf3.rds")

data <-
  readRDS(file.data.proc) |>
  _[,
    .(year,
      adm0,
      drought_mod, drought_sev, fire,
      it_type,
      it_type_2017, it_type_2018, it_type_2019,
      it_type_2020, it_type_2021, it_type_2022,
      pa_type,
      pa_type_2017, pa_type_2018, pa_type_2019,
      pa_type_2020, pa_type_2021, pa_type_2022,
      overlap,
      pandemic,
      mort, mort.id, mortlag1, mortlag1.id,
      driver,
      ed_east, ed_north,
      ea_east, ea_north,
      hex,
      som_x, som_y, som_bmu)]

cf.types <- paste0("cf", 1:4)


data.pan <- data[pandemic == "yes"]

set.seed(19360429)
year.sam <- sample(2017:2019, nrow(data.pan), replace = T)


data.cf1 <- 
  copy(data.pan) |>
  _[, 
    `:=`(year.fac = year,
         year = year.sam,
         pandemic.fac = pandemic,
         pandemic = factor("no", levels = levels(pandemic), ordered = TRUE))]
setorder(data.cf1, year)

# IT and PA type are maintained from factual year

# y.seq <- 2017:2019
# for(i in seq_along(y.seq)) {
#   it_type.y <- paste0("it_type_", y.seq[i])
#   pa_type.y <- paste0("pa_type_", y.seq[i])
#   data.cf1[year == y.seq[i],
#            `:=`(it_type = it_type.col,
#                 pa_type = pa_type.col),
#            env = list(it_type.col = it_type.y,
#                       pa_type.col = pa_type.y)]
# }
# data.cf1[, overlap := NA]
# data.cf1[pa_type != "none" & it_type != "none",
#          overlap := paste(it_type, pa_type, sep = ":")]
# data.cf1[is.na(overlap), overlap := "none"]
# data.cf1[, overlap := factor(overlap,
#                              levels = c("none",
#                                         "recognized:indirect_use",
#                                         "recognized:direct_use",
#                                         "not_recognized:indirect_use",
#                                         "not_recognized:direct_use"),
#                              ordered = TRUE)]

data.cf1[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf1", levels = cf.types))]
setcolorder(data.cf1, c("cf.type", "cf.id"))



data.cf2 <-
  copy(data.pan) |>
  _[, 
    `:=`(year.fac = year,
         year = year.sam)]
setorder(data.cf2, year)

# IT and PA type are maintained from factual year

# y.seq <- 2017:2019
# for(i in seq_along(y.seq)) {
#   it_type.y <- paste0("it_type_", y.seq[i])
#   pa_type.y <- paste0("pa_type_", y.seq[i])
#   data.cf1[year == y.seq[i],
#            `:=`(it_type = it_type.col,
#                 pa_type = pa_type.col),
#            env = list(it_type.col = it_type.y,
#                       pa_type.col = pa_type.y)]
# }
# data.cf1[, overlap := NA]
# data.cf1[pa_type != "none" & it_type != "none",
#          overlap := paste(it_type, pa_type, sep = ":")]
# data.cf1[is.na(overlap), overlap := "none"]
# data.cf1[, overlap := factor(overlap,
#                              levels = c("none",
#                                         "recognized:indirect_use",
#                                         "recognized:direct_use",
#                                         "not_recognized:indirect_use",
#                                         "not_recognized:direct_use"),
#                              ordered = TRUE)]

data.cf2[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf2", levels = cf.types))]
setcolorder(data.cf2, c("cf.type", "cf.id"))



data.cf3 <- copy(data.pan)
data.cf3[,
         `:=`(pandemic.fac = pandemic,
              pandemic = factor("no", levels = levels(pandemic), ordered = TRUE))]
setorder(data.cf3, year)
data.cf3[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf3", levels = cf.types))]
setcolorder(data.cf3, c("cf.type", "cf.id"))



saveRDS(data.cf1, file.data.cf1)
saveRDS(data.cf2, file.data.cf2)
saveRDS(data.cf3, file.data.cf3)

data.cf1
data.cf2
data.cf3
