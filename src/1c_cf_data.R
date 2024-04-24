args <- commandArgs(trailingOnly = TRUE)
library(data.table)

source("utilities.R")

path.base <- "../"
path.data.proc <- paste0(path.base, "data/processed/")

region <- tolower(as.character(args[1]))
region <- "amz"

file.data.proc <- paste0(path.data.proc, region, ".data.proc.rds")
file.data.cf1 <- paste0(path.data.proc, region, ".data.cf1.rds")
file.data.cf2 <- paste0(path.data.proc, region, ".data.cf2.rds")
file.data.cf3 <- paste0(path.data.proc, region, ".data.cf3.rds")
file.data.cf4 <- paste0(path.data.proc, region, ".data.cf4.rds")

data <-
  readRDS(file.data.proc) |>
  _[,
    .(year,
      for_type,
      it_type,
      pa_type,
      overlap,
      pandemic,
      mort,
      cabinet,
      ed_east, ed_north,
      som_x, som_y)]

cf.types <- paste0("cf", 1:4)

set.seed(19360429)
data.cf1 <- data[pandemic == "yes"]
data.cf1[, 
         `:=`(year = sample(2017:2019, .N, replace = T),
              mort = 0,
              pandemic = factor("no", levels = levels(pandemic), ordered = TRUE))]

setorder(data.cf1, year)
data.cf1[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf1", levels = cf.types))]
setcolorder(data.cf1, c("cf.type", "cf.id"))

data.cf2 <- data[pandemic == "yes"]
data.cf2[, mort := 0]
setorder(data.cf2, year)
data.cf2[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf2", levels = cf.types))]
setcolorder(data.cf2, c("cf.type", "cf.id"))

data.cf3 <- data[pandemic == "yes"]
data.cf3[, pandemic := factor("no", levels = levels(pandemic), ordered = TRUE)]
setorder(data.cf3, year)
data.cf3[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf3", levels = cf.types))]
setcolorder(data.cf3, c("cf.type", "cf.id"))

data.cf4 <- data[pandemic == "yes"]
data.cf4[, 
         `:=`(mort = 0,
              pandemic = factor("no", levels = levels(pandemic), ordered = TRUE))]
setorder(data.cf4, year)
data.cf4[,
         `:=`(cf.id = 1:.N,
              cf.type = factor("cf4", levels = cf.types))]
setcolorder(data.cf4, c("cf.type", "cf.id"))


saveRDS(data.cf1, file.data.cf1)
saveRDS(data.cf2, file.data.cf2)
saveRDS(data.cf3, file.data.cf3)
saveRDS(data.cf4, file.data.cf4)
