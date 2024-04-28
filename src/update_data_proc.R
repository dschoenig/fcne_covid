library(data.table)
source("utilities.R")

data.int <- readRDS("../data/intermediate/amz.data.int.rds")
data.proc <- readRDS("../data/processed/amz.data.proc.rds")

data.proc2 <- 
  merge(data.int, data.proc[, .(id, som_x, som_y, som_bmu)],
        by = "id")

saveRDS(data.proc2, "../data/processed/amz.data.proc.rds")
