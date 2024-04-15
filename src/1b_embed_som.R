args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(kohonen)
source("utilities.R")

## Paths
path.som <- "../models/som/"
path.data <- "../data/"
path.data.int <- paste0(path.data, "intermediate/")

path.data.proc <- paste0(path.data, "processed/")
if(!dir.exists(path.data.proc)){
  dir.create(path.data, recursive = TRUE)
}


region <- tolower(as.character(args[1]))
# region <- "amz"


file.data.int <- paste0(path.data.int, region, ".data.int.rds")
file.som <- paste0(path.som, region, ".som.1e6.rds")
file.data.proc <- paste0(path.data.proc, region, ".data.proc.rds")


## Map covariates to SOM #######################################################


som.fit <- readRDS(file.som)

a <- Sys.time()

data.int <- readRDS(file.data.int)

embedded <-
  egp_embed(data.int[,
                     .(tri, dist_set, dist_roads,
                       dist_rivers, dens_pop, dens_roads)],
            som.fit,
            vars = c("tri", "dist_set", "dist_roads",
                     "dist_rivers", "dens_pop", "dens_roads"),
            scale = TRUE,
            bmu.name = "som_bmu",
            coord = TRUE,
            coord.names = c("som_x", "som_y"),
            list = TRUE)

b <- Sys.time()
print(b-a)

data.int[,
         `:=`(som_bmu = embedded$som_bmu,
              som_x = embedded$som_x,
              som_y = embedded$som_y)
         ]

saveRDS(data.int, file.data.proc)

