source("utilities.R")

library(data.table)
library(sf)
library(units)

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.raw <- paste0(path.data, "raw/")
path.data.proc <- paste0(path.data, "processed/")
file.data.amz <- paste0(path.data.proc, "amz.data.proc.rds")
file.stats.amz <- paste0(path.data.proc, "amz.sumstats.proc.rds")
file.lim.amz <- paste0(path.data.raw, "forests/amz.limit.gpkg")

file.area.amz <- paste0(path.data.proc, "amz.sumstats.area.rds")

data.amz <- readRDS(file.data.amz)
stats.amz <- readRDS(file.stats.amz)
lim.amz <- st_read(file.lim.amz)

n.amz <- nrow(stats.amz)
a.amz <- set_units(lim.amz$area/1e6, km^2)
p.amz <- a.amz/n.amz
n.tmf.amz <- stats.amz[tmf_annual_2016 == 1, .N]
tmf.amz <- n.tmf.amz * p.amz
a.hex.amz <- set_units(((3*sqrt(3))/2) * 25^2, km^2)

# Exclude points outside of analyzed countries
stats.amz <- stats.amz[!is.na(adm0)]

a.tmf.amz <- stats.amz[tmf_annual_2016 == 1, .N*p.amz]


y.seq <- 2017:2022
y.seq.itpa <- 2017:2022
it_cols <- paste0("it_", y.seq)
it_type_cols <- paste0("it_type_", y.seq)
pa_cols <- paste0("pa_", y.seq)
pa_type_cols <- paste0("pa_type_", y.seq)
tmf_cols <- paste0("tmf_undist_", y.seq)


stats.amz.l <- list()

for(i in seq_along(y.seq)) {
  a.env <-
    list(y.foc = y.seq[i],
         it.y = it_cols[i],
         it_type.y = it_type_cols[i],
         pa.y = pa_cols[i],
         pa_type.y = pa_type_cols[i],
         tmf.y = tmf_cols[i])
  if(y.seq[i] %in% y.seq.itpa) {
    stats.amz.l[[i]] <-
      stats.amz[,
                .(year = y.foc,
                  adm0,
                  tmf_annual_1990, tmf_annual_2016, tmf_annual_2022,
                  tmf_deg, tmf_def,
                  it = it.y, it_type = it_type.y,
                  pa = pa.y, pa_type = pa_type.y,
                  ea_east, ea_north, hex),
                env = a.env]
  } else {
    stats.amz.l[[i]] <-
      stats.amz[,
                .(year = y.foc,
                  adm0,
                  tmf_annual_1990, tmf_annual_2016, tmf_annual_2022,
                  tmf_deg, tmf_def,
                  it = NA, it_type = NA,
                  pa = NA, pa_type = NA,
                  ea_east, ea_north, hex),
                env = a.env]
  }
}

stats.amz.y <- rbindlist(stats.amz.l)

stats.amz.y[, `:=`(tmf_undist = FALSE,
                   degradation = FALSE,
                   deforestation = FALSE,
                   disturbance = FALSE)]

stats.amz.y[tmf_annual_2016 == 1 & 
            ((tmf_deg == 0 | tmf_deg >= year) &
             (tmf_def == 0 | tmf_def >= year)),
            tmf_undist := TRUE]

stats.amz.y[tmf_undist == TRUE,
            `:=`(degradation = fifelse(tmf_deg == year, TRUE, FALSE),
                 deforestation = fifelse(tmf_def == year, TRUE, FALSE))]
stats.amz.y[degradation == TRUE | deforestation == TRUE, disturbance := TRUE]


areas.undist.amz <-
  list(
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, adm0)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, it_type)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, pa_type)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, it_type, pa_type)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, adm0, it_type)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, adm0, pa_type)],
       stats.amz.y[tmf_undist == TRUE,
                   .(area = .N*p.amz,
                     area.rel = (.N*p.amz)/a.tmf.amz),
                   by = .(year, adm0, it_type, pa_type)]
                   ) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.amz, year, it_type, pa_type)
 
areas.undist.hex.amz <-
  list(stats.amz.y[order(year, hex),
                   .(area = a.hex.amz * (sum(tmf_undist)/.N),
                     area.rel = sum(tmf_undist)/.N),
                   by = .(year, hex)]) |>
  rbindlist(fill = TRUE)
setorder(areas.undist.hex.amz, year, hex)

areas.undist.som.amz <-
  merge(data.amz[order(year, som_bmu),
                 .(n.bmu = .N, rel.bmu = .N/2e6),
                 by = .(year, som_x, som_y, som_bmu)],
        stats.amz.y[tmf_undist == TRUE,
                    .(area= .N*p.amz),
                    by = .(year)])
areas.undist.som.amz[, area := rel.bmu * area]


areas.amz <-
  list(
       undist = areas.undist.amz,
       undist.hex = areas.undist.hex.amz,
       undist.som = areas.undist.som.amz
  )


saveRDS(areas.amz, file.area.amz)

