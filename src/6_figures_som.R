
args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(sf)
library(stars)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))

n.threads <- 4
region <- "amz"
overwrite <- TRUE

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.raw <- paste0(path.data, "raw/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.som <- paste0(path.base, "models/som/")
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)


file.som <- paste0(path.som, region, ".som.1e6.rds")
cf.name <- "cf1"
files.agg <- paste0(path.agg, region, ".som.", c("fac", cf.name), ".rds")

file.fig.geo <- paste0(path.figures, region, ".som.png")
file.data.vis <- paste0(path.data.vis, region, ".som.rds")

regions <- c("amz")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(21, palette = "Purple-Green")
c.map <- col.div[c(3,7,14,18)]


## MAP SETUP

crs.ea <- 
  list(amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))

map_xlim <- list(amz = c(-240e4, 210e4))
map_ylim <- list(amz = c(-200e4, 185e4))

base.size <- 7 
base.family <- "IBMPlexSansCondensed"
map_theme <-
  theme_light(base_family = base.family,
              base_size = base.size) +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.justification = c(0,1),
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = 3, b = 7),
                                    lineheight = rel(1)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(10, "pt"),
        strip.text = element_text(size = rel(1),
                                  # face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(base.size/1.5,
                                                  base.size/1.5,
                                                  base.size/1.5,
                                                  base.size/1.5)),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "gray95", colour = NA),
        plot.tag = element_text(margin = margin(t = 0, r = 6, b = 6, l = 0),
                                size = rel(1.5)),
        plot.tag.location = "margin"
        )

theme_leg_bottom <-
  theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = rel(0.9), hjust = 0.5,
                                  margin = margin(t = 7, b = 7),
                                  lineheight = rel(1)),
      legend.text = element_text(size = rel(0.75)),
      legend.text.position = "top",
      legend.title.position = "bottom"
      )

# map_guide_fill <-
#   guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
#                                                                          linewidth = 0.2),
#                                              legend.frame = element_rect(colour = "grey5",
#                                                                          linewidth = 0.2),
#                                              legend.text = element_text(hjust = 1),
#                                              legend.text.position = "right",
#                                              legend.key.width = unit(7.5, "pt"),
#                                              legend.key.height = unit(65, "pt"),
#                                              legend.ticks.length = unit(2, "pt")),
#                                draw.ulim = TRUE,
#                                draw.llim = TRUE
#                                ))

map_guide_fill <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             # legend.text = element_text(hjust = 1),
                                             # legend.text.position = "right",
                                             legend.key.width = unit(75, "pt"),
                                             legend.key.height = unit(7.5, "pt"),
                                             legend.ticks.length = unit(2, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE
                               ))



cov.scales <-
  list(amz = 
         list(
              # tri = list(title = "Terrain ruggedness index",
              #            trans = scales::yj_trans(0),
              #            breaks = c(0, 3, 10, 30, 100),
                         # limits = c(0, 100),
                         # labels = scales::label_comma(big.mark =" ", accuracy = 0.1)),
              elevation = list(
                               title = "Elevation (m)",
                               trans = scales::identity_trans(),
                               breaks = seq(0, 5e3, 1e3),
                               limits = c(0, 5e3),
                               labels = scales::label_comma(big.mark =" ")),
              slope = list(
                           title = "Slope (degrees)",
                           trans = scales::identity_trans(),
                           breaks = seq(0, 75, 25),
                           limits = c(0, 75),
                           labels = scales::label_comma(big.mark =" ")),
              cover = list(
                           title = "Tree cover, year 2000 (percent)",
                           trans = scales::identity_trans(),
                           breaks = seq(50, 100, 25),
                           limits = c(50, 100),
                           labels = scales::label_comma(big.mark =" ")),
              sx = list(
                        title = "Mean agricultural suitability index",
                        trans = scales::identity_trans(),
                        breaks = seq(0, 1e4, 2.5e3),
                        limits = c(0, 1e4),
                        labels = scales::label_comma(big.mark =" ")),
              dist_set.km = list(
                                 title = "Distance to settlements (km)",
                                 trans = scales::identity_trans(),
                                 breaks = seq(0, 3e2, 1e2),
                                 limits = c(0, 3e2),
                                 labels = scales::label_comma(big.mark =" ")),
              dist_roads.km = list(
                                   title = "Distance to roads (km)",
                                   trans = scales::identity_trans(),
                                   breaks = seq(0, 2.5e2, 5e1),
                                   limits = c(0, 2.25e2),
                                   labels = scales::label_comma(big.mark =" ")),
              dist_rivers.km = list(
                                    title = "Distance to rivers (km)",
                                    trans = scales::identity_trans(),
                                    breaks = seq(0, 10, 2.5),
                                    limits = c(0, 10),
                                    labels = scales::label_comma(big.mark =" ")),
              dens_pop = list(
                              title = "Population density (individuals / km²)",
                              trans = scales::yj_trans(0),
                              breaks = c(0, 10^(1:4)),
                              limits = c(0, 1e4),
                              labels = scales::label_comma(big.mark =" ")),
              dens_roads = list(
                                title = "Road density (m / km²)",
                                trans = scales::yj_trans(0),
                                breaks = c(0, 10^(1:4)),
                                limits = c(0, 1.75e3),
                                labels = scales::label_comma(big.mark =" "))
              ))

cat.lab <- 
  data.table(cat.label = c("All forests", "Primary forests",
                           "IT, recognized", "IT, not recognized",
                           "PA, category I-IV", "PA, category V-VI"),
             it_type = c(NA, NA,
                         "recognized", "not_recognized",
                         NA, NA),
             pa_type = c(NA, NA,
                         NA, NA,
                         "indirect_use", "direct_use"),
             for_type = c(NA, "primary",
                          NA, NA,
                          NA, NA))
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]

type.lab <-
  data.table(type = c("fac", "cf1", "cf4", "cf3", "cf2"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            "Counterfactual 1:\nNo pandemic",
                            "Counterfactual 2:\nNo change in spatial intensity",
                            "Counterfactual 3:\nNo change of covariate effects",
                            "Counterfactual 4:\nNo mortality effect"))
type.lab[, type.label := factor(type.label, levels = type.label)]

diff.lab <- "Difference\n(factual vs. counterfactual)"

year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
             year.label = c("2020", "2021", "2022", "2020–2022 (average)"))
year.lab[, year.label := factor(year.label, levels = year.label)]

quant.lab <-
  data.table(quant = c(0.05, 0.25, 0.5, 0.75, 0.95),
             quant.label = c("5% posterior quantile",
                             "25% posterior quantile",
                             "Posterior median",
                             "75% posterior quantile",
                             "95% posterior quantile"))
quant.lab[, quant.label := factor(quant.label, levels = quant.label)]

title.wrap <- scales::label_wrap(20)

fl.title <- "Annual forest cover loss\n"
mar.title <- "Absolute difference in\nannual forest cover loss"


## EFFECTS IN COVARIATE SPACE ##################################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {
  
  som.sum <- list()

   # Forest cover loss rate

  message("Rate of forest cover loss …")

  agg.fl <-
    lapply(files.agg, readRDS) |>
    rbindlist()

  som.sum[[region]]$fl <-
    agg.fl[,
           .(
             forestloss.mean = mean(forestloss),
             forestloss.median = median(forestloss),
             forestloss.sd = sd(forestloss),
             forestloss.q2.5 = quantile(forestloss, 0.025),
             forestloss.q5 = quantile(forestloss, 0.05),
             forestloss.q25 = quantile(forestloss, 0.25),
             forestloss.q75 = quantile(forestloss, 0.75),
             forestloss.q95 = quantile(forestloss, 0.95),
             forestloss.q97.5 = quantile(forestloss, 0.975)),
           by = .(type, year, som_x, som_y)]

  som.sum[[region]]$fl <-
    som.sum[[region]]$fl |>
    merge(type.lab, by = "type") |>
    merge(year.lab, by = "year")
  setorder(som.sum[[region]]$fl, type.label, year.label)


  # Difference in forest cover loss rate between factual and
  # counterfactual


  message("Difference between factual and counterfactual …")

  agg.diff <-
    dcast(agg.fl, year + som_x + som_y + .draw ~ type, value.var = "forestloss")
  agg.diff[, diff := fac - cf.col, env = list(cf.col = cf.name)]

  som.sum[[region]]$diff <-
    agg.diff[,
             .(
               diff.mean = mean(diff),
               diff.median = median(diff),
               diff.sd = sd(diff),
               diff.q2.5 = quantile(diff, 0.025),
               diff.q5 = quantile(diff, 0.05),
               diff.q25 = quantile(diff, 0.25),
               diff.q75 = quantile(diff, 0.75),
               diff.q95 = quantile(diff, 0.95),
               diff.q97.5 = quantile(diff, 0.975)),
             by = .(year, som_x, som_y)]

  som.sum[[region]]$diff <-
    som.sum[[region]]$diff |>
    merge(year.lab, by = "year")
  setorder(som.sum[[region]]$diff, year.label)

  # Covariate location for SOM cells
  
  message("Covariates …")

  cov.names <-
    c("elevation", "slope", "sx", "cover",
      "dist_set", "dist_roads", "dist_rivers",
      "dens_pop", "dens_roads")
  cov.trans.km <- c("dist_set", "dist_roads", "dist_rivers")
  cov.trans.names <- paste0(cov.trans.km, ".km")

  som.fit <- readRDS(file.som)

  som.codes <-
    get_codes(som.fit)[[1]] |>
    apply(1, \(x) (x * som.fit$scale$sd) + som.fit$scale$mean) |>
    t() |>
    cbind(get_grid(som.fit)[[1]]) |>
    as.data.table()
  setnames(som.codes, c("x", "y"), c("som_x", "som_y"))
  som.codes[, (cov.trans.names) := lapply(.SD, \(x) x/1000), .SDcols = cov.trans.km]

  som.sum[[region]]$cov <- som.codes

  rm(agg.fl, agg.diff, som.codes)
  gc()


  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(som.sum = som.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## MAPS ########################################################################


message(paste0("Preparing maps for region `", region, "` …"))
maps <- list()


# Covariate maps

cov.scale <- cov.scales[[region]]
covariates <- names(cov.scale)

for(i in seq_along(covariates)) {
  maps[[region]]$cov[[covariates[i]]] <-
    melt(som.sum[[region]]$cov,
         id.vars = c("som_x", "som_y"),
         variable.name = "covariate") |>
    subset(covariate == covariates[i]) |>
    ggplot() + 
      geom_raster(mapping = aes(
                                fill = value,
                                x = som_x, y = som_y),
                  interpolate = FALSE) +
      scale_fill_continuous_sequential(palette = "Viridis"
                                       ,rev = FALSE,
                                       ,trans = cov.scale[[covariates[i]]]$trans
                                       ,breaks = cov.scale[[covariates[i]]]$breaks
                                       ,limits = cov.scale[[covariates[i]]]$limits
                                       ,labels = cov.scale[[covariates[i]]]$labels
                                       ,name = cov.scale[[covariates[i]]]$title
                                       ,oob = scales::squish
                                       ) +
      coord_fixed() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      map_guide_fill +
      labs(x = NULL, y = NULL) +
      map_theme +
      theme_leg_bottom +
      theme(legend.margin = margin(0, 0, 0, 0))
}

# Forest loss rate

maps[[region]]$fl <- 
  ggplot(som.sum[[region]]$fl) +
  geom_raster(mapping = aes(
                            fill = forestloss.mean,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    limits = c(0, 0.0405),
                                    labels = scales::label_percent()) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill


maps[[region]]$fl.fac.q <-
  som.sum[[region]]$fl[type == "fac",
                         .(year.label, som_x, som_y,
                           forestloss.q5, forestloss.q25,
                           forestloss.q50 = forestloss.median,
                           forestloss.q75, forestloss.q95)] |>
  melt(id.vars = c("year.label", "som_x", "som_y"),
       measure.vars = measure(quant = \(x) as.numeric(x)/100, pattern = "forestloss\\.q(.*)"),
       value.name = "forestloss") |>
  merge(quant.lab) |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = forestloss,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    # breaks = seq(0, 0.20, 0.05),
                                    # labels = c("0%", "5%", "10%",
                                    #             "15%", "≥ 20%"),
                                    # limits = c(-0, 0.20),
                                    oob = scales::squish) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(quant.label), switch = "y") +
  map_theme +
  theme_leg_bottom


maps[[region]]$fl.cf.q <-
  som.sum[[region]]$fl[type == cf.name,
                         .(year.label, som_x, som_y,
                           forestloss.q5, forestloss.q25,
                           forestloss.q50 = forestloss.median,
                           forestloss.q75, forestloss.q95)] |>
  melt(id.vars = c("year.label", "som_x", "som_y"),
       measure.vars = measure(quant = \(x) as.numeric(x)/100, pattern = "forestloss\\.q(.*)"),
       value.name = "forestloss") |>
  merge(quant.lab) |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = forestloss,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    # breaks = seq(0, 0.20, 0.05),
                                    # labels = c("0%", "5%", "10%",
                                    #             "15%", "≥ 20%"),
                                    # limits = c(-0, 0.20),
                                    oob = scales::squish) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(quant.label), switch = "y") +
  map_theme +
  theme_leg_bottom


som.sum[[region]]$diff$type.label <- "Difference\n(factual vs. counterfactual)"

maps[[region]]$diff <- 
  som.sum[[region]]$diff |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.01, 0.01, 0.005),
                                   ,labels = c("≤ -1.0%", "-0.5%", "0%",
                                               "+0.5%", "≥ +1.0%"),
                                   ,limits = c(-0.01, 0.01)
                                   ,oob = scales::squish
                                   ) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = mar.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom
  # theme(strip.text.y = element_blank(),
  #       strip.background.y = element_blank())



diff.sum.ci <-
  rbind(som.sum[[region]]$diff[(diff.q25 < 0 & diff.q75 < 0) | (diff.q25 > 0 & diff.q75 > 0),
                               .(year.label, som_x, som_y, diff.mean,
                                 ci.label = "50% credible interval")],
        som.sum[[region]]$diff[(diff.q5 < 0 & diff.q95 < 0) | (diff.q5 > 0 & diff.q95 > 0),
                               .(year.label, som_x, som_y, diff.mean,
                                 ci.label = "90% credible interval")])
diff.sum.ci[, ci.label := factor(ci.label)]


maps[[region]]$diff.ci <- 
  diff.sum.ci |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.01, 0.01, 0.005),
                                   ,labels = c("≤ -1.0%", "-0.5%", "0%",
                                               "+0.5%", "≥ +1.0%"),
                                   ,limits = c(-0.01, 0.01)
                                   ,oob = scales::squish
                                   ) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = mar.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(ci.label), switch = "y") +
  map_theme +
  theme_leg_bottom


# IUFRO 2024
maps[[region]]$diff <- 




## EXPORT ######################################################################


# TODO:
# study area
# Combined
# Forestloss quantiles
# Differnce quantiles
# Difference CIs


# IUFRO 2024
cov.combined <-
  wrap_plots(maps$amz$cov)

png("../results/figures/fcne_covid_cov.png", width = 7, height = 4.5, unit = "in", res = 600)
cov.combined
dev.off()

som.sum[[region]]$diff[sign(diff.q2.5) == sign(diff.q97.5)]

# IUFRO

png("../results/figures/fcne_covid_cov_diff.png", width = 7, height = 3, unit = "in", res = 600)
  som.sum[[region]]$diff |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.01, 0.01, 0.005),
                                   ,labels = c("≤ -1.0%", "-0.5%", "0%",
                                               "+0.5%", "≥ +1.0%"),
                                   ,limits = c(-0.01, 0.01)
                                   ,oob = scales::squish
                                   ) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = mar.title,
       x = NULL, y = NULL) +
  facet_grid(cols = vars(year.label), rows = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom
  # theme(strip.text.y = element_blank(),
  #       strip.background.y = element_blank())
dev.off()

  th.change <- 0.003

  cov.sel <- names(cov.scales$amz)

  som.comp.all <- copy(som.sum$amz$cov)
  som.comp.all[, type := "all"]

  som.comp.inc <-
    som.sum$amz$cov[som.sum$amz$diff[is.na(year) & diff.mean > th.change, .(som_x, som_y)],
                    on = c("som_x", "som_y")]
  som.comp.inc[, type := "increase"]

  sel.del <-
    som.sum$amz$diff[!is.na(year), .(som_x, som_y, year = paste0("y", year), diff.mean)] |>
    dcast(som_x + som_y ~ year) |>
    _[(y2020 < -th.change & (y2021 > th.change | y2022 > th.change)) |
      (y2021 < -th.change & y2022 > th.change),
      .(som_x, som_y)]
  som.comp.del <-
    som.sum$amz$cov[sel.del, on = c("som_x", "som_y")]
  som.comp.del[, type := "delayed"]

  som.comp.dec <-
    som.sum$amz$cov[som.sum$amz$diff[is.na(year) & diff.mean < -th.change, .(som_x, som_y)],
                    on = c("som_x", "som_y")]
  som.comp.dec[, type := "decrease"]

  som.comp <- rbind(som.comp.all, som.comp.del, som.comp.inc, som.comp.dec)
  som.comp[, type := factor(type, levels = c("all", "delayed", "increase", "decrease"))]
  som.comp <-
    melt(som.comp[, c("som_x", "som_y", "type", cov.sel), with = FALSE],
         id.vars = c("som_x", "som_y", "type"),
         measure.vars = cov.sel,
         variable.name = "cov",
         value.name = "value")

  som.comp[, .(cov.mean = mean(value), cov.sd = sd(value), cov.med = median(value)), by = .(type, cov)]

  ggplot(som.comp) +
    geom_violin(aes(x = value, y = type)) +
    facet_wrap(vars(cov), scales = "free_x") +
    scale_x_continuous(trans = "log1p")



  som.sum$amz$diff[is.na(year) & diff.mean < -0.001] |>
  ggplot() +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = som_x, y = som_y),
              interpolate = FALSE) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.01, 0.01, 0.005),
                                   ,labels = c("≤ -1.0%", "-0.5%", "0%",
                                               "+0.5%", "≥ +1.0%"),
                                   ,limits = c(-0.01, 0.01)
                                   ,oob = scales::squish
                                   ) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  map_guide_fill +
  labs(fill = mar.title, x = NULL, y = NULL) +
  facet_grid(cols = vars(year.label), rows = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom
  # theme(strip.text.y = element_blank(),
  #       strip.background.y = element_blank())

diff.avg <- som.sum[[region]]$diff[is.na(year)]
diff.avg[, diff.change := fcase(diff.mean > 0, "increase", diff.mean < 0, "decrease")]
# diff.avg[, diff.change := fcase(diff.mean > 0.005, "increase", diff.mean < -0.005, "decrease")]
# diff.avg[, diff.change := fcase(sign(diff.q5) == sign(diff.q95) & diff.mean > 0, "increase",
#                                 sign(diff.q5) == sign(diff.q95) & diff.mean < 0, "decrease")]
merge(som.sum$amz$cov,
      diff.avg[, .(som_x, som_y, diff.change)]) |>
_[, lapply(.SD, median), by = "diff.change", .SDcols = covariates]




png(file.fig.geo, width = 7, height = 7, unit = "in", res = 600)
maps.combined
dev.off()

maps.combined <-
  with(maps,
       (amz$fl + cam$areas + plot_layout(guides = "collect")) / 
       (amz$fl + cam$fl + plot_layout(guides = "collect")) /
       (amz$mar + cam$mar + plot_layout(guides = "collect")) +
       plot_annotation(tag_levels = list(c("A", "", "B", "", "C", ""))) &
       map_theme
       )

png(file.fig.maps, width = 7, height = 7, unit = "in", res = 600)
maps.combined
dev.off()

maps.fl.ci.combined <-
  with(maps.q5,
       (amz$fl + (cam$fl + theme(plot.title = element_blank())))) /
  with(maps.q25,
       (amz$fl + (cam$fl + theme(plot.title = element_blank())))) /
  with(maps.q75,
       (amz$fl + (cam$fl + theme(plot.title = element_blank())))) /
  with(maps.q95,
       (amz$fl + (cam$fl + theme(plot.title = element_blank())))) +
  plot_annotation(tag_levels = list(c("A", "", "B", "", "C", "", "D", ""))) +
  plot_layout(guides = "collect") &
  map_theme

png(file.fig.fl.ci, width = 7, height = 9.5, unit = "in", res = 600)
maps.fl.ci.combined
dev.off()

maps.mar.ci.combined <-
  with(maps.q5,
       (amz$mar + (cam$mar + theme(plot.title = element_blank())))) /
  with(maps.q25,
       (amz$mar + (cam$mar + theme(plot.title = element_blank())))) /
  with(maps.q75,
       (amz$mar + (cam$mar + theme(plot.title = element_blank())))) /
  with(maps.q95,
       (amz$mar + (cam$mar + theme(plot.title = element_blank())))) +
  plot_annotation(tag_levels = list(c("A", "", "B", "", "C", "", "D", ""))) +
  plot_layout(guides = "collect") &
  map_theme

png(file.fig.mar.ci, width = 7, height = 9.5, unit = "in", res = 600)
maps.mar.ci.combined
dev.off()






var.fl.ex <-
  c("som_x",
    "som_y",
    forestloss.mean


rast.fl

for(i in seq_along(regions)){
  region <- regions[i]

  fl.reg <- som.sum[[region]]$fl
  fl.grid <-
    CJ(som_x = seq(min(fl.reg$som_x),
                         max(fl.reg$som_x),
                         map.res[[region]]),
     som_y = seq(min(fl.reg$som_y),
                        max(fl.reg$som_y),
                        map.res[[region]]))

  rast.fl <-
    fl.reg[fl.grid, on = c("som_x", "som_y")
            ][, -"group.id"] |>
    st_as_stars(dims = c("som_x", "som_y")) |>
    st_set_crs(crs.ea[[region]]) |>
    merge(name = "band") |>
    setNames("forestloss")

  write_stars(rast.fl, paste0(path.geo, region, suf.geo.fl))
  
  mar.reg <- som.sum[[region]]$mar
  mar.grid <-
    CJ(som_x = seq(min(mar.reg$som_x),
                         max(mar.reg$som_x),
                         map.res[[region]]),
     som_y = seq(min(mar.reg$som_y),
                        max(mar.reg$som_y),
                        map.res[[region]]))

  rast.mar <-
    mar.reg[mar.grid, on = c("som_x", "som_y")
            ][, -"group.id"] |>
    st_as_stars(dims = c("som_x", "som_y")) |>
    st_set_crs(crs.ea[[region]]) |>
    merge(name = "band") |>
    setNames("marginal")

    som.sum[[region]]$mar

  write_stars(rast.mar, paste0(path.geo, region, suf.geo.mar))

}



