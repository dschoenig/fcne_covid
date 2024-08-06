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
overwrite <- FALSE

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.raw <- paste0(path.data, "raw/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.limit <- paste0(path.data.raw, "forestloss/", region, ".limit.gpkg")
file.areas <- paste0(path.data.raw, "forestloss/", region, ".areas_union.gpkg")
file.bg.adm0 <- paste0(path.data, "auxiliary/gadm36_levels_neotropics.gpkg")
cf.name <- "cf4"
files.agg <- paste0(path.agg, region, ".geo.", c("fac", cf.name), ".rds")

file.fig.geo <- paste0(path.figures, region, ".geo.png")
file.data.vis <- paste0(path.data.vis, region, ".geo.rds")

regions <- c("amz")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]


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
  theme(panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid = element_line(colour = "grey75"),
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


## EFFECTS IN GEOGRAPHICAL SPACE ###############################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {
  
  poly <- list()
  geo.sum <- list()

  # Prepare data for maps

  # bg_adm0 <- st_read(paste0(path.data, "auxiliary/gadm36_levels.gpkg"),
  #                    query = "SELECT * FROM level0 
  #                             WHERE GID_0 IN ('ABW', 'AIA', 'ARG', 'ATG', 'BES', 
  #                                             'BHS', 'BLM', 'BLZ', 'BOL', 'BRA', 
  #                                             'BRB', 'CHL', 'COL', 'CRI', 'CUB', 
  #                                             'CUW', 'CYM', 'DMA', 'DOM', 'ECU', 
  #                                             'FLK', 'GLP', 'GRD', 'GTM', 'GUF', 
  #                                             'GUY', 'HND', 'HTI', 'JAM', 'KNA', 
  #                                             'LCA', 'MAF', 'MEX', 'MSR', 'MTQ', 
  #                                             'NIC', 'PAN', 'PER', 'PRI', 'PRY', 
  #                                             'SGS', 'SLV', 'SUR', 'SXM', 'TCA', 
  #                                             'TTO', 'UMI', 'URY', 'VCT', 
  #                                             'VEN', 'VGB', 'VIR', 'XCL')"
  #                    )

  bg_adm0 <- st_read(file.bg.adm0)

  bg_coasts <- st_union(bg_adm0, is_coverage = TRUE)

  # Treatment of auxilary geospatial data

  message("Auxiliary geospatial data …")

  poly[[region]] <- list()

  poly[[region]]$areas <-
    st_read(file.areas) |>
    st_transform(crs.ea[[region]])
  poly[[region]]$areas$it_type <-
    factor(poly[[region]]$areas$it_type, levels = c("recognized", "not_recognized"))

  poly[[region]]$limit <-
    st_read(file.limit) |>
    st_transform(crs.ea[[region]])

  poly[[region]]$bg <- st_transform(bg_adm0, crs.ea[[region]])
  poly[[region]]$bg_coasts <- st_transform(bg_coasts, crs.ea[[region]])
  poly[[region]]$bg_is_limit <- st_intersection(poly[[region]]$bg, poly[[region]]$limit)

   
  # Forest cover loss rate

  message("Rate of forest cover loss …")

  agg.fl <-
    lapply(files.agg, readRDS) |>
    rbindlist()

  geo.sum[[region]]$fl <-
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
           by = .(type, year, ea_east.bin, ea_north.bin)]

  geo.sum[[region]]$fl <-
    geo.sum[[region]]$fl |>
    merge(type.lab, by = "type") |>
    merge(year.lab, by = "year")
  setorder(geo.sum[[region]]$fl, type.label, year.label)


  # Difference in forest cover loss rate between factual and
  # counterfactual


  message("Difference between factual and counterfactual …")

  agg.diff <-
    dcast(agg.fl, year + ea_east.bin + ea_north.bin + .draw ~ type, value.var = "forestloss")
  agg.diff[, diff := fac - cf.col, env = list(cf.col = cf.name)]

  geo.sum[[region]]$diff <-
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
             by = .(year, ea_east.bin, ea_north.bin)]

  geo.sum[[region]]$diff <-
    geo.sum[[region]]$diff |>
    merge(year.lab, by = "year")
  setorder(geo.sum[[region]]$diff, year.label)

  rm(agg.fl, agg.diff)
  gc()

  message(paste0("Storing summaries in `", file.data.vis, "` …"))

  list(poly = poly,
       geo.sum = geo.sum) |>
  saveRDS(file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## MAPS ########################################################################


message(paste0("Preparing maps for region `", region, "` …"))
maps <- list()

# Study region: ITs and PAs

maps[[region]]$areas <- 
  ggplot() +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = poly[[region]]$limit, fill = "grey95", colour = NA) +
  geom_sf(data = subset(poly[[region]]$areas, is.na(pa_type)),
          aes(fill = it_type), colour = NA) +
  geom_sf_pattern(data = subset(poly[[region]]$areas, is.na(it_type)),
                  aes(pattern = pa_type), colour = c.map[3], linewidth = 0.2,, fill = NA,
                  pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.005,
                  pattern_density = 0.35) +
  geom_sf(data = poly[[region]]$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_manual(values = c.map[1:2],
                    breaks = c("recognized", "not_recognized"),
                    labels = c("Recognized", "Not recognized"),
                    name = "Indigenous territories",
                    guide = guide_legend(byrow = TRUE,
                                         order = 1)) +
  scale_pattern_manual(values = c("indirect_use" = "stripe", "direct_use" = "none"),
                             breaks = c("indirect_use", "direct_use"),
                             labels = c("IUCN category I-IV", "IUCN category V-VI"),
                             name = "Protected areas",
                             guide = guide_legend(override.aes = list(pattern_spacing = 0.01,
                                                                      linewidth = 1),
                                                  theme = theme(legend.key.spacing.y = unit(2, "pt")),
                                                  byrow = TRUE,
                                                  order = 2)) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  labs(x = NULL, y = NULL) +
  map_theme

# Forest loss rate

maps[[region]]$fl <- 
  # ggplot(geo.sum[[region]]$fl[year == 2020 & type == "fac"]) +
  # ggplot(geo.sum[[region]]$fl[year == 2020]) +
  ggplot(geo.sum[[region]]$fl) +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = forestloss.mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    limits = c(0, 0.2),
                                    labels = scales::label_percent()) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_graticule = "SE") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill


maps[[region]]$fl.fac.q <-
  geo.sum[[region]]$fl[type == "fac",
                         .(year.label, ea_east.bin, ea_north.bin,
                           forestloss.q5, forestloss.q25,
                           forestloss.q50 = forestloss.median,
                           forestloss.q75, forestloss.q95)] |>
  melt(id.vars = c("year.label", "ea_east.bin", "ea_north.bin"),
       measure.vars = measure(quant = \(x) as.numeric(x)/100, pattern = "forestloss\\.q(.*)"),
       value.name = "forestloss") |>
  merge(quant.lab) |>
  ggplot() +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = forestloss,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    breaks = seq(0, 0.20, 0.05),
                                    labels = c("0%", "5%", "10%",
                                                "15%", "≥ 20%"),
                                    limits = c(-0, 0.20),
                                    oob = scales::squish) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_graticule = "SE") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  map_guide_fill +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(quant.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill


maps[[region]]$fl.cf.q <-
  geo.sum[[region]]$fl[type == cf.name,
                         .(year.label, ea_east.bin, ea_north.bin,
                           forestloss.q5, forestloss.q25,
                           forestloss.q50 = forestloss.median,
                           forestloss.q75, forestloss.q95)] |>
  melt(id.vars = c("year.label", "ea_east.bin", "ea_north.bin"),
       measure.vars = measure(quant = \(x) as.numeric(x)/100, pattern = "forestloss\\.q(.*)"),
       value.name = "forestloss") |>
  merge(quant.lab) |>
  ggplot() +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = forestloss,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                    rev = FALSE,
                                    breaks = seq(0, 0.20, 0.05),
                                    labels = c("0%", "5%", "10%",
                                                "15%", "≥ 20%"),
                                    limits = c(-0, 0.20),
                                    oob = scales::squish) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_graticule = "SE") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  map_guide_fill +
  labs(fill = fl.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(quant.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill


geo.sum[[region]]$diff$type.label <- "Difference\n(factual vs. counterfactual)"

maps[[region]]$diff <- 
  geo.sum[[region]]$diff |>
  ggplot() +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.10, 0.10, 0.05),
                                   ,labels = c("≤ -10%", "-5%", "0%",
                                               "+5%", "≥ +10%"),
                                   ,limits = c(-0.10, 0.10)
                                   ,oob = scales::squish
                                   ) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_graticule = "SE") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  map_guide_fill +
  labs(fill = mar.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(type.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill
  # theme(strip.text.y = element_blank(),
  #       strip.background.y = element_blank())



diff.sum.ci <-
  rbind(geo.sum[[region]]$diff[(diff.q25 < 0 & diff.q75 < 0) | (diff.q25 > 0 & diff.q75 > 0),
                               .(year.label, ea_east.bin, ea_north.bin, diff.mean,
                                 ci.label = "50% credible interval")],
        geo.sum[[region]]$diff[(diff.q5 < 0 & diff.q95 < 0) | (diff.q5 > 0 & diff.q95 > 0),
                               .(year.label, ea_east.bin, ea_north.bin, diff.mean,
                                 ci.label = "90% credible interval")])
diff.sum.ci[, ci.label := factor(ci.label)]


maps[[region]]$diff.ci <- 
  diff.sum.ci |>
  ggplot() +
  geom_sf(data = poly[[region]]$bg, fill = "grey30", colour = NA) +
  geom_raster(mapping = aes(
                            fill = diff.mean,
                            x = ea_east.bin, y = ea_north.bin),
              interpolate = FALSE) +
  geom_sf(data = poly[[region]]$bg_coasts, fill = NA, colour = "grey35", size = 0.3) +
  scale_fill_continuous_divergingx(palette = "Roma",
                                   ,mid = 0
                                   ,rev = TRUE,
                                   ,breaks = seq(-0.10, 0.10, 0.05),
                                   ,labels = c("≤ -10%", "-5%", "0%",
                                               "+5%", "≥ +10%"),
                                   ,limits = c(-0.10, 0.10)
                                   ,oob = scales::squish
                                   ) +
  coord_sf(crs = crs.ea[[region]], expand = FALSE, 
           xlim = map_xlim[[region]], ylim = map_ylim[[region]],
           label_graticule = "SE") +
  scale_x_continuous(breaks = seq(-170, 0, 10)) +
  scale_y_continuous(breaks = seq(-80, 30, 10)) +
  annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                   style = "ticks", text_cex = 0.5,
                   line_width = 0.5, text_family = "IBMPlexSans") +
  map_guide_fill +
  labs(fill = mar.title, x = NULL, y = NULL) +
  facet_grid(rows = vars(year.label), cols = vars(ci.label), switch = "y") +
  map_theme +
  theme_leg_bottom +
  map_guide_fill



## EXPORT ######################################################################


# TODO:
# study area
# Combined
# Forestloss quantiles
# Differnce quantiles
# Difference CIs

# IUFRO GEO
png("../results/figures/fcne_covid_geo.png", width = 7, height = 3, unit = "in", res = 600)
maps[[region]]$diff +
  facet_grid(cols = vars(year.label), rows = vars(type.label), switch = "y")
dev.off()

diff2020 <- 
  geo.sum[[region]]$diff[year == 2020, .(ea_east.bin, ea_north.bin, diff.mean)] |>
  st_as_stars()
diff2021 <- 
  geo.sum[[region]]$diff[year == 2021, .(ea_east.bin, ea_north.bin, diff.mean)] |>
  st_as_stars()
diff2022 <- 
  geo.sum[[region]]$diff[year == 2022, .(ea_east.bin, ea_north.bin, diff.mean)] |>
  st_as_stars()
diffavg <- 
  geo.sum[[region]]$diff[is.na(year), .(ea_east.bin, ea_north.bin, diff.mean)] |>
  st_as_stars()

diff.exp <- c(diff2020, diff2021, diff2022, diffavg)
names(diff.exp) <- c("2020", "2021", "2022", "Average")

diff.exp <-
  merge(diff.exp) |>
  setNames("diff.mean") |>
  st_set_dimensions(names = c("x", "y", "year")) |>
  st_set_crs(crs.ea$amz)

write_stars(diff.exp, "../results/geo/fcne_covid_geo.tif")

maps.combined <-
  with(maps, amz$fl + amz$diff) +
  plot_annotation(tag_levels = list(c("A", "B")))


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
  c("ea_east.bin",
    "ea_north.bin",
    forestloss.mean


rast.fl

for(i in seq_along(regions)){
  region <- regions[i]

  fl.reg <- geo.sum[[region]]$fl
  fl.grid <-
    CJ(ea_east.bin = seq(min(fl.reg$ea_east.bin),
                         max(fl.reg$ea_east.bin),
                         map.res[[region]]),
     ea_north.bin = seq(min(fl.reg$ea_north.bin),
                        max(fl.reg$ea_north.bin),
                        map.res[[region]]))

  rast.fl <-
    fl.reg[fl.grid, on = c("ea_east.bin", "ea_north.bin")
            ][, -"group.id"] |>
    st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
    st_set_crs(crs.ea[[region]]) |>
    merge(name = "band") |>
    setNames("forestloss")

  write_stars(rast.fl, paste0(path.geo, region, suf.geo.fl))
  
  mar.reg <- geo.sum[[region]]$mar
  mar.grid <-
    CJ(ea_east.bin = seq(min(mar.reg$ea_east.bin),
                         max(mar.reg$ea_east.bin),
                         map.res[[region]]),
     ea_north.bin = seq(min(mar.reg$ea_north.bin),
                        max(mar.reg$ea_north.bin),
                        map.res[[region]]))

  rast.mar <-
    mar.reg[mar.grid, on = c("ea_east.bin", "ea_north.bin")
            ][, -"group.id"] |>
    st_as_stars(dims = c("ea_east.bin", "ea_north.bin")) |>
    st_set_crs(crs.ea[[region]]) |>
    merge(name = "band") |>
    setNames("marginal")

    geo.sum[[region]]$mar

  write_stars(rast.mar, paste0(path.geo, region, suf.geo.mar))

}



