args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(sf)
library(stars)
library(stringi)
library(ggplot2)
library(ggdist)
library(ggspatial)
library(ggpattern)
library(patchwork)
library(colorspace)

source("utilities.R")

region <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

n.threads <- 4
# region <- "amz"
# overwrite <- FALSE

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.raw <- paste0(path.data, "raw/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)
path.geo <- paste0(path.base, "results/geo/")
if(!dir.exists(path.geo)) dir.create(path.geo, recursive = TRUE)

file.data.proc <- paste0(path.data, "processed/", region, ".data.proc.rds")
file.limit <- paste0(path.data.raw, "forests/", region, ".limit.gpkg")
file.areas <- paste0(path.data.raw, "forests/", region, ".areas_union_2022.gpkg")
file.bg.adm0 <- paste0(path.data, "auxiliary/gadm41_levels_neotropics.gpkg")
file.bg.coasts <- paste0(path.data, "auxiliary/bg_coasts.gpkg")
file.hex <- paste0(path.data.raw, "forests/", region, ".hex_25.poly.gpkg")
file.area <- paste0(path.data.proc, region, ".sumstats.area.rds")
cf.name <- "cf1"
files.agg <- paste0(path.agg, region, ".dis.geo.", c("fac", cf.name), ".rds")

file.fig.areas <- paste0(path.figures, region, ".areas.png")
file.fig.geo <- paste0(path.figures, region, ".geo.png")
file.fig.geo.area.mar.post <- paste0(path.figures, region, ".geo.area.mar.post.png")
file.fig.geo.area.fac.post <- paste0(path.figures, region, ".geo.area.fac.post.png")
file.fig.geo.area.cf1.post <- paste0(path.figures, region, ".geo.area.cf1.post.png")
file.data.vis <- paste0(path.data.vis, region, ".geo.rds")
file.geo.gpkg <- paste0(path.geo, region, ".geo.gpkg")


## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]


## MAP SETUP

crs.ea <- 
  list(amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))

map_xlim <- list(amz = c(-240e4, 210e4))
map_ylim <- list(amz = c(-200e4, 185e4))

map_theme <-  
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(panel.background = element_rect(fill = "grey99", colour = NA),
        panel.grid = element_line(colour = "grey75"),
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_text(size = rel(0.75), hjust = 0,
                                    margin = margin(t = 3, b = 7),
                                    lineheight = rel(1)),
        legend.text = element_text(size = rel(0.65)),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(10, "pt"),
        strip.text = element_text(size = rel(1),
                                  lineheight = rel(1.15),
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  color = "black",
                                  margin = margin(5, 5, 5, 5)),
        strip.background = element_rect(fill = "gray93", colour = NA),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.85),
                                     margin = margin(t = 6, b = 3)),
        plot.tag = element_text(margin = margin(t = 0, r = 6, b = 6, l = 0),
                                size = rel(1.5)),
        plot.tag.location = "margin"
        )

# theme_leg_bottom <-
#   theme(
#       legend.position = "bottom",
#       legend.justification = "center",
#       legend.title = element_text(size = rel(0.9), hjust = 0.5,
#                                   margin = margin(t = 7, b = 7),
#                                   lineheight = rel(1)),
#       legend.text = element_text(size = rel(0.75)),
#       legend.text.position = "top",
#       legend.title.position = "bottom"
#       )


map_guide_fill <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.text = element_text(hjust = 1),
                                             legend.text.position = "right",
                                             legend.key.width = unit(7.5, "pt"),
                                             legend.key.height = unit(45, "pt"),
                                             legend.ticks.length = unit(2, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE,
                               order = 1))


map_guide_alpha <-
  guides(alpha = guide_bins(theme = theme(
                                          legend.ticks = element_blank(),
                                          legend.frame = element_blank(),
                                          legend.text = element_text(hjust = 1),
                                          legend.text.position = "right",
                                          legend.key.width = unit(7.5, "pt"),
                                          legend.key.height = unit(7.5, "pt")
                                          ),
                            override.aes = list(fill = "black"),
                            order = 2))

type.lab <-
  data.table(type = c("fac", "cf1", "mar"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal\n(absolute difference)"
                            ))
type.lab[, type.label := factor(type.label, levels = type.label)]


year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
             year.label = c("2020", "2021", "2022", "2020–2022 (average)"))
year.lab[, year.label := factor(year.label, levels = year.label)]

area.mar.est.lab <-
  data.table(est_type = paste0("area.mar.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
area.mar.est.lab[, est.label := factor(est.label, levels = est.label)]

area.fac.est.lab <-
  data.table(est_type = paste0("area.fac.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
area.fac.est.lab[, est.label := factor(est.label, levels = est.label)]

area.cf1.est.lab <-
  data.table(est_type = paste0("area.cf1.", c("q5", "q25", "median", "q75", "q95")),
             est.label = c("5% quantile", "25% quantile", "Median", "75% quantile", "95% quantile"))
area.cf1.est.lab[, est.label := factor(est.label, levels = est.label)]


wrap_title <- function(x, width = 25, ...) {
  paste(stri_wrap(x,
                  width = width,
                  whitespace_only = TRUE),
        collapse = "\n")
}

dist.title <- "Disturbance probability"
area.mar.title.l <- "Marginal difference between actual and expected disturbances (km² per tile)"
area.mar.title.2l <- "Marginal difference between actual and\nexpected disturbances (km² per tile)"
area.mar.title <- wrap_title(area.mar.title.l)
prob.title.l <- "Posterior probability that marginal difference is < 0 or > 0"
prob.title <- wrap_title(prob.title.l)
area.fac.title.l <- "Area affected by disturbances under factual conditions (km² per tile)"
area.fac.title.2l <- "Area affected by disturbances under\nfactual conditions (km² per tile)"
area.fac.title <- wrap_title(area.fac.title.l)
area.cf.title.l <- "Area affected by disturbances under counterfactual conditions (km² per tile)"
area.cf.title.2l <- "Area affected by disturbances under\ncounterfactual conditions (km² per tile)"
area.cf.title <- wrap_title(area.cf.title.l)
tmf.title.l <- "Proportion covered by undisturbed tropical moist forests before 2020"
tmf.title <- wrap_title(tmf.title.l)

area.mar.lim <- list(amz = c(-50, 50))
area.mar.breaks <- list(amz = seq(-50, 50, 12.5))
area.mar.labels <- list(amz = c("≤ –50", "", "–25", "", "0", "", "+25", "", "≥ +50"))
area.fac.lim <- list(amz = c(0, 100))
area.fac.breaks <- list(amz = seq(0, 100, 25))
area.fac.labels <- list(amz = c("0", "25", "50", "75", "≥ 100"))
area.cf.lim <- list(amz = c(0, 100))
area.cf.breaks <- list(amz = seq(0, 100, 25))
area.cf.labels <- list(amz = c("0", "25", "50", "75", "≥ 100"))
tmf.lim <- list(amz = c(0, 1))
tmf.breaks <- list(amz = seq(0, 1, 0.1))
tmf.labels <- list(amz = c("0 %", "", "20 %", "", "40 %", "", "60 %", "", "80 %", "", "100 %"))
# tmf.labels <- list(amz = format(tmf.breaks$amz), nsmall = 1)
# tmf.labels$amz[2*(1:5)] <- ""

## EFFECTS IN GEOGRAPHICAL SPACE ###############################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {
  
  poly <- list()

  # Prepare data for maps

  # bg_adm0 <- st_read(paste0(path.data, "auxiliary/gadm_410-levels.gpkg"),
  #                    query = "SELECT * FROM ADM_0 
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
  # st_make_valid(bg_adm0) |>
  # st_write(file.bg.adm0, append = FALSE)
  # bg_coasts <-
  #   st_make_valid(bg_adm0) |>
  #   st_union() |>
  #   st_exterior_ring()
  # st_write(bg_coasts, file.bg.coasts)

  bg_adm0 <- st_read(file.bg.adm0)
  bg_coasts <- st_read(file.bg.coasts)
  hex <- st_read(file.hex)
  hex$hex <- 1:nrow(hex)


  # Treatment of auxilary geospatial data

  message("Auxiliary geospatial data …")

  poly <- list()

  poly$areas <-
    st_read(file.areas) |>
    st_transform(crs.ea[[region]])
  poly$areas$it_type <-
    factor(poly$areas$it_type, levels = c("recognized", "not_recognized"))

  poly$limit <-
    st_read(file.limit) |>
    st_transform(crs.ea[[region]])

  poly$bg <- st_transform(bg_adm0, crs.ea[[region]])
  poly$bg_coasts <- st_transform(bg_coasts, crs.ea[[region]])
  poly$bg_is_limit <- st_intersection(poly$bg, poly$limit)
  poly$hex <- st_intersection(hex, poly$limit[, c("geom")])



  data.proc <- readRDS(file.data.proc)

  n.obs.fac <-
    rbind(
          data.proc[, .(n.fac = .N), by = .(hex)],
          data.proc[, .(n.fac = .N), by = .(year, hex)],
          fill = TRUE)
  setorder(n.obs.fac, year, hex)


  area.undist <- readRDS(file.area)$undist.hex

  agg.mar <-
    rbindlist(lapply(files.agg, readRDS)) |>
    dcast(... ~ type, value.var = "disturbance") |>
    _[, mar := fac - cf1] |>
    merge(area.undist, by = c("year", "hex"), all.x = TRUE)


  agg.area.y <-
    agg.mar[!is.na(year) & !is.na(area),
             .(year, hex, .draw,
               area.prop.mar = mar * area,
               area.prop.fac = fac * area,
               area.prop.cf1 = cf1 * area)]
  agg.area.all <-
    agg.area.y[,
               .(area.prop.mar = mean(area.prop.mar),
                 area.prop.fac = mean(area.prop.fac),
                 area.prop.cf1 = mean(area.prop.cf1)),
               by = .(hex, .draw)]
  agg.area <- rbind(agg.area.all, agg.area.y, fill = TRUE)


  agg.post <-
    merge(agg.mar[!is.na(area)], agg.area, by = c("year", "hex", ".draw"))

  rm(agg.mar, agg.area, agg.area.all, agg.area.y)
  gc()

  sum.cols <- c("hex", "year")
  agg.sum <-
    agg.post[,
             .(
               mar.mean = mean(mar),
               mar.median = median(mar),
               mar.sd = sd(mar),
               mar.mad = mad(mar),
               mar.q5 = quantile(mar, 0.05),
               mar.q25 = quantile(mar, 0.25),
               mar.q75 = quantile(mar, 0.75),
               mar.q95 = quantile(mar, 0.95),
               fac.mean = mean(fac),
               fac.median = median(fac),
               fac.sd = sd(fac),
               fac.mad = mad(fac),
               fac.q5 = quantile(fac, 0.025),
               fac.q25 = quantile(fac, 0.25),
               fac.q75 = quantile(fac, 0.75),
               fac.q95 = quantile(fac, 0.975),
               cf1.mean = mean(cf1),
               cf1.median = median(cf1),
               cf1.sd = sd(cf1),
               cf1.mad = mad(cf1),
               cf1.q5 = quantile(cf1, 0.025),
               cf1.q25 = quantile(cf1, 0.25),
               cf1.q75 = quantile(cf1, 0.75),
               cf1.q95 = quantile(cf1, 0.975),
               area.mar.mean = mean(area.prop.mar),
               area.mar.median = median(area.prop.mar),
               area.mar.sd = sd(area.prop.mar),
               area.mar.mad = mad(area.prop.mar),
               area.mar.q5 = quantile(area.prop.mar, 0.05),
               area.mar.q25 = quantile(area.prop.mar, 0.25),
               area.mar.q75 = quantile(area.prop.mar, 0.75),
               area.mar.q95 = quantile(area.prop.mar, 0.95),
               area.fac.mean = mean(area.prop.fac),
               area.fac.median = median(area.prop.fac),
               area.fac.sd = sd(area.prop.fac),
               area.fac.mad = mad(area.prop.fac),
               area.fac.q5 = quantile(area.prop.fac, 0.05),
               area.fac.q25 = quantile(area.prop.fac, 0.25),
               area.fac.q75 = quantile(area.prop.fac, 0.75),
               area.fac.q95 = quantile(area.prop.fac, 0.95),
               area.cf1.mean = mean(area.prop.cf1),
               area.cf1.median = median(area.prop.cf1),
               area.cf1.sd = sd(area.prop.cf1),
               area.cf1.mad = mad(area.prop.cf1),
               area.cf1.q5 = quantile(area.prop.cf1, 0.05),
               area.cf1.q25 = quantile(area.prop.cf1, 0.25),
               area.cf1.q75 = quantile(area.prop.cf1, 0.75),
               area.cf1.q95 = quantile(area.prop.cf1, 0.95),
               mar.prob.pos = sum(mar > 0)/.N,
               mar.prob.neg = sum(mar < 0)/.N),
               by = sum.cols] |>
  merge(year.lab, by = "year")
  setorder(agg.sum, year.label, hex)

  agg.sum[, mar.prob.bs := max(c(mar.prob.pos, mar.prob.neg)), by = .I]
  agg.sum[, mar.un := fcase(
                            mar.prob.bs < 0.75, "high",
                            mar.prob.bs >= 0.75 & mar.prob.bs < 0.95, "moderate",
                            mar.prob.bs >= 0.95, "low")]
  agg.sum[, mar.un := factor(mar.un, levels = c("high", "moderate", "low"))]


  agg.data <- list(poly = poly, agg.sum = agg.sum, area.undist = area.undist)
  saveRDS(agg.data, file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## MAPS ########################################################################


message(paste0("Preparing maps for region `", region, "` …"))
maps <- list()

# Study region: ITs and PAs

maps$areas <- 
  ggplot() +
  geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = poly$limit, fill = "grey95", colour = NA) +
  geom_sf(data = subset(poly$areas, is.na(pa_type)),
          aes(fill = it_type), colour = NA) +
  geom_sf_pattern(data = subset(poly$areas, is.na(it_type)),
                  aes(pattern = pa_type), colour = c.map[3], linewidth = 0.2,, fill = NA,
                  pattern_colour = NA, pattern_fill = c.map[3], pattern_spacing = 0.005,
                  pattern_density = 0.35) +
  geom_sf(data = poly$bg_is_limit, fill = NA, colour = "grey30", size = 0.3) +
  geom_sf(data = poly$bg_coasts, fill = NA, colour = "black", alpha = 0.3, size = 0.01) +
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


maps$area.undist <-
    merge(area.undist[year == 2020],
          as.data.table(poly$hex), sort = FALSE) |>
    ggplot() +
    geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly$limit, fill = "grey90", colour = NA) +
    geom_sf(mapping = aes(geometry = geom,
                          fill = as.numeric(area.rel)),
            colour = NA) +
    geom_sf(data = poly$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly$bg_coasts, fill = NA, colour = "black", alpha = 0.3, size = 0.01) +
    scale_fill_binned_sequential(palette = "YlGnBu",
                                 limits = tmf.lim[[region]],
                                 breaks = tmf.breaks[[region]],
                                 labels = tmf.labels[[region]]) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-100, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    labs(
         fill = tmf.title,
         x = NULL, y = NULL) +
    map_theme

maps$area.mar.year <-
    merge(agg.sum[ ,
                  .(year, year.label, hex, area.mar.median, mar.prob.bs)],
          as.data.table(poly$hex), sort = FALSE) |>
    ggplot() +
    geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly$limit, fill = "grey90", colour = NA) +
    geom_sf(mapping = aes(geometry = geom,
                          fill = as.numeric(area.mar.median),
                          alpha = mar.prob.bs),
                          # ),
            colour = NA) +
    geom_sf(data = poly$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly$bg_coasts,
            fill = NA, colour = "black",
            alpha = 0.3, size = 0.01) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.mar.lim[[region]],
                                 breaks = area.mar.breaks[[region]],
                                 labels = area.mar.labels[[region]],
                                 oob = scales::squish
                                 ) +
    scale_alpha_binned(limits = c(0.5, 1), range = c(0.1, 1), breaks = c(0.5, 0.75, 0.95, 1)) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-100, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    map_guide_alpha +
    facet_wrap(vars(year.label)) +
    labs(
         fill = area.mar.title,
         alpha = prob.title,
         x = NULL, y = NULL) +
    map_theme




agg.sum.area.mar.l <-
  melt(agg.sum,
       measure.vars = c("area.mar.q5", "area.mar.q25", "area.mar.median",
                        "area.mar.q75", "area.mar.q95"),
              variable.name = "est_type", value.name = "estimate") |>
  merge(area.mar.est.lab)
agg.sum.area.fac.l <-
  melt(agg.sum,
       measure.vars = c("area.fac.q5", "area.fac.q25", "area.fac.median",
                        "area.fac.q75", "area.fac.q95"),
              variable.name = "est_type", value.name = "estimate") |>
  merge(area.fac.est.lab)
agg.sum.area.cf1.l <-
  melt(agg.sum,
       measure.vars = c("area.cf1.q5", "area.cf1.q25", "area.cf1.median",
                        "area.cf1.q75", "area.cf1.q95"),
              variable.name = "est_type", value.name = "estimate") |>
  merge(area.cf1.est.lab)


maps$area.mar.post <-
    merge(agg.sum.area.mar.l[,
                         .(year, year.label, hex, est.label, estimate)],
          as.data.table(poly$hex), sort = FALSE) |>
    ggplot() +
    geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly$limit, fill = "grey90", colour = NA) +
    geom_sf(mapping = aes(geometry = geom,
                          fill = as.numeric(estimate)),
            colour = NA) +
    geom_sf(data = poly$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly$bg_coasts,
            fill = NA, colour = "black",
            alpha = 0.3, size = 0.01) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.mar.lim[[region]],
                                 breaks = area.mar.breaks[[region]],
                                 labels = area.mar.labels[[region]],
                                 oob = scales::squish
                                 ) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-100, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    map_guide_alpha +
    facet_grid(rows = vars(est.label), cols = vars(year.label)) +
    labs(
         fill = area.mar.title,
         x = NULL, y = NULL) +
    map_theme



maps$area.fac.post <-
    merge(agg.sum.area.fac.l[,
                             .(year, year.label, hex, est.label, estimate)],
          as.data.table(poly$hex), sort = FALSE) |>
    ggplot() +
    geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly$limit, fill = "grey90", colour = NA) +
    geom_sf(mapping = aes(geometry = geom,
                          fill = as.numeric(estimate)),
            colour = NA) +
    geom_sf(data = poly$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly$bg_coasts,
            fill = NA, colour = "black",
            alpha = 0.3, size = 0.01) +
    scale_fill_viridis_c(option = "C",
                         limits = area.fac.lim[[region]],
                         breaks = area.fac.breaks[[region]],
                         labels = area.fac.labels[[region]],
                         oob = scales::squish) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-100, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    map_guide_alpha +
    facet_grid(rows = vars(est.label), cols = vars(year.label)) +
    labs(
         fill = area.fac.title,
         x = NULL, y = NULL) +
    map_theme


maps$area.cf1.post <-
    merge(agg.sum.area.cf1.l[,
                             .(year, year.label, hex, est.label, estimate)],
          as.data.table(poly$hex), sort = FALSE) |>
    ggplot() +
    geom_sf(data = poly$bg, fill = "grey30", colour = "grey50", size = 0.3) +
    geom_sf(data = poly$limit, fill = "grey90", colour = NA) +
    geom_sf(mapping = aes(geometry = geom,
                          fill = as.numeric(estimate)),
            colour = NA) +
    geom_sf(data = poly$bg, fill = NA, colour = "grey50", size = 0.15) +
    geom_sf(data = poly$bg_coasts,
            fill = NA, colour = "black",
            alpha = 0.3, size = 0.01) +
    scale_fill_viridis_c(option = "C",
                         limits = area.cf.lim[[region]],
                         breaks = area.cf.breaks[[region]],
                         labels = area.cf.labels[[region]],
                         oob = scales::squish) +
    coord_sf(crs = crs.ea[[region]], expand = FALSE, 
             xlim = map_xlim[[region]], ylim = map_ylim[[region]]) +
    scale_x_continuous(breaks = seq(-100, 0, 10)) +
    scale_y_continuous(breaks = seq(-80, 30, 10)) +
    annotation_scale(width_hint = 0.125, height = unit(2, "mm"),
                     style = "ticks", text_cex = 0.5,
                     line_width = 0.5, text_family = "IBMPlexSansCondensed") +
    map_guide_fill +
    map_guide_alpha +
    facet_grid(rows = vars(est.label), cols = vars(year.label)) +
    labs(
         fill = area.cf.title,
         x = NULL, y = NULL) +
    map_theme





## EXPORT ##############################################################




png(file.fig.areas, width = 7, height = 2.75, unit = "in", res = 600)
maps$areas + theme(plot.margin = margin(r = 7)) +
  maps$area.undist +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list("A")) &
  theme(
        legend.title = element_text(margin = margin(t = 0, b = 5)),
        legend.margin = margin(),
        plot.tag = element_text(size = rel(1.15),
                                family = "IBMPlexSansCondensed",
                                face = "bold",
                                margin = margin(0, 7/2, 7, 0)))
dev.off()


png(file.fig.geo, width = 7, height = 5, unit = "in", res = 600)
maps$area.mar.year
dev.off()


png(file.fig.geo.area.mar.post, width = 7, height = 8.5, unit = "in", res = 600)
maps$area.mar.post
dev.off()

png(file.fig.geo.area.fac.post, width = 7, height = 8.5, unit = "in", res = 600)
maps$area.fac.post
dev.off()

png(file.fig.geo.area.cf1.post, width = 7, height = 8.5, unit = "in", res = 600)
maps$area.cf1.post
dev.off()


## EXPORT (GEODATA) ############################################################


message("Exporting geodata …")

agg.sf <-
  agg.sum[,
          .(year, hex,
            area.mar.median, area.mar.q5, area.mar.q25, area.mar.q75, area.mar.q95,
            area.fac.median, area.fac.q5, area.fac.q25, area.fac.q75, area.fac.q95,
            area.cf1.median, area.cf1.q5, area.cf1.q25, area.cf1.q75, area.cf1.q95,
            mar.median, mar.q5, mar.q25, mar.q75, mar.q95)] |>
  merge(as.data.table(poly$hex), sort = FALSE) |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON")

st_write(agg.sf, file.geo.gpkg, append = FALSE)

