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

file.data.proc <- paste0(path.data.proc, region, ".data.proc.rds")
file.som <- paste0(path.base, "models/som/", region, ".som.1e6.rds")
file.area <- paste0(path.data, "processed/", region, ".sumstats.area.rds")
cf.name <- "cf1"
files.agg <- paste0(path.agg, region, ".dis.som.", c("fac", cf.name), ".rds")

file.fig.som.area.mar <- paste0(path.figures, region, ".som.area.mar.png")
file.fig.som.area.mar.post <- paste0(path.figures, region, ".som.area.mar.post.png")
file.fig.som.area.fac.post <- paste0(path.figures, region, ".som.area.fac.post.png")
file.fig.som.area.cf1.post <- paste0(path.figures, region, ".som.area.cf1.post.png")
file.fig.cov.mar.oscale <- paste0(path.figures, region, ".cov.mar.oscale.png")
file.fig.cov.mar.qscale <- paste0(path.figures, region, ".cov.mar.qscale.png")
file.fig.cov.comp.oscale <- paste0(path.figures, region, ".cov.comp.oscale.png")
file.fig.cov.comp.qscale <- paste0(path.figures, region, ".cov.comp.qscale.png")
file.fig.cov.som <- paste0(path.figures, region, ".cov.som.png")
file.data.vis <- paste0(path.data.vis, region, ".som.rds")


## SETUP ###############################################################

## COLOURS AND LABELS

## Colours for tenure categories
col.div <- diverging_hcl(20, palette = "Purple-Green")
c.map <- col.div[c(3,6,17)]


## MAP SETUP

base.size <- 7 
base.family <- "IBMPlexSansCondensed"
map_theme <-  
  theme_minimal(base_family = "IBMPlexSans", base_size = 7) +
  theme(panel.background = element_rect(fill = "grey99", colour = NA),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = rel(1),
                                    lineheight = rel(1.15),
                                    margin = margin(t = 0.75*base.size)),
        axis.title.y = element_text(size = rel(1),
                                    margin = margin(r = 0.75*base.size),
                                    lineheight = rel(1.15)),
        axis.text.x = element_text(color = "black",
                                   size = rel(1),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(1),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2)),
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
                                  margin = margin(
                                                  5,
                                                  5,
                                                  5,
                                                  5)),
        strip.background = element_rect(fill = "gray93", colour = NA),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.85),
                                     margin = margin(t = 6, b = 3)),
        plot.tag = element_text(margin = margin(t = 0, r = 6, b = 6, l = 0),
                                size = rel(1.5)),
        plot.tag.location = "margin"
        )


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

map_guide_fill_bottom <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.text = element_text(hjust = 0.5),
                                             legend.text.position = "top",
                                             legend.key.height = unit(5, "pt"),
                                             legend.key.width = unit(65, "pt"),
                                             legend.ticks.length = unit(1.5, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE,
                               order = 1))


base.size <- 7 
cov_theme <-
  theme_light(base_family = "IBMPlexSansCondensed",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(0.9),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size/2)),
        axis.title.y = element_text(size = rel(0.9),
                                    margin = margin(r = base.size/2)),
        axis.text.x = element_text(color = "black",
                                   size = rel(0.9),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(0.9),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2),
                                   hjust = 1),
        axis.ticks = element_line(colour = "grey30"),
        legend.title = element_text(size = rel(0.9),
                                    margin = margin(b = base.size/2)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size, "pt"),
        legend.text = element_text(size = rel(0.8), margin = margin(l = base.size/3,
                                                                    t = base.size/2,
                                                                    b = base.size/2)),
        # legend.margin = margin(l = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(base.size, "pt"),
        panel.spacing.y = unit(base.size, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1),
                                     margin = margin(b = 2*base.size)),
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(size = rel(0.8),
                                  lineheight = rel(1),
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  color = "black",
                                  margin = margin(
                                                  0.75*base.size,
                                                  0.75*base.size,
                                                  0.75*base.size,
                                                  0.75*base.size)),
        strip.background = element_rect(fill = "gray93", colour = NA)
        # strip.background = element_rect(fill = NA, colour = NA)
  )


cov.var <-
  c("elevation", "slope", "sx", "cmi_min",
    "dist_set", "dist_roads", "dist_rivers",
    "dens_pop", "dens_roads", "travel_time")
cov.labs <-
  c("Elevation\n[m]", "Slope\n[deg]",
    "Agricultural suitability\nindex", "Minimum climate moisture\nindex [kg m⁻² month⁻¹]",
    "Distance to built-up areas\n[m]", "Distance to roads\n[m]", "Distance to rivers\n[m]",
    "Population density\n[km⁻¹]", "Road density\n[m km⁻¹]",
    "Travel time to cities\n[min]")
cov.labs <- factor(cov.labs, levels = cov.labs)
names(cov.labs) <- cov.var
cov.labs.short <-
  c("Elevation", "Slope",
    "Agr. suit", "CMI min.",
    "Dist. to BUA", "Dist. to Rd.", "Dist. to Riv.",
    "Pop. dens.", "Rd. dens.",
    "Trav. time")
# cov.labs.short <-
#   c("Elevation", "Slope",
#     "AgSuit", "CMImin",
#     "DistBuilt", "DistRd", "DistRiv",
#     "DensPop", "DensRd",
#     "TravTime")
cov.labs.short <- factor(cov.labs.short, levels = cov.labs.short)
names(cov.labs.short) <- cov.var
cov.labs.short2 <-
  c("Elevation\n[m]", "Slope\n[deg]",
    "Agr. suit.\n[index]", "CMI min.\n[kg m⁻² month⁻¹]",
    "Dist. to BUA\n[km]", "Dist. to Rd.\n[km]", "Dist. to Riv.\n[km]",
    "Pop. dens.\n[km⁻²]", "Rd. dens.\n[m km⁻²]",
    "Trav. time\n[min]")
# cov.labs.short2 <-
#   c("Elevation\n[m]", "Slope\n[deg]",
#     "AgSuit\n[index]", "CMImin\n[kg m⁻² month⁻¹]",
#     "DistBuilt\n[km]", "DistRd\n[km]", "DistRiv\n[km]",
#     "DensPop\n[km⁻²]", "DensRd\n[m km⁻²]",
#     "TravTime\n[min]")
cov.labs.short2 <- factor(cov.labs.short2, levels = cov.labs.short2)
names(cov.labs.short2) <- cov.var

type.lab <-
  data.table(est.type = c("fac", "cf1", "mar",
                      "area.prop.fac", "area.prop.cf1", "area.prop.mar"),
             type.label = c(
                            "Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal difference\n(pre-pandemic conditionsd\nvs. COVID-19 pandemic)",
                            "Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal difference\n(pre-pandemic conditions\nvs. COVID-19 pandemic)"
                            ))
type.lab[, type.label := factor(type.label, levels = type.label[1:3])]


type.comp.lab <-
  data.table(est.type = c("area.fac.median", "area.cf1.median", "area.mar.median"),
             type.label = c(
                            "Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal difference\n(pre-pandemic conditions\nvs. COVID-19 pandemic)"
                            ))
type.comp.lab[, type.label := factor(type.label, levels = type.label[1:3])]



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
area.mar.title.l <- "Marginal difference between actual and expected disturbances (km² per SOM unit)"
area.mar.title.2l <- "Marginal difference between actual and\nexpected disturbances (km² per SOM unit)"
area.mar.title <- wrap_title(area.mar.title.l)
prob.title.l <- "Posterior probability that marginal difference is < 0 or > 0"
prob.title <- wrap_title(prob.title.l)
area.fac.title.l <- "Area affected by disturbances under factual conditions (km² per SOM unit)"
area.fac.title.2l <- "Area affected by disturbances under\nfactual conditions (km² per SOM unit)"
area.fac.title <- wrap_title(area.fac.title.l)
area.cf.title.l <- "Area affected by disturbances under counterfactual conditions (km² per SOM unit)"
area.cf.title.2l <- "Area affected by disturbances under\ncounterfactual conditions (km² per SOM unit)"
area.cf.title <- wrap_title(area.cf.title.l)
tmf.title.l <- "Proportion of undisturbed tropical moist forest before 2020"
tmf.title <- wrap_title(tmf.title.l)
x.title <- "SOM dimension 1"
y.title <- "SOM dimension 2"

# area.mar.lim <- list(amz = c(-100, 100))
# area.mar.breaks <- list(amz = seq(-100, 100, 25))
# area.mar.labels <- list(amz = c("≤ –100", "", "–50", "", "0", "", "+50", "", "≥ +100"))
area.mar.lim <- list(amz = c(-50, 50))
area.mar.breaks <- list(amz = seq(-50, 50, 12.5))
area.mar.labels <- list(amz = c("≤ –50", "", "–25", "", "0", "", "+25", "", "≥ +50"))
area.comp.lim <- list(amz = c(0, 200))
area.comp.breaks <- list(amz = seq(0, 200, 50))
area.comp.labels <- list(amz = c("0", "50", "100", "150", "≥ 200"))
area.fac.lim <- list(amz = c(0, 200))
area.fac.breaks <- list(amz = seq(0, 200, 50))
area.fac.labels <- list(amz = c("0", "50", "100", "150", "≥ 200"))
area.cf.lim <- list(amz = c(0, 200))
area.cf.breaks <- list(amz = seq(0, 200, 50))
area.cf.labels <- list(amz = c("0", "50", "100", "150", "≥ 200"))
tmf.lim <- list(amz = c(0, 1))
tmf.breaks <- list(amz = seq(0, 1, 0.1))
tmf.labels <- list(amz = format(tmf.breaks$amz), nsmall = 1)
tmf.labels$amz[2*(1:5)] <- ""


## EFFECTS IN COVARIATE SPACE ##########################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {

  area.undist <-
    readRDS(file.area)$undist.som |>
    _[, .(year, som_x, som_y, area)]

  agg.mar <-
    rbindlist(lapply(files.agg, readRDS)) |>
    dcast(... ~ type, value.var = "disturbance") |>
    _[, mar := fac - cf1] |>
    merge(area.undist, by = c("year", "som_x", "som_y"), all.x = TRUE)


  agg.area.y <-
    agg.mar[!is.na(year) & !is.na(area),
             .(year, som_x, som_y, .draw,
               area.prop.mar = mar * area,
               area.prop.fac = fac * area,
               area.prop.cf1 = cf1 * area)]
  agg.area.all <-
    agg.area.y[,
               .(area.prop.mar = mean(area.prop.mar),
                 area.prop.fac = mean(area.prop.fac),
                 area.prop.cf1 = mean(area.prop.cf1)),
               by = .(som_x, som_y, .draw)]
  agg.area <- rbind(agg.area.all, agg.area.y, fill = TRUE)


  agg.post <-
    merge(agg.mar[, -c("area")], agg.area,
          by = c("year", "som_x", "som_y", ".draw"))

  rm(agg.mar, agg.area, agg.area.all, agg.area.y)
  gc()

  sum.cols <- c("som_x", "som_y", "year")
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
  setorder(agg.sum, year.label, som_x, som_y)

  agg.sum[, mar.prob.bs := max(c(mar.prob.pos, mar.prob.neg)), by = .I]
  agg.sum[, mar.un := fcase(
                            mar.prob.bs < 0.75, "high",
                            mar.prob.bs >= 0.75 & mar.prob.bs < 0.95, "moderate",
                            mar.prob.bs >= 0.95, "low")]
  agg.sum[, mar.un := factor(mar.un, levels = c("high", "moderate", "low"))]

  

  som <- readRDS(file.som)
  data.proc <- readRDS(file.data.proc)

  cov.names <- names(som$scale$mean)
  rm(som)

  dat.sel <- c("id", "year", "som_x", "som_y", cov.names)
  data.proc <- data.proc[, ..dat.sel]
  gc()
  
  n.quant <- c(20, 50, 100)
  draws.range <- agg.post[, range(.draw)]
  draws.chunk <- chunk_seq(draws.range[1], draws.range[2], 50)

  # draws.chunk <- chunk_seq(1, 12, 4) 
  # n.quant <- c(20, 100)

  cov.tab.q <- list()
  cov.sum.q <- list()

  for(q in seq_along(n.quant)) {

    cov.sum.i <- list()
    cov.tab.i <- list()
    message("Processing quantiles ", q, "/", length(n.quant), " …")

    for(i in seq_along(cov.names)) {
      cov.foc <- cov.names[i]
      cov.foc.bin <- paste0(cov.foc, ".bin")
      message("Processing covariate `", cov.foc, "` (", i, "/", length(cov.names), ") …")
      cov.env <- list(cov.col = cov.foc,
                      cov.bin.col = cov.foc.bin)
      cov.p <- seq(1/n.quant[q], 1, length.out = n.quant[q])
      cov.q.raw <- unname(data.proc[,
                          quantile(cov.col, probs = cov.p),
                          env = cov.env])
      cov.q <- unique(cov.q.raw)
      cov.dat <-
        data.proc[, .(id, year, som_x, som_y, cov.col), env = cov.env]
      cov.breaks <- unique(c(min(cov.dat[[cov.foc]], na.rm = TRUE), cov.q))
      cov.mp <- cov.breaks[-1] - (cov.breaks[-1] - shift(cov.breaks, 1)[-1])/2
      cov.tab.i[[i]] <-
        merge(data.table(cov = cov.foc,
                         quant = cov.q.raw,
                         prob = cov.p),
              data.table(quant = cov.breaks[-1],
                         cov.bin = cov.mp), all = TRUE)
      cov.dat[,
              cov.bp := cut(cov.col, cov.breaks,
                            labels = FALSE,
                            include.lowest = TRUE),
              env = cov.env]
      cov.dat[,
              cov.bin.col := cov.mp[cov.bp],
              env = cov.env]
      cov.w <-
        rbind(
              cov.dat[,
                      .(w = as.double(.N)),
                      by = .(year, som_x, som_y, cov.bin.col),
                      env = cov.env],
              cov.dat[year >= 2020,
                      .(w = as.double(.N)),
                      by = .(som_x, som_y, cov.bin.col),
                      env = cov.env],
              fill = TRUE)
      cov.w[, w := w/sum(w), by = .(year, cov.bin.col), env = cov.env]
      rm(cov.dat); gc()
      setkeyv(cov.w, c("year", cov.foc.bin))
      cov.sum.j <- list()
      prog <- txtProgressBar(min = 0,
                             max = length(draws.chunk$from),
                             char = "=", width = NA, title = "Progress", style = 3)
      for(j in seq_along(draws.chunk$from)) {
        cov.post <-
          agg.post[.(.draw = draws.chunk$from[j]:draws.chunk$to[j]),
                   on = ".draw"] |>
          merge(cov.w,
                by = c("year", "som_x", "som_y"),
                allow.cartesian = TRUE)
        setkeyv(cov.post, c("year", cov.foc.bin, ".draw"))
        cov.sum.j[[j]]  <-
          cov.post[,
                .(cov = cov.foc,
                  mar = sum(mar*w),
                  fac = sum(fac*w),
                  cf1 = sum(cf1*w),
                  area.prop.mar = sum(area.prop.mar*w),
                  area.prop.fac = sum(area.prop.fac*w),
                  area.prop.cf1 = sum(area.prop.cf1*w)
                  ),
                by = .(year, cov.bin.col, .draw),
                env = cov.env]
        setnames(cov.sum.j[[j]], cov.foc.bin, "cov.bin")
        rm(cov.post); gc()
        setTxtProgressBar(prog, j)
      }
      close(prog)
      cov.sum.i[[i]] <- rbindlist(cov.sum.j)
      rm(cov.w, cov.sum.j); gc()
    }
    cov.sum.q[[q]] <- rbindlist(cov.sum.i)
    cov.sum.q[[q]][, n.quant := n.quant[q]]
    cov.tab.q[[q]] <- rbindlist(cov.tab.i)
    cov.tab.q[[q]][, n.quant := n.quant[q]]
    rm(cov.sum.i, cov.tab.i); gc()
  }

  cov.sum <- rbindlist(cov.sum.q)
  setcolorder(cov.sum, c("n.quant", "cov", "cov.bin"))

  cov.tab <- rbindlist(cov.tab.q)
  setcolorder(cov.tab, c("n.quant", "cov"))

  rm(cov.sum.q, cov.tab.q); gc()

  cov.sum <- merge(cov.sum, cov.tab, all = TRUE)

  agg.data <- list(agg.sum = agg.sum, cov.sum = cov.sum, area.undist = area.undist)
  saveRDS(agg.data, file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## SOM #################################################################

message(paste0("Preparing SOM maps for region `", region, "` …"))
som.maps <- list()

som.maps$area.mar.year <-
    agg.sum |>
    ggplot() +
    geom_raster(aes(x = som_x, y = som_y,
                    fill = as.numeric(area.mar.median),
                    alpha = mar.prob.bs)) +
    scale_fill_continuous_divergingx(palette = "Roma",
                                 mid = 0,
                                 rev = TRUE,
                                 limits = area.mar.lim[[region]],
                                 breaks = area.mar.breaks[[region]],
                                 labels = area.mar.labels[[region]],
                                 oob = scales::squish
                                 ) +
    scale_alpha_binned(limits = c(0.5, 1), range = c(0.1, 1), breaks = c(0.5, 0.75, 0.95, 1)) +
    scale_x_continuous(expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(add = 1)) +
    coord_fixed() +
    map_guide_fill +
    map_guide_alpha +
    facet_wrap(vars(year.label)) +
    labs(
         fill = area.mar.title,
         alpha = prob.title,
         x = x.title, y = y.title) +
    map_theme


# agg.sum.comp.l <-
#   melt(agg.sum[,
#               .(som_x, som_y, year.label,
#                 area.fac.median, area.cf1.median, area.mar.median,
#                 mar.prob.bs)],
#       measure.vars = paste0("area.", c("fac", "cf1", "mar"), ".median"),
#       variable.name = "est.type", value.name = "estimate") |>
#   merge(type.comp.lab, by = "est.type")
# som.maps$area.mar.year <-
#   agg.sum.comp.l[est.type %in% c("area.mar.median")] |>
#   ggplot() +
#   geom_raster(aes(x = som_x, y = som_y,
#                   fill = as.numeric(estimate),
#                   alpha = mar.prob.bs)) +
#   scale_fill_continuous_divergingx(palette = "Roma",
#                                 mid = 0,
#                                 rev = TRUE,
#                                 limits = area.mar.lim[[region]],
#                                 breaks = area.mar.breaks[[region]],
#                                 labels = area.mar.labels[[region]],
#                                 oob = scales::squish
#                                 ) +
#   scale_alpha_binned(limits = c(0.5, 1), range = c(0.1, 1), breaks = c(0.5, 0.75, 0.95, 1)) +
#   scale_x_continuous(expand = expansion(add = 1)) +
#   scale_y_continuous(expand = expansion(add = 1)) +
#   coord_fixed() +
#   map_guide_fill +
#   map_guide_alpha +
#   facet_grid(cols = vars(year.label), rows = vars(type.label)) +
#   labs(
#        tag = "A",
#        fill = area.mar.title,
#        alpha = prob.title,
#        x = x.title, y = y.title) +
#   map_theme
# som.maps$area.comp.year <-
#   agg.sum.comp.l[est.type %in% c("area.fac.median", "area.cf1.median")] |>
#   ggplot() +
#   geom_raster(aes(x = som_x, y = som_y,
#                   fill = as.numeric(estimate))) +
#   scale_fill_viridis_c(option = "C",
#                         limits = area.comp.lim[[region]],
#                         breaks = area.comp.breaks[[region]],
#                         labels = area.comp.labels[[region]],
#                         oob = scales::squish) +
#   scale_x_continuous(expand = expansion(add = 1)) +
#   scale_y_continuous(expand = expansion(add = 1)) +
#   coord_fixed() +
#   map_guide_fill +
#   facet_grid(cols = vars(year.label), rows = vars(type.label)) +
#   labs(tag = "B",
#        fill = area.mar.title,
#        x = x.title, y = y.title) +
#   map_theme


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


som.maps$area.mar.post <-
  agg.sum.area.mar.l |>
  ggplot() +
  geom_raster(aes(x = som_x, y = som_y,
                  fill = as.numeric(estimate))) +
  scale_fill_continuous_divergingx(palette = "Roma",
                               mid = 0,
                               rev = TRUE,
                               limits = area.mar.lim[[region]],
                               breaks = area.mar.breaks[[region]],
                               labels = area.mar.labels[[region]],
                               oob = scales::squish
                               ) +
  coord_fixed() +
  map_guide_fill +
  facet_grid(rows = vars(est.label), cols = vars(year.label)) +
  labs(
       fill = area.mar.title,
       alpha = prob.title,
       x = x.title, y = y.title) +
  map_theme



som.maps$area.fac.post <-
  agg.sum.area.fac.l |>
  ggplot() +
  geom_raster(aes(x = som_x, y = som_y,
                  fill = as.numeric(estimate))) +
    scale_fill_viridis_c(option = "C",
                         limits = area.fac.lim[[region]],
                         breaks = area.fac.breaks[[region]],
                         labels = area.fac.labels[[region]],
                         oob = scales::squish) +
  coord_fixed() +
  map_guide_fill +
  facet_grid(rows = vars(est.label), cols = vars(year.label)) +
  labs(
       fill = area.fac.title,
       x = x.title, y = y.title) +
  map_theme

som.maps$area.cf1.post <-
  agg.sum.area.cf1.l |>
  ggplot() +
  geom_raster(aes(x = som_x, y = som_y,
                  fill = as.numeric(estimate))) +
    scale_fill_viridis_c(option = "C",
                         limits = area.cf.lim[[region]],
                         breaks = area.cf.breaks[[region]],
                         labels = area.cf.labels[[region]],
                         oob = scales::squish) +
  coord_fixed() +
  map_guide_fill +
  facet_grid(rows = vars(est.label), cols = vars(year.label)) +
  labs(
       fill = area.cf.title,
       x = x.title, y = y.title) +
  map_theme


## COVARIATES ##########################################################

cov.sum[, cov.label := cov.labs[as.character(cov)]]
cov.sum[, cov.label.short := cov.labs.short[as.character(cov)]]
cov.sum[, cov.label.short2 := cov.labs.short2[as.character(cov)]]

cov.sum.comp.l <-
  melt(cov.sum, measure.vars = c("fac", "cf1", "mar",
                                "area.prop.fac", "area.prop.cf1", "area.prop.mar"),
      variable.name = "est.type", value.name = "estimate") |>
  merge(type.lab) |>
  merge(year.lab, by = "year")



col.est <- c("#e41a1c", "#377eb8", "#984ea3")
names(col.est) <- levels(type.lab$type.label)


cov.scales <-
  list(
    elevation = list(
                     trans = scales::log_trans(),
                     breaks = c(30, 100, 300, 1000, 3000),
                     labels = scales::label_number(big.mark = " "),
                     lim = c(12.5, 3500)),
       slope = list(
                    trans = scales::log_trans(),
                    breaks = c(1, 3, 10, 30),
                    labels = scales::label_number(big.mark = " "),
                    lim = c(0.25, 57.5)),
       sx = list(
                 trans = "identity",
                 breaks = waiver(),
                 labels = scales::label_number(big.mark = " "),
                 lim = c(400, 7250)),
       cmi_min = list(
                      trans = "identity",
                      breaks = waiver(),
                      labels = scales::label_number(big.mark = " "),
                       lim = c(NA, NA)),
       dist_set = list(trans = scales::log_trans(),
                       breaks = c(1000, 10000, 1e5),
                       labels = scales::label_number(big.mark = " "),
                       lim = c(250, 1e5)),
       dist_roads = list(trans = scales::log_trans(),
                         breaks = c(1000, 10000, 1e5),
                         labels = scales::label_number(big.mark = " "),
                         lim = c(400, 1.8e5)),
       dist_rivers = list(trans = scales::log_trans(),
                          breaks = c(30, 100, 300, 1000, 3000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(15, 6500)),
       dens_pop = list(trans = scales::log_trans(),
                       # breaks = unique(round(exp(seq(0, log(cov.sum[cov == "dens_pop", max(cov.val)]),
                       #                               length.out = 4))/50) * 50),
                       breaks = c(1e-3, 1, 1e3),
                       labels = scales::label_log(),
                       lim = c(NA, 8500)),
       dens_roads = list(
                         trans = "identity",
                         breaks = waiver(),
                         labels = waiver(),
                         lim = c(0, 200)),
       travel_time = list(
                          trans = scales::log_trans(),
                          breaks = c(100, 300, 1000, 3000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(30, 5250))
  )



cov.rug <-
  rbind(unique(cov.sum[n.quant == 50 & cov != "dens_roads", .(cov, prob, quant, cov.bin)]),
        unique(cov.sum[n.quant == 100 & cov == "dens_roads", .(cov, prob, quant, cov.bin)]))
cov.rug[, cov.min := min(cov.bin, na.rm = TRUE), by = .(cov)]
cov.rug[is.na(cov.bin), cov.bin := cov.min]


# cov.rug[,
#         .(
#           qmin = min(quant, na.rm = TRUE),
#           covmin = min(cov.bin, na.rm = TRUE),
#           qmax = max(quant, na.rm = TRUE),
#           covmax = max(cov.bin, na.rm = TRUE)
#           ),
#         by = "cov"]


cov.mar.oscale <- list()
cov.comp.oscale <- list()
# cov.mar.qscale <- list()
# cov.comp.qscale <- list()

for(i in seq_along(cov.var)) {

  quant.sel <- ifelse(cov.var[i] == "dens_roads", 100, 50)

  cov.mar.oscale[[i]] <-
    cov.sum.comp.l[!is.na(.draw) & n.quant == quant.sel &
                  est.type %in% c("mar") & cov == cov.var[i]] |>
    ggplot() +
      stat_lineribbon(aes(x = cov.bin, y = estimate,
                          colour = type.label,
                          fill = type.label,
                          fill_ramp = after_stat(level)),
                      linewidth = 0.3,
                      .width = c(0.5, 0.9),
                      point_interval = median_hdci) +
      geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", colour = "grey5") +
      geom_rug(data = cov.rug[cov == cov.var[i]],
               aes(x = cov.bin), sides = "b", alpha = 0.3,
               length = unit(base.size/2, "pt")) +
      scale_colour_manual(values = col.est) +
      scale_fill_manual(values = col.est) +
      scale_fill_ramp_discrete(range = c(0.25, 0.65), labels = rev(c("0.50", "0.90"))) +
      scale_x_continuous(
                        trans = cov.scales[[cov.var[i]]]$trans,
                        breaks = cov.scales[[cov.var[i]]]$breaks,
                        labels = cov.scales[[cov.var[i]]]$labels,
                        ) +
      scale_y_continuous(
                         breaks = scales::breaks_pretty(n = 3),
                         labels = scales::label_number(scale = 100, accuracy = 0.01,
                                                       style_positive = "plus", suffix = " pp."),
                         expand = expansion(mult = c(0.2, 0.1)),
                         ) +
      coord_cartesian(
                      xlim = cov.scales[[cov.var[i]]]$lim,
                      ) +
      facet_grid(rows = vars(cov.label.short2), cols = vars(year.label), scales = "free") +
      guides(
            #  colour = guide_legend(order = 1),
            fill = "none", colour = "none", fill_ramp = "none"
            ) +
      labs(x = "Covariate value",
           y = "Change in disturbance rate (under COVID-19 pandemic vs. pre-pandemic conditions)",
           colour = "", fill = "",
           fill_ramp = "Uncertainty interval") +
      cov_theme
      
  cov.comp.oscale[[i]] <-
    cov.sum.comp.l[!is.na(.draw) & n.quant == quant.sel &
                  est.type %in% c("fac", "cf1") & cov == cov.var[i]] |>
    ggplot() +
      stat_lineribbon(aes(x = cov.bin, y = estimate,
                          colour = type.label,
                          fill = type.label,
                          fill_ramp = after_stat(level)),
                      linewidth = 0.3,
                      .width = c(0.9),
                      point_interval = median_hdci) +
      geom_rug(data = cov.rug[cov == cov.var[i]],
               aes(x = cov.bin), sides = "b", alpha = 0.3,
               length = unit(base.size/2, "pt")) +
      scale_colour_manual(values = col.est) +
      scale_fill_manual(values = col.est) +
      # scale_fill_ramp_discrete(range = c(0.25, 0.65), labels = rev(c("0.50", "0.90"))) +
      scale_x_continuous(
                        trans = cov.scales[[cov.var[i]]]$trans,
                        breaks = cov.scales[[cov.var[i]]]$breaks,
                        labels = cov.scales[[cov.var[i]]]$labels,
                        ) +
      scale_y_continuous(
                         breaks = scales::breaks_pretty(n = 3),
                         labels = scales::label_number(scale = 100, accuracy = 0.1,
                                                       style_positive = "none", suffix = " %"),
                         expand = expansion(mult = c(0.2, 0.1)),
                         ) +
      expand_limits(y = 0) +
      coord_cartesian(
                      xlim = cov.scales[[cov.var[i]]]$lim,
                      ) +
      facet_grid(rows = vars(cov.label.short2), cols = vars(year.label), scales = "free") +
      guides(
            #  colour = guide_legend(order = 1),
            fill = "none", colour = "none", fill_ramp = "none"
            ) +
      labs(x = "Covariate value",
           y = "Disturbance rate",
           colour = "", fill = "",
           fill_ramp = "Uncertainty interval") +
      cov_theme

  # cov.mar.qscale[[i]] <-
  #   cov.sum.comp.l[!is.na(.draw) & n.quant == quant.sel &
  #                 est.type %in% c("mar") & cov == cov.var[i]] |>
  #   ggplot() +
  #     stat_lineribbon(aes(x = prob, y = estimate,
  #                         colour = type.label,
  #                         fill = type.label,
  #                         fill_ramp = after_stat(level)),
  #                     linewidth = 0.3,
  #                     .width = c(0.5, 0.9),
  #                     point_interval = median_hdci) +
  #     geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", colour = "grey5") +
  #     scale_colour_manual(values = col.est) +
  #     scale_fill_manual(values = col.est) +
  #     scale_fill_ramp_discrete(range = c(0.25, 0.65), labels = rev(c("0.50", "0.90"))) +
  #     scale_x_continuous(
  #                        limits = c(0, 1)
  #                       ) +
  #     scale_y_continuous(
  #                        breaks = scales::breaks_pretty(n = 3),
  #                        labels = scales::label_number(scale = 100, accuracy = 0.01,
  #                                                      style_positive = "plus", suffix = " pp."),
  #                        expand = expansion(mult = 0.1),
  #                        ) +
  #     facet_grid(rows = vars(cov.label.short2), cols = vars(year.label), scales = "free") +
  #     guides(
  #           #  colour = guide_legend(order = 1),
  #           fill = "none", colour = "none", fill_ramp = "none"
  #           ) +
  #     labs(x = "Covariate quantile",
  #          y = "Change in disturbance rate (under COVID-19 pandemic vs. pre-pandemic conditions)",
  #          colour = "", fill = "",
  #          fill_ramp = "Uncertainty interval") +
  #     cov_theme

  # cov.comp.qscale[[i]] <-
  #   cov.sum.comp.l[!is.na(.draw) & n.quant == quant.sel &
  #                 est.type %in% c("fac", "cf1") & cov == cov.var[i]] |>
  #   ggplot() +
  #     stat_lineribbon(aes(x = prob, y = estimate,
  #                         colour = type.label,
  #                         fill = type.label,
  #                         fill_ramp = after_stat(level)),
  #                     linewidth = 0.3,
  #                     .width = c(0.5, 0.9),
  #                     point_interval = median_hdci) +
  #     scale_colour_manual(values = col.est) +
  #     scale_fill_manual(values = col.est) +
  #     scale_fill_ramp_discrete(range = c(0.25, 0.65), labels = rev(c("0.50", "0.90"))) +
  #     scale_x_continuous(
  #                        limits = c(0, 1)
  #                       ) +
  #     scale_y_continuous(
  #                        breaks = scales::breaks_pretty(n = 3),
  #                        labels = scales::label_number(scale = 100, accuracy = 0.1,
  #                                                      style_positive = "none",
  #                                                      suffix = " %"),
  #                        expand = expansion(mult = c(0.05, 0.1)),
  #                        ) +
  #     expand_limits(y = 0) +
  #     facet_grid(rows = vars(cov.label.short2), cols = vars(year.label), scales = "free") +
  #     guides(
  #           #  colour = guide_legend(order = 1),
  #           fill = "none", colour = "none", fill_ramp = "none"
  #           ) +
  #     labs(x = "Covariate quantile",
  #         y = "Change in disturbance rate (under COVID-19 pandemic vs. pre-pandemic conditions)",
  #           colour = "", fill = "",
  #           fill_ramp = "Uncertainty interval") +
  #     cov_theme

}

names(cov.mar.oscale) <- cov.var
names(cov.comp.oscale) <- cov.var
# names(cov.mar.qscale) <- cov.var
# names(cov.comp.qscale) <- cov.var



## COVARIATES OVER SOM #################################################


som <- readRDS(file.som)
som.map <-
  get_mapping(som, prefix = "som_") |>
  melt(measure.vars = cov.var, variable.name = "cov", value.name = "cov.val")
som.map[, cov.label := cov.labs[as.character(cov)]]

cov.scales.som <-
  list(
    elevation = list(
                     trans = scales::log_trans(),
                     breaks = c(10, 100, 1000),
                     labels = scales::label_number(big.mark = " "),
                     lim = c(10, 3500)),
       slope = list(
                    trans = scales::log_trans(),
                    breaks = c(1, 3, 10, 30),
                    labels = scales::label_number(big.mark = " "),
                    lim = c(1, 55)),
       sx = list(
                 trans = "identity",
                 breaks = waiver(),
                 labels = scales::label_number(big.mark = " "),
                 lim = c(0, 7250)),
       cmi_min = list(
                      trans = "identity",
                      breaks = waiver(),
                      labels = scales::label_number(big.mark = " "),
                      lim = c(NA, NA)),
       dist_set = list(trans = scales::log_trans(),
                       breaks = c(1000, 10000, 1e5),
                       labels = scales::label_number(big.mark = " "),
                      #  labels = scales::label_log(),
                       lim = c(250, 1e5)),
       dist_roads = list(trans = scales::log_trans(),
                         breaks = c(1000, 10000, 1e5),
                         labels = scales::label_number(big.mark = " "),
                        #  labels = scales::label_log(),
                         lim = c(300, 1.8e5)),
       dist_rivers = list(trans = scales::log_trans(),
                          breaks = c(100, 1000, 5000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(100, 5000)),
       dens_pop = list(trans = scales::log_trans(),
                       # breaks = unique(round(exp(seq(0, log(cov.sum[cov == "dens_pop", max(cov.val)]),
                       #                               length.out = 4))/50) * 50),
                       breaks = c(1e-3, 1, 1e3),
                       labels = c(0.001, 1, 1000),
                       lim = c(0.001, 8500)),
       dens_roads = list(
                         trans = "identity",
                         breaks = c(0, 500, 1000),
                         labels = c("0", "500", "≥ 1000"),
                         lim = c(0, 1000)),
       travel_time = list(
                          trans = scales::log_trans(),
                          breaks = c(10, 100, 1000),
                          labels = scales::label_number(big.mark = " "),
                          lim = c(10, 5750))
  )


som.map[(cov == "dens_pop" | cov == "travel_time") &
        cov.val <= 0,
        cov.val := .Machine$double.eps]


cov.sum.map <- list()
for(i in seq_along(cov.var)) {
  cov.sum.map[[i]] <-
    som.map[cov == cov.var[i]] |>
    ggplot() +
    geom_raster(aes(x = som_x, y = som_y,
                    fill = cov.val)) +
      scale_fill_viridis_c(option = "D",
                          limits = cov.scales.som[[cov.var[i]]]$lim,
                          trans = cov.scales.som[[cov.var[i]]]$trans,
                          breaks = cov.scales.som[[cov.var[i]]]$breaks,
                          labels = cov.scales.som[[cov.var[i]]]$labels,
                          oob = scales::squish) +
    coord_fixed() +
    facet_wrap(vars(cov.label)) +
    scale_x_continuous(expand = expansion(0, 0)) +
    scale_y_continuous(expand = expansion(0, 0)) +
    labs(fill = "",
         x = x.title, y = y.title) +
    map_guide_fill_bottom +
    map_theme
}
names(cov.sum.map) <- cov.var

p.cov.som <-
  wrap_plots(cov.sum.map, axes = "collect") &
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = rel(0.8),
                                  margin = margin(t = 7 / 4)),
        axis.text.y = element_text(size = rel(0.8),
                                  margin = margin(r = 7 / 4)),
        plot.margin = unit(c(7/2, 7/2, 7/2, 7/2), "pt"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        strip.text = element_text(size = rel(0.9), margin = margin(7/2, 7/3, 7/2, 7/3)),
        legend.margin = margin(b = 7, t = 0),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 0, b = 7/3)),
        legend.position = "bottom",
        legend.box.margin = margin(),
        legend.spacing = unit(0, "pt")
        )



## EXPORT ##############################################################

png(file.fig.som.area.mar, width = 5, height = 4, unit = "in", res = 600)
som.maps$area.mar.year
dev.off()


png(file.fig.som.area.mar.post, width = 7, height = 8.5, unit = "in", res = 600)
som.maps$area.mar.post
dev.off()

png(file.fig.som.area.fac.post, width = 7, height = 8.5, unit = "in", res = 600)
som.maps$area.fac.post
dev.off()

png(file.fig.som.area.cf1.post, width = 7, height = 8.5, unit = "in", res = 600)
som.maps$area.cf1.post
dev.off()




png(file.fig.cov.mar.oscale, width = 6, height = 7.25, unit = "in", res = 600)
((cov.mar.oscale$elevation +
  theme(strip.text.x = element_text(margin = margin(7/3, 7/3, 7/3, 7/3)))
) /
 (cov.mar.oscale$slope +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$sx +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$cmi_min +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$dist_set +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$dist_roads +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$dist_rivers +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$dens_pop +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$dens_roads +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.mar.oscale$travel_time +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 )
) +
plot_layout(ncol = 1, axis_title = "collect") &
theme(axis.title.y = element_text(margin = margin(r = 7)),
      axis.title.x = element_text(margin = margin(t = 7)),
      strip.text.y = element_text(size = rel(0.75), margin = margin(7/2, 7/3, 7/3, 7/2)),
      plot.margin = margin(7/2, 7, 7/2, 7)
      )
dev.off()


png(file.fig.cov.comp.oscale, width = 6, height = 7.25, unit = "in", res = 600)
((cov.comp.oscale$elevation +
  theme(strip.text.x = element_text(margin = margin(7/3, 7/3, 7/3, 7/3)))
) /
 (cov.comp.oscale$slope +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$sx +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$cmi_min +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$dist_set +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$dist_roads +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$dist_rivers +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$dens_pop +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$dens_roads +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 ) /
 (cov.comp.oscale$travel_time +
  theme(plot.subtitle = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
 )
) +
plot_layout(ncol = 1, axis_title = "collect") &
theme(axis.title.y = element_text(margin = margin(r = 7)),
      axis.title.x = element_text(margin = margin(t = 7)),
      strip.text.y = element_text(size = rel(0.75), margin = margin(7/2, 7/3, 7/3, 7/2)),
      plot.margin = margin(7/2, 7, 7/2, 7)
      )
dev.off()


# png(file.fig.cov.mar.qscale, width = 6, height = 7.25, unit = "in", res = 600)
# ((cov.mar.qscale$elevation +
#   theme(strip.text.x = element_text(margin = margin(7/3, 7/3, 7/3, 7/3)))
# ) /
#  (cov.mar.qscale$slope +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$sx +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$cmi_min +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$dist_set +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$dist_roads +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$dist_rivers +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$dens_pop +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$dens_roads +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.mar.qscale$travel_time +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  )
# ) +
# plot_layout(ncol = 1, axis_title = "collect") &
# theme(axis.title.y = element_text(margin = margin(r = 7)),
#       axis.title.x = element_text(margin = margin(t = 7)),
#       strip.text.y = element_text(size = rel(0.75), margin = margin(7/2, 7/3, 7/3, 7/2)),
#       plot.margin = margin(7/2, 7, 7/2, 7)
#       )
# dev.off()


# png(file.fig.cov.comp.qscale, width = 6, height = 7.25, unit = "in", res = 600)
# ((cov.comp.qscale$elevation +
#   theme(strip.text.x = element_text(margin = margin(7/3, 7/3, 7/3, 7/3)))
# ) /
#  (cov.comp.qscale$slope +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$sx +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$cmi_min +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$dist_set +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$dist_roads +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$dist_rivers +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$dens_pop +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$dens_roads +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  ) /
#  (cov.comp.qscale$travel_time +
#   theme(plot.subtitle = element_blank(),
#         strip.text.x = element_blank(),
#         strip.background.x = element_blank())
#  )
# ) +
# plot_layout(ncol = 1, axis_titles = "collect") &
# theme(axis.title.y = element_text(margin = margin(r = 7)),
#       axis.title.x = element_text(margin = margin(t = 7)),
#       strip.text.y = element_text(size = rel(0.75), margin = margin(7/2, 7/3, 7/3, 7/2)),
#       plot.margin = margin(7/2, 7, 7/2, 7)
#       )
# dev.off()




png(file.fig.cov.som, width = 7, height = 7, unit = "in", res = 600)
p.cov.som
dev.off()