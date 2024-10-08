args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(colorspace)
source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))

n.threads <- 4
region <- "amz"


setDTthreads(n.threads)

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

files.agg <- paste0(path.agg, region, ".dis.adm.", c("fac", "cf1"), ".rds")
# files.agg <- paste0(path.agg, region, ".adm.", c("fac", "cf1", "cf2", "cf3", "cf4"), ".rds")
# files.agg <- paste0(path.agg, region, ".adm.", c("fac", "cf1", "cf2", "cf4"), ".rds")
file.bl <- paste0(path.agg, region, ".dis.bl.rds")


file.fig.reg <- paste0(path.figures, region, ".dis.reg.png")
file.fig.adm <- paste0(path.figures, region, ".dis.adm.png")
file.data.vis <- paste0(path.data.vis, region, ".dis.adm.rds")



# col.div <- diverging_hcl(20, palette = "Purple-Green")
# c.type <- col.div[c(3, 6, 17, 14)]

base.size <- 7 
base.family <- "IBMPlexSansCondensed"
plot_theme <-
  theme_light(base_family = base.family,
              base_size = base.size) +
  theme(
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.25),
                                  margin = margin(l = 0, b = base.size, t = base.size/3)),
        plot.tag = element_text(face = "bold"),
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(margin = margin(t = base.size/2)),
        axis.title.y = element_text(margin = margin(r = base.size/2)),
        axis.text.y = element_text(color = "black", size = rel(1.2)),
        axis.text.x = element_blank(),
        axis.ticks = element_line(colour = "grey30"),
        axis.ticks.x = element_blank(),
        legend.title = element_text(margin = margin(b = base.size/3)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size*2.5, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size*3, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        strip.text = element_text(size = rel(1),
                                  # face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  # margin = margin(base.size,
                                  #                 base.size,
                                  #                 base.size,
                                  #                 base.size)),
                                  margin = margin(base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2)),
        strip.background = element_rect(fill = "gray93", colour = NA)
        # strip.background = element_rect(fill = NA, colour = NA)
  )

adm_guide_fill <-
  guides(fill = guide_colorbar(theme = theme(legend.ticks = element_line(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.frame = element_rect(colour = "grey5",
                                                                         linewidth = 0.2),
                                             legend.text = element_text(hjust = 1),
                                             legend.text.position = "right",
                                             legend.key.width = unit(7.5, "pt"),
                                             legend.key.height = unit(65, "pt"),
                                             legend.ticks.length = unit(2, "pt")),
                               draw.ulim = TRUE,
                               draw.llim = TRUE
                               ))


type.lab <-
  data.table(type = c("fac", "cf1"),
  # data.table(type = c("fac", "cf1", "cf4", "cf3", "cf2"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            # "C₁: No pandemic",
                            # "C₂: No change in\nspatial intensity",
                            # "C₃: No modification of\ncovariate effects",
                            # "C₄: No mortality effect"))
                            "Counterfactual\n(pre-pandemic conditions)"))
                            # "Counterfactual 2:\nNo change in spatial intensity",
                            # "Counterfactual 3:\nNo change of covariate effects",
                            # "Counterfactual 4:\nNo mortality effect"))
type.lab[, type.label := factor(type.label, levels = type.label)]

year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
             year.label = c("2020", "2021", "2022", "2020–2022 (average)"))
year.lab[, year.label := factor(year.label, levels = year.label)]

reg.lab <- list()
reg.lab$amz <-
  data.table(adm0 = c("BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN", NA),
             reg.label = c("Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela", 
                           "Amazon")
             )
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]



# col.type <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
# names(col.type) <- type.lab$type.label

col.type <- c("#D55E00", "#0072B2")
names(col.type) <- type.lab$type.label
# col.type <- c("#e41a1c", "#377eb8")
# names(col.type) <- type.lab$type.label

col.type.light <- c("#D55E004D", "#0072B24D")
names(col.type.light) <- type.lab$type.label
# col.type.light <- c("#e41a1c4d", "#377eb84d")
# names(col.type.light) <- type.lab$type.label




agg <-
  lapply(files.agg, readRDS) |>
  rbindlist() |>
  merge(type.lab, by = "type") |>
  merge(reg.lab[[region]], by = "adm0") |>
  merge(year.lab, by = "year")
setorder(agg, type.label, year.label, reg.label)

# bl <-
#   readRDS(file.bl)
# fl.bl <- bl[is.na(adm0) & is.na(year), mean(disturbance)]

fl.bl <- agg[is.na(adm0) & type == "cf1", .(disturbance.bl = mean(disturbance)), by = "year.label"]

fl.lim <- agg[is.na(adm0), max(disturbance)*100]

# p.adm <-
#   agg[is.na(adm0)] |>
#   ggplot() +
#     # geom_hline(yintercept = fl.bl * 100,
#     #            linetype = "dashed", linewidth = 0.3) +
#     geom_hline(data = fl.bl,
#                aes(yintercept = disturbance.bl * 100),
#                linetype = "dashed", linewidth = 0.3) +
#     stat_pointinterval(aes(y = disturbance*100, x = type.label, colour = type.label),
#                        point_interval = "mean_qi",
#                        point_size = 1.25,
#                        interval_size_range = c(0.5, 1.25), 
#                        fatten_point = 1.25, shape = 21, fill = "white",
#                        .width = c(0.5, 0.95)) +
#     scale_colour_manual(values = col.type) +
#     coord_cartesian(ylim = c(0.4, fl.lim), expand = FALSE) +
#     # coord_cartesian(ylim = c(0, 1.5), expand = FALSE) +
#     facet_wrap(vars(year.label), nrow = 1, scales = "free_y") +
#     labs(x = NULL, y = "Yearly forest loss rate (percent)", colour = NULL) +
#     plot_theme
# p.adm



p.reg <-
  agg[is.na(adm0)] |>
  ggplot() +
    # geom_hline(yintercept = fl.bl * 100,
    #            linetype = "dashed", linewidth = 0.3) +
    geom_hline(data = fl.bl,
               aes(yintercept = disturbance.bl * 100),
               linetype = "dashed", linewidth = 0.3) +
    stat_halfeye(aes(y = disturbance*100, x = type.label,
                     colour = type.label,
                     fill = type.label),
                       scale = 0.5,
                       point_interval = "mean_qi",
                       point_size = 1,
                       interval_size_range = c(0.5, 1.25), 
                       fatten_point = 1.25, shape = 21,
                       .width = c(0.5, 0.95),
                       point_fill = "white",
                       normalize = "panels",
                       n = 1001,
                       density = density_bounded(bandwidth = "SJ", bounder = "range", trim = TRUE)
                 ) +
    scale_colour_manual(values = col.type) +
    scale_fill_manual(values = col.type.light,
                      # guide = guide_legend(override.aes = list(point_fill = NA, colour = NA))) +
                      guide = "none") +
    coord_cartesian(ylim = c(0.35, fl.lim), expand = TRUE) +
    # coord_cartesian(ylim = c(0, 1.5), expand = FALSE) +
    facet_wrap(vars(year.label), nrow = 1, scales = "free_y") +
    labs(x = NULL, y = "Yearly forest loss rate (percent)", colour = NULL) +
    plot_theme
p.reg

png(file.fig.reg, width = 7, height = 2.25, unit = "in", res = 600)
p.reg
dev.off()


fl.bl.adm <- agg[!is.na(adm0) & type == "cf1",
                 .(disturbance.bl = mean(disturbance)),
                 by = c("year.label", "reg.label")]

p.adm <-
  agg[!is.na(adm0)] |>
  ggplot() +
    # geom_hline(yintercept = fl.bl * 100,
    #            linetype = "dashed", linewidth = 0.3) +
    geom_hline(data = fl.bl.adm,
               aes(yintercept = disturbance.bl * 100),
               linetype = "dashed", linewidth = 0.3) +
    stat_halfeye(aes(y = disturbance*100, x = type.label,
                     colour = type.label,
                     fill = type.label),
                       scale = 0.5,
                       point_interval = "mean_qi",
                       point_size = 1,
                       interval_size_range = c(0.5, 1.25), 
                       fatten_point = 1.25, shape = 21,
                       .width = c(0.5, 0.95),
                       point_fill = "white",
                       normalize = "panels",
                       # normalize = "xy"
                       n = 1001,
                       density = density_bounded(bandwidth = "SJ", bounder = "range", trim = TRUE)
                 ) +
    scale_colour_manual(values = col.type) +
    scale_fill_manual(values = col.type.light,
                      # guide = guide_legend(override.aes = list(point_fill = NA, colour = NA))) +
                      guide = "none") +
    # coord_cartesian(ylim = c(0.4, fl.lim), expand = TRUE) +
    # coord_cartesian(ylim = c(0, 1.5), expand = FALSE) +
    # facet_wrap(vars(year.label), nrow = 1, scales = "free_y") +
    facet_grid(rows = vars(reg.label), cols = vars(year.label), scales = "free_y") +
    labs(x = NULL, y = "Yearly forest loss rate (percent)", colour = NULL) +
    plot_theme
p.adm

png(file.fig.adm, width = 7, height = 8, unit = "in", res = 600)
p.adm
dev.off()


# merge(agg[is.na(adm0) & year > 2020 & type == "cf1", .(bl =
# mean(disturbance)), by = "year"],
#       agg[is.na(adm0) & year > 2020 & type == "fac", .(lossrate =
#       mean(disturbance)), by = "year"]) |>
# _[, .(lossyear = year, lossrate.diff = lossrate - bl, lossrate.diff.rel = (lossrate-bl)/bl)]






