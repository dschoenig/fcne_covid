args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(colorspace)
library(patchwork)
source("utilities.R")

region <- tolower(as.character(args[1]))
overwrite <- as.logical(as.character(args[2]))

region <- "amz"
overwrite <- TRUE

if(is.na(overwrite)) {
  overwrite <- FALSE
}

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.agg <- paste0(path.base, "models/gam/agg/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

files.agg <- paste0(path.agg, region, ".dis.adm.", c("fac", "cf1"), ".rds")
files.agg.nd <- paste0(path.agg, region, ".dis.adm.", c("fac", "cf1"), ".no_drought.rds")
file.area <- paste0(path.data.proc, region, ".sumstats.area.rds")

file.fig.adm.c <- paste0(path.figures, region, ".dis.adm.c.png")
file.fig.adm.all <- paste0(path.figures, region, ".dis.adm.all.png")
file.fig.adm.nd <- paste0(path.figures, region, ".dis.adm.nd.png")
file.data.vis <- paste0(path.data.vis, region, ".dis.adm.rds")



# col.div <- diverging_hcl(20, palette = "Purple-Green")
# c.type <- col.div[c(3, 6, 17, 14)]

base.size <- 7 
base.family <- "IBMPlexSansCondensed"
post_theme <-
  theme_light(base_family = "IBMPlexSansCondensed",
              base_size = base.size) +
  theme(
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(size = rel(1),
                                    lineheight = rel(1.15),
                                    margin = margin(t = base.size/2)),
        axis.title.y = element_text(size = rel(1),
                                    margin = margin(r = 0.75*base.size),
                                    lineheight = rel(1.15)),
        axis.text.x = element_text(color = "black",
                                   size = rel(1.1),
                                   margin = margin(t = base.size/2)),
        axis.text.y = element_text(color = "black",
                                   size = rel(1),
                                   lineheight = rel(1.15),
                                   margin = margin(r = base.size/2)),
        axis.ticks = element_line(colour = "grey30"),
        legend.title = element_text(size = rel(1),
                                    margin = margin(b = base.size/3)),
        legend.position = "right",
        legend.justification = "center",
        # legend.key.size = unit(base.size*1.5, "pt"),
        legend.text = element_text(size = rel(1), margin = margin(l = base.size/3,
                                                                  t = base.size/2,
                                                                  b = base.size/2)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        # panel.spacing.x = unit(base.size*2, "pt"),
        panel.spacing.y = unit(base.size, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0,
                                  # face = "bold",
                                  face = "plain",
                                  size = rel(1.5),
                                  margin = margin(l = 0, b = base.size*2, t = base.size/3)),
        plot.subtitle = element_text(size = rel(1.35),
                                     margin = margin(b = 14)),
        plot.tag = element_text(face = "plain", margin = margin(r = base.size/2)),
        strip.text = element_text(size = rel(1),
                                  lineheight = rel(1.15),
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


# adm_guide_fill <-
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


dr.lab <-
  data.table(dr_type = c("all", "no_drought"),
             dr.label = c("All areas", "Excl. drought"))
dr.lab[, dr.label := factor(dr.label, levels = dr.label)]

type.lab <-
  data.table(type = c("fac", "cf1", "mar"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal\n(absolute difference)"
                            ))
type.lab[, type.label := factor(type.label, levels = type.label)]

year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
             year.label = c("2020", "2021", "2022", "2020–2022\n(average)"))
year.lab[, year.label := factor(year.label, levels = year.label)]

reg.lab <- list()
reg.lab$amz <-
  data.table(adm0 = c(NA,
                      "BOL", "BRA", "COL", "ECU",
                      "GUF", "GUY", "PER", "SUR",
                      "VEN"),
             reg.label = c("Amazon",
                           "Bolivia", "Brazil", "Colombia", "Ecuador",
                           "French Guiana", "Guyana", "Peru", "Suriname",
                           "Venezuela")
             )
reg.lab$amz[, reg.label := factor(reg.label, levels = reg.label)]

abs.title <- "Proportion of area affected"
mar.title <- "Absolute marginal difference in proportion of area affected"
mar.title.2l <- "Absolute marginal difference in\nproportion of area affected"
abs.area.title <- "Area affected (km²)"
mar.area.title <- "Absolute marginal difference in area affected (km²)"
mar.area.title.2l <- "Absolute marginal difference\nin area affected (km²)"


col.est <- c("#e41a1c", "#377eb8", "#984ea3")
names(col.est) <- type.lab$type.label
col.est.pt <- col.est
col.est.pt[2] <- "#FFFFFFFF"
size.est.pt <- c(0.2, 0.4, 0.3)
names(size.est.pt) <- names(col.est)
size.est.pt.comp <- c(0.15, 0.3, 0.225)
names(size.est.pt.comp) <- names(col.est)
stroke.pt.comp <- 0.5
linewidth.comp <- 0.3
year.cat.angle <- 45


## DATA PREPARATION ####################################################



if(!file.exists(file.data.vis) | overwrite == TRUE) {

  agg.d <- rbindlist(lapply(files.agg, readRDS))
  agg.nd <- rbindlist(lapply(files.agg.nd, readRDS))
  agg.d[, dr_type := factor("all", levels = c("all", "no_drought"))]
  agg.nd[, dr_type := factor("no_drought", levels = c("all", "no_drought"))]

  area.undist <-
    readRDS(file.area)$undist |>
    _[is.na(it_type) & is.na(pa_type), .(year, adm0, area)]

  agg.mar <-
    rbind(agg.d, agg.nd) |>
    dcast(... ~ type, value.var = "disturbance") |>
    _[, mar := fac - cf1] |>
    merge(area.undist, by = c("year", "adm0"), all.x = TRUE)


  agg.area.y <-
    agg.mar[!is.na(year) & !is.na(area),
             .(year, adm0, dr_type, .draw,
               area.prop.mar = mar * area,
               area.prop.fac = fac * area,
               area.prop.cf1 = cf1 * area)]
  agg.area.all <-
    agg.area.y[,
               .(area.prop.mar = mean(area.prop.mar),
                 area.prop.fac = mean(area.prop.fac),
                 area.prop.cf1 = mean(area.prop.cf1)),
               by = .(adm0, dr_type, .draw)]
  agg.area <- rbind(agg.area.all, agg.area.y, fill = TRUE)


  agg.post <-
    merge(agg.mar[, -"area"], agg.area, by = c("year", "adm0", "dr_type", ".draw"))

  rm(agg.mar, agg.area, agg.area.all, agg.area.y)
  gc()


  sum.cols <- c("dr_type", "adm0", "year")
  agg.post.hdi <-
    agg.post[,
             c(
               hdci2(mar, .width = c(0.9), "mar."),
               hdci2(mar, .width = c(0.5), "mar."),
               hdci2(fac, .width = c(0.9), "fac."),
               hdci2(fac, .width = c(0.5), "fac."),
               hdci2(cf1, .width = c(0.9), "cf."),
               hdci2(cf1, .width = c(0.5), "cf."),
               hdci2(as.numeric(area.prop.mar), .width = c(0.9), "area.mar."),
               hdci2(as.numeric(area.prop.mar), .width = c(0.5), "area.mar."),
               hdci2(as.numeric(area.prop.fac), .width = c(0.9), "area.fac."),
               hdci2(as.numeric(area.prop.fac), .width = c(0.5), "area.fac."),
               hdci2(as.numeric(area.prop.cf1), .width = c(0.9), "area.cf."),
               hdci2(as.numeric(area.prop.cf1), .width = c(0.5), "area.cf.")
               ),
             by = sum.cols]
  agg.sum <-
    agg.post[,
             .(
               mar.mean = mean(mar),
               mar.median = median(mar),
               mar.sd = sd(mar),
               fac.mean = mean(fac),
               fac.median = median(fac),
               fac.sd = sd(fac),
               cf.mean = mean(cf1),
               cf.median = median(cf1),
               cf.sd = sd(cf1),
               area.mar.mean = mean(area.prop.mar),
               area.mar.median = median(area.prop.mar),
               area.mar.sd = sd(area.prop.mar),
               area.fac.mean = mean(area.prop.fac),
               area.fac.median = median(area.prop.fac),
               area.fac.sd = sd(area.prop.fac),
               area.cf.mean = mean(area.prop.cf1),
               area.cf.median = median(area.prop.cf1),
               area.cf.sd = sd(area.prop.cf1),
               mar.prob.pos = sum(mar > 0)/.N,
               mar.prob.neg = sum(mar < 0)/.N
               ),
             by = sum.cols] |>
  merge(agg.post.hdi, by = sum.cols) |>
  merge(reg.lab[[region]], by = "adm0") |>
  merge(year.lab, by = "year") |>
  merge(dr.lab, by = "dr_type")
  setorder(agg.sum, year.label, reg.label, dr.label)

  agg.sum.l <-
    agg.sum |>
    melt(measure.vars = list(est.median = c("fac.median", "cf.median", "mar.median"),
                             est.hdi90l = c("fac.hdi90l", "cf.hdi90l", "mar.hdi90l"),
                             est.hdi90u = c("fac.hdi90u", "cf.hdi90u", "mar.hdi90u")),
         variable.name = "est_type")
  agg.sum.l[, est.label := type.lab$type.label[as.integer(est_type)]]
  agg.sum.l[, est.group := factor(fifelse(est_type == "3",
                                                    "Attributed effect",
                                                    "Comparison against reference"),
                                            levels = c("Comparison against reference",
                                                       "Attributed effect"))]

  agg.sum.area.l <-
    agg.sum |>
    melt(measure.vars = list(est.median = c("area.fac.median", "area.cf.median", "area.mar.median"),
                             est.hdi90l = c("area.fac.hdi90l", "area.cf.hdi90l", "area.mar.hdi90l"),
                             est.hdi90u = c("area.fac.hdi90u", "area.cf.hdi90u", "area.mar.hdi90u")),
         variable.name = "est_type")
  agg.sum.area.l[, est.label := type.lab$type.label[as.integer(est_type)]]
  agg.sum.area.l[, est.group := factor(fifelse(est_type == "3",
                                               "Attributed effect",
                                               "Comparison against reference"),
                                       levels = c("Comparison against reference",
                                                  "Attributed effect"))]


  agg.data <- list(agg.sum = agg.sum, agg.sum.l = agg.sum.l)
  saveRDS(agg.data, file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


# p.area.comp.c <-
#   agg.sum.area.l[order(-est.label)][is.na(adm0) & est_type %in% c("1", "2")] |>
#   ggplot() +
#     geom_pointrange(aes(x = year.label, y = as.numeric(est.median),
#                         ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
#                         group = est.label, fill = est.label,
#                         colour = est.label, size = est.label),
#                         shape = 21, stroke = stroke.pt.comp,
#                         linewidth = linewidth.comp) +
#     scale_fill_manual(values = col.est.pt, drop = TRUE) +
#     scale_colour_manual(values = col.est, drop = TRUE) +
#     scale_y_continuous(label = scales::label_number(accuracy = 100, style_positive = "none"),
#                        expand = expansion(mult = c(0.05, 0.2))) +
#     scale_size_manual(values = size.est.pt.comp, guide = "none") +
#     facet_grid(rows = vars(dr.label)) +
#     expand_limits(y = 0) +
#     labs(colour = "", fill = "", y = abs.area.title, x = "") +
#     post_theme +
#     theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))

# p.area.mar.c <-
#   agg.sum.area.l[order(-est.label)][is.na(adm0) & est_type %in% c("3")] |>
#   ggplot() +
#     geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
#     geom_pointrange(aes(x = year.label, y = as.numeric(est.median),
#                         ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
#                         group = est.label, fill = est.label,
#                         colour = est.label, size = est.label),
#                         shape = 21, stroke = stroke.pt.comp,
#                         linewidth = linewidth.comp) +
#     scale_fill_manual(values = col.est.pt, drop = TRUE) +
#     scale_colour_manual(values = col.est, drop = TRUE) +
#     scale_y_continuous(label = scales::label_number(accuracy = 100, style_positive = "none"),
#                        expand = expansion(mult = c(0.05, 0.2))) +
#     scale_size_manual(values = size.est.pt.comp, guide = "none") +
#     facet_grid(rows = vars(dr.label)) +
#     expand_limits(y = 0) +
#     labs(colour = "", fill = "", y = mar.area.title.2l, x = "") +
#     post_theme +
#     theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))


p.prop.comp.c <-
  agg.sum.l[order(-est.label)][is.na(adm0) & est_type %in% c("1", "2")] |>
  ggplot() +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                    style_positive = "none", suffix = " %"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(dr.label)) +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = abs.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))

p.prop.mar.c <-
  agg.sum.l[order(-est.label)][is.na(adm0) & est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(dr.label)) +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = mar.title.2l, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))



p.prop.c <-
  (
   ((p.prop.comp.c &
     theme(
          plot.margin = unit(c(10, 5, 0, 10), "pt")/2,
           )) +
    (p.prop.mar.c &
     theme(
          plot.margin = unit(c(10, 5, 0, 25), "pt")/2,
           )))) +
  plot_layout(guides = "collect", tag_level = "new") +
  plot_annotation(tag_levels = "A") &
  guides(fill = guide_legend(override.aes = list(size = 0.25))) &
  theme(
        axis.title = element_text(size = rel(0.7)),
        axis.title.y = element_text(margin = margin(r = 0.5*base.size)),
        axis.text.y = element_text(size = rel(0.8), margin = margin(r = base.size/4)),
        axis.text.x = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.65)),
        legend.key.size = unit(1.5*base.size, "pt"),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = rel(0.7),
                                  margin = margin(
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2
                                                  )),
        plot.tag = element_text(size = rel(1),
                                family = "IBMPlexSansCondensed",
                                face = "bold",
                                margin = margin(
                                                0,
                                                base.size/2,
                                                base.size,
                                                0
                                                ))
        )


png(file.fig.adm.c, width = 5.5, height = 2.35, unit = "in", res = 600)
p.prop.c
dev.off()


p.all.comp <-
  agg.sum.l[order(-est.label)][dr_type == "all" & est_type %in% c("1", "2")] |>
  ggplot() +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.01, scale = 100,
                                                    style_positive = "none", suffix = " %"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = abs.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))

p.all.mar <-
  agg.sum.l[order(-est.label)][dr_type == "all" & est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.01, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = mar.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))

p.nd.comp <-
  agg.sum.l[order(-est.label)][dr_type == "no_drought" & est_type %in% c("1", "2")] |>
  ggplot() +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.01, scale = 100,
                                                    style_positive = "none", suffix = " %"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = abs.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))

p.nd.mar <-
  agg.sum.l[order(-est.label)][dr_type == "no_drought" & est_type %in% c("3")] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = year.label, y = est.median,
                        ymin = est.hdi90l, ymax = est.hdi90u,
                        group = est.label, fill = est.label,
                        colour = est.label, size = est.label),
                        shape = 21, stroke = stroke.pt.comp,
                        linewidth = linewidth.comp) +
    scale_fill_manual(values = col.est.pt, drop = TRUE) +
    scale_colour_manual(values = col.est, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.01, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.2, 0.2))) +
    scale_size_manual(values = size.est.pt.comp, guide = "none") +
    facet_grid(rows = vars(reg.label), cols = vars(est.group), scales = "free_y") +
    expand_limits(y = 0) +
    labs(colour = "", fill = "", y = mar.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = year.cat.angle, hjust = 1))



png(file.fig.adm.all, width = 5.5, height = 8.5, unit = "in", res = 600)
  p.all.comp +
  (p.all.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  guides(fill = guide_legend(override.aes = list(size = 0.25))) &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.65)),
        legend.margin = margin(0, 0, 0, 0))
dev.off()


png(file.fig.adm.nd, width = 5.5, height = 8.5, unit = "in", res = 600)
  p.nd.comp +
  (p.nd.mar + theme(plot.margin = margin(l = 20))) +
  plot_layout(guides = "collect") &
  guides(fill = guide_legend(override.aes = list(size = 0.25))) &
  theme(legend.spacing.y = unit(0, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.65)),
        legend.margin = margin(0, 0, 0, 0))
dev.off()
