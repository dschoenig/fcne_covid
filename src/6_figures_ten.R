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
path.mar <- paste0(path.base, "models/marginal/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

files.mar <- paste0(path.mar, region, ".dis.ten.", c("fac", "cf1"), ".itpa.rds")
file.area <- paste0(path.data.proc, region, ".sumstats.area.rds")

file.fig.ten.area <- paste0(path.figures, region, ".dis.ten.area.png")
file.fig.ten.prop <- paste0(path.figures, region, ".dis.ten.prop.png")
file.data.vis <- paste0(path.data.vis, region, ".dis.ten.rds")



## SETUP ###############################################################

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


cat.lab <- 
  data.table(cat.label = c("Reference",
                            "IL, recognized", "IL, not recognized",
                            "PA, category I-IV", "PA, category V-VI",
                            "IL, rec.; PA, cat. I-IV",
                            "IL, rec.; PA, cat. V-VI",
                            "IL, not rec.; PA, cat. I-IV",
                            "IL, not rec.; PA, cat. V-VI"),
                            # "IL, recognized &\nPA, category I-IV",
                            # "IL, recognized &\nPA, category V-VI",
                            # "IL, not recognized &\nPA, category I-IV",
                            # "IL, not recognized &\nPA, category V-VI"),
             it_type = c("none",
                         "recognized", "not_recognized",
                         "none", "none",
                         "recognized", "recognized",
                         "not_recognized", "not_recognized"),
             pa_type = c("none",
                         "none", "none",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use",
                         "indirect_use", "direct_use"))
# cat.lab[, cat.label := stri_pad_left(cat.label, width = max(stri_width(cat.label)))]
cat.lab[, cat.label := factor(cat.label, levels = cat.label)]


est.lab <-
  data.table(est_type = as.factor(1:7),
             ten.ref = c("Tenure regime", "Reference", "Tenure regime",
                         "Tenure regime", "Reference", "Tenure regime",
                         "Tenure regime"),
             type.cat = c(rep("Factual", 3),
                         rep("Counterfactual", 3),
                         "Difference in difference"),
             type.cat.l = c(rep("Factual\n(under COVID-19 pandemic)", 3),
                             rep("Counterfactual\n(pre-pandemic conditions)", 3),
                             "Difference in difference"),
             panel = c(
                        rep("Comparison against reference", 2), "Avoided disturbances",
                        rep("Comparison against reference", 2), "Avoided disturbances",
                        "Difference in avoided disturbances"))
est.lab[,
        `:=`(ten.ref = factor(ten.ref,
                              levels = c("Reference", "Tenure regime")),
             type.cat = factor(type.cat, 
                               levels = c("Factual",
                                           "Counterfactual",
                                           "Difference in difference")),
             type.cat.l = factor(type.cat.l,
                                 levels = c(
                                            "Counterfactual\n(pre-pandemic conditions)",
                                            "Factual\n(under COVID-19 pandemic)",
                                            "Difference in difference"
                                            )),
             panel = factor(panel,
                            levels = c("Comparison against reference",
                                        "Avoided disturbances", 
                                        "Difference in avoided disturbances")))]

type.lab <-
  data.table(type = c("fac", "cf1", "mar"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            "Counterfactual\n(pre-pandemic conditions)",
                            "Marginal\n(absolute difference)"
                            ))
type.lab[, type.label := factor(type.label, levels = type.label)]

year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
            #  year.label = c("2020", "2021", "2022", "2020–2022\n(average)"))
             year.label = c("2020", "2021", "2022", "2020–2022 (average)"))
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

abs.prop.title <- "Proportion of area affected"
mar.prop.title <- "Avoided disturbances"
did.prop.title <- "Change in avoided disturbances"
abs.area.title <- "Area affected (km²)"
mar.area.title <- "Avoided disturbances (km²)"
did.area.title <- "Change in avoided disturbances (km²)"



col.est <- c("#377eb8", "#e41a1c", "#984ea3")
names(col.est) <- levels(est.lab$type.cat.l)
col.type.cat <- col.est
shape.ten.ref <- c(`Reference` = 21, `Tenure regime` = 16)
size.ten.ref <- c(`Reference` = 0.25, `Tenure regime` = 0.175)
pt.size.mar <- 0.3
stroke.pt.comp <- 0.3
linewidth.comp <- 0.3
cat.angle <- 45


## DATA PREPARATION ####################################################

if(!file.exists(file.data.vis) | overwrite == TRUE) {


  mar.l <-lapply(files.mar, readRDS)
  names(mar.l) <- c("fac", "cf1")

  mar <-
    rbindlist(mar.l, fill = TRUE, idcol = "type") |>
    setnames(c("factual", "counterfactual", "marginal"),
            c("ten", "ref", "mar")) |>
    melt(measure.vars = c("ten", "ref", "mar"),
        variable.name = "est.type", value.name = "estimate") |>
    dcast(... ~ type + est.type, value.var = "estimate", sep = ".")
  mar[, year := as.integer(as.character(year))]
  mar[, did := fac.mar - cf1.mar]

  area.undist <-
    readRDS(file.area)$undist |>
    _[!is.na(it_type) & !is.na(pa_type),
     .(year, adm0, it_type, pa_type, area)]


  agg.mar <-
    merge(mar,
          area.undist, by = c("year", "adm0", "it_type", "pa_type"),
          all.x = TRUE)


  agg.area.y <-
    agg.mar[!is.na(year) & !is.na(area),
             .(year, adm0, it_type, pa_type, .draw,
               area.prop.fac.ten = fac.ten * area,
               area.prop.fac.ref = fac.ref * area,
               area.prop.fac.mar = fac.mar * area,
               area.prop.cf1.ten = cf1.ten * area,
               area.prop.cf1.ref = cf1.ref * area,
               area.prop.cf1.mar = cf1.mar * area,
               area.prop.did = did * area)]
  agg.area.all <-
    agg.area.y[,
               .(
                 area.prop.fac.ten = mean(area.prop.fac.ten),
                 area.prop.fac.ref = mean(area.prop.fac.ref),
                 area.prop.fac.mar = mean(area.prop.fac.mar),
                 area.prop.cf1.ten = mean(area.prop.cf1.ten),
                 area.prop.cf1.ref = mean(area.prop.cf1.ref),
                 area.prop.cf1.mar = mean(area.prop.cf1.mar),
                 area.prop.did = mean(area.prop.did)),
               by = .(adm0, it_type, pa_type, .draw)]
  agg.area <- rbind(agg.area.all, agg.area.y, fill = TRUE)


  agg.post <-
    merge(agg.mar[, -"area"], agg.area,
          by = c("year", "adm0", "it_type", "pa_type", ".draw"))

  rm(agg.mar, agg.area, agg.area.all, agg.area.y)
  gc()


  sum.cols <- c("adm0", "year", "it_type", "pa_type")
  agg.post.hdi <-
    agg.post[,
             c(
               hdci2(fac.ten, .width = c(0.9), "fac.ten."),
               hdci2(fac.ten, .width = c(0.5), "fac.ten."),
               hdci2(fac.ref, .width = c(0.9), "fac.ref."),
               hdci2(fac.ref, .width = c(0.5), "fac.ref."),
               hdci2(fac.mar, .width = c(0.9), "fac.mar."),
               hdci2(fac.mar, .width = c(0.5), "fac.mar."),
               hdci2(cf1.ten, .width = c(0.9), "cf1.ten."),
               hdci2(cf1.ten, .width = c(0.5), "cf1.ten."),
               hdci2(cf1.ref, .width = c(0.9), "cf1.ref."),
               hdci2(cf1.ref, .width = c(0.5), "cf1.ref."),
               hdci2(cf1.mar, .width = c(0.9), "cf1.mar."),
               hdci2(cf1.mar, .width = c(0.5), "cf1.mar."),
               hdci2(did, .width = c(0.9), "did."),
               hdci2(did, .width = c(0.5), "did."),
               hdci2(as.numeric(area.prop.fac.ten), .width = c(0.9), "area.prop.fac.ten."),
               hdci2(as.numeric(area.prop.fac.ten), .width = c(0.5), "area.prop.fac.ten."),
               hdci2(as.numeric(area.prop.fac.ref), .width = c(0.9), "area.prop.fac.ref."),
               hdci2(as.numeric(area.prop.fac.ref), .width = c(0.5), "area.prop.fac.ref."),
               hdci2(as.numeric(area.prop.fac.mar), .width = c(0.9), "area.prop.fac.mar."),
               hdci2(as.numeric(area.prop.fac.mar), .width = c(0.5), "area.prop.fac.mar."),
               hdci2(as.numeric(area.prop.cf1.ten), .width = c(0.9), "area.prop.cf1.ten."),
               hdci2(as.numeric(area.prop.cf1.ten), .width = c(0.5), "area.prop.cf1.ten."),
               hdci2(as.numeric(area.prop.cf1.ref), .width = c(0.9), "area.prop.cf1.ref."),
               hdci2(as.numeric(area.prop.cf1.ref), .width = c(0.5), "area.prop.cf1.ref."),
               hdci2(as.numeric(area.prop.cf1.mar), .width = c(0.9), "area.prop.cf1.mar."),
               hdci2(as.numeric(area.prop.cf1.mar), .width = c(0.5), "area.prop.cf1.mar."),
               hdci2(as.numeric(area.prop.did), .width = c(0.9), "area.prop.did."),
               hdci2(as.numeric(area.prop.did), .width = c(0.5), "area.prop.did.")
               ),
             by = sum.cols]
  agg.sum <-
    agg.post[,
             .(
               fac.ten.mean = mean(fac.ten),
               fac.ten.median = median(fac.ten),
               fac.ten.sd = sd(fac.ten),
               fac.ref.mean = mean(fac.ref),
               fac.ref.median = median(fac.ref),
               fac.ref.sd = sd(fac.ref),
               fac.mar.mean = mean(fac.mar),
               fac.mar.median = median(fac.mar),
               fac.mar.sd = sd(fac.mar),
               cf1.ten.mean = mean(cf1.ten),
               cf1.ten.median = median(cf1.ten),
               cf1.ten.sd = sd(cf1.ten),
               cf1.ref.mean = mean(cf1.ref),
               cf1.ref.median = median(cf1.ref),
               cf1.ref.sd = sd(cf1.ref),
               cf1.mar.mean = mean(cf1.mar),
               cf1.mar.median = median(cf1.mar),
               cf1.mar.sd = sd(cf1.mar),
               did.mean = mean(did),
               did.median = median(did),
               did.sd = sd(did),
               area.prop.fac.ten.mean = mean(area.prop.fac.ten),
               area.prop.fac.ten.median = median(area.prop.fac.ten),
               area.prop.fac.ten.sd = sd(area.prop.fac.ten),
               area.prop.fac.ref.mean = mean(area.prop.fac.ref),
               area.prop.fac.ref.median = median(area.prop.fac.ref),
               area.prop.fac.ref.sd = sd(area.prop.fac.ref),
               area.prop.fac.mar.mean = mean(area.prop.fac.mar),
               area.prop.fac.mar.median = median(area.prop.fac.mar),
               area.prop.fac.mar.sd = sd(area.prop.fac.mar),
               area.prop.cf1.ten.mean = mean(area.prop.cf1.ten),
               area.prop.cf1.ten.median = median(area.prop.cf1.ten),
               area.prop.cf1.ten.sd = sd(area.prop.cf1.ten),
               area.prop.cf1.ref.mean = mean(area.prop.cf1.ref),
               area.prop.cf1.ref.median = median(area.prop.cf1.ref),
               area.prop.cf1.ref.sd = sd(area.prop.cf1.ref),
               area.prop.cf1.mar.mean = mean(area.prop.cf1.mar),
               area.prop.cf1.mar.median = median(area.prop.cf1.mar),
               area.prop.cf1.mar.sd = sd(area.prop.cf1.mar),
               area.prop.did.mean = mean(area.prop.did),
               area.prop.did.median = median(area.prop.did),
               area.prop.did.sd = sd(area.prop.did)
               ),
             by = sum.cols] |>
  merge(agg.post.hdi, by = sum.cols) |>
  merge(reg.lab[[region]], by = "adm0") |>
  merge(year.lab, by = "year") |>
  merge(cat.lab, by = c("it_type", "pa_type"))
  setorder(agg.sum, year.label, reg.label, cat.label)


  agg.sum.l <-
    agg.sum |>
    melt(measure.vars = list(
                             est.median = c("fac.ten.median", "fac.ref.median", "fac.mar.median",
                                            "cf1.ten.median", "cf1.ref.median", "cf1.mar.median",
                                            "did.median"),
                             est.hdi90l = c("fac.ten.hdi90l", "fac.ref.hdi90l", "fac.mar.hdi90l",
                                            "cf1.ten.hdi90l", "cf1.ref.hdi90l", "cf1.mar.hdi90l",
                                            "did.hdi90l"),
                             est.hdi90u = c("fac.ten.hdi90u", "fac.ref.hdi90u", "fac.mar.hdi90u",
                                            "cf1.ten.hdi90u", "cf1.ref.hdi90u", "cf1.mar.hdi90u",
                                            "did.hdi90u")
                                            ),
         variable.name = "est_type") |>
    merge(est.lab, by = "est_type")
  agg.sum.l[, est_type := as.integer(as.character(est_type))]


  agg.sum.area.l <-
    agg.sum |>
    melt(measure.vars = list(
                             est.median = c("area.prop.fac.ten.median", "area.prop.fac.ref.median", "area.prop.fac.mar.median",
                                            "area.prop.cf1.ten.median", "area.prop.cf1.ref.median", "area.prop.cf1.mar.median",
                                            "area.prop.did.median"),
                             est.hdi90l = c("area.prop.fac.ten.hdi90l", "area.prop.fac.ref.hdi90l", "area.prop.fac.mar.hdi90l",
                                            "area.prop.cf1.ten.hdi90l", "area.prop.cf1.ref.hdi90l", "area.prop.cf1.mar.hdi90l",
                                            "area.prop.did.hdi90l"),
                             est.hdi90u = c("area.prop.fac.ten.hdi90u", "area.prop.fac.ref.hdi90u", "area.prop.fac.mar.hdi90u",
                                            "area.prop.cf1.ten.hdi90u", "area.prop.cf1.ref.hdi90u", "area.prop.cf1.mar.hdi90u",
                                            "area.prop.did.hdi90u")
                                            ),
         variable.name = "est_type") |>
    merge(est.lab, by = "est_type")
  agg.sum.area.l[, est_type := as.integer(as.character(est_type))]


  agg.data <- list(agg.sum = agg.sum, agg.sum.l = agg.sum.l, agg.sum.area.l = agg.sum.area.l)
  saveRDS(agg.data, file.data.vis)

} else {

  message("Loading data for visualization …")
  stored <- readRDS(file.data.vis)
  attach(stored)

}


## TENURE EFFECTS #######################################################

setorder(agg.sum.l, ten.ref, -type.cat)
setorder(agg.sum.area.l, ten.ref, -type.cat)

agg.comp.area <-
  agg.sum.area.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(1, 2, 4, 5)]

p.area.comp.c <-
  agg.sum.area.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(1, 2, 4, 5)] |>
  ggplot() +
    geom_pointrange(data = agg.comp.area[est_type %in% c(2, 5)],
                    aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        shape = ten.ref,
                        size = ten.ref
                        ),
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    geom_pointrange(data = agg.comp.area[est_type %in% c(1, 4)],
                    aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        shape = ten.ref,
                        size = ten.ref
                        ),
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_shape_manual(values = shape.ten.ref) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 100, style_positive = "none"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    scale_size_manual(values = size.ten.ref, guide = "none") +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", shape = "", y = abs.area.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))

p.area.mar.c <-
  agg.sum.area.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(3, 6)] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        ),
                    shape = 16,
                    size = pt.size.mar,
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 100, style_positive = "plus"),
                       expand = expansion(mult = c(0.1, 0.1))) +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", y = mar.area.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))

p.area.did.c <-
  agg.sum.area.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(7)] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        ),
                    shape = 16,
                    size = pt.size.mar,
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 100, style_positive = "plus"),
                       expand = expansion(mult = c(0.1, 0.1))) +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", y = did.area.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))


agg.comp.prop <-
  agg.sum.l[order(ten.ref)][is.na(adm0) &
            est_type %in% c(1, 2, 4, 5)]

p.prop.comp.c <-
  agg.sum.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(1, 2, 4, 5)] |>
  ggplot() +
    geom_pointrange(data = agg.comp.prop[est_type %in% c(2, 5)],
                    aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        shape = ten.ref,
                        size = ten.ref
                        ),
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    geom_pointrange(data = agg.comp.prop[est_type %in% c(1, 4)],
                    aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        shape = ten.ref,
                        size = ten.ref
                        ),
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_shape_manual(values = shape.ten.ref) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                    style_positive = "none", suffix = " %"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    scale_size_manual(values = size.ten.ref, guide = "none") +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", shape = "", y = abs.prop.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))

p.prop.mar.c <-
  agg.sum.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(3, 6)] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        ),
                    shape = 16,
                    size = pt.size.mar,
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.1, 0.1))) +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", y = mar.prop.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))

p.prop.did.c <-
  agg.sum.l[order(ten.ref)][is.na(adm0) &
                 est_type %in% c(7)] |>
  ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
    geom_pointrange(aes(x = cat.label, y = as.numeric(est.median),
                        ymin = as.numeric(est.hdi90l), ymax = as.numeric(est.hdi90u),
                        group = type.cat.l,
                        colour = type.cat.l,
                        ),
                    shape = 16,
                    size = pt.size.mar,
                    fill = "#FFFFFF",
                    stroke = stroke.pt.comp,
                    linewidth = linewidth.comp,
                    position = position_dodge2(width = 0.5, padding = 0.1)) +
    scale_colour_manual(values = col.type.cat, drop = TRUE) +
    scale_y_continuous(label = scales::label_number(accuracy = 0.1, scale = 100,
                                                    style_positive = "plus", suffix = " pp."),
                       expand = expansion(mult = c(0.1, 0.1))) +
    facet_grid(rows = vars(year.label)) +
    expand_limits(y = 0) +
    labs(colour = "", y = did.prop.title, x = "") +
    post_theme +
    theme(axis.text.x = element_text(angle = cat.angle, hjust = 1))


## EXPORT ##############################################################


png(file.fig.ten.area, width = 7, height = 5, unit = "in", res = 600)
(
  (p.area.comp.c +
    guides(
      colour = guide_legend(override.aes = list(shape = 19, size = 0.25)),
      shape = guide_legend(override.aes = list(size = 0.25)),
    )
  ) +
  (p.area.mar.c +
    guides(colour = "none")
  ) +
  (p.area.did.c +
    guides(
      colour = guide_legend(override.aes = list(shape = 19, size = 0.25)),
    )
  )
) +
  plot_layout(guides = "collect") &
  theme(
        axis.title = element_text(size = rel(0.7)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(1.15), margin = margin(r = base.size * 2/3, l = base.size)),
        axis.text.y = element_text(size = rel(0.8), margin = margin(r = base.size/4)),
        axis.text.x = element_text(size = rel(0.9)),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = rel(0.75), margin = margin(t = 7/4, b = 7/4, l = 7/2)),
        legend.key.width = unit(base.size, "pt"),
        legend.key.height = unit(2*base.size, "pt"),
        legend.spacing.x = unit(base.size/3, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(7/2, 7/2, 0, 7/2),
        strip.text = element_text(size = rel(0.7),
                                  margin = margin(
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2
                                                  ))
        )
dev.off()



png(file.fig.ten.prop, width = 7, height = 5, unit = "in", res = 600)
(
  (p.prop.comp.c +
    guides(
      colour = guide_legend(override.aes = list(shape = 19, size = 0.25)),
      shape = guide_legend(override.aes = list(size = 0.25)),
    )
  ) +
  (p.prop.mar.c +
    guides(colour = "none")
  ) +
  (p.prop.did.c +
    guides(
      colour = guide_legend(override.aes = list(shape = 19, size = 0.25)),
    )
  )
) +
  plot_layout(guides = "collect") &
  theme(
        axis.title = element_text(size = rel(0.7)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(1.15), margin = margin(r = base.size * 2/3, l = base.size)),
        axis.text.y = element_text(size = rel(0.8), margin = margin(r = base.size/4)),
        axis.text.x = element_text(size = rel(0.9)),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = rel(0.75),
                                   margin = margin(t = 7/4, b = 7/4, l = 7/2)),
        legend.key.width = unit(base.size, "pt"),
        legend.key.height = unit(2*base.size, "pt"),
        legend.spacing.x = unit(base.size/3, "pt"),
        legend.title = element_blank(),
        legend.margin = margin(7/2, 7/2, 0, 7/2),
        strip.text = element_text(size = rel(0.7),
                                  margin = margin(
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2
                                                  ))
        )
dev.off()
