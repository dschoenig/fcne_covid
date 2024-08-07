args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(ggplot2)
library(ggdist)
library(colorspace)
source("utilities.R")

n.threads <- as.integer(args[1])
region <- tolower(as.character(args[2]))
mort_type <- tolower(as.character(args[3]))

n.threads <- 4
region <- "amz"


setDTthreads(n.threads)

path.base <- "../"
path.data <- paste0(path.base, "data/")
path.data.proc <- paste0(path.data, "processed/")
path.data.vis <- paste0(path.data, "visualization/")
if(!dir.exists(path.data.vis)) dir.create(path.data.vis, recursive = TRUE)
path.mar <- paste0(path.base, "models/marginal/", region, "/")
path.figures <- paste0(path.base, "results/figures/")
if(!dir.exists(path.figures)) dir.create(path.figures, recursive = TRUE)

file.mar.mort <- paste0(path.mar, region, ".covid.mort.rds")
file.mar.mortlag1 <- paste0(path.mar, region, ".covid.mortlag1.rds")


file.fig.mort <- paste0(path.figures, region, ".covid.png")
file.data.vis <- paste0(path.data.vis, region, ".covid.rds")


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
  data.table(type = c("fac", "cf1", "diff"),
             type.label = c("Factual\n(under COVID-19 pandemic)",
                            "Counterfactual 1:\nNo pandemic",
                            "Difference\n(factual vs. counterfactual)"))
type.lab[, type.label := factor(type.label, levels = type.label)]
type.lab[, type := factor(type)]

year.lab <-
  data.table(year = c(2020, 2021, 2022, NA),
             year.label = c("2020", "2021", "2022", "2020–2022 (average)"))
year.lab[, year := factor(year)]
year.lab[, year.label := factor(year.label, levels = year.label)]

itpa.lab <- 
  data.table(it_type = c("recognized", "not_recognized", NA, NA),
             pa_type = c(NA, NA, "indirect_use", "direct_use"),
             itpa.label = c("IT, recognized", "IT, not recognized", 
                          "PA, IUCN I-IV", "PA, IUCN V-VI"))
itpa.lab[, `:=`(
                it_type = factor(it_type,
                                 levels = c("none", "recognized", "not_recognized"),
                                 ordered = TRUE),
                pa_type = factor(pa_type,
                                 levels = c("none", "indirect_use", "direct_use"),
                                 ordered = TRUE),
                itpa.label = factor(itpa.label, levels = itpa.label))]



col.div <- diverging_hcl(21, palette = "Purple-Green")
col.type <- col.div[c(2,4,19,17)]
names(col.type) <- itpa.lab$itpa.label



mar.mort <- readRDS(file.mar.mort)
mar.mortlag1 <- readRDS(file.mar.mortlag1)

mar.mort[, .(mar.mean = mean(marginal)), by = .(group.id, mort.class, year)]
mar.mortlag1[, .(mar.mean = mean(marginal)), by = .(group.id, mortlag1.class, year)]

mar[, year := factor(year)]

mar.bl <-
  mar[is.na(year) & pandemic == "no",
      .(it_type, pa_type, .draw,
        ref.factual = factual,
        ref.counterfactual = counterfactual,
        ref.marginal = marginal)]

mar.comp <-
  mar[pandemic == "yes"] |>
  merge(mar.bl) |>
  merge(year.lab, by = "year") |>
  merge(itpa.lab, by = c("it_type", "pa_type"))
mar.comp[, did := marginal - ref.marginal]


# bl <-
#   readRDS(file.bl)
# fl.bl <- bl[is.na(adm0) & is.na(year), mean(forestloss)]

p.ten <-

  mar.comp |>
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    stat_pointinterval(aes(y = did*100, x = itpa.label, colour = itpa.label),
                       point_size = 1.25,
                       interval_size_range = c(0.5, 1.25), 
                       fatten_point = 1.25, shape = 21, fill = "white",
                       .width = c(0.5, 0.95)) +
    scale_colour_manual(values = col.type) +
    coord_cartesian(ylim = c(-0.275, 0.05)) +
    facet_grid(rows = vars(year.label), cols = vars(type.label)) +
    labs(x = NULL, y = "Annual forest loss rate (percent)", colour = NULL) +
    plot_theme


png(file.fig.ten, width = 7, height = 7, unit = "in", res = 600)
p.ten
dev.off()


# IUFRO 2024
p.ten <-



png("../results/figures/fcne_covid_ten.png", width = 7, height = 1.75, unit = "in", res = 600)
  mar[type == "diff"] |>

  mar.comp |>
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    stat_pointinterval(aes(y = did, x = itpa.label, colour = itpa.label),
                       point_size = 1.25,
                       interval_size_range = c(0.5, 1.25), 
                       fatten_point = 1.25, shape = 21, fill = "white",
                       .width = c(0.5, 0.95)) +
    scale_colour_manual(values = col.type) +
    scale_y_continuous(labels = scales::label_percent()) +
    coord_cartesian(ylim = c(-0.00275, 0.0005)) +
    facet_wrap(vars(year.label), nrow = 1, scales = "free_y") +
    labs(x = NULL,
         y = "Absolute change in effectivness\n(difference in annual forest loss rate)",
         colour = NULL) +
    plot_theme

dev.off()


labs(fill = ,
