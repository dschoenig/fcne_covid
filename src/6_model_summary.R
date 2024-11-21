args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(mgcv)

source("utilities.R")

region <- tolower(as.character(args[1]))
model.resp <- tolower(as.character(args[2]))

# region <- "amz"
# model.resp <- "dis"

path.gam <- "../models/gam/"
file.model <- paste0(path.gam, region, ".m1.", model.resp, ".rds")
file.post <- paste0(path.gam, region, ".m1.", model.resp, ".post.rds")

file.sum <- paste0(path.gam, region, ".m1.", model.resp, ".sum.rds")

model <- readRDS(file.model)
post <- readRDS(file.post)


mod.sum <- model_summary(model, post)

p.desc <-
  data.table(p.id = 1,
             p.label = "(Intercept)",
             p.desc = "Intercept",
             p.term = "\\alpha")
sm.desc <-
  data.table(sm.label =
               c(paste0("s(ed_east,ed_north)", ":year", 2017:2022),
                 paste0("s(ed_east,ed_north):it_type_", 2017:2022, "recognized"),
                 paste0("s(ed_east,ed_north):it_type_", 2017:2022, "not_recognized"),
                 paste0("s(ed_east,ed_north):pa_type_", 2017:2022, "indirect_use"),
                 paste0("s(ed_east,ed_north):pa_type_", 2017:2022, "direct_use"),
                 paste0("s(ed_east,ed_north):overlap_", 2017:2022, "recognized:indirect_use"),
                 paste0("s(ed_east,ed_north):overlap_", 2017:2022, "recognized:direct_use"),
                 paste0("s(ed_east,ed_north):overlap_", 2017:2022, "not_recognized:indirect_use"),
                 paste0("s(ed_east,ed_north):overlap_", 2017:2022, "not_recognized:direct_use"),
                 paste0("s(som_x,som_y)", ":year", 2017:2022)),
             sm.desc =
               c(paste0("Geographic variation (", 2017:2022, ", overall)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, recognized)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, not recognized)"),
                 paste0("Geographic variation (", 2017:2022, ", PA, category I‑IV)"),
                 paste0("Geographic variation (", 2017:2022, ", PA, category V‑VI)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, rec.; PA cat. I‑IV)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, rec.; PA cat. V‑VI)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, not rec.; PA cat. I‑IV)"),
                 paste0("Geographic variation (", 2017:2022, ", IL, not rec.; PA cat. V‑VI)"),
                 paste0("Variation in covariate space (SOM ", 2017:2022, ")")),
             sm.term =
               c(paste0("f^{\\textrm{GEO}}_{", 2017:2022, "}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{rec,", 2017:2022,"}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{notrec,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{strict,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{multi,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{rec,strict,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{rec,multi,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{notrec,strict,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{GEO}}_{\\textit{notrec,multi,", 2017:2022, "}}(u_i)"),
                 paste0("f^{\\textrm{SOM}}{\\textit{", 2017:2022, "}}(s_i)")))


term.desc <- rbind(p.desc, sm.desc, fill = TRUE)
term.desc[, term.id := 1:.N]

terms <-
  rbind(mod.sum$p.terms, mod.sum$sm.terms, fill = TRUE) |>
  merge(term.desc, sort = FALSE)

terms[, `:=`(p.sd = sqrt(p.var))]


col.format <- c("p.est", "p.sd", "lsp.est.lambda0", "lsp.est.lambda1")
col.na.rm <-
  c("p.comb", "sm.k", "sm.edf", "lsp.est.lambda0", "lsp.est.lambda1")

# terms[, (col.format) := lapply(.SD, format_power, mag = 1, digits = 2), .SDcols = col.format]
terms[,
      (col.format) := lapply(.SD, \(x) trimws(format(round(x, 2), nsmall = 2))),
      .SDcols = col.format]

terms[!is.na(p.id), p.comb := paste0(p.est, " (", p.sd, ")")]
terms[, sm.edf := fifelse(is.na(sm.edf),
                          NA_character_,
                          format(round(sm.edf, digits = 2), nsmall = 2))]
terms[, sm.k := as.character(sm.k)]
terms[, (col.na.rm) := lapply(.SD, fill_na_empty), .SDcols = col.na.rm] 
terms[, `:=`(term.desc = fifelse(is.na(sm.id), p.desc, sm.desc),
             term = fifelse(is.na(sm.id), p.term, sm.term))]

terms <-
  terms[, .(term.id, term.desc, term,
            p.comb,
            sm.k, sm.edf,
            lsp.est.lambda0,
            lsp.est.lambda1)]


model.summary <- list(terms = terms,
                      n = mod.sum$n,
                      dev.expl = mod.sum$dev.expl,
                      aic = mod.sum$aic,
                      unformatted = mod.sum)

saveRDS(model.summary, file.sum)

