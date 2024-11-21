source("utilities_egp.R")


## MAPPING HELPERS ##############################################################

crs.ea <- 
  list(cam = st_crs('PROJCS["Central_America_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",14.89],PARAMETER["longitude_of_center",-87.48],PARAMETER["standard_parallel_1",19.69],PARAMETER["standard_parallel_2",8.34],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900002"]]'),
       amz = st_crs('PROJCS["Amazon_Albers_Equal_Area_Conic",GEOGCS["SIRGAS 2000",DATUM["Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6674"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4674"]],PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-5.59],PARAMETER["longitude_of_center",-62.05],PARAMETER["standard_parallel_1",3.81],PARAMETER["standard_parallel_2",-15.62],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH]AUTHORITY["USER","900004"]]'))


bin_cols <- function(data, columns, bin.res, bin.min = NULL, bin.round = NULL, bin.names = NULL, append = FALSE, right = TRUE) {
  bins.l <- list()
  for (i in 1:length(columns)) {
    if(is.null(bin.min)) {
      c.min <- min(data[[columns[i]]])
    } else {
      c.min <- bin.min[i]
    }
    c.max <- max(data[[columns[i]]])
    if(is.null(bin.round)) {
      b.lower <- c.min - bin.res[i]
      b.upper <- c.max + bin.res[i]
    } else {
      b.lower <- round(c.min, bin.round) - bin.res[i]
      b.upper <- round(c.max, bin.round) + bin.res[i]
    }
    b.breaks <- seq(from = b.lower, 
                    to =  b.upper, 
                    by = bin.res[i])
    b.center <- b.breaks[1:(length(b.breaks)-1)] + bin.res[i] / 2 
    cuts <- cut(data[[columns[i]]], breaks = b.breaks, labels = FALSE, right = right)
    bins.l[[i]] <- b.center[cuts]
  }
  if(is.null(bin.names)) {
    names(bins.l) <- paste0(columns, ".bin")
  } else {
    names(bins.l) <- bin.names
  }
  if(append) {
    return(cbind(data, do.call(cbind, bins.l)))
  } else {
    return(bins.l)
  }
}

## GENERAL HELPERS ##############################################################


hdci2 <- function(x,
                  .width = 0.90,
                  var.name = "",
                  prefix = "hdi",
                  suffix = c("l", "u"), ...) {
  hint <- as.list(ggdist::hdci(x = x, .width = .width))
  var.names <- paste0(var.name, prefix, .width*100, suffix)
  names(hint) <- var.names
  return(hint)
}


posterior_summary <- function(x,
                              probs = c(0.05, 0.95),
                              prefix = NULL,
                              sep = ".",
                              point.fun = mean,
                              point.name = "mean",
                              ...) {
  if(is.null(prefix)) {
    prefix <- as.character(match.call()[2])
  }
  if(prefix == "") {
    sep <- ""
  }
  sum.names <-
    paste0(prefix, sep,
           c(point.name,
             paste0("q", probs*100)))
  y <-
    c(list(point.fun(x)),
      as.list(quantile(x, probs = probs, names = FALSE, ...)))
  names(y) <- sum.names
  return(y)
}


model_summary <- function(model,
                          post) {

  if(is.null(colnames(post))) {
    colnames(post) <- names(coef(model))
  }

  sp.est <- model$sp
  lsp.var <- diag(sp.vcov(model))
  if(length(lsp.var) > length(sp.est)) {
    lsp.var <- lsp.var[1:length(sp.est)]
    log.scale.var <- lsp.var[length(lsp.var)]
  } else {
    log.scale.var <- NA
  }
  names(lsp.var) <- names(sp.est)

  lsp.tab <- data.table(sp.label = names(sp.est), lsp.est = log(sp.est), lsp.var = lsp.var)

  n.sm <- length(model$smooth)

  sm.tab.l <- list()
  all.para.sm <- list()

  for(i in 1:n.sm) {
    para.range <- with(model$smooth[[i]], first.para:last.para)
    sp.range <- with(model$smooth[[i]], first.sp:last.sp)
    label <- model$smooth[[i]]$label
    sp.labels <- names(model$smooth[[i]]$sp)
    k <- model$smooth[[i]]$bs.dim
    kp <- length(para.range)
    edf <- sum(model$edf[para.range])
    sp.id <- paste0("lambda", 0:(length(sp.labels)-1))
    sm.tab.l[[i]] <-
      data.table(sm.id = i,
                 sm.label = label,
                 sm.k = k,
                 sm.kp = kp,
                 sm.edf = edf,
                 sp.id,
                 sp.label = sp.labels)
    all.para.sm[[i]] <- para.range
  }

  sm.tab <-
    rbindlist(sm.tab.l) |>
    merge(lsp.tab, sort = FALSE) |>
    dcast(sm.id + sm.label + sm.k + sm.kp + sm.edf ~ sp.id,
          value.var = c("lsp.est", "lsp.var"), sep = ".", sort = FALSE)

  pars.idx <- 1:ncol(post)
  p.sel <- pars.idx[!pars.idx %in% unlist(all.para.sm)]
  post.p <- post[, p.sel , drop = FALSE]
  p.est <- apply(post.p, 2, mean)
  p.var <- apply(post.p, 2, var)
  p.tab <-
    data.table(p.id = p.sel,
               p.label = names(p.est),
               p.est = p.est,
               p.var = p.var)

  return(list(p.terms = p.tab,
              sm.terms = sm.tab,
              scale.est = model$sig2,
              log.scale.var = log.scale.var,
              n = nobs(model),
              dev.expl = with(model, (null.deviance - deviance) / null.deviance),
              aic = AIC(model)))

  }

fill_na_empty <- function(x, empty = "") {
  if(!is.character(x)) {
    x <- as.character(x)
  }
  y <- x
  y[is.na(x) | x == "NA"] <- empty
  return(y)
}

## PLOTTING HELPERS #############################################################

label_arc <- function(x, ndec = 0, psign = TRUE) {
  per <- format(round(100 * x, ndec), nsmall = ndec, trim = TRUE)
  per[which(x > 0)] <- paste0("+", per[which(x > 0)])
  if(psign) {
    per <- paste0(per, "%")
  }
  per[is.na(x)] <- NA
  return(per)
}

label_per <- function(x, ndec = 0, psign = TRUE) {
  per <- format(round(100 * x, ndec), nsmall = ndec, trim = TRUE)
  if(psign) {
    per <- paste0(per, "%")
  }
  per[is.na(x)] <- NA
  return(per)
}


