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


