#####################################################################################
# Title:   Prepare CRU-TS 3.22 Persistent Datasets
# Date:    December 2014
# Project: HarvestChoice
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Note that all downloaded time-series are used as-is, so we can use drop-in replacements
# without any further transformation, but we need to pre-process district summaries for
# extra speed using stats.cntr() below.

# Note: also saving this file to shared Dropbox /_global_codes/R/ for documentation

library(stringr)
library(data.table)
library(reshape2)
library(raster)
library(rgdal)

setwd("/home/projects/shiny/rainfall")

## CRU variables are in directory tree
# Note that each 1901-2013 time series is 2.6GB uncrompressed
d <- c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet")
f <- "cru_ts3.22.1901.2013.cld.dat.nc.gz"
f <- str_replace(f, "cld", d)
f <- str_replace(f, "cru_ts", paste0(d, "/cru_ts"))

# Retrieve netCDF files one variable at a time (pre, tmp, tmn, tmx)
baseurl <- "http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/"

for (i in 8) {
  download.file(paste0(baseurl, f[i]), paste0("../../hc-data/CRU_TS.3.22/", basename(f[i])), mode="wb")
  system(paste0("gzip -d ../../hc-data/CRU_TS.3.22/", basename(f[i])))
  assign(d[i], brick(paste0("../../hc-data/CRU_TS.3.22/", str_replace(basename(f[i]), ".gz", ""))))
}


## CRU temperature anomalies
url <- "http://www.cru.uea.ac.uk/cru/data/temperature/CRUTEM.4.3.0.0.anomalies.nc"
download.file(url, "../../hc-data/CRU_TS.3.22/CRUTEM.4.3.0.0.anomalies.nc", mode="wb")

# Explore
ano <- brick("../../hc-data/CRU_TS.3.22/CRUTEM.4.3.0.0.anomalies.nc")
extent(ano)
names(ano)
tm <- seq(as.Date("1850-01-16"), as.Date("2014-10-16"), "month")
ano <- setZ(ano, tm, "month")
r <- crop(ano, g2[g2$ADM0_NAME=="Ghana",])
hist(r)


## Palmer Drought Severity Index (PDSI)
url <- "http://www.cgd.ucar.edu/cas/catalog/climind/pdsisc.monthly.maps.1850-2012.fawc=1.r2.5x2.5.ipe=2.nc.gz"
download.file(url, "../../hc-data/CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc.gz", mode="wb")
system("gzip -d ../../hc-data/CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc.gz")

# Explore
pdsi <- brick("../../hc-data/CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc")
extent(pdsi)
# class       : Extent
# xmin        : -180
# xmax        : 180
# ymin        : -60
# ymax        : 77.5

summary(raster(pdsi,1))
#         X1850.04162597656
# Min.               -4.997
# 1st Qu.            -1.809
# Median             -0.525
# 3rd Qu.             1.838
# Max.                4.891
# NA's             7718.000

head(names(pdsi), 13)
#  [1] "X1850.04162597656" "X1850.125"         "X1850.20837402344"
#  [4] "X1850.29162597656" "X1850.375"         "X1850.45837402344"
#  [7] "X1850.54162597656" "X1850.625"         "X1850.70837402344"
# [10] "X1850.79162597656" "X1850.875"         "X1850.95837402344"
# [13] "X1851.04162597656"

tail(names(pdsi), 13)
#  [1] "X2011.95837402344" "X2012.04162597656" "X2012.125"
#  [4] "X2012.20837402344" "X2012.29162597656" "X2012.375"
#  [7] "X2012.45837402344" "X2012.54162597656" "X2012.625"
# [10] "X2012.70837402344" "X2012.79162597656" "X2012.875"
# [13] "X2012.95837402344"

nlayers(pdsi)
# [1] 1956

tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")

g <- g2.web[g2.web$ADM0_NAME=="Ghana",]
spplot(crop(pdsi, g), 700)

dt <- extract(pdsi, g, fun=mean, na.rm=T, df=T, small=T)
dt <- cbind(g@data, dt)
dt <- data.table(dt)
setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
dt <- melt(dt, id.vars=c(names(g), "ID"), variable.name="month", variable.factor=F)
dt[, month := as.Date(month)]
# Limit to 1960 onwards
dt <- dt[month>=as.Date("1960-01-01")]
summary(dt$value)

# Test time serie decomposition
library(TTR)
library(zoo)
dt.ts <- dt[ID==1][order(month)]
dt.ts <- ts(dt.ts, start=c(dt[, min(year(month))], 1), frequency=12)
plot.ts(dt.ts)
dt.ts <- decompose(na.StructTS(dt.xts))
plot(dt.ts)
dt.ts <- dt.ts$trend
dt.ts <- data.table(trend=dt.ts)
dt <- cbind(dt, dt.ts)



#####################################################################################
# Pre-process District Summaries (faster to map)
#####################################################################################

setwd("/home/projects/shiny/rainfall")

library(data.table)
library(reshape2)
library(raster)
library(rgdal)

load("../../hc-cell5m/rdb/g2.rda")
# Use webified boundaries (updated with GAUL 2013 v14 version)
g2.web <- readRDS("../../hc-cell5m/rdb/g2_2013v14.web.rds")

# Helper - summarize rasters over districts
stats.cntr <- function(x, y) {
  g <- g2[g2$ADM0_CODE==x,]
  dt <- extract(get(y), g, fun=mean, na.rm=T, df=T, small=T)
  dt <- cbind(g@data, dt)
  dt <- data.table(dt)
  setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
  dt <- melt(dt, id.vars=c(names(g), "ID"), variable.name="month", variable.factor=F)
  dt[, month := as.Date(month)]
  # Limit to 1960 onwards
  dt <- dt[month>=as.Date("1960-01-01")]
  saveRDS(dt, file=paste0("./data/rds/", y, x, ".rds"), compress=T)
}


# Helper - symbolize GeoJSON
json.cntr <- function(x, y, col) {
  # Load pre-processed district X month records
  dt <- readRDS(paste0("./data/rds/", y, x, ".rds"))
  # Summarize each district over entire period for mapping (mm/month)
  dt <- dt[, list(
    mean=mean(value, na.rm=T),
    min=min(value, na.rm=T),
    max=max(value, na.rm=T),
    sd=sd(value, na.rm=T)), keyby=ADM2_CODE]

  # Load country GeoJSON as list
  f <- paste0("./data/json/g2web", x)
  m <- jsonlite::fromJSON(f, simplifyVector=F)

  # Construct symbology from district means (note that PDSI has fewer values)
  dt <- dt[J(sapply(m$features, function(x) x$properties$ADM2_CODE))]
  rg <- range(dt$mean, na.rm=T)
  cv <- try(classInt::classIntervals(dt$mean, n=min(c(5, dt[,length(unique(mean))])))$brks)

  if (class(cv)=="try-error") {
    # classInt fails apply only 1 color (white)
    dt[, cl := "white"]
  } else {
    # Symbolize normally
    dt[, cl := cut(mean, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := colorRampPalette(col)(length(cv)+1)[cl]]
  }

  # Add symbology to GeoJSON. Note that the districts do not necessarily match here
  # since we used the original g2 to generate stats, and g2.web to generate GeoJSON
  for (i in 1:length(m$features)) {
    m$features[[i]]$properties$mean <- dt[i, round(mean, 2)]
    m$features[[i]]$properties$min <- dt[i, round(min, 2)]
    m$features[[i]]$properties$max <- dt[i, round(max, 2)]
    m$features[[i]]$properties$sd <- dt[i, round(sd, 2)]
    m$features[[i]]$style <- list(fillColor=dt[i, cl], weight=.6, color="white", fillOpacity=0.7)
  }

  # Write to RDS file
  saveRDS(m, file=paste0("./data/rds/", y, x, ".json.rds"), compress=T)
}

# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `pre`
pre <- brick("../../hc-data/CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")
col <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))
for (i in cntr) stats.cntr(i, "pre")
for (i in cntr) json.cntr(i, "pre", col)
# 3 countries 6, 102, 74 failed to overlay (31, 42, 48)

# Pre-process `pdsi`
pdsi <- brick("../../hc-data/CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
col <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")
for (i in cntr) stats.cntr(i, "pdsi")
for (i in cntr) json.cntr(i, "pdsi", col)
# countries 6, 102, 74 failed to overlay


#####################################################################################
# Simplify GAUL 2008 District Boundaries
#####################################################################################

rm(list=ls())
setwd("/home/projects/shiny/rainfall")

library(rgeos)
library(rgdal)
library(data.table)

# Load GAUL 2008 (CELL5M version)
load("../../hc-cell5m/rdb/g2.rda")

# I believe gSimplify uses the same libraries as GRASS
g2.web <- gSimplify(g2, 0.05, topologyPreserve=T)
g2.web <- SpatialPolygonsDataFrame(g2.web, g2@data)
plot(g2.web[g2.web$ADM0_NAME=="Ghana", ])

# Export to GRASS v.in.ogr with a 0.06 snapping threshold, then reload
writeOGR(g2.web, "../../hc-cell5m/rdb", "g2.web", "ESRI Shapefile")
g2.web <- readOGR("../../hc-cell5m/rdb", "g2.web")
#load("../../hc-cell5m/rdb/g2_2008v09.web.rda")
g2.web@data <- g2.web@data[, -c(3:6,11:12)]

# Add X,Y centroids
dt <- data.table(g2.web@data)
setcolorder(dt, c(3:6,1,2))
dt[, X := coordinates(g2.web)[, 1]]
dt[, Y := coordinates(g2.web)[, 2]]
g2.web@data <- dt

# Save webified version for re-use
saveRDS(g2.web, "../../hc-cell5m/rdb/g2_2008v09.web.rds", compress=T)

# Create as many geojson files as SSA countries
for (i in unique(g2.web@data$ADM0_CODE)) {
  writeOGR(g2.web[g2.web$ADM0_CODE==i,], paste0("./data/json/g2web", i), paste0(i), "GeoJSON",
    overwrite_layer=T)
}


# Create well-formatted country, province, district list for re-use in input controls
d2 <- data.table(g2.web@data)
d2 <- d2[, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]
d2 <- d2[, lapply(.SD, as.character), .SDcols=1:3]
setkey(d2, ADM0_NAME, ADM1_NAME, ADM2_NAME)
d2 <- split(d2, d2$ADM0_NAME)
d2 <- lapply(d2, function(x) x[, list(ADM1_NAME, ADM2_NAME)])
d2 <- lapply(d2, function(x) split(x, x$ADM1_NAME))
d2 <- lapply(d2, function(x) lapply(x, function(y) y$ADM2_NAME))

saveRDS(d2, "../../hc-cell5m/rdb/g2_2008v09.list.rds", compress=T)


# Compare original and webified versions
dt <- data.table(g2@data)
dtw <- data.table(g2.web@data)
tmp <- dt[, length(unique(ADM2_CODE)), keyby=ADM0_NAME]
tmpw <- dtw[, length(unique(ADM2_CODE)), keyby=ADM0_NAME]
# Which countries are missing?
tmp[!tmpw][, ADM0_NAME]
# [1] British Indian Ocean Territory Cape Verde                     Glorioso Island
# [4] Juan de Nova Island            Mauritius                      Réunion
# [7] Saint Helena                   Sao Tome and Principe          Seychelles
tmp <- tmpw[tmp]
setnames(tmp, 2:3, c("g2.web", "g2"))
tmp <- tmp[g2!=g2.web]
tmp[, diff := g2-g2.web]
tmp[, sum(diff)]
# [1] 398 missing districts



#####################################################################################
# 2015.01.29 Update: GAUL 2015
#####################################################################################
# Switch to latest FAO GAUL 2015 boundaries
# http://www.fao.org/geonetwork/srv/en/resources.get?id=12691&fname=g2015_2014_2.zip
# Switch to rstudio/leaflet R package allowing binding leaflet to sp objects (no need
# to pre-process json lists)
# We only need to pre-process the raster summaries across all districts `dt2`
# Also add ERA monthly estimates
# Then update the shiny app to use rstudio/leaflet and the new input datasets

library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(data.table)

setwd("/home/projects/shiny")

## ECMWF Re-analysis Dataset (ERA)
## Synoptic monthly means, Surface, Total precipitation, Volumetric soil water layer 1,
## interim_full_month, 1979-01-01...2014-12-01, Forecast, ERA Interim
# Documented at http://www.hydrol-earth-syst-sci.net/19/389/2015/hess-19-389-2015.pdf
url <- "http://download.ecmwf.org/data/web219/netcdf-web219-20150202010350-10683-25927.nc"
download.file(url, "../../hc-data/CRU_TS.3.22/era-interim.monthly.pre.water.1979-2014.nc", mode="wb")

era <- brick("../../hc-data/CRU_TS.3.22/era-interim.monthly.pre.water.1979-2014.nc", varname="tp")
extent(era)
head(names(era))
tail(names(era))
tm <- seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month")
era <- setZ(era, tm, "month")

tmp <- g2.web[g2.web$ADM0_NAME=="Nigeria",]
tmp <- crop(era, tmp)
spplot(tmp, 50, add=T)
plot(tmp, col="red")
# Looks ok, note that unit is meter, not millimeter


## GAUL 2015
# List of SSA countries
ssa <- fread("../../hc-cell5m/rdb/ssa.csv")
paste0(ssa, collapse="', '")

# GAUL 2014 (2015 eds) was converted and simplified on local using QGIS, load here
# Used QGIS simplify with tolerance=
g2 <- readOGR("../../hc-cell5m/rdb", "g2015_2014_2_SSA")
plot(g2[g2$ADM0_NAME=="Ghana",])

# Merge all features by admin codes
g2.dt <- data.table(g2@data)
g2.dt[, rn := row.names(g2)]
setkey(g2.dt, ADM0_CODE, ADM1_CODE, ADM2_CODE)
tmp <- unique(g2.dt)
nrow(g2.dt)
nrow(tmp)
g2.dt[duplicated(g2.dt)]
g2.dt[ADM2_CODE==22602]
plot(g2[g2$ADM2_CODE==22602,])
# Nigeria Abia Member State has duplicated ADM2_CODE, correct here
g2.dt[ADM2_NAME=="Ukwa West", ADM2_CODE := 22603]
setkey(g2.dt, rn)
g2@data <- g2.dt[row.names(g2)]
writeOGR(g2, "../../hc-cell5m/rdb", "g2015_2014_2_SSA_web", "ESRI Shapefile")


# Simplify features (used service at http://mapshaper.org/)
g2.web <- readOGR("../../hc-cell5m/rdb", "g2015_2014_2_SSA_web")
proj4string(g2.web) <- CRS("+init=epsg:4326")
# Merge attributes using `FID`
setkey(g2.dt, rn)
g2.web.dt <- g2.dt[as.character(g2.web$FID)]
g2.web@data <- g2.web.dt
plot(g2.web[g2.web$ADM0_NAME=="Ghana",])
plot(g2[g2$ADM0_NAME=="Togo",], add=T)


# Create well-formatted country, province, district list for re-use in input controls
d2 <- data.table(g2.web@data)
d2 <- d2[, .N, by=list(ADM0_NAME, ADM1_NAME, ADM2_NAME)]
d2 <- d2[, lapply(.SD, as.character), .SDcols=1:3]
setkey(d2, ADM0_NAME, ADM1_NAME, ADM2_NAME)
d2 <- split(d2, d2$ADM0_NAME)
d2 <- lapply(d2, function(x) x[, list(ADM1_NAME, ADM2_NAME)])
d2 <- lapply(d2, function(x) split(x, x$ADM1_NAME))
d2 <- lapply(d2, function(x) lapply(x, function(y) y$ADM2_NAME))


# Save
f <- c("ADM0_CODE", "ADM0_NAME", "ADM1_CODE", "ADM1_NAME", "ADM2_CODE", "ADM2_NAME")
g2 <- g2[, f]
g2.web <- g2.web[, f]
saveRDS(g2, "../../hc-cell5m/rdb/g2_2014v15.rds", compress=T)
saveRDS(g2.web, "../../hc-cell5m/rdb/g2_2014v15.web.rds", compress=T)
saveRDS(d2, "../../hc-cell5m/rdb/g2_2014v15.list.rds", compress=T)


# Helper - summarize raster over districts
genStats <- function(var) {
  dt <- extract(get(var), g2.web, fun=mean, na.rm=T, df=T, small=T)
  dt <- cbind(g2.web@data, dt)
  dt <- data.table(dt)
  setnames(dt, 8:dim(dt)[2], format(tm, "%Y-%m-%d"))
  dt <- melt(dt, id.vars=c(names(g2.web), "ID"), variable.name="month", variable.factor=F)
  dt[, month := as.Date(month)]
  # Limit to 1960 onwards
  dt <- dt[month>=as.Date("1960-01-01")]
  return(dt)
}

# Helper - add symbology (period mean)
genSymbol <- function(x, col) {
  # Summarize each district over entire period for mapping (mm/month)
  dt <- x[, list(
    mean=mean(value, na.rm=T),
    min=min(value, na.rm=T),
    `85th`=quantile(value, 0.85, na.rm=T),
    max=max(value, na.rm=T),
    sd=sd(value, na.rm=T)), keyby=list(ADM0_CODE, ADM1_CODE, ADM2_CODE)]

  # Construct symbology from district means (note that PDSI has fewer values)
  rg <- range(dt$mean, na.rm=T)
  cv <- try(classInt::classIntervals(dt$mean, n=min(c(5, dt[,length(unique(mean))])))$brks)

  if (class(cv)=="try-error") {
    # classInt fails apply only 1 color (white)
    dt[, cl := "white"]
  } else {
    # Symbolize normally
    dt[, cl := cut(mean, unique(c(rg[1]-1, cv, rg[2]+1)), cutlabels=F, ordered_result=T)]
    dt[, cl := colorRampPalette(col)(length(cv)+1)[cl]]
  }
  return(dt)
}


##  Pre-process district summaries for speed
# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `pre`
pre <- brick("../../hc-data/CRU_TS.3.22/cru_ts3.22.1901.2013.pre.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
pre <- setZ(pre, tm, "month")
col <- rev(c("#2F6FBF", "#69DB4D", "#F9EF58", "#DC5207", "#830000"))
dt2.pre <- genStats("pre")
dt2.pre.web <- genSymbol(dt2.pre, col)
saveRDS(dt2.pre, "./tmp/dt2.pre.rds")


# Pre-process `pdsi`
pdsi <- brick("../../hc-data/CRU_TS.3.22/pdsisc.monthly.maps.1850-2012.nc")
tm <- seq(as.Date("1850-01-01"), as.Date("2012-12-31"), "month")
pdsi <- setZ(pdsi, tm, "month")
col <- c("#FF9900", "#FFFF66", "#FFFFFF", "#99FF99", "#009900")
dt2.pdsi <- genStats("pdsi")
dt2.pdsi.web <- genSymbol(dt2.pdsi, col)
saveRDS(dt2.pdsi, "./tmp/dt2.pdsi.rds")


# Pre-process `eratp` total precipitation (tp)
eratp <- brick("../../hc-data/CRU_TS.3.22/era-interim.monthly.pre.water.1979-2014.nc", varname="tp")
tm <- seq(as.Date("1979-01-01"), as.Date("2014-12-01"), "month")
eratp <- setZ(eratp, tm, "month")
col <- brewer.pal(9, "YlGnBu")
dt2.eratp <- genStats("eratp")
dt2.eratp[, value := value*1000]
dt2.eratp.web <- genSymbol(dt2.eratp, col)
saveRDS(dt2.eratp, "./tmp/dt2.eratp.rds")


# Save all
save(d2, g2, g2.web,
  dt2.pre, dt2.pdsi, dt2.eratp,
  dt2.pre.web, dt2.pdsi.web, dt2.eratp.web,
  file="./tmp/rainfall_2014v15.RData", compress=T)


## Create country .rds files (until rstudio/leaflet package is fixed)
var <- c("pre", "pdsi", "eratp")
cntr <- unique(g2.web@data$ADM0_CODE)

for(j in var) {
  for (i in cntr) {
    dt <- get(paste0("dt2.", j, ".web"))[ADM0_CODE==i]
    dt[is.nan(mean) | is.infinite(mean), mean := NA]
    dt[is.nan(min) | is.infinite(min), min := NA]
    dt[is.nan(max) | is.infinite(max), max := NA]
    dt[is.nan(`85th`) | is.infinite(`85th`), `85th` := NA]
    dt[is.nan(sd) | is.infinite(sd), sd := NA]

    dt[, mean := round(mean, 1)]
    dt[, min := round(min, 1)]
    dt[, max := round(max, 1)]
    dt[, `85th` := round(`85th`, 1)]
    dt[, sd := round(sd, 1)]

    # Add admin names
    t <- get(paste0("dt2.", j))[ADM0_CODE==i]
    setkey(dt, ADM1_CODE, ADM2_CODE)
    setkey(t, ADM1_CODE, ADM2_CODE)
    tmp <- unique(t)
    dt <- tmp[, .SD, .SDcols=1:6][dt]

    g <- g2.web[g2.web$ADM0_CODE==i,]
    g@data <- dt[J(g$ADM1_CODE, g$ADM2_CODE)]
    f <- paste0("./data/json/", j, i)
    writeOGR(g, f, g$ADM2_CODE, "GeoJSON", overwrite_layer=T)

    m <- jsonlite::fromJSON(f, simplifyVector=F)
    for (x in 1:length(m$features)) {
      m$features[[x]]$style <- list(fillColor=g@data[x, cl], weight=.6, color="white", fillOpacity=0.7)
    }
    saveRDS(m, paste0("./data/rds/", j, i, ".json.rds"), compress=T)
    saveRDS(t, file=paste0("./data/rds/", j, i, ".rds"), compress=T)
  }
}



#####################################################################################
# 2015.03.18 Update: Add CRU monthly temperatures
#####################################################################################

rm(list=ls())
load("./tmp/rainfall_2014v15.RData")

##  Pre-process district summaries for speed
# Country code list
cntr <- unique(g2.web@data$ADM0_CODE)

# Pre-process `tmp`
tmp <- brick("../../hc-data/CRU_TS.3.22/cru_ts3.22.1901.2013.tmp.dat.nc")
tm <- seq(as.Date("1901-01-16"), as.Date("2013-12-16"), "month")
tmp <- setZ(tmp, tm, "month")
col <- c("#801FEF", "#0000FF", "#4169E1", "#1C90FF", "#00BFFF", "#8CCDEF", "#FFFFC8",
  "#FFE131", "#FFAA00", "#FF6E00", "#FF0000", "#C80000", "#FFB1B1")
dt2.tmp <- genStats("tmp")
dt2.tmp.web <- genSymbol(dt2.tmp, col)
saveRDS(dt2.tmp, "./data/dt2.tmp.rds")

# Save all
save(d2, g2, g2.web,
  dt2.pre, dt2.pdsi, dt2.eratp, dt2.tmp,
  dt2.pre.web, dt2.pdsi.web, dt2.eratp.web, dt2.tmp.web,
  file="./tmp/rainfall_2014v15.RData", compress=T)

# Save country .rds
for (i in cntr) {
  dt <- dt2.tmp.web[ADM0_CODE==i]
  dt[is.nan(mean) | is.infinite(mean), mean := NA]
  dt[is.nan(min) | is.infinite(min), min := NA]
  dt[is.nan(max) | is.infinite(max), max := NA]
  dt[is.nan(`85th`) | is.infinite(`85th`), `85th` := NA]
  dt[is.nan(sd) | is.infinite(sd), sd := NA]

  dt[, mean := round(mean, 1)]
  dt[, min := round(min, 1)]
  dt[, max := round(max, 1)]
  dt[, `85th` := round(`85th`, 1)]
  dt[, sd := round(sd, 1)]

  # Add admin names
  t <- dt2.tmp[ADM0_CODE==i]
  setkey(dt, ADM1_CODE, ADM2_CODE)
  setkey(t, ADM1_CODE, ADM2_CODE)
  temp <- unique(t)
  dt <- temp[, .SD, .SDcols=1:6][dt]

  g <- g2.web[g2.web$ADM0_CODE==i,]
  g@data <- dt[J(g$ADM1_CODE, g$ADM2_CODE)]
  f <- paste0("./data/json/tmp", i)
  writeOGR(g, f, g$ADM2_CODE, "GeoJSON", overwrite_layer=T)

  m <- jsonlite::fromJSON(f, simplifyVector=F)
  for (x in 1:length(m$features)) {
    m$features[[x]]$style <- list(fillColor=g@data[x, cl], weight=.6, color="white", fillOpacity=0.7)
  }
  saveRDS(m, paste0("./data/rds/tmp", i, ".json.rds"), compress=T)
  saveRDS(t, file=paste0("./data/rds/tmp", i, ".rds"), compress=T)
}


#####################################################################################
# TODO 2015.03.18 Update: Use SEAS package for normal and departure statistics
#####################################################################################

library(seas)


#####################################################################################
# TODO 2015.03.24 Update: Add new indicators
#####################################################################################
# total annual precipitation
# precipitation on the days of heavy rain
# maximum number of consecutive dry days in the year
# annual mean maximum and mean minimum temperatures
# Suggested in http://file.scirp.org/Html/7-2360181_50081.htm



#####################################################################################
# 2015.08.02 Update: Update leaflet, merge all data files
#####################################################################################

# Get all rds files in ./data/rds, not *json*
# Combine them all and compute mean stats by districts

pre <- readRDS("./tmp/dt2.pre.rds")
tmp <- readRDS("./tmp/dt2.tmp.rds")
pdsi <- readRDS("./tmp/dt2.pdsi.rds")

pre[, var := "pre"]
tmp[, var := "tmp"]
pdsi[, var := "pdsi"]

dt <- rbind(pre, tmp, pdsi)
saveRDS(dt, file="./tmp/dt2.rds")


