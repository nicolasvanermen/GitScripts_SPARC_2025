# TOEPASSING VAN SEDIMENTATIETOOL VOOR GGG SPARC GEBIEDEN 
# Nicolas Vanermen
# laatste export & check op 07/07/2025

#### import libraries ####
library(tidyverse)
library(sf)
library(raster)
library(terra)

raster_output <- "../04_sedimentation/Exports/Rasters/"

#### import data ####
DTMs <-  c("GrootBroek_2022", "GrootSchoorBornem_2022", "GrootSchoorHamme_2025", 
           "KleinBroek_2025", "Uiterdijk_2022")

raster_path <- "../03_habitats/GIS/Rasters/"

for (i in DTMs){
  file <- raster(paste0(raster_path, i, "_estuarien1.tif"))
  assign(i, file)
}

mean_m_TAW <- read.csv("./Exports/mean_m_TAW_Ontpoldering.csv")
model <- read.csv("./Exports/Results simulation/model_summaries_Ontpoldering_DTM_split.csv")

#### sedimentation prediction ####
for (i in DTMs){
# turn rastervalues into their respective percentile values
file <- rast(get(i))
vals <- values(file)
ecdf_fun <- ecdf(vals)
r_pct <- app(file, fun = ecdf_fun)
r_pct100 <- r_pct * 100
r_pct100_tif <- raster(r_pct100)

# extract relevant coefficients
P0 <- mean_m_TAW %>% filter(gebied == sub("_....", "", i)) %>% pull(m_TAW_P0) %>% first()
intercept <- model %>% dplyr::select(sub("_....", "", i)) %>% first() %>% pull()
slope <- model %>% dplyr::select(sub("_....", "", i)) %>% last() %>% pull()

# turn percentile raster values into the 2050 prediction & export
DTM_2050 <- exp(intercept + slope*log(r_pct100)) + P0
DTM_2050_tif <- raster(DTM_2050)
assign(paste(sub("_....", "", i), "2050", sep = "_"), DTM_2050_tif)
writeRaster(DTM_2050_tif, paste0(raster_output, sub("_....", "", i), "_2050.tif"), overwrite = TRUE)
}
