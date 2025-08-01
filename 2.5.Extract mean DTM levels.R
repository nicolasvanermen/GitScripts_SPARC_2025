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

DTMs_2050 <-  c("GrootBroek_2050", "GrootSchoorBornem_2050", "GrootSchoorHamme_2050", 
           "KleinBroek_2050", "Uiterdijk_2050")

raster_path_2 <- "./Exports/Rasters/"

for (i in DTMs_2050){
  file <- raster(paste0(raster_path_2, i, ".tif"))
  assign(i, file)
}

#### get mean DTM levels #### 
mean_reference <- NULL
for (i in DTMs){
  # turn rastervalues into their respective percentile values
  file <- rast(get(i))
  mean <- mean(values(file), na.rm = T)
  mean_reference <- cbind(mean_reference, mean)
  colnames(mean_reference)[ncol(mean_reference)] <- i
  }
mean_reference
  

mean_reference_2050 <- NULL
for (i in DTMs_2050){
  # turn rastervalues into their respective percentile values
  file <- rast(get(i))
  mean <- mean(values(file), na.rm = T)
  mean_reference_2050 <- cbind(mean_reference_2050, mean)
  colnames(mean_reference_2050)[ncol(mean_reference_2050)] <- i
}
mean_reference_2050

DTMs_rownames <-  c("GrootBroek", "GrootSchoorBornem", "GrootSchoorHamme", 
                "KleinBroek", "Uiterdijk")

mean_DTM_levels <- as.data.frame(cbind(DTMs_rownames, as.vector(mean_reference), as.vector(mean_reference_2050)))
colnames(mean_DTM_levels) <- c("Areas","DTM_reference","DTM_2050")
mean_DTM_levels <- mean_DTM_levels %>% mutate(delta = as.numeric(DTM_2050) - as.numeric(DTM_reference))
mean_DTM_levels
write.csv(mean_DTM_levels, "./Exports/mean_DTM_levels.csv")
