# SCRIPT TER VOORBEREIDING VAN SEDIMENTATIETOOL IKV SPARC 
# Nicolas Vanermen
# laatste export & check op 02/07/2025

#### import libraries ####
library(tidyverse)
library(sf)
library(raster)

#### import data ####
DTMs <-  c("DeBunt_2022", "GrootBroek_2022", "GrootSchoorBornem_2022", "GrootSchoorHamme_2025", 
            "KleinBroek_2025", "Uiterdijk_2022", "Vlassenbroek_2022", "WalZwijn_2022")


raster_path <- "../03_habitats/GIS/Rasters/"
for (i in DTMs){
file <- raster(paste0(raster_path, i, "_estuarien1.tif"))
assign(i, file)
}
rm(file)

#### extract DTM values ####
Extraction <- NULL
for (i in DTMs){
  Extraction_DTM <- as.data.frame(get(i))
  colnames(Extraction_DTM) <- "value"
  Extraction_DTM <- Extraction_DTM %>% 
    filter(!is.na(value)) %>% 
    mutate(DTM = i) %>% 
    relocate(DTM, .before =  NULL)
  Extraction <- rbind(Extraction, Extraction_DTM)
}
rm(Extraction_DTM)

summary(Extraction)
table(Extraction$DTM)

#### calculate percentiles ####
Percentiles <- NULL
for (i in DTMs)
  {
  Percentiles_DTM <- Extraction %>% 
      filter(DTM == i) %>%
      pull(value) %>% 
      quantile(seq(from = 0, to = 1, by = 0.01))
  Percentiles_DTM <- as.data.frame(Percentiles_DTM) %>% 
    mutate(percentiel = seq(from = 0, to = 100, by = 1),
           DTM = i) %>% 
    rename(value = Percentiles_DTM) %>% 
  dplyr::select(percentiel, DTM, value)
    
  Percentiles <- rbind(Percentiles, Percentiles_DTM)
  }
rm(Percentiles_DTM)
summary(Percentiles)

#### export files ####
write.csv(Extraction, "./Exports/Extraction_DTMs_estuarien.csv")
write.csv(Percentiles, "./Exports/Percentiles_DTMs_estuarien.csv")

#### graphical check #### 
for (i in DTMs)
{
  print(ggplot(Percentiles %>% filter(DTM == i), aes(percentiel, value)) + geom_line() + labs(title = i))
}
