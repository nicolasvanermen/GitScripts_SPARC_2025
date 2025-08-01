# SCRIPT TER VOORBEREIDING VAN HABITATMAPPING IKV SPARC 
# Extractie van DD percentielen binnen een 4QN1QE tijdsreeks voor tijposten waarvoor WL geen 'statistics' tabellen aanleverde (GGG posten)
# Geschreven door Nicolas Vanermen

# laatste export op 09/07/2025

pad_output <- "./Exports 2025 07 09/"

if(!dir.exists(pad_output))
  dir.create(pad_output, recursive = TRUE)

#### libraries laden ####
library(tidyverse)
library(foreign)
library(writexl)

#### inputs ####
QN4QE1_SPARC <- read_csv("./Exports 2025 06 19/QN4QE1_SPARC.csv")

#### calculate DD percentiles #### 
study_areas <- as.vector(unique(QN4QE1_SPARC$area))
study_areas

DD <- NULL
for (i in study_areas)
{
  temp <- QN4QE1_SPARC %>% 
    filter(area == i) %>%
    pull(h) %>% 
    quantile(c(0.04, 0.35, 0.60, 0.85, 0.995))
  DD <- bind_cols(DD, temp)
}

colnames(DD) <- study_areas
DD

DD <- DD %>% mutate(DD_percentile = c(0.04, 0.35, 0.60, 0.85, 0.995)) %>% 
  relocate(DD_percentile, .before = NULL) %>% 
  arrange(DD_percentile)
DD
DD <- DD %>% 
  rename(DeBunt = De_Bunt,
         WalZwijn = Grote_Wal_Zwijn)

#### wrangle the table to the appropriate python input ####
DD_adapted <- pivot_longer(DD, c(2:4)) %>% 
  rename(Area = name, hoogte = value)

DD_adapted

#### export ####
write_xlsx(list(hoogte_DD_04 = DD_adapted %>% filter(DD_percentile == 0.04),
                hoogte_DD_35 = DD_adapted %>% filter(DD_percentile == 0.35),
                hoogte_DD_60 = DD_adapted %>% filter(DD_percentile == 0.6),
                hoogte_DD_85 = DD_adapted %>% filter(DD_percentile == 0.85),
                hoogte_DD_995 = DD_adapted %>% filter(DD_percentile == 0.995)),
           paste0(pad_output, "DD_percentielen_GGG_SPARC.xls"))
