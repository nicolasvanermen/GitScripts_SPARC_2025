# SCRIPT VOOR DE INTERPOLATIE VAN GETIJGEGEVENS IKV SPARC 
# initieel geschreven ikv IP door Joost Vanoverbeke & aangepast door Nicolas Vanermen

# laatste export op 28/05/2025
# laatste check op 19/06 & opgeslagen zonder (commando)aanpassingen

pad_output <- "./CHECK/"

if(!dir.exists(pad_output))
  dir.create(pad_output, recursive = TRUE)

#### libraries laden ####
library(tidyverse)
library(readxl)
library(writexl)
library(mgcv)
library(foreign)

#### parameters definiëren ####
# river <- "Durme"
# Segment_ID <- 3
# min_afstand <- 0
# max_afstand <- 7
# knots <- 3
# n_poly <- 2
# model_BE <- "predict_hoogte_OF_BE_lm"

river <- "Zeeschelde"
Segment_ID <- 1
min_afstand <- 30
max_afstand <- 80
knots <- 7
n_poly <- 3
model_BE <- "predict_hoogte_OF_BE_gam"

#### inlezen WL data ####
HW_LW_basis <-
  read_excel("../01_data/00_basisdata_WL/Water level data/Percentiles_HW_LW_2025-05-14_Aanvulling/Statistics HW LW 4QNplusQE NVE.xls")

# oplijsten tijposten in de dataset
tijposten <- names(HW_LW_basis %>% select(!"Row"))
tijposten
# we kregen van het WL data van 45 tijposten

#### inlezen GIS-gegenereerde tabellen #### 
tijpostdist_new <- 
  read.dbf("../03_habitats/GIS/Shapes/TimeSeries_post_coordinates_Join_2_Thalweg.dbf")
# deze file is het resultaat van een near-bewerking in ArcGIS tussen de coordinaten van (45 + 8 = 53) posten en
# een shapefile met de afstanden tot de monding (Thalweg_50m_INBO);  
tijpostdist_new <- tijpostdist_new %>% select(SegmentID, Dist, Station_na) %>% 
  rename(segment = SegmentID,
         afstand = Dist,
         tijpost = Station_na)
tijpostdist_new %>% count(segment)

# test of er geen mismatch is tussen de tijpost namen
setdiff(tijposten, tijpostdist_new$tijpost)
setdiff(tijpostdist_new$tijpost, tijposten)
# dit zijn de 8 extra posten in de gebieden zelf, dewelke dus niet opgenomen zijn in de standaard 'statistics' tabellen van het WL

ALLOCNR <- 
  read.dbf(paste0("../03_habitats/GIS/Shapes/Allocatie_Join_Thalweg_", river, ".dbf")) %>% 
  rename(segment = SegmentID,
         afstand = Dist) %>% 
  select(afstand, segment, AllocNR) %>% 
  mutate(afstand = round(afstand, 2))
# deze tabel is het resultaat van een join tussen de thalweg en het allocatiegrid

#### extractie HW|LW en OF uit 'Row' ####
HW_LW_basis <-
  HW_LW_basis %>% 
  mutate(HWLW = str_extract(Row, "HW|LW"),
         OFperc = str_extract(Row, "[:digit:]+(\\.[:digit:]+)?") %>% 
           as.numeric()) %>% 
  select(-Row)

HW_LW_basis %>% count(HWLW, OFperc) %>% summary()

#### herschikken data van 'wide' naar 'long' ####
HW_LW_basis_PL <- 
  HW_LW_basis %>% 
  pivot_longer(all_of(tijposten), 
               names_to = "tijpost",
               values_to = "hoogte")

#### afstanden toevoegen aan de dataset ####
HW_LW_basis_PL <- 
  HW_LW_basis_PL %>% 
  left_join(tijpostdist_new) 

#### controlefiguren ####
HW_LW_basis_PL %>% 
  dplyr::filter(HWLW == "HW",
         segment == Segment_ID) %>% 
  ggplot(aes(afstand, hoogte, group = OFperc, color = OFperc)) +
  geom_line() + 
  geom_point(size = 2) +
  labs(title = str_c("Hoogwater", " 4QN+QE"))

HW_LW_basis_PL %>% 
  dplyr::filter(HWLW == "LW",
         segment == Segment_ID) %>% 
  ggplot(aes(afstand, hoogte, group = OFperc, color = OFperc)) +
  geom_line() + 
  geom_point(size = 2) +
  labs(title = str_c("Laagwater", " 4QN+QE"))

#### interpolatie hoogwaterstanden 90% ####
HW_4QNQE <-
  HW_LW_basis_PL %>% 
  dplyr::filter(HWLW == "HW",
         segment == Segment_ID,
         OFperc == 90,
         afstand < max_afstand,
         afstand > min_afstand)

##### GAM model #####
gam_hoogte_OF_HW_90 <- 
  gam(hoogte ~ s(afstand, bs = "tp", k = knots), 
      data = HW_4QNQE)
# controle
summary(gam_hoogte_OF_HW_90)

##### interpolatiewaarden genereren ##### 
predict_hoogte_OF_HW_90 <- 
  expand.grid(HWLW = "HW",
              afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>%
  {bind_cols(., predict(gam_hoogte_OF_HW_90, newdata = ., se = TRUE))} %>% # predicties op basis van GAM
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = Segment_ID) 

##### koppelen aan AllocNR ##### 
predict_hoogte_OF_HW_90 <-
  predict_hoogte_OF_HW_90 %>% 
  left_join(ALLOCNR)

##### controlefiguur ##### 
predict_hoogte_OF_HW_90 %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line() + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5)+
  geom_point(data = HW_4QNQE, colour ="red") +
  labs(x = "Afstand tot de grens (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - 90% overspoelingshoogte HW"))
ggsave(paste0(pad_output, "interpolatie_waterstand_4QN+QE_HW_90_", river, ".jpeg"),
       dpi = 300,
       width = 7, height = 5)

#### interpolatie laagwaterstanden 10% ####
LW_4QNQE <- 
  HW_LW_basis_PL %>% 
  dplyr::filter(HWLW == "LW",
         segment == Segment_ID,
         OFperc == 10,
         afstand < max_afstand,
         afstand > min_afstand)

##### GAM model #####
gam_hoogte_OF_LW_10 <- 
  gam(hoogte ~ s(afstand, bs = "tp", k = knots), 
      data = LW_4QNQE)
# controle
summary(gam_hoogte_OF_LW_10)

##### interpolatiewaarden genereren ##### 
predict_hoogte_OF_LW_10 <- 
  expand.grid(HWLW = "LW",
              afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>% 
  {bind_cols(., predict(gam_hoogte_OF_LW_10, newdata = ., se = TRUE))} %>% # predicties op basis van GAM
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = Segment_ID) 

##### koppelen aan AllocNR ##### 
predict_hoogte_OF_LW_10 <-
  predict_hoogte_OF_LW_10 %>% 
  left_join(ALLOCNR)

##### controlefiguur #####
predict_hoogte_OF_LW_10 %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line() + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5)+
  geom_point(data = LW_4QNQE, colour ="red") +
  labs(x = "Afstand tot de grens (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - 10% overspoelingshoogte LW"))
ggsave(paste0(pad_output, "interpolatie_waterstand_4QN+QE_LW_10_", river, ".jpeg"),
       dpi = 300,
       width = 7, height = 5)

#### interpolatie hoogwaterstanden 0.5% ####
BE_4QNQE <- 
  HW_LW_basis_PL %>% 
  dplyr::filter(HWLW == "HW",
                segment == Segment_ID,
                OFperc == 0.5,
                afstand < max_afstand,
                afstand > min_afstand)

##### GAM & polynomiaal model ##### 
gam_hoogte_OF_BE <- 
  gam(hoogte ~ s(afstand, bs = "tp", k = knots), 
      data = BE_4QNQE)

lm_hoogte_OF_BE <- 
     lm(hoogte ~ poly(afstand, n_poly), 
         data = BE_4QNQE)

# controle
summary(gam_hoogte_OF_BE)
summary(lm_hoogte_OF_BE)

##### interpolatiewaarden & controlefiguren genereren ##### 
predict_hoogte_OF_BE_lm <- 
  expand.grid(HWLW = "HW",
              afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>% 
  {bind_cols(., predict(lm_hoogte_OF_BE, newdata = ., se = TRUE))} %>% 
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = Segment_ID) 

predict_hoogte_OF_BE_lm %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line() + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5)+
  geom_point(data = BE_4QNQE, colour ="red") +
  labs(x = "Afstand tot de grens (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - 0.5% overspoelingshoogte HW lm"))

predict_hoogte_OF_BE_gam <- 
  expand.grid(HWLW = "HW",
              afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>% 
  {bind_cols(., predict(gam_hoogte_OF_BE, newdata = ., se = TRUE))} %>% 
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = Segment_ID) 

predict_hoogte_OF_BE_gam %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line() + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5) +
  geom_point(data = BE_4QNQE, colour ="red") +
  labs(x = "Afstand tot de grens (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - 0.5% overspoelingshoogte HW gam"))

##### koppelen aan AllocNR##### 
predict_hoogte_OF_BE <-
  get(model_BE) %>% 
  left_join(ALLOCNR)


##### finale controlefiguur #####
predict_hoogte_OF_BE %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line() + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5)+
  geom_point(data = BE_4QNQE, colour ="red") +
  labs(x = "Afstand tot de grens (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - 0.5% overspoelingshoogte HW"))
ggsave(paste0(pad_output, "interpolatie_waterstand_4QN+QE_BE_", river, ".jpeg"),
       dpi = 300,
       width = 7, height = 5)


#### resultaten wegschrijven ####
file_output <- paste0("interpolatie_waterstand_4QN+QE_", river, ".xls")

write_xlsx(list(hoogte_OF_HW_90 = predict_hoogte_OF_HW_90,
                hoogte_OF_LW_10 = predict_hoogte_OF_LW_10,
                hoogte_OF_BE = predict_hoogte_OF_BE), 
           str_c(pad_output, file_output))

