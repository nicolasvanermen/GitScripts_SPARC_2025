# SCRIPT VOOR DE INTERPOLATIE VAN GETIJGEGEVENS IKV SPARC 
# initieel geschreven ikv IP door Joost Vanoverbeke & aangepast door Nicolas Vanermen

# laatste export op 28/05/2025
# laatste check op 19/06 & opgeslagen zonder (commando)aanpassingen

pad_output <- "./CHECK/"

if(!dir.exists(pad_output)) # creeer folder voor output als die nog niet bestaat
  dir.create(pad_output, recursive = TRUE)

#### libraries laden ####
library(tidyverse)
library(readxl)
library(writexl)
library(mgcv)
library(foreign)

#### parameters definiëren ####
# river <- "Durme"
# segment_ID <- 3
# min_afstand <- 0
# max_afstand <- 7
# knots <- 3

river <- "Zeeschelde"
segment_ID <- 1
min_afstand <- 30
max_afstand <- 80
knots <- 7

#### inlezen WL data ####
OD_basis <-
  read_excel("../01_data/00_basisdata_WL/Water level data/Overspoeling_percentiles/Statistics (overspoeling) 4QNplusQE.xls")

# oplijsten tijposten in de dataset
tijposten <- names(OD_basis %>% select(!"Row"))
tijposten
# we kregen van het WL data van 45 tijposten

#### inlezen GIS-gegenereerde tabellen #### 
tijpostdist_new <- 
  read.dbf("../03_habitats/GIS/Shapes/TimeSeries_post_coordinates_Join_2_Thalweg.dbf")
# deze tabel is het resultaat van een near-bewerking in ArcGIS tussen de coordinaten van (45 + 8 = 53) tijposten en
# een shapefile met de afstanden tot de grens / monding (Thalweg_50m_INBO);  
tijpostdist_new <- tijpostdist_new %>% select(SegmentID, Dist, Station_na) %>% 
  rename(segment = SegmentID,
         afstand = Dist,
         tijpost = Station_na)
tijpostdist_new %>% count(segment)

# test of er geen mismatch is tussen de tijpost-namen
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
summary(ALLOCNR)

#### omzetten overspoelingsduur naar droogvalduur ####
OD_basis <-
  OD_basis %>% 
  mutate(ODperc = str_remove_all(Row, " |WL|\\%") %>% 
           as.numeric(),
         DD = 100 - ODperc) %>% 
  select(-Row)

#### herschikken data van 'wide' naar 'long' ####
OD_basis_PL <- 
  OD_basis %>% 
  pivot_longer(all_of(tijposten), 
               names_to = "tijpost",
               values_to = "hoogte")

#### afstanden toevoegen aan de dataset ####
OD_basis_PL <- 
  OD_basis_PL %>% 
  left_join(tijpostdist_new) %>% 
  filter(!is.na(tijpost)) # selecteer enkel de rijen met gegevens over tijpost
 
#### controlefiguur ####
OD_basis_PL %>% 
  filter(segment == segment_ID) %>% 
  ggplot(aes(afstand, hoogte, group = DD, color = DD)) +
  geom_line() + 
  geom_point(size = 2) +
  labs(title = str_c(river, " 4QN+QE"))

#### subsets definieëren ####
DD_4QNQE <- 
  OD_basis_PL %>% 
  filter(segment == segment_ID,
         DD %in% c(25, 50, 75),
         afstand > min_afstand, 
         afstand < max_afstand)

DD_4QNQE %>% 
  ggplot(aes(afstand, hoogte, group = DD, color = DD)) +
  geom_line() + 
  geom_point(size = 2)

# selectie data voor DD percentages 25 en 75
DD_4QNQE_75 <- 
  DD_4QNQE %>% 
  filter(DD == 75)
DD_4QNQE_25 <- 
  DD_4QNQE %>% 
  filter(DD == 25)

#### GAM modellen ####
gam_hoogte_DD_75 <- 
  gam(hoogte ~ s(afstand, bs = "tp", k = knots), 
      data = DD_4QNQE_75)

gam_hoogte_DD_25 <- 
  gam(hoogte ~ s(afstand, bs = "tp", k = knots), 
      data = DD_4QNQE_25)

# controle
summary(gam_hoogte_DD_75)
summary(gam_hoogte_DD_25)

####  interpolatiewaarden genereren #### 
predict_hoogte_DD_75 <- 
  expand.grid(afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>% 
  {bind_cols(., predict(gam_hoogte_DD_75, newdata = ., se = TRUE))} %>% # predicties op basis van GAM
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = segment_ID) 

predict_hoogte_DD_25 <- 
  expand.grid(afstand = round(seq(min_afstand, max_afstand, by = 0.05), 2)) %>% 
  {bind_cols(., predict(gam_hoogte_DD_25, newdata = ., se = TRUE))} %>% # predicties op basis van GAM
  rename(hoogte = fit,
         se_hoogte = se.fit) %>% 
  mutate(lwr_hoogte = hoogte - 1.96*se_hoogte, # 95% confidentie interval
         upr_hoogte = hoogte + 1.96*se_hoogte,
         segment = segment_ID) 

#### koppeling met ALLOCNR ####
predict_hoogte_DD_75 <-
  predict_hoogte_DD_75 %>% 
  left_join(ALLOCNR)
summary(predict_hoogte_DD_75)

predict_hoogte_DD_25 <-
  predict_hoogte_DD_25 %>% 
  left_join(ALLOCNR)
summary(predict_hoogte_DD_25)

#### controlefiguren #### 
predict_hoogte_DD_75 %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line () + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5) +
  geom_point(data = DD_4QNQE_75, colour ="red") +
  labs(x = "Afstand tot de monding (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - droogvalduur = 75%"))
ggsave(paste0(pad_output, "interpolatie_overspoeling_4QN+QE_75_", river, ".jpeg"),
       dpi= 300,
       width = 7, height = 5)

predict_hoogte_DD_25 %>% 
  ggplot(aes(x = afstand, y = hoogte)) +
  geom_line () + 
  geom_ribbon (aes(ymin = lwr_hoogte, ymax = upr_hoogte), alpha = 0.5) +
  geom_point(data = DD_4QNQE_25, colour ="red") +
  labs(x = "Afstand tot de monding (km)",
       y = "Voorspelde waterhoogte (m TAW)",
       title = paste0(river, " - droogvalduur = 25%"))
ggsave(paste0(pad_output, "interpolatie_overspoeling_4QN+QE_25_", river, ".jpeg"),
       dpi= 300,
       width = 7, height = 5)

#### resultaten wegschrijven ####
file_output <- paste0("interpolatie_overspoeling_4QN+QE_", river, ".xls")

write_xlsx(list(hoogte_DD_25 = predict_hoogte_DD_25,
                hoogte_DD_75 = predict_hoogte_DD_75), 
           str_c(pad_output, file_output))

