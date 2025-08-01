# SCRIPT TER VOORBEREIDING VAN SEDIMENTATIETOOL IKV SPARC 
# Nicolas Vanermen
# laatste export & check op 02/07/2025

pad_output <- "./Exports/"
pad_output_graphs <- "./Exports/Tide graphs/"

#### import libraries ####
library(readxl)
library(tidyverse)
library(Tides)
library(ggplot2)

#### read WL data ####
QN <- read_excel("../01_data/00_basisdata_WL/Water level data/TimeSeries_WaterLevel.xlsx", sheet = "Water level QN")
QE <- read_excel("../01_data/00_basisdata_WL/Water level data/TimeSeries_WaterLevel.xlsx", sheet = "Water level QE")

# specify data type
QN <- QN %>% mutate(type = "QN")
QE <- QE %>% mutate(type = "QE")

# rename & select relevant columns
QN_QE <- rbind(QN, QE) %>% 
  rename("time" = "Time",
         "Grote_Wal_Zwijn" = "Gtote Wal Zwijn",
         "De_Bunt" = "De Bunt") %>% 
  dplyr::select("time","type",
         "Grote_Wal_Zwijn","De_Bunt","Vlassenbroek", "Durme extra 1", "Temse","Sint-Amands", "Zeeschelde extra 4")

#### prepare files for Tides package #### 
QN_QE_PL <- QN_QE %>% 
  pivot_longer(3:9) %>% 
  rename(area = name,
         h = value) %>% 
  mutate(time = dmy_hms(time))

#### extract HW & LW applying Tides ####  
# applying default settings but with hoffset = 0.1 & h0 = -0.5 to avoid 'edge effects' and to allow negative values (Temse)
QN_QE_tides <-
  QN_QE_PL %>% 
  group_by(area, type) %>%
  nest() %>% 
  transmute(HL = map(data, ~ extrema(., h0 = -0.5, T2 = 5*60*60, hoffset = 0.1, filtconst = 1)$HL)) %>% 
  unnest(cols = everything()) %>% 
  ungroup() %>%
  rename(fase = HL) %>% 
  mutate(fase = recode(fase, L = "LW", H = "HW")) %>% 
  dplyr::select(!h0)

#### join all tabels into a QN4QE1 file & export #### 
QN_QE_tides_final <- left_join(QN_QE_PL, QN_QE_tides) %>% arrange(area, time)

QN_QE_tides_final %>% glimpse()
QN_QE_tides_final %>% count(fase)
QN_QE_tides_final %>% count(area)
QN_QE_tides_final %>% count(type)

# er worden aan het begin en het eind van een datareeks soms verkeerde HW's en LW's toegekend, we snijden daarom de 1e en laatste dag van elke reeks:
QN_QE_tides_final %>% filter(fase == "LW", h > 3)
QN %>% mutate(Time = dmy_hms(Time)) %>% summarise(min = min(Time), max = max(Time))
QE %>% mutate(Time = dmy_hms(Time)) %>% summarise(min = min(Time), max = max(Time))

QN_QE_tides_final <- QN_QE_tides_final %>% 
  mutate(Date  = as.Date(time)) %>% 
  mutate(dummy1 = case_when(type == "QN" & Date > "2013-08-01" & Date < "2013-11-01" ~ "keep"),
         dummy2 = case_when(type == "QE" & Date > "2013-08-20" & Date < "2013-09-06" ~ "keep")) %>% 
  filter(dummy1 == "keep" | dummy2 == "keep") %>% 
  select(!c(Date, dummy1, dummy2))

QN_QE_tides_final %>% filter(fase == "LW", h > 3)
#check!

QN4QE1_tides_final <- rbind(QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QE"))

write.csv(QN4QE1_tides_final, paste0(pad_output,"QN4QE1_SPARC_all_areas.csv"))


#### plot & save QN & QE series per area #### 
study_areas <- colnames(QN_QE %>% dplyr::select(3:9))

for (j in c("QE","QN"))
  {
for (i in study_areas)
  {
  plot <- ggplot() +
    geom_line(data = QN_QE_tides_final %>% filter(area == i, type == j), 
              aes(x = time, y = h), color = "red4", linewidth = 0.1) +
    labs(x = "Time", y = "Waterstand (m TAW)", title = paste(j, "_", i, sep = "")) +
    theme_bw()
  
  ggsave(
    plot,
    filename = paste(i, "_", j, ".png", sep = ""), 
    path = pad_output_graphs,
    width = 15, height = 10, units = "cm", dpi = 300)
  }}
plot

####  plot & save QN_HW_LW & QE_HW_LW series per area #### 
for (i in study_areas)
{
  plot <- ggplot() +
    geom_line(data = QN_QE_tides_final %>% filter(area == i, type == "QN", month(time) == 8), 
              aes(x = time, y = h), color = "blue4", linewidth = 0.1) +
    geom_point(data = QN_QE_tides_final %>% filter(area == i, type == "QN", fase == "HW", month(time) == 8), 
               aes(x = time, y = h), color = "red4", size = 0.5) +  
    geom_point(data = QN_QE_tides_final %>% filter(area == i, type == "QN", fase == "LW", month(time) == 8),
               aes(x = time, y = h), color = "green4", size = 0.5) +
    labs(x = "Time", y = "Waterstand (m TAW)", title = paste("QN_", i, sep = "")) +
    theme_bw()
  
  ggsave(
    plot,
    filename = paste(i, "_QN_HW_LW.png", sep = ""), 
    path = pad_output_graphs,
    width = 15, height = 10, units = "cm", dpi = 300)
}
plot

for (i in study_areas)
{
  plot <- ggplot() +
    geom_line(data = QN_QE_tides_final %>% filter(area == i, type == "QE"), 
              aes(x = time, y = h), color = "blue4", linewidth = 0.1) +
    geom_point(data = QN_QE_tides_final %>% filter(area == i, type == "QE", fase == "HW"), 
               aes(x = time, y = h), color = "red4", size = 0.5) +  
    geom_point(data = QN_QE_tides_final %>% filter(area == i, type == "QE", fase == "LW"),
               aes(x = time, y = h), color = "green4", size = 0.5) +
    labs(x = "Time", y = "Waterstand (m TAW)", title = paste("QE_", i, sep = "")) +
    theme_bw()
  
  ggsave(
    plot,
    filename = paste(i, "_QE_HW_LW.png", sep = ""), 
    path = pad_output_graphs,
    width = 15, height = 10, units = "cm", dpi = 300)
}
plot

