# SCRIPT TER VOORBEREIDING VAN HABITATMAPPING IKV SPARC 
# Genereren van 4QN1QE tijdsreeks voor tijposten waarvoor WL geen 'statistics' tabellen aanleverde (GGG posten)
# Geschreven door Nicolas Vanermen

# laatste export & check op 19/06/2025

pad_output <- "./Exports 2025 06 19/"

if(!dir.exists(pad_output))
  dir.create(pad_output, recursive = TRUE)

#### libraries laden ####
library(readxl)
library(tidyverse)
library(Tides)
library(ggplot2)

#### inlezen WL data ####
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
  select("time","type",
         "Grote_Wal_Zwijn","De_Bunt","Vlassenbroek")

#### prepare files for Tides package #### 
QN_QE_PL <- QN_QE %>% 
  pivot_longer(3:5) %>% 
  rename(area = name,
         h = value) %>% 
  mutate(time = dmy_hms(time))

#### extract HW & LW applying Tides ####  
# applying default settings but with hoffset = 0.1 to avoid 'edge effects' 
QN_QE_tides <-
  QN_QE_PL %>% 
  group_by(area, type) %>% 
  nest() %>% 
  transmute(area, HL = map(data, ~ extrema(., h0 = 0, T2 = 5*60*60, hoffset = 0.1, filtconst = 1)$HL)) %>% 
  unnest(cols = everything()) %>% 
  ungroup() %>% 
  rename(fase = HL) %>% 
  mutate(fase = recode(fase, L = "LW", H = "HW")) %>% 
  select(!h0)

#### join all tabels into a QN4QE1 file & export #### 
QN_QE_tides_final <- left_join(QN_QE_PL, QN_QE_tides) %>% arrange(area, time)

QN_QE_tides_final %>% glimpse()
QN_QE_tides_final %>% count(fase)
QN_QE_tides_final %>% count(area)
QN_QE_tides_final %>% count(type)

QN4QE1_tides_final <- rbind(QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QN"),
                            QN_QE_tides_final %>% filter(type == "QE"))

# write.csv(QN4QE1_tides_final, paste0(pad_output,"QN4QE1_SPARC.csv"))


#### plot & save QN & QE series per area #### 
study_areas <- colnames(QN_QE %>% select(3:5))

# for (j in c("QE","QN"))
#   {
# for (i in study_areas)
#   {
#   plot <- ggplot() +
#     geom_line(data = QN_QE_PL %>% filter(area == i, type == j), 
#               aes(x = time, y = h), color = "red4", linewidth = 0.1) +
#     labs(x = "Time", y = "Waterstand (m TAW)", title = paste(j, "_", i, sep = "")) +
#     theme_bw()
#   
#   ggsave(
#     plot,
#     filename = paste(i, "_", j, ".png", sep = ""), 
#     path = pad_output,
#     width = 15, height = 10, units = "cm", dpi = 300)
#   }}
# plot
# 
# ####  plot & save QN_HW_LW & QE_HW_LW series per area #### 
# for (i in study_areas)
# {
#   plot <- ggplot() +
#     geom_line(data = QN_QE_PL %>% filter(area == i, type == "QN", month(time) == 8), 
#               aes(x = time, y = h), color = "blue4", linewidth = 0.1) +
#     geom_point(data = QN_QE_tides %>% filter(area == i, type == "QN", fase == "HW", month(time) == 8), 
#                aes(x = time, y = h), color = "red4", size = 0.5) +  
#     geom_point(data = QN_QE_tides %>% filter(area == i, type == "QN", fase == "LW", month(time) == 8),
#                aes(x = time, y = h), color = "green4", size = 0.5) +
#     labs(x = "Time", y = "Waterstand (m TAW)", title = paste("QN_", i, sep = "")) +
#     theme_bw()
#   
#   ggsave(
#     plot,
#     filename = paste(i, "_QN_HW_LW.png", sep = ""), 
#     path = pad_output,
#     width = 15, height = 10, units = "cm", dpi = 300)
# }
# plot
# 
# for (i in study_areas)
# {
#   plot <- ggplot() +
#     geom_line(data = QN_QE_PL %>% filter(area == i, type == "QE"), 
#               aes(x = time, y = h), color = "blue4", linewidth = 0.1) +
#     geom_point(data = QN_QE_tides %>% filter(area == i, type == "QE", fase == "HW"), 
#                aes(x = time, y = h), color = "red4", size = 0.5) +  
#     geom_point(data = QN_QE_tides %>% filter(area == i, type == "QE", fase == "LW"),
#                aes(x = time, y = h), color = "green4", size = 0.5) +
#     labs(x = "Time", y = "Waterstand (m TAW)", title = paste("QE_", i, sep = "")) +
#     theme_bw()
#   
#   ggsave(
#     plot,
#     filename = paste(i, "_QE_HW_LW.png", sep = ""), 
#     path = pad_output,
#     width = 15, height = 10, units = "cm", dpi = 300)
# }
# plot


Sys.setlocale("LC_ALL", "English")
for (j in c("QE","QN"))
{
  for (i in study_areas)
  {
    plot <- ggplot() +
      geom_line(data = QN_QE_PL %>% filter(area == i, type == j),
                aes(x = time, y = h), color = "red4", linewidth = 0.1) +
      labs(x = NULL, y = "Water level (m above TAW)", title = paste(j, " ", str_replace_all(i, "_", " "), sep = "")) +
      theme_bw() + 
      theme(plot.title = element_text(size = 12))
    
    # ggsave(
    #   plot,
    #   filename = paste(i, "_", j, "_ENG.png", sep = ""),
    #   path = pad_output,
    #   width = 10, height = 8, units = "cm", dpi = 300)
  }}
plot



for (j in c("QE","QN"))
{
    plot <- ggplot() +
      geom_line(data = QN_QE_PL %>% filter(area == "Vlassenbroek", type == j),
                aes(x = time, y = h), color = "red4", linewidth = 0.1) +
      labs(x = NULL, y = "Water level (m above TAW)", title = paste(j, " ", str_replace_all("Vlassenbroek", "_", " "), sep = "")) +
      scale_y_continuous(limits = c(2.4, 3.4)) +
      theme_bw() + 
      theme(plot.title = element_text(size = 12))
    
    ggsave(
      plot,
      filename = paste("Vlassenbroek", "_", j, "_ENG_b.png", sep = ""),
      path = pad_output,
      width = 10, height = 8, units = "cm", dpi = 300)
}
