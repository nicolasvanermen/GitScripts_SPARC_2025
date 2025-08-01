# load required packages
library(sf)
library(ggplot2)
library(tidyverse)

# folder path
gdb_folder <- "./GIS/SPARC_maps_FINAL"

# all file geodatabases in the folder
gdb_paths <- list.files(gdb_folder, pattern = "\\.gdb", full.names = TRUE)

# create base for loop
fysiotopes_agg <- as.data.frame(
  cbind(NULL, Fysiotoop = c("supralitoraal", "hoog slik", "middelhoog slik", "laag slik", 
                            "ondiep subtidaal", "getijdeplas")))

# loop through each geodatabase
for (gdb_path in gdb_paths) 
  {
  fysiotopes <- st_read(dsn = gdb_path, layer = "fysiotopes_p_final", quiet = TRUE)
  fysiotopes <- fysiotopes %>% 
    st_drop_geometry() %>% 
    filter(Fysiotoop != "buiten estuarien gebied") %>% 
    group_by(Fysiotoop) %>%
    summarise(total_area = sum(Shape_Area)) %>% 
    mutate(total_area = case_when(str_detect(gdb_path, "2050") & Fysiotoop == "getijdeplas" ~ NA, .default = total_area))
  
  colname <- str_remove(gdb_path, ".gdb")
  colname <- str_remove(colname, "./GIS/SPARC_maps_FINAL/SPARC_habitat_maps_")
  
  colnames(fysiotopes)[2] <- paste(colname)
  
  fysiotopes_agg <- left_join(fysiotopes_agg, fysiotopes)
}

# check the result
fysiotopes_agg
# write.csv(fysiotopes_agg, "./Exports/Fysiotopes_per_area.csv")

# wrangle the result
fysiotopes_PL <- fysiotopes_agg %>% 
  pivot_longer(2:14) %>% 
  mutate(
    temp = str_replace_all(name, "(?<=[a-z])(?=[A-Z])", " "),
    Year = str_extract(temp, "\\d{4}$"),
    Year = case_when(Year == "2022" ~ "2025", .default = Year),
    Area = str_remove(temp, "\\d{4}$"),
    Ecotope_ha = value / 10000,
    Fysiotoop = factor(Fysiotoop, levels = c(
      "buiten estuarien gebied",
      "getijdeplas",
      "ondiep subtidaal",
      "laag slik",
      "middelhoog slik",
      "hoog slik",
      "supralitoraal")),
    Fysiotope = recode(
      Fysiotoop, 
      "buiten estuarien gebied" = "Non-estuarine habitat",
      "getijdeplas" = "Tidal pool",
      "ondiep subtidaal" = "Shallow subtidal",
      "laag slik" = "Low tidal mudflat",
      "middelhoog slik" = "Medium high tidal mudflat",
      "hoog slik" = "High tidal mudflat",
      "supralitoraal" = "Tidal marsh")) %>%
  select(!c(name, temp, value)) %>% 
  relocate(Area, Year, .before = NULL)

levels(fysiotopes_PL$Fysiotope)

# kleurtjes nabootsen
SL <- rgb(205, 205, 102, maxColorValue = 255)
HS <- rgb(205, 137, 102, maxColorValue = 255)
MS <- rgb(255, 211, 127, maxColorValue = 255)
LS <- rgb(255, 255, 115, maxColorValue = 255)
OS <- rgb(190, 232, 255, maxColorValue = 255)
BE <- rgb(225, 225, 225, maxColorValue = 255)
GP <- rgb(0, 132, 168, maxColorValue = 255)

# plotjes in een loop steken
areas <- tibble(area = unique(fysiotopes_PL$Area))
areas_OP <- areas %>% filter(!area %in% c("De Bunt","Vlassenbroek","Wal Zwijn"))
areas_GGG <- areas %>% filter(area %in% c("De Bunt","Vlassenbroek","Wal Zwijn"))

for (i in areas_OP$area)
{
plot <- ggplot(fysiotopes_PL %>% filter(Area == i), aes(x = Year, y = Ecotope_ha, fill = Fysiotope)) +
  geom_bar(stat = "identity") + 
  labs(x = "", y = "Surface area (ha)", fill = i) +
  scale_fill_manual(values = c("Non-estuarine habitat" = BE, 
                               "Tidal marsh" = SL,
                               "High tidal mudflat" = HS,
                               "Medium high tidal mudflat" = MS,
                               "Low tidal mudflat" = LS,
                               "Shallow subtidal" = OS,
                               "Tidal pool" = GP)) + 
  theme_bw()
print(plot)
i_alt <- str_remove_all(i, " ")
ggsave(paste0("./Exports/SPARC_ecotope_areas_", i_alt, "_ENG.jpeg"),
       dpi = 300,
       width = 7, height = 5)
}

