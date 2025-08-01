# Script voor simulatie van sedimentatie op "quasi-2D" niveau voor de GGG SPARC-gebieden
# (Nicolas Vanermen - laatste edits op 07/07/2025 14:30)

# Dit script voorspelt in eerste instantie de (1D) sedimentatie voor de lager en hoger gelegen helft van het DTM (op basis van de gemiddelde hoogteligging). De bekomen sedimentatiehoeveelheden worden vervolgens bij de respectievelijke helften van de 'cumulatieve frequentieplot' van het originele DTM geteld, en de bekomen 'gesplitte curve' wordt vervolgens gemodelleerd adhv een powerfunctie.

# Deze methode geeft een beduidend beter resulaat vergeleken met een uniforme ophoging van het gebied (waarbij de hogere regionen al te sterk dreigen op te hogen) en een voorspelde ophoging per percentiel-interval (waarbij er dan weer een te grote afvlakking van het profiel wordt bekomen).

# Het fitten van de power-funtie tot slot komt tegemoet aan de vaststelling dat de allerlaagst gelegen delen van het DTM over het algemeen niet (sterk) ophogen, allicht omdat deze samenvallen met het uitstroompunt van de hoofdkreek.

#### import libraries ####
library(tidyverse)
library(ggplot2)

#### import data ####
HWLW <- read.csv("./Exports/QN4QE1_SPARC_all_areas.csv")
HWLW <- HWLW %>% filter(!area %in% c("De_Bunt", "Vlassenbroek","Grote_Wal_Zwijn"))

Percentiles <- read.csv("./Exports/Percentiles_DTMs_estuarien.csv")
Percentiles <- Percentiles %>% dplyr::select(!X) %>% filter(!DTM %in% c("DeBunt_2022", "Vlassenbroek_2022","WalZwijn_2022"))
unique(Percentiles$DTM)

#### tijposten aan gebieden koppelen ####
HWLW <- HWLW %>% 
  rename(tijpost = area) %>% 
  mutate(tijpost = case_when(str_detect(tijpost, "Durme") ~ "Durme_1",
                             str_detect(tijpost, "Amands") ~ "Sint_Amands",
                             str_detect(tijpost, "Zeeschelde") ~ "Zeeschelde_4", .default = tijpost))
unique(HWLW$tijpost)

Percentiles <- Percentiles %>% 
  rename(m_TAW_0 = value) %>% 
  mutate(tijpost = case_when(str_detect(DTM, "Broek") ~ "Durme_1",
                             str_detect(DTM, "Hamme") ~ "Sint_Amands",
                             str_detect(DTM, "Bornem") ~ "Temse",
                             str_detect(DTM, "Uiterdijk") ~ "Zeeschelde_4")) %>% 
  arrange(DTM) %>% 
  separate(DTM,
           into = c("gebied", "jaar"),
           sep = "_") %>% 
  mutate(jaar = 2025) %>% 
  relocate(tijpost, .after = "gebied") 

summary(Percentiles)
unique(Percentiles$tijpost)

sum(sort(unique(Percentiles$tijpost)) == sort(unique(HWLW$tijpost))) == 4

#### theoretische schorhoogtes berekenen ####
# HW85 <- NULL
# for (i in unique(HWLW$tijpost))
# {
#   file <- HWLW %>% filter(tijpost == i, fase == "HW") %>% 
#     pull(h) %>% 
#     quantile(0.15)
#   file <- as.data.frame(file)
#   
#   names(file) <- paste(i)
#   
#   HW85 <- bind_cols(HW85, file)
# }
# HW85

HW90 <- NULL
for (i in unique(HWLW$tijpost))
{
  file <- HWLW %>% filter(tijpost == i, fase == "HW") %>% 
    pull(h) %>% 
    quantile(0.10)
  file <- as.data.frame(file)
  
  names(file) <- paste(i)
  
  HW90 <- bind_cols(HW90, file)
}
HW90

#### grenshoogtes buiten-estuarien gebied berekenen ####
HW05 <- NULL
for (i in unique(HWLW$tijpost))
{
  file <- HWLW %>% filter(tijpost == i, fase == "HW") %>% 
    pull(h) %>% 
    quantile(0.995)
  file <- as.data.frame(file)
  
  names(file) <- paste(i)
  
  HW05 <- bind_cols(HW05, file)
}
HW05


#### grafische check ####
# for (i in unique(Percentiles$gebied))
#   {
#   tijpost <-  Percentiles %>% filter(gebied == i) %>% distinct(tijpost)
#   
#   print(ggplot() + 
#           geom_line(data = Percentiles %>% filter(gebied == i, percentiel <= 99), aes(percentiel, m_TAW_0, col = as.factor(jaar))) +
#           geom_hline(yintercept = HW85 %>% dplyr::select(tijpost$tijpost) %>% pull(), colour = "green4") + 
#           geom_hline(yintercept = HW05 %>% dplyr::select(tijpost$tijpost) %>% pull(), colour = "red2") + 
#           scale_colour_brewer(palette = "Dark2") +
#           # scale_y_continuous(limits = c(0, 6.5)) +
#           labs(col = i, y = "m TAW"))
#   }

# wrangle input files for sedimenation loop
# mean_m_TAW
median_m_TAW <- Percentiles %>% 
  group_by(gebied) %>% 
  summarise(m_TAW_median = median(m_TAW_0))
median_m_TAW

mean_m_TAW_split_temp <- left_join(Percentiles, median_m_TAW) %>% 
  mutate(DTM_split = case_when(m_TAW_0 < m_TAW_median ~ "min", .default = "plus"))
table(mean_m_TAW_split_temp$DTM_split)

mean_m_TAW_split <- mean_m_TAW_split_temp %>% 
  group_by(gebied, tijpost, jaar, DTM_split) %>% 
  summarise(m_TAW_0 = mean(m_TAW_0)) %>% 
  ungroup()

mean_m_TAW_split

mean_m_TAW_split <- left_join(mean_m_TAW_split, 
							Percentiles %>% 
								filter(percentiel == 0) %>% 
								dplyr::select(!c(percentiel, jaar)) %>% 
								rename(m_TAW_P0 = m_TAW_0)) 
mean_m_TAW_split
# write.csv(mean_m_TAW_split, "./Exports/mean_m_TAW_Ontpoldering.csv")

# waterlevels
waterlevels <- HWLW %>% 
  filter(fase == "HW")
table(waterlevels$tijpost)

# model_summaries
model_summaries <- NULL

# start the loop
for (i in  unique(mean_m_TAW_split$gebied))
{
  # generate area specific data
  tijpost <- mean_m_TAW_split %>% filter(gebied == i) %>% pull(tijpost) %>% unique()
  
  mean_m_TAW_sim <- mean_m_TAW_split %>% filter(gebied == i) %>% dplyr::select(!c(jaar, m_TAW_P0))
  
  for(j in c(1:25))
  {
  # calculation of the expected (1D) sedimentation across the study period for the DTM parts below and above median height
  temp <- mean_m_TAW_sim %>% dplyr::select(1:3, ncol(mean_m_TAW_sim))
    
	mean_m_TAW_join <- right_join(waterlevels, temp, relationship = "many-to-many")
	
	colnames(mean_m_TAW_join)[ncol(mean_m_TAW_join)] <- "m_TAW_old"
  
	simulation <- mean_m_TAW_join %>% 
      mutate(WD = ifelse(h - m_TAW_old < 0, 0, h - m_TAW_old)) %>% 
      group_by(gebied, DTM_split) %>%
      summarise(mean_WD = mean(WD),
                m_TAW_old = mean(m_TAW_old)) %>% 
      mutate(m_TAW_new = ifelse((-0.0565 + 0.2306 * mean_WD) > 0, m_TAW_old + (-0.0565 + 0.2306 * mean_WD), m_TAW_old)) 
    
  mean_m_TAW_sim <- cbind(mean_m_TAW_sim, simulation$m_TAW_new)
  colnames(mean_m_TAW_sim)[j + 4] <- paste("m_TAW", j + 2025, sep = "_")
  }
  mean_m_TAW_sim
  
  mean_m_TAW_sim <- mean_m_TAW_sim %>% 
    mutate(delta = m_TAW_2050 - m_TAW_0) 
  
  # prepare files to model the (quasi 2D) sedimentation across the study period
  m_TAW_P0 <- mean_m_TAW_split %>% filter(gebied == i) %>% pull(m_TAW_P0) %>% unique()
  
  percentiles_simulation <- Percentiles %>% 
    filter(gebied == i,
           percentiel %in% c(1:99)) %>% 
    mutate(m_TAW_sim = case_when(percentiel < 50 ~ m_TAW_0 + mean_m_TAW_sim$delta[1] - m_TAW_P0,
                                 percentiel >= 50 ~ m_TAW_0 + mean_m_TAW_sim$delta[2] - m_TAW_P0))
  
  # model a power function across P4-P96 and save the coefficient estimates
  model_percentiles <- lm(log(m_TAW_sim) ~ log(percentiel), data = percentiles_simulation)
  
  model_summary_area <- as.data.frame(summary(model_percentiles)$coefficients[,1])
  colnames(model_summary_area)[ncol(model_summary_area)] <- i
  model_summaries <- bind_cols(model_summaries, model_summary_area)
  
  # model prediction
  predict_percentiles <- as.data.frame(cbind(percentiel = seq(from = 0, to = 100, by = 0.5), NULL))
  
  percentiles_simulations_area <- rbind(
    Percentiles %>% filter(gebied == i) %>% mutate(jaar = "2025 DTM"),
    bind_cols(percentiel = seq(from = 0, to = 100, by = 0.5), 
              gebied = i,
              tijpost = tijpost,
              jaar = "2050 model",
              m_TAW_0 = exp(predict(model_percentiles, predict_percentiles)) + m_TAW_P0))
  
  # chech how the model fits the splitted curve
  # plot1 <- ggplot() + 
  #   geom_line(data = percentiles_simulations_area %>% filter(gebied == i, percentiel <= 99), 
  #             aes(percentiel, m_TAW_0, col = as.factor(jaar))) +
  #   
  #   geom_line(data = percentiles_simulation %>% filter(percentiel < 50), 
  #             aes(percentiel, m_TAW_sim + m_TAW_P0, colour = "2050 sim < 50%"), linetype = "dashed", linewidth = 0.25) +
  #   
  #   geom_line(data = percentiles_simulation %>% filter(percentiel >= 50), 
  #             aes(percentiel, m_TAW_sim + m_TAW_P0, colour = "2050 sim > 50%"), linetype = "dotdash", linewidth = 0.25) +
  #   
  #   geom_hline(yintercept = HW85 %>% dplyr::select(all_of(tijpost)) %>% pull(), color = "grey4", linetype = "dotdash") + 
  #   
  #   scale_color_manual(
  #     name = i,
  #     values = c("2025 DTM" = "green3",
  #                "2050 model" = "red3",
  #                "2050 sim < 50%" = "red3",
  #                "2050 sim > 50%" = "red3")) +  
  #                
  #   labs(x = "Percentile", y = "DTM height (m above TAW)") +
  #   theme_bw()
    # theme(legend.position = c(0.80, 0.20))
  
  # print(plot1)
    # ggsave(
    # plot1,
    # filename = paste("Frequency plots simulation_", i, "_DTM_split_1c.png", sep = ""),
    # path = "./Exports/Results simulation/",
    # width = 14, height = 10, units = "cm", dpi = 300)
  
  # plot2 <- ggplot() + 
  #   
  #   geom_line(data = percentiles_simulations_area %>% 
  #               filter(gebied == i, jaar == "2025 DTM", percentiel <= 99), 
  #             aes(percentiel, m_TAW_0, col = as.factor(jaar)), linewidth = 0.5) + 
  #   
  #   geom_line(data = percentiles_simulations_area %>% 
  #               filter(gebied == i, jaar == "2050 model", percentiel <= 99), 
  #             aes(percentiel, m_TAW_0, col = as.factor(jaar)), linewidth = 0.5) +
  #   
  #   geom_hline(yintercept = HW85 %>% dplyr::select(all_of(tijpost)) %>% pull(), colour = "grey4", linetype = "dotdash") + 
  #   
  #   scale_color_manual(
  #     name = i,
  #     values = c("2025 DTM" = "green3",
  #                "2050 model" = "red3")) +
  #   
  #   labs(x = "Percentile", y = "DTM height (m above TAW)") +
  #   theme_bw() +
  #   theme(legend.position = c(0.80, 0.20))
  # 
  # print(plot2)
  #   ggsave(
  #   plot2,
  #   filename = paste("Frequency plots simulation_", i, "_DTM_split_2c.png", sep = ""),
  #   path = "./Exports/Results simulation/",
  #   width = 14, height = 10, units = "cm", dpi = 300)
  
  plot1b <- ggplot() + 
    geom_line(data = percentiles_simulations_area %>% filter(gebied == i, percentiel <= 99), 
              aes(percentiel, m_TAW_0, col = as.factor(jaar))) +
    
    geom_line(data = percentiles_simulation %>% filter(percentiel < 50), 
              aes(percentiel, m_TAW_sim + m_TAW_P0, colour = "2050 sim < 50%"), linetype = "dashed", linewidth = 0.25) +
    
    geom_line(data = percentiles_simulation %>% filter(percentiel >= 50), 
              aes(percentiel, m_TAW_sim + m_TAW_P0, colour = "2050 sim > 50%"), linetype = "dotdash", linewidth = 0.25) +
    
    geom_hline(yintercept = HW90 %>% dplyr::select(all_of(tijpost)) %>% pull(), color = "grey4", linetype = "dotdash") + 
    
    scale_color_manual(
      guide = guide_legend(nrow = 2),
      name = NULL,
      values = c("2025 DTM" = "green3",
                 "2050 model" = "red3",
                 "2050 sim < 50%" = "red4",
                 "2050 sim > 50%" = "red4")) +  
    
    labs(title = str_replace_all(i, "(?<=[a-z])(?=[A-Z])", " "), x = "Percentile", y = "DTM height (m above TAW)") +
    theme_bw() +
    theme(legend.position = c(0.70, 0.12), plot.title = element_text(size = 12))
  
  print(plot1b)
  ggsave(
  plot1b,
  filename = paste("Frequency plots simulation_", i, "_DTM_split_1e.png", sep = ""),
  path = "./Exports/Results simulation/",
  width = 14, height = 10, units = "cm", dpi = 300)
  }
model_summaries

# write.csv(model_summaries, "./Exports/Results simulation/model_summaries_Ontpoldering_DTM_split.csv")