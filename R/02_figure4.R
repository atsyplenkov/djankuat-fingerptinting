################################################################################
#
# Supplementary material for:
# Tsyplenkov, A., Vanmaercke, M., Collins, A.L., Kharchenko, S., Golosov, V.,
# 2021. Elucidating suspended sediment dynamics in a glacierized catchment
# after an exceptional erosion event: The Djankuat catchment, Caucasus Mountains,
# Russia. CATENA 203, 105285. https://doi.org/10.1016/j.catena.2021.105285
#
# Contact: Anatoly Tsyplenkov (atsyplenkov@gmail.com)
# 
# Figure 4. Annual exceedance probabilities of maximum daily (a) and weekly (b)
# rainfalls based on data from the Cheget, Mestia and Terskol meteorological
# stations   
#
################################################################################
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(fasstr)
library(readxl)
library(extrafont)
library(hrbrthemes)
library(scales)
library(ggpubr)
library(here)

# 1) Read meteo data
### CHEGET ###
# Data from colleagues
all_meteo <- readxl::read_xlsx(here("data","raw","meteo","toropov_CHEGET-TERSKOL.xlsx"),
                               skip = 2,
                               range = "A3:F4021") %>% 
  set_colnames(c("date", "t_terskol", "p_terskol", "h_terskol",
                 "t_cheget", "p_cheget")) %>% 
  mutate(date = as_date(date))

# Data parsed from http://www.pogodaiklimat.ru/
cheget_pik <- read_xlsx(here("data","raw","meteo","pik_CHEGET.xlsx")) %>%
  mutate(date = with_tz(date, "Europe/Moscow")) %>% 
  arrange(date) %>% 
  dplyr::select(date, temp, prec) %>%
  mutate(date = as_date(date)) %>% 
  group_by(date) %>% 
  summarise(temp = mean(temp, na.rm = T),
            p_day = sum(prec, na.rm = T),
            .groups = "drop")

# Merge together
cheget <- all_meteo %>% 
  dplyr::select(date,
                temp = t_cheget,
                p_day = p_cheget) %>% 
  bind_rows(cheget_pik)

# Prepare dataset
cheget_tidy <- cheget %>% 
  dplyr::filter(temp > 2) %>%
  complete(date = seq(min(date),
                      max(date),
                      by = "1 day")) %>% 
  mutate(p_day = replace(p_day,
                         is.na(p_day),
                         0)) %>% 
  mutate(p_week = rollsum(p_day, 7,
                          fill = 0,
                          align = "left")) %>% 
  group_by(Year = year(date)) %>% 
  summarise(week_max = max(p_week,
                           na.rm = T),
            day_max = max(p_day,
                          na.rm = T),
            .groups = "drop") %>% 
  gather(Measure, Value, -Year) %>% 
  mutate(Measure = paste0("Cheget ", Measure))

### TERSKOL ###
# Data from colleagues
terskol_toropov <- readxl::read_xlsx(here("data","raw","meteo","toropov_TERSKOL.xlsx"),
                                     range = "D1:H15341") %>% 
  dplyr::select(date = 5,
         p_terskol = 2,
         t_terskol = 1) %>% 
  na_if(-999.9)

# Prepare dataset
terskol_tidy <- terskol_toropov %>% 
  dplyr::filter(t_terskol > 2) %>% # Keep only liquid precipitation. see thresholds estimated by Jennings et al. (2018) 
  dplyr::filter(date != as.Date("1987-05-17")) %>% # extremum
  dplyr::filter(date != as.Date("2004-03-05")) %>% # it was snow
  dplyr::select(Date = date, p_terskol) %>% 
  complete(Date = seq(min(Date), max(Date), by = "1 day")) %>% 
  mutate(p_terskol = replace(p_terskol, is.na(p_terskol), 0)) %>% 
  mutate(p_week = zoo::rollsum(p_terskol,
                               k = 7,
                               fill = 0,
                               align = "center")) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarise(week_max = max(p_week, na.rm = T),
            day_max = max(p_terskol, na.rm = T),
            .groups = "drop") %>% 
  gather(Measure, Value, -Year) %>% 
  mutate(Measure = paste0("Terskol ", Measure)) 

### MESTIA ###
# Load data from NOAA
# https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
mestia_noaa <- read_csv(here("data","raw","meteo","noaa_MESTIA.csv")) %>% 
  select(date = DATE, p_mestia = PRCP)

# Prepare dataset
mestia_tidy <- mestia_noaa %>% 
  # Keep only summer months
  filter(month(date) %in% c(6:8)) %>% 
  dplyr::select(Date = date, p_mestia) %>% 
  arrange(Date) %>% 
  complete(Date = seq(min(Date), max(Date), by = "1 day")) %>% 
  mutate(p_mestia = replace(p_mestia, is.na(p_mestia), 0)) %>% 
  mutate(p_week = zoo::rollsum(p_mestia,
                               k = 7,
                               align = "center",
                               fill = 0)) %>% 
  mutate(Year = year(Date)) %>% 
  dplyr::filter(Year != 1968) %>% 
  group_by(Year) %>% 
  summarise(week_max = max(p_week, na.rm = T),
            day_max = max(p_mestia, na.rm = T),
            .groups = "drop") %>% 
  gather(Measure, Value, -Year) %>% 
  mutate(Measure = paste0("Mestia ", Measure))

# 2) Preprocess
cau_day <- bind_rows(terskol_tidy,
                     cheget_tidy,
                     mestia_tidy) %>% 
  filter(str_detect(Measure, "day")) %>% 
  mutate(Measure = str_remove(Measure, " day_max")) %>% 
  group_by(Measure) %>% 
  arrange(-Value, .by_group = T) %>%   
  mutate(Pm = row_number()/(n() + 1)) %>% 
  ungroup()

cau_week <- bind_rows(terskol_tidy,
                      cheget_tidy,
                      mestia_tidy) %>% 
  filter(str_detect(Measure, "week")) %>% 
  mutate(Measure = str_remove(Measure, " week_max")) %>% 
  group_by(Measure) %>% 
  arrange(-Value, .by_group = T) %>%   
  mutate(Pm = row_number()/(n() + 1)) %>% 
  ungroup()

# 3) Calculate frequency PIII ---------------------------------------------
results_week <- compute_frequency_analysis(
  data = cau_week, 
  use_max = T,
  fit_quantiles = seq(0.001, 0.999, by = 0.01),
  values = Value
)

results_day <- compute_frequency_analysis(
  data = cau_day,
  use_max = T,
  fit_quantiles = seq(0.001, 0.995, by = 0.01),
  values = Value
)

# 4) Plots ----------------------------------------------------------------
probs <- c(0.0001, 0.001, 0.01,
           0.1, 0.3, 0.5, 0.7,
           0.9, 0.99, 0.999)

aep_week <- results_week$Freq_Fitted_Quantiles %>% 
  dplyr::select(-1) %>% 
  gather(Measure, p, -Probability, -`Return Period`) %>% 
  ggplot() +
  geom_line(aes(x = qnorm(Probability),
                y = p,
                color = Measure)) +
  geom_point(data = cau_week,
             alpha = .6,
             aes(x = qnorm(Pm), y = Value, color = Measure)) +
  scale_y_continuous(name = "Max. weekly rainfall, mm",
                     breaks = seq(0, 200, 25),
                     labels = scales::comma,
                     expand = c(.05, .05)) +
  scale_x_continuous(name = "Annual exceedance probability, %",
                     breaks = qnorm(probs),
                     labels = str_c(probs * 100,'%'),
                     expand = c(.001, .001),
                     sec.axis = dup_axis(name = 'Return period',
                                         labels = round(1/probs, 2))) +
  hrbrthemes::theme_ipsum_rc() +
  hrbrthemes::scale_color_ft(name = "Meteostation") +
  labs(subtitle = "(b)")

aep_day <- results_day$Freq_Fitted_Quantiles %>% 
  select(-1) %>% 
  gather(Measure, p, -Probability, -`Return Period`) %>% 
  ggplot() +
  geom_line(aes(x = qnorm(Probability),
                y = p,
                color = Measure)) +
  geom_point(data = cau_day,
             alpha = .6,
             aes(x = qnorm(Pm), y = Value, color = Measure)) +
  scale_y_continuous(name = "Max. daily rainfall, mm",
                     labels = scales::comma,
                     breaks = seq(0, 150, 25),
                     expand = c(.05, .05)) +
  scale_x_continuous(name = "Annual exceedance probability, %",
                     breaks = qnorm(probs),
                     labels = str_c(probs * 100,'%'),
                     expand = c(.001, .001),
                     sec.axis = dup_axis(name = 'Return period',
                                         labels = round(1/probs, 2))) +
  hrbrthemes::theme_ipsum_rc() +
  hrbrthemes::scale_color_ft(name = "Meteostation") +
  labs(subtitle = "(a)")

# Combine plots
aeps <- ggpubr::ggarrange(aep_day, aep_week,
                          ncol = 2,
                          common.legend = T,
                          legend = "bottom")

# Save
ggsave(here("figures", "fig4_aep.png"),
       aeps,
       dpi = 500,
       w = 10,
       h = 5)