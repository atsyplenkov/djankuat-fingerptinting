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
# Table 3. Suspended sediment loads (SSLOUT) and concentrations(SSC) at
# the outlet gauging station (OUT) of the Djankuat study catchment
# (cf. Fig. 1) for the period 2015–2019.  
#
################################################################################
library(tidyverse) # CRAN v1.3.0
library(magrittr)  # CRAN v2.0.1
library(lubridate) # CRAN v1.7.10
library(pangaear)  # CRAN v1.0.0
library(readxl)    # CRAN v1.3.1
library(extrafont) # CRAN v0.17
library(loadflux)  # Github v0.0.1 (https://github.com/atsyplenkov/loadflux)
library(scales)    # CRAN v1.1.1
library(repmis)    # CRAN v0.5
library(writexl)   # CRAN v1.3.1
library(here)      # CRAN v1.0.1

repmis::source_data("https://github.com/atsyplenkov/intra-event-djankuat/blob/master/data/tidy/djan17.Rdata?raw=true")
repmis::source_data("https://github.com/atsyplenkov/intra-event-djankuat/blob/master/data/tidy/ssc-ntu_formula.Rdata?raw=true")

# 1) Read Djankuat river database -----------------------------------------
# Database: https://doi.pangaea.de/10.1594/PANGAEA.894807
# Paper: https://www.earth-syst-sci-data.net/11/1463/2019/

# Download Hydrological data
# https://doi.pangaea.de/10.1594/PANGAEA.895099
djan_essd <- pg_data(doi = '10.1594/PANGAEA.895099')[[1]]$data %>% 
  dplyr::select(datetime = 1, q = 2, ntu = `Turbidity [NTU]`,
                ssc = `Turbidity [arbitrary units]`) %>%
  # Convert date/time to a POSIXct
  mutate(datetime = as.POSIXct(strptime(datetime, "%Y-%m-%dT%H:%M"),
                               tz = "Europe/Moscow"),
         datetime = lubridate::force_tz(datetime, "Europe/Moscow")) %>% 
  arrange(datetime)

# 2019
djan19_q <- read_xlsx(here("data", "raw", "hydro", "djan-2019.xlsx"),
                      sheet = 1) %>% 
  mutate(date = force_tz(date, "Europe/Moscow")) %>% 
  arrange(date) %>% 
  complete(date = seq(min(date), max(date), by = "1 hour"))

djan19_ntu <- read_xlsx(here("data", "raw", "hydro", "djan-2019.xlsx"),
                        sheet = 2) %>% 
  mutate(date = force_tz(date, "Europe/Moscow")) %>% 
  arrange(date) %>% 
  drop_na(ntu)

djan19 <- full_join(djan19_q, djan19_ntu, by = "date") %>% 
  arrange(date) %>% 
  mutate(q = zoo::na.approx(q, rule = 2))

# 2) Edit the 2017 data ---------------------------------------------------
# Make it similiar to Tsyplenkov et al., 2020
# Check the turbidity calculations
djan_hydro <- djan_essd %>% 
  bind_rows(djan19 %>% rename(datetime = date)) %>% 
  mutate(ssc = ifelse(ntu > 1000,
                      predict(lm2, .),
                      predict(lm1, .)),
         year = year(datetime))

djan_hydro[djan_hydro$year == 2017,] <- NA

djan_hydro %<>% 
  filter(!is.na(datetime)) %>% 
  dplyr::select(-ntu, -year) %>% 
  bind_rows(df17 %>% dplyr::select(datetime:ssc)) %>% 
  arrange(datetime)

# 3) Split to hydrological events ---------------------------------------
djan_hydro %<>% 
  group_by(year = year(datetime)) %>% 
  mutate(q = zoo::na.approx(q, rule = 2),
         q_real = q) %>%
  nest() %>% 
  mutate(data = ifelse(year != 2019,
                       map(data, ~hydro_events(.x, q, datetime, 9)),
                       map(data, ~hydro_events(.x, q, datetime, 15)))) %>%
  unnest(data) %>% 
  ungroup() %>% 
  mutate(he = glue::glue("{year}_{as.character(he)}"),
         he = as_factor(he),
         q = q_real) %>% 
  dplyr::select(-q_real)

# 4) Read data from raincollector -----------------------------------------
rain <- read_xlsx(here("data", "raw", "meteo", "raincollector.xlsx")) %>% 
  mutate(start = as.POSIXct(start, tz = "Europe/Moscow"), # rain begin
         end = as.POSIXct(end, tz = "Europe/Moscow"), # rain end
         p = as.double(p), # rainfall amount [mm]
         duration = difftime(end, start, tz = "Europe/Moscow"),
         duration = as.double(duration) / 60, # length of an event [h]
         intensity = signif(p / duration, 3)) %>%   # intensity [mm/h]
  mutate_at(vars(start, end), ~force_tz(., "Europe/Moscow")) %>% 
  mutate_at(vars(start, end), ~lubridate::round_date(., "hours"))

# 5) Merge Rain dataframe with hydrological event dataframe -------------
rain %>% 
  rename(datetime = start) %>%
  select(-end) %>%
  # mutate(datetime = lubridate::round_date(datetime, "hours")) %>%
  full_join(djan_hydro, ., by = "datetime") %>% 
  arrange(datetime) %>% 
  mutate(he = zoo::na.locf(he)) -> djan_hydro

                   # Create a column where an effect of rain on events will be described:
# SOURCE: https://stackoverflow.com/a/51884231/9300556
# TRUE - if there was rainfall during measurement
# FALSE - if there was no rain at all
# Overlap timeseries and time interval
djan_hydro <- sqldf::sqldf('select djan_hydro.*, case when rain.p is not null then "TRUE" else "FALSE" end as factor_rain 
      from djan_hydro
      left join rain
      on start <= datetime and
      datetime <= end') %>% 
  as_tibble()

# 6) Number rain events ---------------------------------------------------
djan_rain <- djan_hydro %>% 
  mutate(fr = data.table::rleid(factor_rain),
         fr = ifelse(factor_rain == F, NA, fr)) %>% 
  group_by(fr) %>% 
  nest() %>% 
  rowid_to_column(var = "re") %>% 
  mutate(re = re - 1) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(re = na_if(re, 0)) %>% 
  dplyr::select(-fr) %>% 
  arrange(datetime) 

# 7) Calculate the timelength of dry time -------------------------------
djan_rain %<>% 
  group_by(re) %>% 
  mutate(RRR2 = ifelse(!is.na(re), zoo::na.locf(p, na.rm = TRUE), NA)) %>% 
  ungroup() %>% 
  # mutate(RRR2 = p) %>%
  # fill(RRR2) %>%  
  group_by(temp = data.table::rleid(RRR2)) %>% 
  mutate(temp2 = seq_along(temp)) %>% 
  group_by(year, RRR2, temp) %>% 
  mutate(diff = ifelse(!is.na(he),
                       difftime(datetime, datetime[temp2 == 1], units="hours"),
                       NA)) %>% 
  ungroup() %>% 
  mutate(diff = case_when(!is.na(re) ~ 0,
                          year %in% c(2007:2015) ~ -9999,
                          TRUE ~ diff),
         diff = na_if(diff, -9999)) %>% 
  dplyr::select(-factor_rain:-temp2) 

# 8) Calculate cumsum of the rainfall
djan_rain %<>% 
  group_by(year) %>% 
  mutate(p24 = zoo::rollapplyr(p,
                               width = 24,
                               FUN = sum,
                               na.rm = TRUE,
                               partial = TRUE),
         p48 = zoo::rollapplyr(p,
                               width = 48,
                               FUN = sum,
                               na.rm = TRUE,
                               partial = TRUE),
         p72 = zoo::rollapplyr(p,
                               width = 72,
                               FUN = sum,
                               na.rm = TRUE,
                               partial = TRUE)) %>% 
  ungroup()

# 9) Rainfall events database
djan_rain_db <- djan_rain %>% 
  mutate(r = q*ssc) %>% 
  # Calculate EEI
  mutate(EEI = ssc/q) %>% 
  filter(!is.na(re)) %>% 
  group_by(re) %>% 
  summarise(peak.time = datetime[which.max(q)[1]],
            rain.start = datetime[1],
            delay = difftime(peak.time, rain.start),
            p = sum(p, na.rm = T),
            intensity = mean(intensity, na.rm = T),
            EEI_mean = mean(EEI, na.rm = T),
            EEI_max = max(EEI, na.rm = T),
            EEI_med = median(EEI, na.rm = T),
            EEI_sd = sd(EEI, na.rm = T),
            EEI_cv = EEI_sd/EEI_mean,
            dq = max(q, na.rm = T) - min(q, na.rm = T),
            dssc = max(ssc, na.rm = T) - min(ssc, na.rm = T),
            dr = max(r, na.rm = T) - min(r, na.rm = T),
            .groups = "drop") %>% 
  na_if(-Inf)

# 10) Create hydrological event database
djan_hydro_db <- djan_rain %>% 
  mutate(r = ssc * q) %>% 
  group_by(he) %>% 
  summarise(start = first(datetime),
            end = last(datetime),
            mean_date = mean.POSIXct(c(start, end)),
            length = as.double(signif(difftime(end, start, units = "hours"), 3)), # hours
            q.mean = mean(q, na.rm = T), # m^3/s  
            q.max = max(q, na.rm = T), # m^3/s  
            ssc.mean = mean(ssc, na.rm = T), # g/m^3
            # r = q.mean * ssc.mean /10^3,
            r = mean(r, na.rm = T),
            p = sum(p, na.rm = T),
            p24.start = first(p24), 
            p48.start = first(p48),
            p72.start = first(p72),
            dq = max(q, na.rm = T) - first(q),
            dssc = max(ssc, na.rm = T) - min(ssc, na.rm = T),
            dry = diff[which.max(q)],
            dry2 = first(diff), .groups = "drop") %>%  # kg/s
  na_if(-Inf) %>%
  arrange(start)

# Water discharge
djan_hydro %>% 
  group_by(he) %>% 
  summarise(q = mean(q, na.rm = T)) %>% 
  summarise_at(vars(q), list(~mean(., na.rm = T),
                             ~median(., na.rm = T),
                             ~sd(., na.rm = T)))

q_table <- djan_hydro %>%
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarise(mean = mean(q, na.rm = T),
            med = median(q, na.rm = T),
            sd = sd(q, na.rm = T),
            max = max(q, na.rm = T),
            max.date = datetime[which.max(q)],
            min = min(q, na.rm = T),
            min.date = datetime[which.min(q)]) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>% 
  mutate_if(is.POSIXct, ~as.character(.)) %>% 
  mutate_at(vars(mean:max, min), funs( ifelse (year %in% c(2007, 2009:2010),
                                               paste0("(", ., ")"), .)))

# Suspended Sediment Concentration
djan_hydro %>% 
  group_by(he) %>% 
  summarise(ssc = mean(ssc, na.rm = T)) %>% 
  summarise_at(vars(ssc), list(~mean(., na.rm = T),
                               ~median(., na.rm = T),
                               ~sd(., na.rm = T))) 

ssc_table <- djan_hydro %>% 
  group_by(year) %>% 
  filter(year >= 2015) %>% 
  summarise(mean = mean(ssc, na.rm = T),
            med = median(ssc, na.rm = T),
            sd = sd(ssc, na.rm = T),
            max = max(ssc, na.rm = T),
            max.date = datetime[which.max(ssc)],
            min = min(ssc, na.rm = T),
            min.date = datetime[which.min(ssc)]) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>% 
  mutate_if(is.POSIXct, ~as.character(.))

# Suspended sediment load
djan_hydro_db %>% 
  # filter(year(mean_date) == 2019) %>% View
  mutate(ssl = round((r * 3600 * length / 10^6), 2)) %>%
  summarise_at(vars(ssl), list(~mean(., na.rm = T),
                               ~median(., na.rm = T),
                               ~sd(., na.rm = T))) 

ssl_table <-  djan_hydro_db %>% 
  mutate(ssl = round((r * 3600 * length / 10^6), 2)) %>% # t/event
  dplyr::mutate_if(is.numeric, list(~signif(., 2))) %>%
  group_by(year = year(start)) %>% 
  filter(year >= 2015) %>% 
  summarise(n = sum(!is.na(ssl)),
            `Observation period` = sum(ssl, na.rm = T),
            Total = 1.02 * `Observation period`,
            Mean = mean(ssl, na.rm = T),
            SD = sd(ssl, na.rm = T),
            Median = median(ssl, na.rm = T),
            Max = max(ssl, na.rm = T),
            Min = min(ssl, na.rm = T))  

# 8) Table 3 --------------------------------------------------------------
# Bind SSL and SSC
table3 <- ssl_table %>% 
  transmute(
    Year = as.integer(year),
    `SSL during the observation period [t]` = `Observation period`,
    `Estimated annual SSL [t]` = Total,
    `SSLMean [t event]` = Mean,
    `St. Dev. [t event]` = SD,
    `SSLMed [t event]` = Median,
    `SSLMax [t event]` = Max,
    `SSLMin [t event]` = Min
  ) %>% 
  bind_cols(
    ssc_table %>% 
      transmute(
        `SSCmed [g m3]` = med,
        `SSCmax [g m3]` = max,
        `Time of SSCmax` = max.date
      )
  )

# Save to excel
table3 %>% 
  mutate_if(is.numeric,
            ~atslib::smart_round(.)) %>% 
  write_xlsx(path = here("tables", "table3.xlsx"))
