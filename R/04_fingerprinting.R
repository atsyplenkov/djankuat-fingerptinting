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
# Fingerprinting part:
# - Table 5. Summary statistics for the tracers shortlisted in the final
#            composite signature. 
# - Table 6. Preliminary mean sediment source contributions to the riverbed
#            and suspended (Phillips tube) target sediment samples (standard
#            deviations in parentheses) and corresponding mean GOF based on
#            the FingerPro software. 
# - Figure 6. LDA plot of the different potential sediment sources in the
#             Djankuat study catchment. 
# - Figure 7. Results of the un-mixing procedure for suspended sediment samples
#             collected using the Phillips Tube (PT) (a) and riverbed target
#             sediment samples (b–d). 
#
################################################################################
library(tidyverse) # CRAN v1.3.0
library(magrittr)  # CRAN v2.0.1
library(readxl)    # CRAN v1.3.1
library(writexl)   # CRAN v1.3.1
library(sf)        # CRAN v0.9-7
library(MASS)      # CRAN v7.3-53.1
library(mapview)   # CRAN v2.9.0 
library(extrafont) # CRAN v0.17
library(fingerPro) # CRAN v1.1
library(ggsci)     # CRAN v2.9
library(atslib)    # Github v0.0.1 (https://github.com/atsyplenkov/atslib)
library(here)      # CRAN v1.0.1
library(scales)    # CRAN v1.1.1

# Load custom functions
list.files("R/functions/",
           full.names = T) %>% 
  map(source)

# 1) Read data ------------------------------------------------------------
# 2017
fing2017 <- read_xlsx("data/raw/chem/djan_chem_2017.xlsx",
                      sheet = 1) %>% 
  dplyr::select(-Type)

points2017 <- read_xlsx("data/raw/chem/djan_chem_2017.xlsx",
                        sheet = "Description", skip = 1) %>% 
  dplyr::select(sample, X, Y) %>% 
  filter(!is.na(sample))

fing2017 <- left_join(fing2017, points2017, by = "sample") %>% 
  dplyr::select(sample, X, Y, 2:37)

# 2019
fing2019 <- read_xlsx("data/raw/chem/djan_chem_2019.xlsx",
                      sheet = 1) %>% 
  rename(id = 1, sample = 2) %>% 
  dplyr::filter(!is.na(id)) %>% 
  dplyr::select(-id) %>% 
  mutate_at(vars(2:33), ~as.numeric(.))

cs137 <- read_xlsx("data/raw/chem/djan_cs137_2019.xlsx",
                   sheet = 2) %>% 
  dplyr::select(sample = 1, Cs137 = 10)

fing2019 <- left_join(fing2019, cs137, by = "sample")

points2019 <- read_xlsx("data/raw/chem/djan_cs137_2019.xlsx") %>% 
  dplyr::select(sample = Name, X = X_WGS, Y = Y_WGS)

fing2019 <- left_join(fing2019, points2019, by = "sample")

# Bind 2017 and 2019
fing <- bind_rows(fing2017, fing2019) %>%  
  mutate_at(vars(Mg, Al, Si, K, Ca), ~. * 1000)

# Read sediment source zones
zones <- st_read("data/spatial/djan_fingerprinting-zones.shp")

# Convert points table to spatial point dataframe
fing_xy <- fing %>% 
  st_as_sf(coords = c("X", "Y"),
           crs = 4326,
           remove = F) %>% 
  st_transform(st_crs(zones))

# Overlay zones and points
fing <- st_join(fing_xy, zones, join = st_intersects) %>%
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  mutate(zone = as.character(zone),
         zone = case_when(grepl("ukl", sample) ~ "mix",
                          grepl("PT", sample) ~ "mix",
                          # sample == "Sed1" ~ "Buried Ice",
                          # sample == "Sed4" ~ "Tributary",
                          sample == "Sed2" ~ "Waterfall",
                          TRUE ~ zone)) %>% 
  dplyr::filter(zone != "Waterfall")

# Explore zones and points location
# library(mapview) # CRAN v2.9.0
# mapview(zones) + mapview(fing_xy)

# 2) Prepare dataframe for the analysis -----------------------------------
# Rename samples and remove outliers
fing %<>% 
  arrange(sample) %>% 
  dplyr::filter(
    !sample %in% c("Sed19", "Sed10"),
    !sample %in% c("ukl2", "ukl3", "ukl4", "ukl6")
  ) %>% 
  mutate(sample = case_when(sample == "ukl1" ~ "1",
                            sample == "ukl5" ~ "2",
                            sample == "ukl7" ~ "3",
                            sample == "PT1901" ~ "PT",
                            TRUE ~ sample)
  )

# Amount of samples per year
fing %>% 
  mutate(year = ifelse(grepl("Sed", sample), 2017, 2019),
         year = case_when(sample == "ЦД21" ~ 2017,
                          TRUE ~ year)) %>% 
  group_by(year) %>%
  dplyr::filter(zone != "mix") %>% count

# Remove elements with zeros
df <- fing %>% 
  select_if(~ !sum(is.na(.)) > 35) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(zone != "mix") %>% 
  dplyr::select(-sample:-Y, -zone)

# Check for multicollinearity in the data
df <- is_multicol(df)

# 3) Linear discriminant analysis (LDA)  ----------------------------------
# Prepare dataset
fing_lda <- bind_cols(df, fing %>% 
                        dplyr::filter(zone != "mix") %>% 
                        dplyr::select(zone))

# Perform LDA
lda <- lda(zone ~ ., fing_lda,
           prior = rep(1, length(unique(fing_lda$zone)))/length(unique(fing_lda$zone)))

# Predict
plda <-  predict(object = lda,
                 newdata = fing_lda)

# LDA proportion explained
prop.lda <- lda$svd^2/sum(lda$svd^2)

# Plot
djankuat_lda <- cbind(fing_lda, plda$x,
                      fing %>%
                        dplyr::filter(zone != "mix") %>% 
                        dplyr::select(sample)) %>%
  as_tibble() %>% 
  ggplot(aes(x = LD1, y = LD2,
             color = zone)) + 
  geom_point(size = 2.5) +
  stat_ellipse(type = "t", 
               aes(fill = zone), 
               size = 1,
               geom = "polygon",
               alpha = 0.2,
               level = 0.9,
               show.legend = F) +
  stat_ellipse(type = "t", size = 1, 
               aes(colour = zone),
               level = 0.9,
               show.legend = F) +
  labs(x = paste("LD1 (", scales::percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", scales::percent(prop.lda[2]), ")", sep="")) +
  # ggrepel::geom_text_repel(aes(label = sample), show.legend = F) +
  ggsci::scale_color_d3(name = "Source") +
  ggsci::scale_fill_d3(name = "") +
  atslib::theme_clean()

# Save
ggsave(here::here("figures", "fig6_lda.png"),
       djankuat_lda,       
       dpi = 600,
       w = 8,
       h = 5)

# 4) Data preprocessing ---------------------------------------------------
# Prepare df as FingerPro input
fing_pro <- fing %>% 
  mutate(zone = case_when(zone == "Buried Ice" ~ "Left Bank",
                          TRUE ~ zone)) %>%
  select_if(~ !sum(is.na(.)) > 40) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::select(-X:-Y) %>% 
  rowid_to_column(var = "id") %>% 
  dplyr::select(id, sample, Source = zone, Cs137:Bi) %>% 
  as.data.frame()

# Sample IDs
fing_id <- fing %>% 
  mutate(zone = case_when(zone == "Buried Ice" ~ "Left Bank",
                          TRUE ~ zone)) %>%
  select_if(~ !sum(is.na(.)) > 40) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::select(-X:-Y) %>% 
  rowid_to_column(var = "id") %>% 
  dplyr::select(id, sample)

                   # Create list of df
fing_map <- fing_pro %>% 
  dplyr::filter(Source == "mix") %>% 
  pull(sample) %>% 
  unique() %>% 
  purrr::set_names(paste) %>%
  purrr::map(~filter(fing_pro, sample == .)) %>% 
  map(~bind_rows(fing_pro %>% filter(Source != "mix"),.)) %>% 
  map(~dplyr::select(., -sample))

# 5) Statistical tests ----------------------------------------------------
# Range, KW and DFA Test
fing_both <- fing_map %>% 
  map(~rangeTest(.)) %>% 
  map(~fingerPro::KWTest(., 0.08)) %>% 
  map(~fingerPro::DFATest(.)) 

# Vector of seleceted tracer elements
fing_both %>% 
  map(~names(.)) %>% 
  unique() -> significant_elements

significant_elements[which.max(sapply(significant_elements, length))] %>% 
  unlist() -> significant_elements

# Select tracers
fing_select <- fing_map %>%
  map(~dplyr::select(., id, Source, Mg, Ca, Zn, W, Hg)) 

# Check how the selected tracers discriminate between sources
# fing_select$`3` %>%  
#   LDAPlot(.) 

# 6) Table 5 --------------------------------------------------------------
# Summary statistics for the tracers shortlisted in the
# final composite signature

table5 <- fing_pro %>% 
  as_tibble() %>%
  dplyr::select(all_of(significant_elements), -id) %>% 
  mutate_at(vars(Mg, Ca, Hg), ~./1000) %>%
  rename(`Mg [g/kg]` = Mg, `Ca [g/kg]` = Ca, `Hg [g/kg]` = Hg,
         `Zn [mg/kg]` = Zn, `Rb [mg/kg]` = Rb, `W [mg/kg]` = W) %>% 
  dplyr::filter(Source != "mix") %>% 
  gather(Element, value, -Source) %>% 
  group_by(Source, Element) %>% 
  summarise(Mean = mean(value),
            `St.Dev` = sd(value),
            Median = median(value),
            Max = max(value),
            Min = min(value)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~signif(., 3)) %>% 
  mutate_if(is.numeric, ~as.character(.)) %>% 
  as.data.frame()

# Save
write_xlsx(table5,
           here("tables", "table5.xlsx"))


# 7) Unmix ----------------------------------------------------------------
fing_select %>%
  map(~unmix_s(., samples = 100, iter = 500)) -> fing_map_res

# Figure 7
# Results of the un-mixing procedure for suspended sediment samples collected
# using the Phillips Tube (PT) (a) and riverbedtarget sediment samples (b–d). 
density_result <- fing_map_res %>% 
  map(~gather(.,source, role, -id, -GOF) %>% 
        ggplot(aes(x = role,
                   y = ..scaled..)) +
        geom_density(aes(group = source,
                         color = source,
                         fill = source),
                     alpha = 0.35,
                     n = 200,
                     bw = 0.06) +
        scale_x_continuous(breaks = seq(0, 1, .25),
                           labels = glue::glue("{seq(0, 100, 25)}%")) +
        labs(x = "Source contribution",
             y = "Density") +
        scale_fill_manual(
          values = c(
            "#FF7F0EFF",  ## Glacier
            "#2CA02CFF",  ## Left Bank
            "#D62728FF",  ## Right Bank
            "#9467BDFF"),  ## Tributary
          name = NULL,
          labels = c("Glacier    ",
                     "Burried Ice    ",
                     "Right Bank    ",
                     "Tributary    ")
        ) +
        scale_color_manual(
          values = c(
            "#FF7F0EFF",  ## Glacier
            "#2CA02CFF",  ## Left Bank
            "#D62728FF",  ## Right Bank
            "#9467BDFF"),  ## Tributary
          name = NULL,
          labels = c("Glacier    ",
                     "Burried Ice    ",
                     "Right Bank    ",
                     "Tributary    ")
        ) + 
        facet_wrap(~ id, scales = "free",
                   labeller = labeller(id = setNames(fing_id$sample,
                                                     fing_id$id))) +
        theme_clean()) %>% 
  ggpubr::ggarrange(plotlist = .,
                    labels = glue::glue("({letters[1:4]})"),
                    font.label = list(face = "plain",
                                      size = 10,
                                      family = "Ubuntu Condensed"),
                    align = "hv",
                    common.legend = T, legend = "bottom")

# Save
ggsave(here("figures", "fig7_density-plot.png"),
       density_result,
       dpi = 600,
       w = 8,
       h = 5)

# Summary
fing_tables <- fing_map_res %>% 
  bind_rows(.id = "sample") %>%
  as_tibble() %>% 
  gather(source, role, -id, -GOF, -sample) %>%
  dplyr::select(-id) %>% 
  group_by(sample, source) %>% 
  summarise(role.mean = mean(role),
            role.median = median(role),
            role.sd = sd(role),
            GOF.mean = mean(GOF)) %>% 
  mutate(role.mean = scales::percent(role.mean),
         role.sd = scales::percent(role.sd),
         role.mean = paste0(role.mean, " (",
                            role.sd, ")"),
         GOF.mean = scales::percent(GOF.mean)) %>% 
  ungroup() %>% 
  mutate(`Distance from the snout, km` = case_when(sample == "1" ~ 0,
                                                   sample == "2" ~ 0.84,
                                                   sample == "3" ~ 0.84 + 0.77,
                                                   sample == "PT" ~ 0.84 + 0.77)) %>% 
  arrange(`Distance from the snout, km`) %>% 
  dplyr::select(Sample = sample,
                `Distance from the snout, km`,
                `Sediment source` = source,
                `Proportional contribution [%]` = role.mean,
                `GOF [%]` = GOF.mean) %>% 
  mutate_if(is.numeric, ~signif(., 3)) %>%
  mutate_if(is.numeric, ~as.character(.)) %>% 
  as.data.frame()

# Save
write_xlsx(fing_tables,
           here("tables", "table6.xslx"))

