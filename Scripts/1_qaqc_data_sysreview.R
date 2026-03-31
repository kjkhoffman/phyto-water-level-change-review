qaqc_data_sysreview <- function(data_path){
  
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(readr)
  library(here)
  
  #Read in data
  data <- data_path
  init_data <- read.csv(here(data))
  init_data <- clean_names(init_data)
  #head(init_data)
  
  # QAQC
  # Excluded papers
  #Li 2024 flow speeds are too fast, is a river
  
  data_excl <- init_data |> 
    filter(!name_yr %in% c("Beaver 2013","Jia 2022","Cao 2016","Wang 2022","Kisand 2004", "Hart 2004", "Nõges 2010", "Tuvikene 2011", "Christensen 2015", "Braga 2015", "Braga 2020", "Lacerda 2018", "Abirhire 2019", "Sakharova 2018", "Noges 1999", "Li 2024")) |>
    filter(!res_name == "Serra Serrada Reservoir") |> 
    filter(!res_name == "Bera Lake") |> 
    filter(!title %in% c("Steady-state assemblages in a Mediterranean hypertrophic reservoir. The role of Microcystis ecomorphological variability in maintaining an apparent equilibrium")) |>
    filter(!notes %in% c("Ignore this version, everything has been added to multiple reservoirs sheet! KKH"))
  
  excluded_papers <- anti_join(init_data, data_excl)
  
  duplicate_waterbodies <- data_excl |> 
    select(name_yr, res_name, country, study_years, consecutive, phys_response, notes)
  
  # Fixing typos and assigning trophic levels
  
  data <- data_excl |> 
    mutate(tp = mean_tp) |> 
    separate_wider_delim(mean_tp, " ", names = c("tp_ugL", "unit_tp"), too_few = "align_start") |> 
    mutate(mean_chla = ifelse(mean_chla == "0.07 to 34.62 ug/L", "8", mean_chla)) |> 
    mutate(mean_chla = ifelse(mean_chla == "15.7ug/L", "15.7 ug/L", mean_chla)) |> 
    mutate(chla = mean_chla) |> 
    separate_wider_delim(mean_chla, " ", names = c("chla_ugL", "unit_chla"), too_few = "align_start") |>
    mutate(res_name = ifelse(res_name == "Rybinsk", "Rybinsk Reservoir",
                             ifelse(res_name == "Cruzeta man-made lake", "Cruzeta", 
                                    ifelse(res_name == "Castanhao", "Castanhão reservoir",
                                           ifelse(name_yr == "Bouvy 2003", "Tapacura Reservoir", res_name))))) |> 
    filter((!is.na(cov_num)|!is.na(num_water)|!is.na(lat_dd))) |> 
    mutate(fullpond_sa = ifelse(fullpond_sa == "~600", 600, fullpond_sa),
           fullpond_sa = as.numeric(fullpond_sa)) |> 
    mutate(obs_maxdepth = ifelse(obs_maxdepth == "SEK had 12, 12 max at sampling station", 12, obs_maxdepth),
           mindepth = ifelse(mindepth == "SEK had 9, 9 was min at sampling station", 9, mindepth)) |> 
    mutate(elev_decrease_max = ifelse(grepl("2001", elev_decrease_max), "-6", elev_decrease_max),
           elev_increase_max = ifelse(elev_increase_max == 0, NA, elev_increase_max)) |> 
    mutate(long_dd = ifelse(res_name == "Lake Buchanan", -98.4, long_dd),
           lat_dd = ifelse(lat_dd == 103.466667 & res_name == "Zipingpu Reservoir", 30.983333, lat_dd), 
           long_dd = ifelse(long_dd == 30.983333 & res_name == "Zipingpu Reservoir", 103.466667, long_dd),
           long_dd = ifelse(long_dd == 6.77888889 & res_name == "Serra Serrada", -6.77888889, long_dd)) |> 
    mutate(tp_ugL = as.numeric(tp_ugL)) |> 
    mutate(mean_tp = case_when(
      unit_tp == "mg/L" ~ tp_ugL * 1000, 
      unit_tp == "mg/m3" ~ tp_ugL / 1000, 
      TRUE ~ tp_ugL)) |> 
    mutate(chla_ugL = as.numeric(chla_ugL)) |>  
    mutate(mean_chla = case_when(
      unit_chla == "mg/L" ~ chla_ugL * 1000, 
      unit_chla == "mg/m3" ~ chla_ugL / 1000, 
      TRUE ~ chla_ugL)) |> 
    mutate(trophic = ifelse(trophic == "Other: according to authors, varies according to water level; never more than mesotrophic unless water level is low", "mesotrophic", trophic)) %>% 
    mutate(trophic = ifelse(trophic == "", "not reported", trophic)) |> 
    mutate(chla_ugL = as.numeric(chla_ugL)) |>  
    mutate(tp_ugL = as.numeric(tp_ugL)) |> 
    mutate(trophic_status_chla = ifelse(trophic == "not reported" & chla_ugL <= 2.6, "oligotrophic", 
                                        ifelse(trophic == "not reported" & chla_ugL >2.6 & chla_ugL <=7.3, "mesotrophic", 
                                               ifelse(trophic == "not reported" & chla_ugL >7.3 & chla_ugL <= 56, "eutrophic", 
                                                      ifelse(trophic == "not reported" & chla_ugL >56, "hypereutrophic",
                                                             ifelse(trophic == "not reported" & is.na(chla_ugL), trophic, trophic)))))) %>% 
    mutate(trophic_status_tp = ifelse(tp_ugL <= 12, "oligotrophic", 
                                      ifelse(tp_ugL >=12 & tp_ugL <=24, "mesotrophic", 
                                             ifelse(tp_ugL >24 & tp_ugL <= 70, "eutrophic", 
                                                    ifelse(tp_ugL >70, "hypereutrophic",
                                                           ifelse(is.na(tp_ugL), trophic, NA)))))) %>%  
    mutate(trophic_status_combined = ifelse(trophic == "not reported" & !is.na(trophic_status_tp), trophic_status_tp,
                                            ifelse(trophic == "not reported" & !is.na(trophic_status_chla), trophic_status_chla, trophic))) |> 
    mutate(trophic_status_reduced = ifelse(trophic_status_combined %in% c("oligotrophic"), "oligotrophic", 
                                           ifelse(trophic_status_combined %in% c("oligo-mesotrophic", "mesotrophic"), "mesotrophic", 
                                                  ifelse(trophic_status_combined %in% c("meso-eutrophic", "eutrophic"), "eutrophic", 
                                                         ifelse(trophic_status_combined %in% c("hypereutrophic", "eu-hypereutrophic"), "hypereutrophic", "not reported"))))) |> 
    filter(res_name != "")
  
  # write_csv(data, here("Data/qaqced.csv"))
  
  return(list(
    data = data,
    excluded_papers = excluded_papers,
    duplicate_waterbodies = duplicate_waterbodies
  ))
  
  
}
