propWL_logreg <- function(data){
# this script calculates proportional water-level changes and recodes phytoplankton and cyanobacterial responses for the logistic regression and chi-square analyses
data |>
  
  # Replace empty strings in obs_maxdepth with NA
  mutate(obs_maxdepth = ifelse(obs_maxdepth == "", NA, obs_maxdepth)) |> 
  
  # Create "usedepth": use observed max depth if available,
  # otherwise rely on full pond max depth
  mutate(usedepth = ifelse(is.na(obs_maxdepth), fullpond_maxdepth, obs_maxdepth)) |> 
  
  # Convert elevation decrease and depthvalues to numeric
  mutate(elev_decrease_max = as.numeric(elev_decrease_max), 
         usedepth = as.numeric(usedepth)) |> 
  
  # Calculate proportional decrease in water level
  # Only calculate if elev_increase_max is NA (i.e., decrease-only cases)
  mutate(prop_1delta_wl_decrease =
           ifelse(is.na(elev_increase_max), elev_decrease_max/usedepth, NA)) |> 
  
  # Calculate proportional decrease relative to depth
  mutate(prop_alldelta_wl_decrease = elev_decrease_max/usedepth) |> 
  
  # Ensure decreases are negative values
  mutate(prop_alldelta_wl_decrease =
           ifelse(prop_alldelta_wl_decrease > 0,
                  prop_alldelta_wl_decrease * -1,
                  prop_alldelta_wl_decrease)) |> 
  
  # Ensure decrease values in the single-change version are also negative
  mutate(prop_1delta_wl_decrease =
           ifelse(prop_1delta_wl_decrease > 0,
                  prop_1delta_wl_decrease * -1,
                  prop_1delta_wl_decrease)) |>
  
  # Calculate proportional increase in water level
  mutate(prop_alldelta_wl_increase = elev_increase_max/usedepth) |> 
  
  # Duplicate proportional increase calculation for "1 delta" variable
  mutate(prop_1delta_wl_increase = elev_increase_max/usedepth) |> 
  
  # Create a combined proportional delta water level variable
  # Use increase if decrease is NA
  #prop_delta_wl prioritizes wl decreases!
  mutate(prop_delta_wl_prioritydecrease =
           ifelse(is.na(prop_alldelta_wl_decrease),
                  prop_alldelta_wl_increase,
                  prop_alldelta_wl_decrease)) |> 
  
  # Alternative combined variable prioritizing increases instead
  mutate(prop_delta_wl_priorityincrease =
           ifelse(is.na(prop_alldelta_wl_increase),
                  prop_alldelta_wl_decrease,
                  prop_alldelta_wl_increase)) |> 


# Prepare dataset used for boxplots and response analysis
  
  # Convert phytoplankton response to binary for water level increase
  # 1 = increase in phytoplankton
  # 0 = decrease or no significant response
  mutate(phyto_binary_increase =
           ifelse(increase_phyto_response == "No significant response", 0, 
                  ifelse(increase_phyto_response == "Decrease in phytoplankton", 0, 
                         ifelse(increase_phyto_response == "Increase in phytoplankton", 1, NA)))) %>% 
  
  # Binary phytoplankton response for water level decrease
  mutate(phyto_binary_decrease =
           ifelse(decrease_phyto_response == "No significant response", 0, 
                  ifelse(decrease_phyto_response == "Decrease in phytoplankton", 0, 
                         ifelse(decrease_phyto_response == "Increase in phytoplankton", 1, NA)))) %>%
  
  # Combine phyto responses depending on direction of water level change
  mutate(combined_phyto_response_binary = case_when(
    is.na(phyto_binary_decrease) & prop_delta_wl_prioritydecrease < 0 ~ NA_real_,
    is.na(phyto_binary_decrease) ~ phyto_binary_increase,
    TRUE ~ phyto_binary_decrease
  )) |> 
  
  # Alternative combined phyto response prioritizing increases
  mutate(combined_phyto_response_binary_increasepriority = case_when(
    prop_delta_wl_priorityincrease < 0 ~ phyto_binary_decrease,
    is.na(phyto_binary_increase) & prop_delta_wl_priorityincrease > 0 ~ phyto_binary_increase,
    TRUE ~ phyto_binary_increase
  )) |> 
  
  # Binary cyanobacteria response for water level increase
  mutate(cyano_binary_increase =
           ifelse(increase_cyano_response == "No significant response", 0, 
                  ifelse(increase_cyano_response == "Decrease in cyanobacteria", 0, 
                         ifelse(increase_cyano_response == "Increase in cyanobacteria", 1, NA)))) %>% 
  
  # Binary cyanobacteria response for water level decrease
  mutate(cyano_binary_decrease =
           ifelse(decrease_cyano_response == "No significant response", 0, 
                  ifelse(decrease_cyano_response == "Decrease in cyanobacteria", 0, 
                         ifelse(decrease_cyano_response == "Increase in cyanobacteria", 1, NA)))) %>%
  
  # cyanobacteria response for water level increase
  mutate(cyano_response_wl_up =
           ifelse(increase_cyano_response == "No significant response", "Not significant", 
                  ifelse(increase_cyano_response == "Decrease in cyanobacteria", "Decrease ", 
                         ifelse(increase_cyano_response == "Increase in cyanobacteria", "Increase ", 
                                ifelse(increase_cyano_response == "shift", "Shift", NA))))) %>% 
  
  # cyanobacteria response for water level decrease
  mutate(cyano_response_wl_down =
           ifelse(decrease_cyano_response == "No significant response", "Not significant", 
                  ifelse(decrease_cyano_response == "Decrease in cyanobacteria", "Decrease", 
                         ifelse(decrease_cyano_response == "Increase in cyanobacteria", "Increase", 
                                ifelse(decrease_cyano_response == "shift",
                                       "Shift to \n Cyanobacteria \n dominated\n community", NA))))) %>% 
  
  # Combined cyanobacteria binary response depending on WL direction
  mutate(combined_cyano_response_binary = case_when(
    is.na(cyano_binary_decrease) & prop_delta_wl_prioritydecrease < 0 ~ NA_real_,
    is.na(cyano_binary_decrease) ~ cyano_binary_increase,
    TRUE ~ cyano_binary_decrease
  )) |>
  
  # Combined cyanobacteria response prioritizing increases
  mutate(combined_cyano_response_binary_increasepriority = case_when(
    prop_delta_wl_priorityincrease < 0 ~ cyano_binary_decrease,
    prop_delta_wl_priorityincrease > 0 ~ cyano_binary_increase,
    TRUE ~ cyano_binary_increase
  )) |> 
  
  # Combined cyanobacteria response text
  mutate(combined_cyano_response =
           ifelse(is.na(decrease_cyano_response),
                  cyano_response_wl_up,
                  decrease_cyano_response)) %>% 
  
  # Combined cyanobacteria response prioritizing increase response text
  mutate(combined_cyano_response_increasepriority =
           ifelse(is.na(cyano_response_wl_up),
                  decrease_cyano_response,
                  cyano_response_wl_up)) %>% 
  
  # phytoplankton response for water level increase
  mutate(phyto_response_wl_up =
           ifelse(increase_phyto_response == "No significant response", "Not significant", 
                  ifelse(increase_phyto_response == "Decrease in phytoplankton", "Decrease ", 
                         ifelse(increase_phyto_response == "Increase in phytoplankton", "Increase ", 
                                ifelse(increase_phyto_response == "shift", "Shift", NA))))) %>% 
  
  # phytoplankton response for water level decrease
  mutate(phyto_response_wl_down =
           ifelse(decrease_phyto_response == "No significant response", "Not significant", 
                  ifelse(decrease_phyto_response == "Decrease in phytoplankton", "Decrease", 
                         ifelse(decrease_phyto_response == "Increase in phytoplankton", "Increase", 
                                ifelse(decrease_phyto_response == "shift",
                                       "Shift to\n Cyanobacteria\n dominated\n community", NA))))) %>% 
  
  # Combined phytoplankton response text
  mutate(combined_phyto_response =
           ifelse(is.na(phyto_response_wl_down),
                  phyto_response_wl_up,
                  phyto_response_wl_down)) |> 
  
  # Combined phytoplankton response prioritizing increase direction
  
  mutate(combined_phyto_response_increasepriority =
           ifelse(is.na(phyto_response_wl_up),
                  phyto_response_wl_down,
                  phyto_response_wl_up))
  
}
