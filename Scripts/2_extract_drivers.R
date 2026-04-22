extract_drivers <- function(data){

  # This script converts extracted text fields into binary indicator columns for measured variables, drivers, and variables that responded to water level change.

  # Create data_ready, a data frame that is ready for grepping.
  # Appending a semicolon to the end of each driver column allows us to
  # protect against accidental string subset grepping below (and still catch
  # an element if it is the last one listed)
  data_ready <- data |>
    mutate(
      chem_drive = paste0(chem_drive, ";"),
      chem_response = paste0(chem_response, ";"),
      chem_vars = paste0(chem_vars, ";")
    )

combined_vars <- data_ready |>
  mutate(
    #---------------- DRIVERS ----------------#
    tp_drive  = ifelse(grepl("TP;|TP \\(", chem_drive), 1, 0),
    tn_drive  = ifelse(grepl("TN;|TN \\(", chem_drive), 1, 0),
    nh4_drive = ifelse(grepl("NH4/", chem_drive), 1, 0),
    ph_drive  = ifelse(grepl("pH;", chem_drive), 1, 0),
    do_drive  = ifelse(grepl("DO;", chem_drive), 1, 0),
    srp_drive = ifelse(grepl("SRP;", chem_drive), 1, 0),
    no3_drive = ifelse(grepl("NO3", chem_drive), 1, 0),
    cond_drive = ifelse(grepl("conductivity;", chem_drive), 1, 0),
    metals_drive = ifelse(grepl("metals;", chem_drive), 1, 0),
    orp_drive = ifelse(grepl("ORP;", chem_drive), 1, 0),
    no2_drive = ifelse(grepl("NO2;", chem_drive), 1, 0),
    dic_drive = ifelse(grepl("DIC;", chem_drive), 1, 0),
    dom_drive = ifelse(grepl("DOM;", chem_drive), 1, 0),
    pom_drive = ifelse(grepl("POM;", chem_drive), 1, 0),
    tkn_drive = ifelse(grepl("TKN;", chem_drive), 1, 0),
    doc_drive = ifelse(grepl("DOC;", chem_drive), 1, 0),
    din_drive = ifelse(grepl("DIN;", chem_drive), 1, 0),
    salinity_drive = ifelse(grepl("salinity", chem_drive), 1, 0),
    other_chem_drive = ifelse(grepl("Other", chem_drive, ignore.case = TRUE), 1, 0),

    water_level_drive = ifelse(grepl("water level", phys_drive, ignore.case = TRUE), 1, 0),
    water_temp_drive  = ifelse(grepl("water temperature", phys_drive, ignore.case = TRUE), 1, 0),
    transparency_drive = ifelse(grepl("transparency", phys_drive, ignore.case = TRUE), 1, 0),
    turb_drive = ifelse(grepl("turbidity", phys_drive, ignore.case = TRUE), 1, 0),
    light_drive = ifelse(grepl("PAR", phys_drive, ignore.case = TRUE), 1, 0),
    tds_drive = ifelse(grepl("tds|total dissolved solid", phys_drive, ignore.case = TRUE), 1, 0),
    tss_drive = ifelse(grepl("total suspended solids|tss|resuspension", phys_drive, ignore.case = TRUE), 1, 0), #adding sediment resuspension does not change this, but it's here for testing
    nor_tss_drive = ifelse(grepl("total suspended solids|tss", phys_drive, ignore.case = TRUE), 1, 0),
    strat_drive = ifelse(grepl("mix|stab|strat", phys_drive, ignore.case = TRUE), 1, 0),
    restime_drive = ifelse(grepl("residence", phys_drive, ignore.case = TRUE), 1, 0),
    flush_discharge_drive = ifelse(grepl("flush|discharge", phys_drive, ignore.case = TRUE), 1, 0),
    inflow_drive = ifelse(grepl("inflow", phys_drive, ignore.case = TRUE), 1, 0),
    precip_drive = ifelse(grepl("precip|rain", phys_drive, ignore.case = TRUE), 1, 0),
    air_temp_drive = ifelse(grepl("air", phys_drive, ignore.case = TRUE), 1, 0),
    other_phys_drive = ifelse(grepl("Other", phys_drive, ignore.case = TRUE), 1, 0),

    #---------------- RESPONSES ----------------#
    not_assessed_response = ifelse(grepl("not assessed", phys_response, ignore.case = TRUE), 1, 0),
    water_temp_response = ifelse(grepl("water temperature", phys_response, ignore.case = TRUE), 1, 0),
    transparency_response = ifelse(grepl("transparency", phys_response, ignore.case = TRUE), 1, 0),
    turb_response = ifelse(grepl("turbidity", phys_response, ignore.case = TRUE), 1, 0),
    light_response = ifelse(grepl("PAR", phys_response, ignore.case = TRUE), 1, 0),
    tds_response = ifelse(grepl("tds|total dissolved solid", phys_response, ignore.case = TRUE), 1, 0),
    tss_response = ifelse(grepl("total suspended solids|tss|resuspension", phys_response, ignore.case = TRUE), 1, 0),
    nor_tss_response = ifelse(grepl("total suspended solids|tss", phys_response, ignore.case = TRUE), 1, 0),
    other_phys_response = ifelse(grepl("Other", phys_response, ignore.case = TRUE), 1, 0),
    watercolor_response = ifelse(grepl("water color", phys_response, ignore.case = TRUE), 1, 0),
    strat_response = ifelse(grepl("mix|stab|strat", phys_response, ignore.case = TRUE), 1, 0),
    restime_response = ifelse(grepl("residence", phys_response, ignore.case = TRUE), 1, 0),
    flush_discharge_response = ifelse(grepl("flush|discharge", phys_response, ignore.case = TRUE), 1, 0),
    inflow_response = ifelse(grepl("inflow", phys_response, ignore.case = TRUE), 1, 0),
    precip_response = ifelse(grepl("precip|rain", phys_response, ignore.case = TRUE), 1, 0),
    air_temp_response = ifelse(grepl("air", phys_response, ignore.case = TRUE), 1, 0),
    other_phys_response = ifelse(grepl("Other", phys_response, ignore.case = TRUE), 1, 0),

    tp_response  = ifelse(grepl("TP;|TP \\(", chem_response), 1, 0),
    tn_response  = ifelse(grepl("TN;|TN \\(", chem_response), 1, 0),
    nh4_response = ifelse(grepl("NH4/", chem_response), 1, 0),
    ph_response  = ifelse(grepl("pH;", chem_response), 1, 0),
    do_response  = ifelse(grepl("DO;|DO \\(", chem_response), 1, 0),
    srp_response = ifelse(grepl("SRP;|SRP \\(", chem_response), 1, 0),
    no3_response = ifelse(grepl("NO3;|NO3 \\(", chem_response), 1, 0),
    cond_response = ifelse(grepl("conductivity;", chem_response), 1, 0),
    metals_response = ifelse(grepl("metals;", chem_response), 1, 0),
    orp_response = ifelse(grepl("ORP;", chem_response), 1, 0),
    no2_response = ifelse(grepl("NO2;|NO2 \\(", chem_response), 1, 0),
    doc_response = ifelse(grepl("DOC;", chem_response), 1, 0),
    dic_response = ifelse(grepl("DIC;", chem_response), 1, 0),
    dom_response = ifelse(grepl("DOM;", chem_response), 1, 0),
    pom_response = ifelse(grepl("POM;", chem_response), 1, 0),
    tkn_response = ifelse(grepl("TKN;", chem_response), 1, 0),
    din_response = ifelse(grepl("DIN;|DIN \\(", chem_response), 1, 0),
    salinity_response = ifelse(grepl("salinity;", chem_response), 1, 0),
    other_chem_response = ifelse(grepl("Other", chem_response, ignore.case = TRUE), 1, 0),

    #---------------- VARIABLES MEASURED ----------------#
    filt_chla_vars = ifelse(grepl("filtered chlorophyll", bio_vars, ignore.case = TRUE), 1, 0),
    phyto_micros_vars = ifelse(grepl("phytoplankton microscopy", bio_vars, ignore.case = TRUE), 1, 0),
    single_cyano_taxon_vars = ifelse(grepl("single cyanobacterial taxon", bio_vars, ignore.case = TRUE), 1, 0),
    sensor_chla_vars = ifelse(grepl("sensor", bio_vars, ignore.case = TRUE), 1, 0),
    other_bio_vars = ifelse(grepl("Other", bio_vars, ignore.case = TRUE), 1, 0),

    tp_vars  = ifelse(grepl("TP \\(", chem_vars), 1, 0),
    tn_vars  = ifelse(grepl("TN \\(", chem_vars), 1, 0),
    nh4_vars = ifelse(grepl("NH4/", chem_vars), 1, 0),
    ph_vars  = ifelse(grepl("pH;", chem_vars), 1, 0),
    do_vars  = ifelse(grepl("DO \\(", chem_vars), 1, 0),
    srp_vars = ifelse(grepl("SRP \\(", chem_vars), 1, 0),
    no3_vars = ifelse(grepl("NO3 \\", chem_vars), 1, 0),
    cond_vars = ifelse(grepl("conductivity;", chem_vars), 1, 0),
    metals_vars = ifelse(grepl("metals;", chem_vars), 1, 0),
    orp_vars = ifelse(grepl("ORP \\(", chem_vars), 1, 0),
    no2_vars = ifelse(grepl("NO2 \\(", chem_vars), 1, 0),
    doc_vars = ifelse(grepl("DOC \\(", chem_vars), 1, 0),
    dic_vars = ifelse(grepl("DIC \\(", chem_vars), 1, 0),
    dom_vars = ifelse(grepl("DOM \\(", chem_vars), 1, 0),
    pom_vars = ifelse(grepl("POM \\(", chem_vars), 1, 0),
    tkn_vars = ifelse(grepl("TKN \\(", chem_vars), 1, 0),
    din_vars = ifelse(grepl("DIN \\(", chem_vars), 1, 0),
    salinity_vars = ifelse(grepl("salinity;", chem_vars), 1, 0),
    other_chem_vars = ifelse(grepl("Other", chem_vars, ignore.case = TRUE), 1, 0),

    water_level_vars = ifelse(grepl("water level", phys_vars, ignore.case = TRUE), 1, 0),
    water_temp_vars  = ifelse(grepl("water temperature", phys_vars, ignore.case = TRUE), 1, 0),
    transparency_vars = ifelse(grepl("transparency", phys_vars, ignore.case = TRUE), 1, 0),
    turb_vars = ifelse(grepl("turbidity", phys_vars, ignore.case = TRUE), 1, 0),
    light_vars = ifelse(grepl("PAR", phys_vars, ignore.case = TRUE), 1, 0),
    tds_vars = ifelse(grepl("tds|total dissolved solid", phys_vars, ignore.case = TRUE), 1, 0),
    tss_vars = ifelse(grepl("total suspended solids|tss|resuspension", phys_vars, ignore.case = TRUE), 1, 0),
    nor_tss_vars = ifelse(grepl("total suspended solids|tss", phys_vars, ignore.case = TRUE), 1, 0),
    other_phys_vars = ifelse(grepl("Other", phys_vars, ignore.case = TRUE), 1, 0),
    strat_vars = ifelse(grepl("mix|stab|strat", phys_vars, ignore.case = TRUE), 1, 0),
    restime_vars = ifelse(grepl("residence", phys_vars, ignore.case = TRUE), 1, 0),
    flush_discharge_vars = ifelse(grepl("flush|discharge", phys_vars, ignore.case = TRUE), 1, 0),
    inflow_vars = ifelse(grepl("inflow", phys_vars, ignore.case = TRUE), 1, 0),
    precip_vars = ifelse(grepl("precip|rain", phys_vars, ignore.case = TRUE), 1, 0),
    air_temp_vars = ifelse(grepl("air", phys_vars, ignore.case = TRUE), 1, 0),
    other_phys_vars = ifelse(grepl("Other", phys_vars, ignore.case = TRUE), 1, 0)) |>

# If a variable has a 1 in its _drive or _response column,
# ensure the corresponding _vars column is also set to 1
  # added 4/10 to check this
mutate(
  # Chemical vars
  tp_vars       = ifelse(tp_drive == 1       | tp_response == 1,       1, tp_vars),
  tn_vars       = ifelse(tn_drive == 1       | tn_response == 1,       1, tn_vars),
  nh4_vars      = ifelse(nh4_drive == 1      | nh4_response == 1,      1, nh4_vars),
  ph_vars       = ifelse(ph_drive == 1       | ph_response == 1,       1, ph_vars),
  do_vars       = ifelse(do_drive == 1       | do_response == 1,       1, do_vars),
  srp_vars      = ifelse(srp_drive == 1      | srp_response == 1,      1, srp_vars),
  no3_vars      = ifelse(no3_drive == 1      | no3_response == 1,      1, no3_vars),
  cond_vars     = ifelse(cond_drive == 1     | cond_response == 1,     1, cond_vars),
  metals_vars   = ifelse(metals_drive == 1   | metals_response == 1,   1, metals_vars),
  orp_vars      = ifelse(orp_drive == 1      | orp_response == 1,      1, orp_vars),
  no2_vars      = ifelse(no2_drive == 1      | no2_response == 1,      1, no2_vars),
  doc_vars      = ifelse(doc_drive == 1      | doc_response == 1,      1, doc_vars),
  dic_vars      = ifelse(dic_drive == 1      | dic_response == 1,      1, dic_vars),
  dom_vars      = ifelse(dom_drive == 1      | dom_response == 1,      1, dom_vars),
  pom_vars      = ifelse(pom_drive == 1      | pom_response == 1,      1, pom_vars),
  tkn_vars      = ifelse(tkn_drive == 1      | tkn_response == 1,      1, tkn_vars),
  din_vars      = ifelse(din_drive == 1      | din_response == 1,      1, din_vars),
  salinity_vars = ifelse(salinity_drive == 1 | salinity_response == 1, 1, salinity_vars),

  # Physical vars
  water_level_vars      = ifelse(water_level_drive == 1,                                    1, water_level_vars),
  water_temp_vars       = ifelse(water_temp_drive == 1      | water_temp_response == 1,     1, water_temp_vars),
  transparency_vars     = ifelse(transparency_drive == 1    | transparency_response == 1,   1, transparency_vars),
  turb_vars             = ifelse(turb_drive == 1            | turb_response == 1,           1, turb_vars),
  light_vars            = ifelse(light_drive == 1           | light_response == 1,          1, light_vars),
  tds_vars              = ifelse(tds_drive == 1             | tds_response == 1,            1, tds_vars),
  tss_vars              = ifelse(tss_drive == 1             | tss_response == 1,            1, tss_vars),
  nor_tss_vars          = ifelse(nor_tss_drive == 1         | nor_tss_response == 1,        1, nor_tss_vars),
  strat_vars            = ifelse(strat_drive == 1           | strat_response == 1,          1, strat_vars),
  restime_vars          = ifelse(restime_drive == 1,                                        1, restime_vars),
  flush_discharge_vars  = ifelse(flush_discharge_drive == 1 | flush_discharge_response == 1,1, flush_discharge_vars),
  inflow_vars           = ifelse(inflow_drive == 1          | inflow_response == 1,         1, inflow_vars),
  precip_vars           = ifelse(precip_drive == 1          | precip_response == 1,         1, precip_vars),
  air_temp_vars         = ifelse(air_temp_drive == 1        | air_temp_response == 1,       1, air_temp_vars))


}
