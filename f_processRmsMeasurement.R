


# Load rms_measurement datafiles from 2017-2020 reporting years, bind.
rms_measurement <- list(
  ry_2017 <- read_csv("./data/device-reports/rms-measurement-2017-2021-05-07.csv"),
  ry_2018 <- read_csv("./data/device-reports/rms-measurement-2018-2021-05-07.csv"),
  ry_2019 <- read_csv("./data/device-reports/rms-measurement-2019-2021-05-05.csv"),
  ry_2020 <- read_csv("./data/device-reports/rms-measurement-2020-2021-05-07.csv")
)

rms_measurement <- bind_rows(rms_measurement)

# Clean up rms_measurement.
rms_measurement <- rms_measurement_raw %>% 
  select(wr_id = APPLICATION_NUMBER,
         n_pods = POD_COUNT,
         county = COUNTY,
         watershed = WATERSHED,
         romd_sub_date = DATE_REPORT_FILED,
         meas_required = MEASURMENT_REQUIRED,
         div_is_measured = DIVERSION_IS_MEASURED,
         div_measured_by_wm = DIVERSION_MEAS_BY_WATERMASTER,
         device_id = MEASURING_DEVICE_ID,
         telemtery_required = TELEMETRY_REQUIRED,
         telemtry_url = TELEMETRY_WEBSITE)

# Roll up multi-device info.
rms_measurement <- rms_measurement %>% 
  group_by(wr_id) %>% 
  
  ## ---> This expression can be written better: <---
  summarise(
    ## ---> Need another source for commented data <---
    # county = paste0(unique(county), collapse = ", "),
    # n_pods = paste0(unique(n_pods), collapse = ", "),
    # watershed = paste0(unique(watershed), collapse = ", "),
    romd_sub_date = paste0(unique(romd_sub_date), collapse = ", "),
    meas_required = paste0(unique(meas_required), collapse = ", "),
    div_is_measured = paste0(unique(div_is_measured), collapse = ", "),
    div_measured_by_wm = paste0(unique(div_measured_by_wm), collapse = ", "),
    device_id = paste0(unique(device_id), collapse = ", "),
    telemtery_required = paste0(unique(telemtery_required), collapse = ", "),
    telemtry_url = paste0(unique(telemtry_url), collapse = ", "),
    .groups = "drop"
  ) %>% 
  distinct()
