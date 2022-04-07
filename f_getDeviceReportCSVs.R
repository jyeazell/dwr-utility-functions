##' If save_file = TRUE, will save a date-stamped copy of downloaded report
##' to compliance_reports. This directory is created if it doesn't exist
##' in project home directory
##'
##'Function returns file path to downloaded reporting compliance file.

getDeviceReportCSVs <- function(start_year = 2017,
                                end_year = 2020,
                                cred_file = "ciwqs-creds.csv",
                                save_file = TRUE) {
  
  ## Initialization. ----
  
  # Load required packages if not already loaded.
  if (!("package:dplyr" %in% search())) {
    suppressMessages(library(dplyr))
  }
  if (!("package:readr" %in% search())) {
    suppressMessages(library(readr))
  }
  if (!("package:openxlsx" %in% search())) {
    suppressMessages(library(openxlsx))
  }
  if (!("package:stringr" %in% search())) {
    suppressMessages(library(stringr))
  }
  if (!("package:RSelenium" %in% search())) {
    suppressMessages(library(RSelenium))
  }
  if (!("package:netstat" %in% search())) {
    suppressMessages(library(netstat))
  }
  
  # Initialization.
  scrape_browser <- "firefox"
  
  # Load log-in credentials from file.
  creds <- read_csv(cred_file)

    # Define Chrome options.
    eCaps <- list(
      chromeOptions =
        list(prefs = list("profile.default_content_settings.popups" = 0L,
                          "download.prompt_for_download" = FALSE,
                          "directory_upgrade" = TRUE,
                          "download.default_directory" = normalizePath(getwd()))))
    
    # Create Chrome driver
    driver<- rsDriver(port = free_port(),
                      browser= "chrome",
                      chromever = "91.0.4472.101",
                      extraCapabilities = eCaps)
 

  # Open connection.
  remDr <- driver[["client"]]
  
  # Navigate to CIWQS Login page.
  remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/index.jsp")
  
  # Enter login credentials.
  username <- remDr$findElement(using = "name", value = "accountname")
  username$sendKeysToElement(list(creds$u_name))
  passwd <- remDr$findElement(using = "name", value = "password")
  passwd$sendKeysToElement(list(creds$p_word, "\uE007"))
  
  # Navigate to Reporting Compliance download page.
  remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/reportingMeasurementDownloadSetup.do")
  
  
  # Initialize rms_measurement
  devices <- tibble()
  yrs_to_get <- as.character(c(start_year : end_year))
  
  ## Function to download report and add to rms_measurement ----
  for (i in yrs_to_get) {
    # Select year to download from dropdown.
    dd_string <- paste0("//*/option[@value = '", i,"']")
    option <- remDr$findElement(using = "xpath", dd_string)
    option$clickElement()
    
    # Download the data file.
    download <- remDr$findElement("xpath", '//input[@type="submit"]')
    download$clickElement()
    Sys.sleep(5)
    
    # Read file and append to rms_measurement.
    t_df <- read_csv("./rms_measurement.csv")
    devices <- rbind(devices, t_df)
    
    # Delete temp data and files.
    file.remove("./rms_measurement.csv")
    rm(t_df)
    
  }
  
  # Close connection to Selenium server.
  remDr$close()
  driver$server$stop()
  driver$server$process
  rm(creds)
  
  
  # Save downloaded file if flagged to do so.
  if(save_file) {
    
    # Create "device-reports" directory if it doesn't exist.
    new_dir <- "./device-reports/"
    if(!dir.exists(new_dir)) dir.create(new_dir)
    
    save_fname <- paste0("rms-measurement-", start_year, "-to-",end_year, "-",
                         Sys.Date())
    write_csv(devices, file = paste0(new_dir, save_fname, ".csv"))
    # save(devices, file = paste0(new_dir, save_fname,".RData"))
  }
  
  return(devices)
  
  
}
