##' If save_file = TRUE, will save a date-stamped copy of downloaded report
##' to compliance_reports. This directory is created if it doesn't exist
##' in project home directory
##'
##'Function returns file path to downloaded reporting comliance file.

# getEwrimsComplaints <- function(cred_file = "ewrims_key.csv",
#                                       save_file = FALSE) {
cred_file = "ewrims-key.csv"
save_file = FALSE

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

    # Load login credentials from file.
    creds <- read_csv(cred_file)

    # Define Chrome options.
    eCaps <- list(
        chromeOptions =
            list(prefs = list("profile.default_content_settings.popups" = 0L,
                              "download.prompt_for_download" = FALSE,
                              "directory_upgrade" = TRUE,
                              "download.default_directory" = normalizePath(getwd())))
    )

    # Create Selenium server.
    driver<- rsDriver(port = free_port(),
                      browser= "chrome",
                      chromever = "100.0.4896.60",
                      extraCapabilities = eCaps)
    remDr <- driver[["client"]]

    # Navigate to CIWQS Login page.
    remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/index.jsp")

    # Enter login credentials.
    username <- remDr$findElement(using = "name", value = "accountname")
    username$sendKeysToElement(list(creds$u_name))
    passwd <- remDr$findElement(using = "name", value = "password")
    passwd$sendKeysToElement(list(creds$p_word, "\uE007"))

    # Navigate to Complaints page.
    remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/EWComplaintServlet?Purpose=getEwrimsComplaintSearch&Page_From=EWMenuAuthorized.jsp&Redirect_Page=EWComplaintSearch.jsp")

    # Click Search button to retrieve complaints.
    
    download <- remDr$findElement("xpath", '//input[@value="Search"]')
    download$clickElement()
    Sys.sleep(5)

    # Close connection to Selenium server.
    remDr$close()
    driver$server$stop()
    driver$server$process

    # Save downloaded file if flagged to do so.
    if(save_file){

        # Create "compliance_reports" directory if it doesn't exist.
        new_dir <- "./compliance_reports/"
        if(!dir.exists(new_dir)) dir.create(new_dir)

        pull_date <- as.Date(file.info("rms_reporting_compliance.csv")[[4]])
        old_file_path <- "./rms_reporting_compliance.csv"
        new_file_path <- paste0(new_dir,
                                rept_year,
                                "_rms_reporting_compliance_",
                                pull_date,
                                ".csv")
        file.rename(from = old_file_path,
                    to = new_file_path)

        # Return path to saved file.
        return(new_file_path)
    }


# }
