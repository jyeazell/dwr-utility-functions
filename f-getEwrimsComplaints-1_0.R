## f-dlEwrimsComplaints.R
##
## Version 1.0 
## 2022-04-09

dlEwrimsComplaints <- function(cred_file = "ewrims-key.csv") {
    
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
    
    # Create temporary directory.
    dl_loc <- tempdir()
    
    # Define Chrome options.
    eCaps <- list(
        chromeOptions =
            list(prefs = list("profile.default_content_settings.popups" = 0L,
                              "download.prompt_for_download" = FALSE,
                              "directory_upgrade" = TRUE,
                              "download.default_directory" = normalizePath(dl_loc)))
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
    complaints_url <- paste0("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/",
                             "EWComplaintServlet?Purpose=getEwrimsComplaintSearch&",
                             "Page_From=EWMenuAuthorized.jsp&Redirect_Page=",
                             "EWComplaintSearch.jsp")
    remDr$navigate(complaints_url)
    
    # Click Search button to retrieve complaints.
    comp_search <- remDr$findElement("xpath", '//input[@value="Search"]')
    comp_search$clickElement()
    Sys.sleep(3)
    
    # Click Download to Excel button to download complaints file.
    download <- remDr$findElement("xpath", '//input[@value="Download to Excel"]')
    download$clickElement()
    Sys.sleep(3)
    
    # Close connection to Selenium server.
    remDr$close()
    driver$server$stop()
    driver$server$process
    
    # Return path of temporary directory where downloaded file is to calling function.
    cs_dl_file <- paste0(dl_loc, "/ComplaintSearch.xls")
    return(cs_dl_file)
    
}
