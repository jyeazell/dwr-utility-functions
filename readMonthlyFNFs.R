# readMonthlyFNFs()
# Version: 1.0
# Modified: 2016-06-09

# Scrapes monthly Full Natural Flow (FNF) values from DWR's CDEC web site.
# FNF values are in Acre-Feet.
#
# Changes:
#   2016-06-09: Converted variable names to lower-case_underscore style
#               Rewrote code so that resulting data frame is tidy-long                   
#       
#

# Load required libraries if not yet loaded ----
if(!("package:lubridate" %in% search())) {
    suppressMessages(library(lubridate))
}
if(!("package:tidyr" %in% search())) {
    suppressMessages(library(tidyr))
}
if(!("package:plyr" %in% search())) {
    suppressMessages(library(plyr))
}
if(!("package:dplyr" %in% search())) {
    suppressMessages(library(dplyr))
}

readMonthlyFNFs <- function(station_list = c("SBB", "FTO", "YRS", "AMF",
                                             "CSN", "MKM", "SNS", "TLG",
                                             "MRC", "SJF"),
                            start_date = "2009-10-01",
                            end_date = "Now",
                            save_csv = TRUE) {
    
    scrape_start <- paste(month(start_date),"%2F", 
                          day(start_date),"%2F", 
                          year(start_date), sep = "")
    
    if (end_date == "Now") {
        scrape_end <- "Now"
        end_date <- today()
    } else {
        scrape_end <- paste(month(end_date),"%2F", 
                            day(end_date),"%2F", 
                            year(end_date), sep = "")
    }
    
    monthly_fnfs <- list()
    
    
    for (i in station_list) {
        cat("Processing Station ",i,"...\n", sep = "")
        source_url <- paste("http://cdec.water.ca.gov/cgi-progs/queryCSV?", 
                            "station_id=", i,
                            "&sensor_num=65&dur_code=M&start_date=", scrape_start,
                            "&end_date=", scrape_end,
                            "&data_wish=View+CSV+Data", sep = "")
        monthly_fnfs[[i]] <- read.csv(source_url, 
                                      skip = 1, 
                                      na.strings = "m",
                                      quote = "'",
                                      stringsAsFactors = FALSE)
        names(monthly_fnfs[[i]]) <- c("date", "time","monthly_fnf_af")
        monthly_fnfs[[i]] <- monthly_fnfs[[i]] %>% 
            mutate(station_id = i) %>%
            select(date, station_id, monthly_fnf_af)
    }
    
    monthly_fnfs <- bind_rows(monthly_fnfs)
    monthly_fnfs$date <- as.Date(as.character(monthly_fnfs$date), "%Y%m%d")
    
    # Rename Monthly FNF station IDs so they match their corresponding
    # Daily FNF station IDs:
    station_id_replacements <- data.frame(
        old = c("SBB", "FTO", "AMF", "CSN", "SNS", "SJF"),
        new = c("BND", "ORO", "FOL", "MHB", "GDW", "MIL"),
        stringsAsFactors = FALSE)
    monthly_fnfs$station_id <- mapvalues(monthly_fnfs$station_id,
                                         station_id_replacements$old,
                                         station_id_replacements$new)
    rm(station_id_replacements)
    
    # Tidy data frame:
    monthly_fnfs <- monthly_fnfs %>% gather(variable, value, -date, -station_id)
    
    
    if(save_csv){
        if(!file.exists("./Monthly_FNF_Downloads")) dir.create("./Monthly_FNF_Downloads")
        fileDest <- sprintf("./Monthly_FNF_Downloads/Monthly_FNFs_%s.csv",
                            format(Sys.time(),"%Y%m%d_%H%M"))
        write.csv(monthly_fnfs, fileDest, row.names = FALSE)
        cat("Data file save to ",fileDest,"\n\n")
    }
    cat("CDEC Monthly FNF webscrape complete.\n")
    
    return(monthly_fnfs)
}
