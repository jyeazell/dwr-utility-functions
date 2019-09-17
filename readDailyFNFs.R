##' readDailyFNFs
##' Version: 2.1
##' Modified: 2016-06-15_1310
##' 
##' Scrapes and returns daily Full Natural Flow (FNF) data in cubic feet per 
##' second (cfs) from DWR's CDEC web site for stations passed to the function in the 
##' 'station_list' vector. If no station list is passed, the function 
##' defaults to the 10 stations used in the Sacramento, San Joaquin, and 
##' Delta analysis areas.
##' 
##' At a minimum, the station_list vector is expected to have at least one three-
##' character station code that reports daily FNFs on CDEC as sensor 8.  
##' 
##' The function returns a tidy-long data frame with the following columns:
##'     date: Measurement date
##'     station_id: Three-character station id code
##'     variable: Measurement variable, in this case, 'daily_fnf_cfs'
##'     value: value of the measurement variable.
##' 
##' If save_csv is true, the functions saves a date-stamped csv file of the resulting
##' data frame to the Daily_FNF_Downloads directory in the home directory. The directory
##' is created if it does not already exist.
##' 
##' Changes:
##'   2015-10-14: Added ability to specify end date.
##'   2016-06-09: Converted variable names to lower-case_underscore style.
##'               Rewrote code so that resulting data frame is tidy-long.
##'   2016-06-13: Added more detailed comments.                                

# Load required libraries.

if(!("package:lubridate" %in% search())) {
    suppressMessages(library(lubridate))
}
if(!("package:tidyr" %in% search())) {
    suppressMessages(library(tidyr))
}
if(!("package:dplyr" %in% search())) {
    suppressMessages(library(dplyr))
}


readDailyFNFs <- function(station_list = c("BND", "ORO", "YRS", "FOL", "MHB",
                                           "MKM", "GDW", "TLG", "MRC", "MIL"),
                          start_date = "2014-10-01",
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
    
    daily_fnfs <- list()
    
    for (i in station_list) {
        cat("Processing Station ",i,"...\n", sep = "")
        source_url <- paste("http://cdec.water.ca.gov/cgi-progs/queryCSV?", 
                            "station_id=", i,
                            "&sensor_num=8&dur_code=D&start_date=", scrape_start,
                            "&end_date=", scrape_end,
                            "&data_wish=View+CSV+Data",
                            sep = "")
        daily_fnfs[[i]] <- read.csv(source_url, 
                                    skip = 1, 
                                    na.strings = "m",
                                    stringsAsFactors = FALSE)
        names(daily_fnfs[[i]]) <- c("date", "time","daily_fnf_cfs")
        daily_fnfs[[i]] <- daily_fnfs[[i]] %>% 
            mutate(station_id = i) %>%
            select(date, station_id, daily_fnf_cfs)
    }
    
    daily_fnfs <- bind_rows(daily_fnfs)
    daily_fnfs$date <- as.Date(as.character(daily_fnfs$date), "%Y%m%d")
    
    # Tidy data frame.
    
    daily_fnfs <- daily_fnfs %>% gather(variable, value, -date, -station_id)
    
    cat("CDEC Daily FNF webscrape complete.\n\n")
    
    if(save_csv){
        if(!file.exists("./Daily_FNF_Downloads")) dir.create("./Daily_FNF_Downloads")
        fileDest <- sprintf("./Daily_FNF_Downloads/Daily_FNFs_%s.csv", 
                            format(Sys.time(),"%Y%m%d_%H%M"))
        write.csv(daily_fnfs, fileDest, row.names = FALSE)
        cat("Data file saved to ",fileDest,"\n\n")
    }
    
    return(daily_fnfs)
}
