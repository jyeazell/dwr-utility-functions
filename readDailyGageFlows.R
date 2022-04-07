# @knitr readDailyGageFlows

##' readDailyGageFlows
##' Version: 1.2.3
##' Modified: 2016-09-29
##'
##' Scrapes and returns daily gage flow data in cubic feet per second (cfs) from
##' DWR's CDEC web site for stations passed to the function in the
##' 'station_sensor_list' data frame. If no station list is passed, the function
##' defaults to downloading data for the stations applicable to the Sacramento,
##' San Joaquin, and Delta analysis areas. If multiple measurements are reported
##' for a station in a day (i.e., at hourly or quarterly intervals), the
##' function returns the daily average.
##'
##' At a minimum, the station_sensor_list data frame is expected to have the
##' following three columns (one rowfor each station):
##'     station_id: Three-character station id code
##'     sensor: The sensor number for the data set requested. Typically it is 20
##'             for River discharge in cfs, but should be confirmed on CDEC.
##'     dur_code:   Duration code. Typically "E" (event) or "H" (hourly), but should
##'                 be confirmed on CDEC.
##'
##' The following columns are optional:
##'     station_name: The station's name
##'     sensor_description: Type of measurement the sensor reports
##'
##' The function returns a tidy-long data frame with the following columns:
##'     date: Measurement date
##'     station_id: Three-character station id code
##'     variable: Measurement variable, in this case, 'gage_flow_cfs'
##'     value: value of the measurement variable.
##'
##' Changes:
##'   2016-09-29: Added NaN handling after calculating means of single NAs.
##'   2016-09-22: Modified check for existsence of station_sensor list.
##'   2016-08-09: Added Daily average outflow (ID 23) at WHI to statndard list
##'   2016-07-15: Added code to convert missing values reported as '-9998'
##'               to NAs
##'   2016-06-16: Renamed function to readDailyGageFlows
##'   2016-06-09: Converted variable names to lower-case_underscore style
##'               Rewrote code so that resulting data frame is tidy-long
##'

# Load required libraries during source if not yet loaded ----
if(!("package:lubridate" %in% search())) {
    suppressMessages(library(lubridate))
}
if(!("package:tidyr" %in% search())) {
    suppressMessages(library(tidyr))
}
if(!("package:dplyr" %in% search())) {
    suppressMessages(library(dplyr))
}

readDailyGageFlows <- function(station_sensor_list = NA,
                          start_date = "2015-10-01",
                          end_date = "Now",
                          save_csv = TRUE) {

    # If no station_sensor_list is passed to the function, use the default set, use
    # the 21 stations mapped for the supply/demand network analysis:
    if (class(station_sensor_list) != "data.frame") {
        station_sensor_list <- data.frame(
            station_id = c("BND", "ORO", "YRS", "MRY", "FSB", "VON", "FOL", "FPT",
                           "SJF", "SMN", "NEW", "MMF", "LGN", "MOD", "VNS", "GDW",
                           "OBB", "KOT", "PAR", "CMN", "MHB", "WHI"),
            station_name = c("Sacramento R. at Bend Bridge",
                             "Oroville Dam (Feather R.)",
                             "Yuba R. near Smartvillle",
                             "Yuba R. near Marysville",
                             "Feather R. above Star Bend",
                             "Sacramento R. at Verona",
                             "Folsom Lake (Sacramento R.)",
                             "Sacramento R. at Freeport",
                             "San Joaquin R. below Friant",
                             "San Joaquin R. above Merced near Newman",
                             "San Joaquin R. near Newman",
                             "Merced R. below Merced Falls",
                             "Tuolumne R. below La Grange Dam near La Grange",
                             "Tuolumne R. at Modesto",
                             "San Joaquin R. near Vernalis",
                             "Goodwin Dam (Stanislaus R.)",
                             "Stanislaus R. at Orange Blossom Bridge",
                             "Stanislaus R. at Koetitz Ranch",
                             "Pardee (Mokelumne R.)",
                             "Camanche Reservoir (Mokelumne R.)",
                             "Cosumnes R. at Michigan Bar",
                             "Whiskeytown Dame"),
            sensor = c(41, 23, 41, 20, 20, 20, 23, 20, 20, 20, 20, 20, 41, 20, 20,
                       71, 20, 20, 23, 23, 20, 23),
            dur_code = c("D", "D", "D", "E", "E", "H", "D", "E", "E", "E", "H", "H",
                         "D", "E", "H", "D", "H", "E", "H", "H", "H", "D"),
            sensor_description = c("Flow, Mean Daily (cfs)",
                                   "Reservoir Ouflow (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, Mean Daily (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Reservoir Ouflow (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, Mean Daily (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Discharge, Spillway (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Reservoir Ouflow (cfs)",
                                   "Reservoir Ouflow (cfs)",
                                   "Flow, River Discharge (cfs)",
                                   "Reservoir Ouflow (cfs)"),
            stringsAsFactors = FALSE
        )
    }

    scrape_start <- paste(month(start_date),"%2F", day(start_date),"%2F", year(start_date), sep = "")

    if (end_date == "Now") {
        scrape_end <- "Now"
        end_date <- today()
    } else {
        scrape_end <- paste(month(end_date),"%2F", day(end_date),"%2F", year(end_date), sep = "")
    }

    gage_flows <- list()

    for (i in 1 : nrow(station_sensor_list)) {
        cat("Processing Station ", station_sensor_list$station_id[i] ,"...\n", sep = "")
        source_url <- paste("http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=",
                            station_sensor_list$station_id[i],
                            "&sensor_num=", station_sensor_list$sensor[i],
                            "&dur_code=", station_sensor_list$dur_code[i],
                            "&start_date=", scrape_start,
                            "&end_date=", scrape_end,
                            "&data_wish=View+CSV+Data",
                            sep = "")
        gage_flows[[i]] <- (read.csv(source_url,
                                     skip = 1,
                                     quote = "\'",
                                     na.strings = "m",
                                     stringsAsFactors = FALSE))
        names(gage_flows[[i]]) <- c("date", "time","gage_flow_cfs")
        gage_flows[[i]] <- gage_flows[[i]] %>%
            mutate(station_id = station_sensor_list$station_id[i]) %>%
            select(station_id, everything())
    }

    gage_flows <- bind_rows(gage_flows)
    gage_flows$date <- as.Date(as.character(gage_flows$date), "%Y%m%d")

    # Missing values are flagged by '-9998' in some CDEC data sets.
    # Convert to NAs:
    gage_flows[, 4][gage_flows[, 4] == -9998] <- NA

    daily_gage_flows <- gage_flows %>%
        group_by(station_id, date) %>%
        summarize(gage_flow_cfs = mean(gage_flow_cfs, na.rm =TRUE))%>%
        as.data.frame(.)
    # Convert NaNs to NAs:
    daily_gage_flows[is.nan(daily_gage_flows$gage_flow_cfs), "gage_flow_cfs"] <- NA

    # Tidy data frame:
    daily_gage_flows <- daily_gage_flows %>%
        gather(variable, value, -date, -station_id) %>%
        select(date, station_id, variable, value)

    cat("\nWeb scrape complete.\n\n")

    if(save_csv){
        if(!file.exists("./Daily_Gage_Flow_Downloads")) dir.create("./Daily_Gage_Flow_Downloads")
        fileDest <- sprintf("./Daily_Gage_Flow_Downloads/Gage_Flows_%s.csv",
                            format(Sys.time(),"%Y%m%d_%H%M"))
        write.csv(daily_gage_flows, fileDest, row.names = FALSE)
        cat("\nData file saved to ",fileDest,"\n\n")
    }
    return(daily_gage_flows)
}
