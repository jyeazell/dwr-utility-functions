##' Function usps_delivery_status ----
##' 
##' This function recieves a USPS Trackin Number, then queries the USPS web site
##' for the item's delivery status

usps_delivery_status <- function(user_id, tracking_no) {
    
    # Load required packages if not loaded already.
    
    if (!("package:RCurl" %in% search())) {
        suppressMessages(library(RCurl))
    }
    if (!("package:XML" %in% search())) {
        suppressMessages(library(XML))
    }
    if (!("package:stringr" %in% search())) {
        suppressMessages(library(stringr))
    }
    
    # Remove spaces in USPS tracking number
    
    tracking_no <- str_replace_all(tracking_no, " ", "")
    
    if(grepl("\\d{20}", tracking_no)) {
        usps_query <- paste0("http://production.shippingapis.com/",
                             "ShippingAPI.dll?API=TrackV2",
                             "&XML=<TrackRequest%20USERID=%22", user_id, "%22>",
                             "<TrackID%20ID=%22", tracking_no,
                             "%22></TrackID></TrackRequest>")
        result <- getURL(usps_query)
        result <- xmlToList(xmlParse(result))
        result <- result$TrackInfo$TrackSummary
        return(result)
    } else {
        return("Bad Tracking Number")
    }
}