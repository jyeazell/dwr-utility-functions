##'
##'

load_reporting_rff <- function(rep_year) {
    # Load packages ----
    
    # if (!("package:tibble" %in% search())) {
    #     suppressMessages(library(tibble))
    # }
    # if (!("package:tidyr" %in% search())) {
    #     suppressMessages(library(tidyr))
    # }
    if (!("package:dplyr" %in% search())) {
        suppressMessages(library(dplyr))
    }
    if (!("package:readr" %in% search())) {
        suppressMessages(library(readr))
    }
    
    # Load Reporting Compliance data file.
    
    source_file <-
        "http://jasperreports/EwrimsFlatFile/ewrims_flat_file_annual_report.csv"
    
    reporting <- read_csv(
        source_file,
        col_types = cols(
            ADDITIONAL_INFO = col_character(),
            ADDITIONAL_REMARKS = col_character(),
            ADDITIONAL_TIME_REQUEST_DATE = col_date(format = "%m/%d/%Y %I:%M:%S %p"),
            ALTERNATIVE_COMPL_PLAN_DATE = col_date(format = "%m/%d/%Y %I:%M:%S %p"),
            CERTIFICATE_ID = col_character(),
            COMMERCIAL_ACTIVITY = col_character(),
            COMPLETELY_EMPTY_1 = col_character(),
            COMPLETELY_EMPTY_2 = col_character(),
            CONSERVATION_AMOUNT = col_number(),
            CONTRACT_NUMBER = col_character(),
            DIVERSION_UNDER_PRIOR_RIGHTS = col_character(),
            DEVICE_MAKE = col_character(),
            DEVICE_MODEL_NUMBER = col_character(),
            DEVICE_SERIAL_NUMBER = col_character(),
            DIVERTED_AND_USED_UNDER = col_character(),
            FREQUENCY_DATA_RECORDING = col_character(),
            FROM_MONTH = col_integer(),
            TO_MONTH = col_integer(),
            FROM_DAY = col_integer(),
            TO_DAY = col_integer(),
            INDOOR_CANOPY_SIZE_SF = col_number(),
            INDOOR_HARVEST_COUNT = col_integer(),
            INDOOR_PLANT_COUNT = col_integer(),
            INTAKE_LOCATION_CHANGE = col_character(),
            INTAKE_LOCATION_CHANGE_DESC = col_character(),
            EXCHANGE_OR_SETTLEMENT = col_character(),
            MIXED_HARVEST_COUNT = col_integer(),
            MIXED_PLANT_COUNT = col_integer(),
            NUMBER_OF_RESERVOIR = col_integer(),
            OUTDOOR_CANOPY_SIZE_SF = col_number(),
            OUTDOOR_HARVEST_COUNT = col_integer(),
            OUTDOOR_PLANT_COUNT = col_integer(),
            OTHER_CHANGE = col_character(),
            PROJECT_STATUS = col_character(),
            PERMIT_ID = col_character(),
            POU_CHANGE = col_character(),
            TOTAL_AMOUNT_USED_AF = col_number(),
            LICENSE_ID = col_character(),
            MEASURE_METHOD_2 = col_character(),
            NOT_COMP_WORK_DONE = col_character(),
            REQUEST_REVOCATION = col_character(),
            COMPLYING_WITH_ALL_TERMS = col_character(),
            ADDITIONAL_REMARKS = col_character(),
            MAX_ROD_REPORT_UNITS = col_character(),
            MEASURING_DEVICE_ID = col_character(),
            MIXED_CANOPY_SIZE_SF = col_number(),
            NOT_COMP_CONS_STARTED = col_character(),
            REVIEWED_WATER_RIGHT_PERMIT = col_character(),
            DIVERSION_AND_USE_COMMENT = col_character(),
            MEASURMENT_REQUIRED = col_character(),
            IS_DIVERSION_MEASURED = col_character(),
            NAME_1 = col_character(),
            NAME_2 = col_character(),
            OTHER_CHANGE_DESC = col_character(),
            OTHER_IRRIGATION = col_character(),
            OTHER_PROVIDER_NAME = col_character(),
            POD_SAME_AS_WR = col_character(),
            POU_CHANGE_DESC = col_character(),
            SPECIFY_AESTHETIC = col_character(),
            SPILLED_THIS_YEAR_1 = col_character(),
            SPILLED_THIS_YEAR_2 = col_character(),
            FEET_BELOW_AT_MAX_1 = col_number(),
            FEET_BELOW_AT_MAX_2 = col_number(),
            FEET_BELOW_AT_MIN_1 = col_number(),
            FEET_BELOW_AT_MIN_2 = col_number(),
            MEASURE_METHOD_1 = col_character(),
            SIGNATURE_NAME = col_character(),
            WATER_CONSERVE_DESC = col_character(),
            CONSERVATION_AMOUNT_UNIT = col_character(),
            RECLAMATION_AMOUNT = col_number(),
            RECLAMATION_AMOUNT_UNIT = col_character(),
            REASON_FOR_NON_COMPLIANCE = col_character(),
            CONJUNCTIVE_AMOUNT_UNIT = col_character(),
            SPECIFY_FISH_AND_WILD_LIFE = col_character(),
            NOT_COMP_CONS_COMPLETED = col_character(),
            NOT_COMP_EST_COMP_DATE = col_date(format = "%m/%d/%Y %I:%M:%S %p"),
            DATE_SUBMITTED = col_date(format = "%m/%d/%Y %I:%M:%S %p"),
            NOT_COMP_USE_COMMENCE = col_character(),
            NOT_COMP_WITHIN_TIME = col_character(),
            POD_PARCEL_NUMBER = col_character(),
            INSTALLED_CAPACITY = col_number(),
            QUANTITY_TRANSFER_AF = col_number(),
            UNIT_POWER_GENERATION = col_character(),
            TELEMETRY_WEBSITE = col_character(),
            TOTAL_COMB_DIV_STO = col_number(),
            TOTAL_STOCK_WATERING = col_character(),
            TOTAL_INDUSTRIAL = col_character(),
            TOTAL_IRRIGATED_SF = col_number(),
            TOTAL_MILLING = col_character(),
            TOTAL_FIRE_PROTECTION = col_character(),
            TOTAL_RECREATION = col_character(),
            TOTAL_FROST_PROTECTION = col_character(),
            TOTAL_DUST_CONTROL = col_character(),
            TOTAL_FISH_CULTURE = col_character(),
            TOTAL_FROST_PROTECTION = col_character(),
            TOTAL_HEAT_PROTECTION = col_character(),
            TOTAL_INCIDENTAL_POWER = col_character(),
            TOTAL_MINING = col_character(),
            TOTAL_SNOW_MAKING = col_character(),
            TRANSFER_APPROVED_BY = col_character(),
            TYPE_OF_DEVICE_METHOD = col_character(),
            WATER_SOURCE = col_character(),
            UNITS_OF_MEASUREMENT = col_character(),
            USE_TYPE_CHANGE = col_character(),
            USE_TYPE_CHANGE_DESC = col_character(),
            YEAR_DIVERSION_COMMENCED = col_integer()
        )
    )
    
    # Filter for target year and remove unused columns from table.
    
    reporting <- reporting %>%
        filter(REPORT_YEAR == rep_year,
               REVISION %in% c(0, 1)) %>%
        select(wr_id = APPLICATION_NUMBER,
               file_date = DATE_SUBMITTED)
    
}