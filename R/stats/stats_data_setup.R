# statistical analysis of soil moisture data

# install packages that are not already installed
list.of.needed.packages <- c("readr", "data.table", "readxl", "lubridate", "dplyr", "grDevices", "officer", "magrittr", "magick", "flextable")
new.packages <- list.of.needed.packages[!(list.of.needed.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
###
library(readr)
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
suppressWarnings(source("R/coagDataImport.R")) # get ambient variables from Colorado Ag Data. 5 minute data averaged to 15 min. Creates coagdata data.table
coagdata_temp <- coagdata[, c("TIMESTAMP_15min", "temp_ambient_C")]
coagdata_temp[, TIMESTAMP_15min := as.POSIXct(TIMESTAMP_15min, format = "%Y-%m-%d %H:%M")]

#create the 'data' directory if it is not already created
dir.create("data", F, F)
#create the 'data' directory if it is not already created
dir.create("data/stats", F, F)

irrigation <- read_excel("data-raw/irrigation.xlsx", col_types = c("date", "text", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric"))
irrigation$DATE <- force_tz(irrigation$DATE, tz = "America/Denver")
timeConvert <- function(t_in) {
  time_decimal <- t_in
  time_seconds <- as.integer(time_decimal * 86400)  # Convert fractions to seconds
  time_hms <- seconds_to_period(time_seconds)
  combined_datetime <- irrigation$DATE + time_hms
  combined_datetime <- format(as.POSIXct(combined_datetime), "%Y-%m-%d %H:%M")
}

irrigation$TIME_ON <- timeConvert(irrigation$TIME_ON)
irrigation$TIME_OFF <- timeConvert(irrigation$TIME_OFF)

#startDate <- as.POSIXct("2023-05-23") earliest possible
startDate <- as.POSIXct("2023-05-25")
endDate <- as.POSIXct("2023-10-01")

coagdata_temp <- coagdata_temp[TIMESTAMP_15min >= startDate, ]
coagdata_temp <- coagdata_temp[TIMESTAMP_15min <= endDate, ]

irrigation |> dplyr::filter(irrigation$DATE >= startDate & irrigation$DATE <= endDate)
irrigation <- dplyr::filter(irrigation, DATE >= startDate & DATE <= endDate)

irrig_start_times_west <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "west") |> pull(TIME_ON) |> as.POSIXct()
irrig_start_times_east <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "east") |> pull(TIME_ON) |> as.POSIXct()

irrig_end_times_west <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "west") |> pull(TIME_OFF) |> as.POSIXct()
irrig_end_times_east <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "east") |> pull(TIME_OFF) |> as.POSIXct()


dlnames <- c("S1T", "S1M", "S1B", 
             "S2T", "S2M", "S2B", 
             "S3T", "S3M", "S3B", 
             "S4T", "S4M", "S4B")  # 
tableNums <- c("1", "2", "3")
tableNums <- c("1")
vars <- c("VWC", "EC", "T")

# for testing
dlname <- "S4B"
tableNum <- c("1")

gal_applied_west <- irrigation |> dplyr::filter(STRIP_ID == "west") |> dplyr::pull(METER_GAL_USE_GAL_X_100)
gal_applied_east <- irrigation |> dplyr::filter(STRIP_ID == "east") |> dplyr::pull(METER_GAL_USE_GAL_X_100)

for (dlname in dlnames) {
  if (dlname %in% c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B")) { 
    west_strips <- 1 
  } else {
    west_strips <- 0
  }
  
  for (tableNum in tableNums) {
    tname <- paste0(dlname, "_Table", tableNum)
    print(paste0("working on table ", dlname))
    #read in and clean up the table
    colnames <- c("TIMESTAMP","RECORD","VWC_1_Avg","EC_1_Avg","T_1_Avg","VWC_2_Avg","EC_2_Avg","T_2_Avg","VWC_3_Avg","EC_3_Avg","T_3_Avg")
    t <- readr::read_csv(paste0("data-raw/", tname, ".dat"), locale = locale(tz = "America/Denver"), col_names = colnames, col_types = 
                           cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S"), RECORD = col_character(),
                                VWC_1_Avg = col_number(), EC_1_Avg = col_number(), T_1_Avg = col_number(), 
                                VWC_2_Avg = col_number(), EC_2_Avg = col_number(), T_2_Avg = col_number(), 
                                VWC_3_Avg = col_number(), EC_3_Avg = col_number(), T_3_Avg = col_number()), 
                         skip = 4, na = c("", "NA", "NAN"))
    #t <- t[-1,] 
    t <- t[, -2] # remove RECORD column
    
    t <- t[t$TIMESTAMP >= startDate, ]
    t <- t[t$TIMESTAMP <= endDate, ]
    t <- merge(t, coagdata_temp, by.x = "TIMESTAMP", by.y = "TIMESTAMP_15min")
    
    # add logger specific infod, columns for strip, biochar, water share
    strip <- substr(dlname, 2, 2)
    biochar <- "N"
    watervol <- 50
    if (strip %in% c("1", "3")) biochar <- "Y"
    if (strip %in% c("3", "4")) watervol <- "100"
    biochar <- rep(biochar, nrow(t))
    water <- rep(watervol, nrow(t))
    strip <- rep(strip, nrow(t))
    
    t <- cbind(t, strip)
    t <- cbind(t, biochar)
    t <- cbind(t, water)
    new_col_order <- c("strip", "biochar", "water", "TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg",  "T_2_Avg",  "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg") #, "temp_ambient_C")
    t <- setcolorder(t, new_col_order)
    outf <- paste0("data/statsdata_", dlname, ".csv")
    write_csv(t, outf)
  }
}