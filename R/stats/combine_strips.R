library(data.table)
library(lubridate)
library(dplyr)

dlnames <- c("S1T", "S1M", "S1B", 
             "S2T", "S2M", "S2B", 
             "S3T", "S3M", "S3B", 
             "S4T", "S4M", "S4B")  
vars <- c("VWC", "EC", "T")
colnames <- c("strip", "biochar", "water", "TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg", "T_2_Avg", "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg", "temp_ambient_C")

# for testing
dlname <- "S4B"
for (dlname in dlnames) {
  in_f <- paste0("data/statsdata_", dlname, ".csv")
  t <- readr::read_csv(in_f, locale = locale(tz = "America/Denver"),  skip = 1, col_names = colnames,
                       col_types = cols(
                         strip = col_character(),
                         biochar = col_factor(levels = c("N", "Y")), 
                         water = col_factor(levels = c("50", "100")), 
                         TIMESTAMP = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                         VWC_1_Avg = col_number(), VWC_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                         T_1_Avg = col_number(), T_2_Avg = col_number(), T_3_Avg = col_number(), 
                         EC_1_Avg = col_number(), EC_2_Avg = col_number(), EC_3_Avg = col_number(), temp_ambient_C = col_number()),
                         na = c("", "NA", "NAN"))
  if (!exists("comb")) {
    comb <- t
  } else {
    comb <- rbind(comb,t)
  }
}

write_csv(comb, "data/statsdata_combined.csv")  

  