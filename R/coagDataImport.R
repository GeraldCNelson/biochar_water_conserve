# CO ag met data
# source this code from coagDataImport.R
# source of url is https://coagmet.colostate.edu/data/url-builder
f <- "data-raw/coagdata.csv"

# startDate <- "2023-06-01"
# endDate <- "2023-08-11"
url <- paste0("https://coagmet.colostate.edu/data/5min/frt03.csv?header=yes&fields=t,rh,dewpt,vp,solarRad,precip,windSpeed,windDir,st5cm,st15cm&from=", startDate, "&to=", endDate, "&tz=co&units=m")
download.file(url, f)

coagdata <- readr::read_csv("data-raw/coagdata.csv", 
                     col_types = cols(Station = col_skip(), 
                                      `Date and Time` = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                      `Air Temp` = col_number(), RH = col_number(), 
                                      Dewpoint = col_number(), `Vapor Pressure` = col_number(), 
                                      `Solar Rad` = col_number(), Precip = col_number(), 
                                      Wind = col_number(), `Wind Dir` = col_number(), 
                                      `5cm Soil Temp` = col_number(), `15cm Soil Temp` = col_number()))
coagdata <- coagdata[-1,]
names(coagdata) <- c("TIMESTAMP", "temp_ambient_C", "RH_%", "dewpoint_C", "VaporPress_kPa", "Solar.Rad_W_m-2", "Precip_mm", "Wind_ms", "Wind.Dir_deg",  "X5cm.Soil.Temp_C", "X15cm.Soil.Temp_C")
coagdata <- as.data.table(coagdata)
coagdata[,TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "America/Denver", format = "%m/%d/%Y %H:%M")]
coagNumeric <- c("temp_ambient_C", "RH_%", "dewpoint_C", "VaporPress_kPa", "Solar.Rad_W_m-2", "Precip_mm", "Wind_ms", "Wind.Dir_deg", "X5cm.Soil.Temp_C", "X15cm.Soil.Temp_C")
coagdata[, (coagNumeric) := lapply(.SD, as.numeric), .SDcols = coagNumeric]

# agg to 15 min
coagdata <- coagdata[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
               by = .(TIMESTAMP_15min = cut(TIMESTAMP, "15 mins")),
               .SDcols = names(coagdata)[!names(coagdata) %in% c("TIMESTAMP", "Station")]]



