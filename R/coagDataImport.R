# CO ag met data
# source this code from coagDataImport.R
# source of url is https://coagmet.colostate.edu/data/url-builder
f <- "data-raw/coagdata.csv"

# startDate <- "2023-06-01"
# endDate <- "2023-08-11"
url <- paste0("https://coagmet.colostate.edu/data/5min/frt03.csv?header=yes&fields=t,rh,dewpt,vp,solarRad,precip,windSpeed,windDir,st5cm,st15cm&from=", startDate, "&to=", endDate, "&tz=co&units=m")
download.file(url, f)

coagdata <- read.csv("data-raw/coagdata.csv")
coagdata <- coagdata[-1,]
names(coagdata) <- c("Station", "TIMESTAMP", "temp_ambient_C", "RH_%", "dewpoint_C", "VaporPress_kPa", "Solar.Rad_W_m-2", "Precip_mm", "Wind_ms", "Wind.Dir_deg",  "X5cm.Soil.Temp_C", "X15cm.Soil.Temp_C")
coagdata <- as.data.table(coagdata)
coagdata[,TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "America/Denver", format = "%m/%d/%Y %H:%M")]
coagNumeric <- c("temp_ambient_C", "RH_%", "dewpoint_C", "VaporPress_kPa", "Solar.Rad_W_m-2", "Precip_mm", "Wind_ms", "Wind.Dir_deg", "X5cm.Soil.Temp_C", "X15cm.Soil.Temp_C")
coagdata[, (coagNumeric) := lapply(.SD, as.numeric), .SDcols = coagNumeric]

# agg to 15 min
coagdata <- coagdata[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
               by = .(TIMESTAMP_15min = cut(TIMESTAMP, "15 mins")),
               .SDcols = names(coagdata)[!names(coagdata) %in% c("TIMESTAMP", "Station")]]

# aggregate to day
coagdata_day <- coagdata[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                     by = .(TIMESTAMP_day = cut(TIMESTAMP, "day")),
                     .SDcols = names(coagdata)[!names(coagdata) %in% c("TIMESTAMP", "Station")]]
write.csv(coagdata_day, paste0("data/", "FruitaAgWeather", "_dayAve.csv"))

t <- t[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
       by = .(TIMESTAMP_24hr = cut(TIMESTAMP, "day")), .SDcols = sdcols]
t[,TIMESTAMP_24hr := as.POSIXct(TIMESTAMP_24hr)]




