# CO agricultural meteorology data, the function currently returns only ambient air temp
# source of url is https://coagmet.colostate.edu/data/url-builder
library(readr)
library(data.table)
f <- "data-raw/coagdata.csv"

metrics <- c("t","rh","dewpt","vp","solarRad","precip","windSpeed","windDir","st5cm","st15cm")
metricsLabels <- c("datetime", "temp_air_degC", "rh_%", "dewpoint_deg_c", "vaporpressure_kpa", "solarrad_wm-2", "precip_mm", "wind_m_s", "winddir_deg_n", "temp_soil_5cm_deg_c", "temp_soil_15cm_deg_c")
coagNumeric <- c("temp_air_degC", "rh_%", "dewpoint_deg_c", "vaporpressure_kpa", "solarrad_wm-2", "precip_mm", "wind_m_s", "winddir_deg_n", "temp_soil_5cm_deg_c", "temp_soil_15cm_deg_c")
station <- "frt03" #Fruita
collectperiod <- "5min"
units <- "m"

getcoagdata <- function(startDate, endDate, metrics, station, collectperiod) {
  combined_string <- paste(metrics, collapse = ",")
  
  url <- paste0("https://coagmet.colostate.edu/data/",collectperiod,"/", station,".csv?header=yes&fields=", combined_string,"&from=", startDate, "&to=", endDate, "&tz=co&units=", units)
  download.file(url, f)
  
  coagdata <- as.data.table(read_csv("data-raw/coagdata.csv", 
                                     col_types = cols(`date time` = col_datetime(format = "%m/%d/%Y %H:%M")), 
                                     skip = 1))
  colnames <- append("station", metricsLabels)
  coagdata <- setnames(coagdata, old = names(coagdata), new = colnames)
  
  coagdata[,datetime := as.POSIXct(datetime, tz = "America/Denver", format = "%m/%d/%Y %H:%M")]
  
  # agg to 15 min
  coagdata <- coagdata[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                       by = .(datetime_15min = cut(datetime, "15 mins")),
                       .SDcols = names(coagdata)[!names(coagdata) %in% c("datetime", "station")]]
  
  coagdata_temp <- coagdata[, c("datetime_15min", "temp_air_degC")]
  coagdata_temp[, datetime_15min := as.POSIXct(datetime_15min, format = "%Y-%m-%d %H:%M")]
  return(coagdata_temp)
}

