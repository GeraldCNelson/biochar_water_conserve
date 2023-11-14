# This code imports a csv file with all observations for all data loggers.
# The drawplot function
# - calculates the mean value of each variable at each dept for all (3) data loggers in a strip
# - plots the strip average for each variable at each of the 3 depths - 6, 12, and 18 inches

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
varnames <- c("VWC", "EC", "T")
stripCombos <- c("E", "W")
depths <- c("6", "12", "18")
# VWCs[, VWC1Mean := mean(VWC_1_Avg, na.rm = TRUE), by = c("strip", "TIMESTAMP")]
# VWCs[, VWC2Mean := mean(VWC_2_Avg, na.rm = TRUE), by = c("strip", "TIMESTAMP")]
# VWCs[, VWC3Mean := mean(VWC_3_Avg, na.rm = TRUE), by = c("strip", "TIMESTAMP")]
# 
# VWC1Mean <- unique(VWCs[, c("strip", "biochar", "water", "TIMESTAMP", "VWC1Mean")])
# VWC1Mean <- VWC1Mean[TIMESTAMP >= startDate & TIMESTAMP <= endDate]
# VWC1Mean <- VWC1Mean[order(TIMESTAMP),]
# if varname is temp
#y_ambient_temp <- t$temp_ambient_C

startDate <- as.POSIXct("2023-06-01 12:00:00")
endDate <- as.POSIXct("2023-08-30 12:00:00")

varname <- "VWC"
depth <- "18"
stripCombo <- "W" # choices are W (strips 1 and 2) and E (strips 3 and 4)
ylab <- "Volumetric Water Content (%)"
if (varname == "T") ylab <- "Temperature (°C)"
if (varname == "EC") ylab <- "Electrical conductivity \n(deciSiemens per meter)"
xlab <- ""
ymin <- 0
ymax <- 0.5


# get irrigation start times for vertical lines
suppressWarnings(irrigation <- read_excel("data-raw/irrigation.xlsx", col_types = c("date", "text", "numeric", 
                                                                                    "numeric", "numeric", "numeric", 
                                                                                    "numeric", "numeric", "numeric", 
                                                                                    "numeric")))
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

colnames <- c("datalogger", "strip", "biochar", "water", "TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg", "T_2_Avg", "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg", "temp_ambient_C")
dt_vars <- as.data.table(readr::read_csv("data/statsdata_combined.csv", locale = locale(tz = "America/Denver"), skip = 1, col_names = colnames,
                                         col_types = cols(
                                           datalogger = col_character(),
                                           strip = col_character(),
                                           biochar = col_factor(levels = c("N", "Y")), 
                                           water = col_factor(levels = c("50", "100")), 
                                           TIMESTAMP = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                                           VWC_1_Avg = col_number(), VWC_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                                           T_1_Avg = col_number(), T_2_Avg = col_number(), T_3_Avg = col_number(), 
                                           EC_1_Avg = col_number(), EC_2_Avg = col_number(), EC_3_Avg = col_number(), temp_ambient_C = col_number()), na = c("", "NA", "NAN")))
dt_vars <- dt_vars[TIMESTAMP >= startDate & TIMESTAMP <= endDate]

drawplot <- function(dt_vars, varname, depth, stripCombo, ylab, plotTitle, plotSubtitle, irrigation) {
  irrigation |> dplyr::filter(irrigation$DATE >= startDate & irrigation$DATE <= endDate)
  irrigation <- dplyr::filter(irrigation, DATE >= startDate & DATE <= endDate)
  
  irrig_start_times_west <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "west") |> pull(TIME_ON) |> as.POSIXct()
  irrig_start_times_east <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "east") |> pull(TIME_ON) |> as.POSIXct()
  
  varsubset <- paste0(varname, "_", c("1", "2", "3"), "_", "Avg")
  date_times <- dt_vars$TIMESTAMP
  s <- dt_vars[, c("datalogger", "strip", "biochar", "water", "TIMESTAMP", varsubset), with = FALSE]
  s[, var_1_mean := mean(get(varsubset[1]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, var_2_mean := mean(get(varsubset[2]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, var_3_mean := mean(get(varsubset[3]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, (varsubset) := NULL]
  s[, datalogger := NULL]
  dt_var1_mean <- unique(s[, c("strip", "biochar", "water", "TIMESTAMP", "var_1_mean"), with = FALSE])
  dt_var2_mean <- unique(s[, c("strip", "biochar", "water", "TIMESTAMP", "var_2_mean"), with = FALSE])
  dt_var3_mean <- unique(s[, c("strip", "biochar", "water", "TIMESTAMP", "var_3_mean"), with = FALSE])
  setnames(dt_var1_mean, old = names(dt_var1_mean), new = c("strip", "biochar", "water", "TIMESTAMP", "var_mean"))
  setnames(dt_var2_mean, old = names(dt_var2_mean), new = c("strip", "biochar", "water", "TIMESTAMP", "var_mean"))
  setnames(dt_var3_mean, old = names(dt_var3_mean), new = c("strip", "biochar", "water", "TIMESTAMP", "var_mean"))
  dt_var1_mean <- dt_var1_mean[order(TIMESTAMP)]
  dt_var2_mean <- dt_var2_mean[order(TIMESTAMP)]
  dt_var3_mean <- dt_var3_mean[order(TIMESTAMP)]
  
  # Generate x axis grid lines 
  gridlines <- seq(min(date_times), max(date_times), by = "5 days")
  
  legendElements <- c("Strip 1 (w/ biochar)", "Strip 2")
  if (stripCombo == "E") legendElements <- c("Strip 3 (w/ biochar)", "Strip 4")
  
  if (depth == "6") plotData <- dt_var1_mean
  if (depth == "12") plotData <- dt_var2_mean
  if (depth == "18") plotData <- dt_var3_mean
  plotData_x <- plotData$TIMESTAMP[which(plotData$strip == "1")]
  plotData_y_1 <- plotData$var_mean[which(plotData$strip == "1")]
  plotData_y_2 <- plotData$var_mean[which(plotData$strip == "2")]
  
  plotTitle <- paste0("Soil moisture, ", stripCombo, " strips average, ", depth, " inch depth")
  plotSubtitle <- paste0("start date: ", format(startDate, "%m-%d"), ", end date: ", format(endDate, "%m-%d"), "\nmin val: ", ymin, ", max val: ", ymax)
  
  plot(plotData_x, plotData_y_1, ylim = c(ymin, ymax), type = "l", xaxt = "n", col = "blue", xlab = xlab, ylab = ylab, main = plotTitle, sub = plotSubtitle, cex.sub = .8)
  lines(plotData_x, plotData_y_2, type = "l", col = "red")
  if (varname == "T") lines(plotData_x, y_ambient_temp, type = "l", col = "gray")
  axis(1, at = gridlines,
       labels = format(gridlines, "%m-%d"), cex.axis = 0.7, lwd = 0.3, xpd = NA, las = 3)
  grid(nx = NA, ny = NULL, lwd = 0.7)
  abline(v = gridlines, col = "lightgray", lty = "dotted", lwd = 0.9) # x axis grid
  if (stripCombo == "W") abline(v = irrig_start_times_west, col = "brown", lwd = 0.5)
  if (stripCombo == "E") abline(v = irrig_start_times_east, col = "brown", lwd = 0.5)
  
  legend("topright", legend = legendElements, col = c("blue", "red"), lty = 1)
}

for (stripCombo in stripCombos) {
  for (depth in depths) {
    drawplot(dt_vars, varname, depth, stripCombo, ylab, plotTitle, plotSubtitle, irrigation)
  }
}
