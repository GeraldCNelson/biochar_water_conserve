# This code imports a csv file with all observations for all data loggers.
# The drawplot function
# - calculates the mean value of each variable at each depth for all (3) data loggers in a strip
# - plots the strip average for each variable at each of the 3 depths - 6, 12, and 18 inches in separate graphs

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
source("R/loadIrrigationData.R")

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

startDate <- as.POSIXct("2023-06-01")
endDate <- as.POSIXct("2023-08-30")

#test data
varname <- "EC"
depth <- "18"
ymin <- 0
ymax <- 0.8
stripCombo <- "W" # choices are W (strips 1 and 2) and E (strips 3 and 4)
xlab <- ""
# end test data

colnames <- c("datalogger", "strip", "biochar", "water", "TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg", "T_2_Avg", "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg", "temp_ambient_C")
dt_vars <- as.data.table(
  readr::read_csv("data/statsdata_combined.csv", locale = locale(tz = "America/Denver"), skip = 1, 
                  col_names = colnames, col_types = cols(
                    datalogger = col_character(),
                    strip = col_character(),
                    biochar = col_character(), 
                    water = col_character(), 
                    TIMESTAMP = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                    VWC_1_Avg = col_number(), VWC_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                    T_1_Avg = col_number(), T_2_Avg = col_number(), T_3_Avg = col_number(), 
                    EC_1_Avg = col_number(), EC_2_Avg = col_number(), EC_3_Avg = col_number(), 
                    temp_ambient_C = col_number()), na = c("", "NA", "NAN")))

dt_vars <- dt_vars[TIMESTAMP >= startDate & TIMESTAMP <= endDate,]

#average to a day
sdcols <- c("VWC_1_Avg", "EC_1_Avg",  "T_1_Avg",   "VWC_2_Avg", "EC_2_Avg",  "T_2_Avg", "VWC_3_Avg", "EC_3_Avg",  "T_3_Avg", "temp_ambient_C" )
dt_vars <- dt_vars[, TIMESTAMP_24hr := format(TIMESTAMP, "%m-%d")][, TIMESTAMP := NULL]
dt_vars[,TIMESTAMP_24hr := as.POSIXct(TIMESTAMP_24hr, format = "%m-%d")]

dt_vars <- dt_vars[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                   by = c("TIMESTAMP_24hr", "strip"), .SDcols = sdcols]

drawplot <- function(dt_vars, varname, depth, stripCombo, ylab, plotTitle, plotSubtitle, irrigation) {
  if (varname == "T") {
    ylab <- "Temperature (°C)"
    temp_ambient_C <- dt_vars$temp_ambient_C
    ymin = 10
  }
  if (varname == "EC") {ylab <- "Electrical conductivity (deciSiemens per meter)";ymax = 1}
  if (varname == "VWC") {ylab <- "Volumetric Water Content (%)"} # (%)
  
  irrigation |> dplyr::filter(irrigation$DATE >= startDate & irrigation$DATE <= endDate)
  irrigation <- dplyr::filter(irrigation, DATE >= startDate & DATE <= endDate)
  
  irrig_start_times_west <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "west") |> pull(TIME_ON) |> as.POSIXct()
  irrig_start_times_east <- irrigation |> dplyr::filter(irrigation$STRIP_ID == "east") |> pull(TIME_ON) |> as.POSIXct()
  
  gal_applied_west <- irrigation |> dplyr::filter(STRIP_ID == "west") |> dplyr::pull(METER_GAL_USE_GAL_X_100)
  gal_applied_east <- irrigation |> dplyr::filter(STRIP_ID == "east") |> dplyr::pull(METER_GAL_USE_GAL_X_100)
  galText_west <- paste0("Gal (000): \n", round(gal_applied_west/1000, 1))
  galText_east <- paste0("Gal (000): \n", round(gal_applied_east/1000, 1))
  
  
  varsubset <- paste0(varname, "_", c("1", "2", "3"), "_", "Avg")
  date_times <- dt_vars$TIMESTAMP_24hr
  # calculate mean of the variable in each strip
   s <- dt_vars[, c("strip",  "TIMESTAMP_24hr", varsubset, "temp_ambient_C"), with = FALSE]
  s[, var_1_mean := mean(get(varsubset[1]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, var_2_mean := mean(get(varsubset[2]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, var_3_mean     := mean(get(varsubset[3]), na.rm = TRUE), by = c("strip", "date_times")]
  s[, temp_ambient_C := mean(temp_ambient_C, na.rm = TRUE), by = c("strip", "date_times")]
  s[, (varsubset) := NULL]
   dt_var1_mean <- unique(s[, c("strip",  "TIMESTAMP_24hr", "var_1_mean", "temp_ambient_C"), with = FALSE])
  dt_var2_mean <- unique(s[, c("strip",  "TIMESTAMP_24hr", "var_2_mean", "temp_ambient_C"), with = FALSE])
  dt_var3_mean <- unique(s[, c("strip",  "TIMESTAMP_24hr", "var_3_mean", "temp_ambient_C"), with = FALSE])
  newNames <- c("strip",  "TIMESTAMP", "var_mean", "temp_ambient_C") #includes converting TIMESTAMP_24hr to TIMESTAMP
  setnames(dt_var1_mean, old = names(dt_var1_mean), new = newNames)
  setnames(dt_var2_mean, old = names(dt_var2_mean), new = newNames)
  setnames(dt_var3_mean, old = names(dt_var3_mean), new = newNames)
  dt_var1_mean <- dt_var1_mean[order(TIMESTAMP)]
  dt_var2_mean <- dt_var2_mean[order(TIMESTAMP)]
  dt_var3_mean <- dt_var3_mean[order(TIMESTAMP)]

    # Generate x axis grid lines 
  gridlines <- seq(min(date_times), max(date_times), by = "5 days")
  if (stripCombo == "W") {legendElements <- c("Strip 1 \n(w/ biochar)", "Strip 2"); strips = c("1", "2")}
  if (stripCombo == "E") {legendElements <- c("Strip 3 \n(w/ biochar)", "Strip 4"); strips = c("3", "4")}
  
  if (depth == "6") plotData <- dt_var1_mean
  if (depth == "12") plotData <- dt_var2_mean
  if (depth == "18") plotData <- dt_var3_mean
  if (varname == "VWC") plotData$var_mean <- plotData$var_mean * 100
  maxs1 = round(max(plotData[strip == strips[1], var_mean]), 2)
  maxs2 = round(max(plotData[strip == strips[2], var_mean]), 2)
  mins1 = round(min(plotData[strip == strips[1], var_mean]), 2)
  mins2 = round(min(plotData[strip == strips[2], var_mean]), 2)
  
  minvalstext <- paste0("Min vals: strip ", strips[1], " - ", mins1, ", strip ", strips[2], " - ", mins2)
  maxvalstext <- paste0("Max vals: strip ", strips[1], " - ", maxs1, ", strip ", strips[2], " - ", maxs2)
  plotSubtitle <- paste0("start date: ", format(startDate, "%m-%d"), ", end date: ", format(endDate, "%m-%d"), "\n", minvalstext,  ", ", maxvalstext)
    plotData_x <- plotData$TIMESTAMP[which(plotData$strip == "1")]
  plotData_y_1 <- plotData$var_mean[which(plotData$strip == strips[1])]
  plotData_y_2 <- plotData$var_mean[which(plotData$strip == strips[2])]
  plotData_ambient_temp <- plotData$temp_ambient_C[which(plotData$strip == strips[2])]
  if (varname == "EC") {plotTitle <- paste0("Electrical conductivity, ", stripCombo, " strips average, \n", depth, " inch depth"); ymin <- 0; ymax <- 1.0}
  if (varname == "VWC") {plotTitle <- paste0("Soil moisture, ", stripCombo, " strips average, \n", depth, " inch depth"); ymin <- 0; ymax <- 50}
  if (varname == "T") {plotTitle <- paste0("Soil and ambient air temperature, ", stripCombo, " strips average,\n ", depth, " inch depth"); ymin <- 15.0; ymax <- 30.0}
  
  plot(plotData_x, plotData_y_1, ylim = c(ymin, ymax), type = "l", xaxt = "n", col = "blue", xlab = xlab, ylab = ylab, main = plotTitle, sub = plotSubtitle, cex.sub = .8)
  lines(plotData_x, plotData_y_2, type = "l", col = "red")
  if (varname == "T") lines(plotData_x, plotData_ambient_temp, type = "l", col = "gray")
  axis(1, at = gridlines,
       labels = format(gridlines, "%m-%d"), cex.axis = 0.7, lwd = 0.3, xpd = NA, las = 3)
  grid(nx = NA, ny = NULL, lwd = 0.7)
  abline(v = gridlines, col = "lightgray", lty = "dotted", lwd = 0.9) # x axis grid
  if (stripCombo == "W") {
    abline(v = irrig_start_times_west, col = "brown", lwd = 0.5)
    textY <- ymin + (ymax - ymin)/5
    galText_west <- paste0("Gal.:\n", gal_applied_west) 
    tin_west <- round_date(irrig_start_times_west, unit = "day")
    text(tin_west, pos = 4, textY, galText_west, cex = 0.6, col = "darkgreen")
    }
  if (stripCombo == "E") {
    abline(v = irrig_start_times_east, col = "brown", lwd = 0.5)
    textY <- ymin + (ymax - ymin)/5
    galText_east <- paste0("Gal.:\n", gal_applied_east) 
    tin_east <- round_date(irrig_start_times_east, unit = "day")
    
    text(tin_east, pos = 4, textY, galText_east, cex = 0.6, col = "darkgreen")
  }
  legendcolnames <- c("blue", "red")
  if (varname == "T") {legendcolnames <- c("blue", "red", "lightgray")
  legendElements <- append(legendElements, "Ambient Air Temp.")}
  legend("topleft", legend = legendElements, col = legendcolnames, cex = .8, y.intersp = 1, lty = 1)
}

for (stripCombo in stripCombos) {
  for (depth in depths) {
    drawplot(dt_vars, varname, depth, stripCombo, ylab, plotTitle, plotSubtitle, irrigation)
  }
}

