# deal with daily average data 

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
source("R/coagDataImport.R")
startDate <- as.POSIXct("2023-05-25")
endDate <- as.POSIXct("2023-10-01")

#create the 'data' directory if it is not already created
dir.create("data", F, F)


#startDate <- as.POSIXct("2023-05-23") earliest possible
coagdata_temp <- getcoagdata(startDate, endDate)
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

for (dlname in dlnames) {
  if (dlname %in% c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B")) { # strip 1 and 2 are the west side of the field
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
    if (nrow(t) == 0) {
      stop(paste0(tname, " has no data after the start date."))
    } else {
      
      # average to a day
      sdcols <- c("VWC_1_Avg", "EC_1_Avg",  "T_1_Avg",   "VWC_2_Avg", "EC_2_Avg",  "T_2_Avg", "VWC_3_Avg", "EC_3_Avg",  "T_3_Avg", "temp_ambient_C" )
      t <- as.data.table(t)
      t <- t[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
             by = .(TIMESTAMP_24hr = cut(TIMESTAMP, "day")), .SDcols = sdcols]
      t[,TIMESTAMP_24hr := as.POSIXct(TIMESTAMP_24hr)]
      write.csv(t,  paste0("data/", tname, "_dayAve.csv"))
      # add Colorado ag stats data for Fruita
      # reorder columns for easier eyeballing
      new_col_order <- c("TIMESTAMP_24hr", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg",  "T_2_Avg",  "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg") #, "temp_ambient_C")
      t <- setcolorder(t, new_col_order)
      print(paste0("logger: ", dlname))
      date_times <- t$TIMESTAMP_24hr
      
      #plot the variables -----
      for (varname in vars) {
        y1 = eval(parse(text = (paste0("t$", varname, "_1_Avg"))))
        y2 = eval(parse(text = (paste0("t$", varname, "_2_Avg"))))
        y3 = eval(parse(text = (paste0("t$", varname, "_3_Avg"))))
        if (varname == "T") {
          textY <- 15
          ylab = "Soil and air temperature (°C)"
          ylim <- c(10, 40)
          y_ambient_temp <- t$temp_ambient_C
        }
        if (varname == "EC") {
          textY <- 0.05
          print(paste0("working on variable ", varname))
          ylab = "Electrical conductivity \n(deciSiemens per meter)"
          ylim <- c(0, 1.0)
        }
        if (varname == "VWC") {
          textY <- 1
          ylab = "Volumetric Water Content (%)"
          ylim <- c(0, 50)
          ylab = "Soil moisture (%)"
           y1 <- y1 * 100; y2 <- y2 * 100; y3 <- y3 * 100
        }
        
        # check for missing data from a sensor
        bad_y1 <- bad_y2 <- bad_y3 <- 0
        if (sum(is.na(y1)) == length(y1)) {print(paste0(tname, " sensor 1 ", "variable ", varname, " might have a bad connection.")); bad_y1 = 1}
        if (sum(is.na(y2)) == length(y2)) {print(paste0(tname, " sensor 2 ", "variable ", varname, " might have a bad connection.")); bad_y2 = 1}
        if (sum(is.na(y3)) == length(y3)) {print(paste0(tname, " sensor 3 ", "variable ", varname, " might have a bad connection.")); bad_y3 = 1}
        ymax <- max(ceiling(c(y1, y2, y3)),na.rm = TRUE)
        ymin <- min(floor(c(y1, y2, y3)), na.rm = TRUE)
        plotTitle <- paste0(dlname, ", ", ylab)
        plotSubtitle <- paste0("start date, time: ", format(date_times[1], "%m-%d"), ", end date, time: ", format(date_times[length(date_times)], "%m-%d"), "\nmin val: ", ymin, ", max val: ", ymax)
        outf <- paste0("graphics/", tname, "_", varname, "_dayAve.png")
        # Generate x axis grid lines 
        grid_lines <- seq(min(t$TIMESTAMP_24hr), max(t$TIMESTAMP_24hr), by = "5 days")
        
        png(outf, width = 4.5, height = 4, units = "in", res = 600, pointsize = 10, bg = "white")
        plot(date_times, y1, type = "l", xaxt = "n", col = "red", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, ylab = ylab, xlab = "", ylim = ylim)
        #      lines(date_times, t$temp_ambient_C, type = "l", col = "darkgray", cex.sub = .4,lwd = 0.5)
        if (!bad_y2 == 1) lines(date_times, y2, type = "l", col = "green")
        if (!bad_y3 == 1) lines(date_times, y3, type = "l", col = "blue")
        if (varname == "T") lines(date_times, y_ambient_temp, type = "l", col = "gray")
        axis(1, at = grid_lines,
             labels = format(grid_lines, "%m-%d"), cex.axis = 0.7, lwd = 0.3, xpd = NA, las = 3)
        grid(nx = NA, ny = NULL, lwd = 0.3)
        abline(v=grid_lines, col = "lightgray", lty = "dotted", lwd = 0.3) # x axis grid
        if (west_strips == 1) { 
          abline(v = irrig_start_times_west, col = "brown", lwd = 0.5)
          abline(v = irrig_end_times_west, col = "darkgreen", lwd = 0.5)
          #       text(irrig_start_times_west, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
          galText_west <- paste0("Gal (000): \n", round(gal_applied_west/1000, 1))
          #         text(irrig_end_times_west, textY, galText_west, cex = 0.5, col = "darkgreen")
          
          tin_west <- round(irrig_start_times_west, units = "day")
          
          text(tin_west, pos = 4, textY, galText_west, cex = 0.3, col = "darkgreen") #offset = 1,
          #       text(irrig_end_times_west, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
        }
        if (west_strips == 0) {
          abline(v = irrig_start_times_east, col = "brown", lwd = 0.5)
          abline(v = irrig_end_times_east, col = "darkgreen", lwd = 0.5)
          #          text(irrig_start_times_east, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
          galText_east <- paste0("Gal (000): \n", round(gal_applied_east/1000, 1))
          #         text(irrig_end_times_east, textY, galText_east, cex = 0.5, col = "darkgreen")
          
          tin_east <- round(irrig_start_times_east, units = "day")
          
          text(tin_east, pos = 4, textY, galText_east, cex = 0.3, col = "darkgreen") #offset = 1,
          #         text(irrig_end_times_east, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
          
        }
        # legend("topleft", legend = c( paste0(varname, "_1 (6\")"), paste0(varname, "_2 (12\")"), paste0(varname, "_3 (18\")")), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
        legend("topleft",   legend = c('6"', '12"', '18"'), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
        if (varname == "T") {
          legend("topleft", legend = c('6"', '12"', '18"', "air"), text.font=3, cex = 0.7, fill = c("red", "green", "blue", "gray"))
        } else {
          legend("topleft", legend = c('6"', '12"', '18"'), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
          
        }
        dev.off()
      }
    }
  }
}

library(officer)
library(magrittr)
library(magick)
library(flextable)

files <- list.files("graphics")
files <- paste0("graphics/",files)
graphicsList_byvars <- character()
for (var in vars) {
  t <- paste0("graphics/", dlnames, "_Table1_", var, ".png")
  graphicsList_byvars <- c(graphicsList_byvars, t)
}
graphicsList_byvars <- graphicsList_byvars[graphicsList_byvars %in% files] #remove missing files

graphicsList_byloggers <- character()
for (dlname in dlnames) {
  t <- paste0("graphics/", dlname, "_Table1_", vars, ".png")
  graphicsList_byloggers <- c(graphicsList_byloggers, t)
}
graphicsList_byloggers <- graphicsList_byloggers[graphicsList_byloggers %in% files] #remove missing files

graphicsList <- graphicsList_byloggers

# Create a data frame with the file paths of your graphics
graphics <- data.frame(
  image_path = graphicsList,
  stringsAsFactors = FALSE
)


# Create a three-column flextable -----

sect_properties <- prop_section(
  page_size = page_size(
    orient = "portrait", width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar(
    bottom = .5, top = .5, right = .5, left = .5,
    header = 0.5,
    footer = 0.5,
    gutter = 0.5
  )
)

num_ncols <- 3
num_rows <- ceiling(length(graphicsList) / num_ncols)
dat <- data.frame(matrix(data = graphicsList, nrow = num_rows, ncol = num_ncols, byrow = T)) # dummy empty table
names(dat) <- c("VWC", "EC", "Temperature")

myft <- flextable(dat)
myft <- colformat_image(
  myft,
  i = c(1:num_rows),
  j = names(dat), width = 2.5, height = 2.5
)
ft <- autofit(myft)
ft

print(ft, preview = "docx", pr_section = sect_properties)

save_as_docx(ft, values = NULL, path = "graphics/loggerdata.docx", pr_section = sect_properties)
