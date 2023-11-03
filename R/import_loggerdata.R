library(readr)
library(data.table)
startDate <- as.POSIXct("2023-05-23")
startDate <- as.POSIXct("2023-07-01")
endDate <- as.POSIXct("2023-08-11")
#endDate <- Sys.time()
source("R/coagDataImport.R") # get ambient variables from Colorado Ag Data. 5 minute data averaged to 15 min. Creates coagdata data.table

dlnames <- c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B", "S3T",  "S3M", "S3B", "S4T", "S4M", "S4B")  # 
tableNums <- c("1", "2", "3")
tableNums <- c("1")
vars <- c("VWC", "EC", "T")

# for testing
dlname <- "S4B"
tableNum <- c("1")

irrig_start_times_west <- c(
  as.POSIXct("2023-05-23 15:25:00"), 
  as.POSIXct("2023-06-01 12:16:00"),
  as.POSIXct("2023-06-26 09:31:00"),
  as.POSIXct("2023-07-17 08:04:00"),
  as.POSIXct("2023-08-05 08:48:00")
)

irrig_end_times_west <- c(
  as.POSIXct("2023-05-23 19:19:00"), 
  as.POSIXct("2023-06-01 18:45:00"),
  as.POSIXct("2023-06-26 16:02:00"),
  as.POSIXct("2023-07-17 16:04:00"),
  as.POSIXct("2023-08-05 14:56:00")
)

irrig_start_times_east <- c(
  as.POSIXct("2023-05-24 09:25:00"), 
  as.POSIXct("2023-06-02 03:43:00"),
  as.POSIXct("2023-06-14 08:25:00"),
  as.POSIXct("2023-06-27 07:53:00"),
  as.POSIXct("2023-07-07 07:46:00"),
  as.POSIXct("2023-07-17 19:19:00"),
  as.POSIXct("2023-07-27 10:00:00"),
  as.POSIXct("2023-08-05 08:51:00")
)

irrig_end_times_east <- c(
  as.POSIXct("2023-05-24 13:19:00"), 
  as.POSIXct("2023-06-02 22:12:00"),
  as.POSIXct("2023-06-14 17:29:00"),
  as.POSIXct("2023-06-27 16:14:00"),
  as.POSIXct("2023-07-07 15:46:00"),
  as.POSIXct("2023-07-17 17:53:00"),
  as.POSIXct("2023-07-27 17:29:00"),
  as.POSIXct("2023-08-05 11:03:00")
)

gal_applied_west <- c(145000, 67300,  89400, 120900,  98500, NA,     NA,      NA)
gal_applied_east <- c(146600, 67500, 128200, 135700, 128500, 160900, 50400, 36600)

notes_west <- c("Strip 1 never really ran off end of field /nand you could walk on hills. Strip 2 ran off end of field in about 1.5 hours \ninto watering and furrows and hills were completely flooded. /nSome water from strip 1 ran off onto Sri's field.", 
                "Started water at 11:16am noticed water stopped /nat 2:07pm for unknown amount of time. The canal valve was off so water was not flowing in system. \nTurned on again at 3:07 pm and finally /noff at 6:45 pm. Only a tiny bit of water at the very end ran onto sri's field, should not be an issue. All berms worked good.")

notes_east <- c("Strip 4 was losing water to ne corner of field /nand didn’t water about 2-3 furrows west from east side. Strips 3 & 4 were running onto strip 2 as well. Water movement was s-sw.", "Water was clean coming out of pipe. Field is good /nand saturated. There was no side movement of water, water ran down respective furrows so 1&2 were not influenced from this watering unlike the first watering.")

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
    date_times <- t$TIMESTAMP
    #   t <- t[!is.na(t$VWC_1_Avg),] 
    if (nrow(t) == 0) {
      stop(paste0(tname, " has no data after the start date."))
    } else {
      # add Colorado ag stats data for Fruita
      t <- merge(t, coagdata, by.x = "TIMESTAMP", by.y = "TIMESTAMP_15min")
      # reorder columns for easier eyeballing
      new_col_order <- c("TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg",  "T_2_Avg",  "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg", "temp_ambient_C")
      t <- t[, new_col_order]
      print(paste0("logger: ", dlname))
#      print(head(t))
      
      #plot the variables
      for (varname in vars) {
        print(paste0("working on variable ", varname))
        if (varname == "EC") {ylab = "Electrical conductivity \n(deciSiemens per meter) and air temp (°C)"; ylim <- c(0, 1.0)}
        if (varname == "T") {ylab = "Soil temperature (°C) and air temp (°C)"; ylim <- c(15, 35)}
        if (varname == "VWC") {ylab = "Volumetric Water Content (%) and air temp (°C)"; ylim <- c(0, 50)}
        y1 = eval(parse(text = (paste0("t$", varname, "_1_Avg"))))
        y2 = eval(parse(text = (paste0("t$", varname, "_2_Avg"))))
        y3 = eval(parse(text = (paste0("t$", varname, "_3_Avg"))))
        if (varname == "VWC") {
          y1 <- y1 *100; y2 <- y2 *100; y3 <- y3 *100
          ylab = "Soil moisture (%)"
        }
        
        # check for missing data from a sensor
        bad_y1 <- bad_y2 <- bad_y3 <- 0
        if (sum(is.na(y1)) == length(y1)) {print(paste0(tname, " sensor 1 ", "variable ", varname, " might have a bad connection.")); bad_y1 = 1}
        if (sum(is.na(y2)) == length(y2)) {print(paste0(tname, " sensor 2 ", "variable ", varname, " might have a bad connection.")); bad_y2 = 1}
        if (sum(is.na(y3)) == length(y3)) {print(paste0(tname, " sensor 3 ", "variable ", varname, " might have a bad connection.")); bad_y3 = 1}
        maxy <- max(ceiling(c(y1, y2, y3)),na.rm = TRUE)
        miny <- min(floor(c(y1, y2, y3)), na.rm = TRUE)
        plotTitle <- paste0(gsub("_", " ", tname), ", ", ylab)
        plotTitle <- paste0(gsub("Table", "Table ", plotTitle))
        plotSubtitle <- paste0("start date, time: ", format(date_times[1], "%m-%d %H:%M"), ", end date, time: ", format(date_times[length(date_times)], "%m-%d %H:%M"), "\nmin val: ", miny, ", max val: ", maxy)
        outf <- paste0("graphics/", tname, "_", varname, ".png")
        # Generate x axis grid lines every x hours
        grid_lines <- seq(min(t$TIMESTAMP), max(t$TIMESTAMP), by = "24 hours")
        
        png(outf,  width = 4.5, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
        plot(date_times, y1, type = "l", xaxt = "n", col = "red", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, ylab = ylab, xlab = "", ylim = ylim)
        lines(date_times, t$temp_ambient_C, type = "l", col = "darkgray", cex.sub = .4,lwd = 0.5)
        if (!bad_y2 == 1) lines(date_times, y2, type = "l", col = "green")
        if (!bad_y3 == 1) lines(date_times, y3, type = "l", col = "blue")
        axis(1, at = grid_lines,
             labels = format(grid_lines, "%m-%d"), cex.axis=0.7, xpd=NA, las = 3)
        grid(nx = NA, ny = NULL)
        abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
        textY <- 1
        if (west_strips == 1) { 
          abline(v = irrig_start_times_west, col = "brown")
          abline(v = irrig_end_times_west, col = "darkgreen")
          if (varname == "T") textY <- 15
          text(irrig_start_times_west, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
          galText_west <- paste0("Gal. applied: ", gal_applied_west) #, "\nend"
          text(irrig_end_times_west, textY, galText_west, cex = 0.5, col = "darkgreen")
          #       text(irrig_end_times_west, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
        }
        if (west_strips == 0) {
          abline(v = irrig_start_times_east, col = "brown")
          abline(v = irrig_end_times_east, col = "darkgreen")
          if (varname == "T") textY <- 15
          text(irrig_start_times_east, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
          galText_east <- paste0("Gal. applied: ", gal_applied_east) #, "\nend"
          text(irrig_end_times_east, textY, galText_east, cex = 0.5, col = "darkgreen")
          #         text(irrig_end_times_east, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
          
        }
        legend("topleft", legend = c("air temp (°C)", paste0(varname, "_1 (6\")"), paste0(varname, "_2 (12\")"), paste0(varname, "_3 (24\")")), text.font=3, cex = 0.7, fill = c("darkgray", "red", "green", "blue"))
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

