library(readr)
dlnames <- c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B", "S3T",  "S3B", "S3M", "S4T", "S4M",   "S4B") 
tableNums <- c("1", "2", "3")
tableNums <- c("1")
vars <- c("VWC", "EC", "T")

# for testing
dlname <- "S4M"


startDate <- as.POSIXct("2023-05-15 16:00:00")

for (dlname in dlnames) {
  for (tableNum in tableNums) {
    tname <- paste0(dlname, "_Table", tableNum)
    
    #read in and clean up the table
    t <- readr::read_csv(paste0("data-raw/", tname, ".dat"), col_types = 
                           cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                VWC_1_Avg = col_number(), 
                                EC_1_Avg = col_number(), T_1_Avg = col_number(), 
                                VWC_2_Avg = col_number(), EC_2_Avg = col_number(), 
                                T_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                                EC_3_Avg = col_number(), T_3_Avg = col_number()), 
                         skip = 1)
    
    t <- t[-1,] 
    t <- t[, -2] # remove RECORD column
    t <- t[t$TIMESTAMP > startDate, ]
 #   t <- t[!is.na(t$VWC_1_Avg),] 
    if (nrow(t) == 0) {
      print(paste0(tname, " has no data after the start date."))
    } else {
      
      # reorder columns for easier eyeballing
      new_col_order <- c("TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg",  "T_2_Avg",  "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg")
      t <- t[, new_col_order]
      print(paste0("logger: ", dlname))
      print(head(t))
      # make sure it knows the location
  #    t$TIMESTAMP <-  as.POSIXct(t$TIMESTAMP,"America/Denver")

      #plot the variables
      for (varname in vars) {
        if (varname == "VWC") {ylab = "Soil moisture (%)"; ylim <- c(0, 100)}
        if (varname == "EC") {ylab = "Electrical conductivity \n(deciSiemens per meter)"; ylim <- c(0, 0.0005)}
        if (varname == "T") {ylab = "Soil temperature (°C)"; ylim <- c(15, 40)}
        date_times <- t$TIMESTAMP
        y1 = eval(parse(text = (paste0("t$", varname, "_1_Avg"))))
        y2 = eval(parse(text = (paste0("t$", varname, "_2_Avg"))))
        y3 = eval(parse(text = (paste0("t$", varname, "_3_Avg"))))
        
        # check for missing data from a sensor
        bad <- 0
        if (sum(is.na(y1)) == length(y1)) {print(paste0(tname, " sensor 1 ", "variable ", varname, " might have a bad connection.")); bad = 1}
        if (sum(is.na(y2)) == length(y2)) {print(paste0(tname, " sensor 2 ", "variable ", varname, " might have a bad connection.")); bad = 1}
        if (sum(is.na(y3)) == length(y3)) {print(paste0(tname, " sensor 3 ", "variable ", varname, " might have a bad connection.")); bad = 1}
        print(head(t))
 #       print(paste0("bad ", bad))
        if (bad == 0) {
          maxy <- max(ceiling(c(y1, y2, y3)))
          miny <- min(floor(c(y1, y2, y3)))
          plotTitle <- paste0(tname, ", ", ylab)
          plotSubtitle <- paste0("start date, time: ", date_times[1], ", end date, time: ", date_times[length(date_times)], "\nmin val: ", 
                                 miny, ", max val: ", maxy)
          outf <- paste0("graphics/", tname, "_", varname, ".png")
          png(outf,  width = 4, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
          plot(date_times, y1, type = "l", xaxt = "n", col = "red", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, ylab = ylab, xlab = "", ylim = ylim)
          lines(date_times, y2, type = "l", col = "green")
          lines(date_times, y3, type = "l", col = "blue")
          axis(1, at = date_times[seq(1, length(date_times), by = 12)],
               labels = format(date_times[seq(1, length(date_times), by = 12)], "%m-%d %H"), cex.axis=0.7, las = 2)
          legend("topleft", legend = c(paste0(varname, "_1_Avg"), paste0(varname, "_2_Avg"), paste0(varname, "_3_Avg")), fill = c("red", "green", "blue"))
          dev.off()
        }
      }
    }
  }
}

# read in the status file
tname_status <- paste0(dlname, "_Status")
S1M_Status <- read_csv("data-raw/",tname_status, ".dat", skip = 1)
