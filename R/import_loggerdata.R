library(readr)
dlnames <- c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B", "S3T",  "S3B", "S3M", "S4T", "S4M",   "S4B") 
tableNums <- c("1", "2", "3")
tableNums <- c("1")
vars <- c("VWC", "EC", "T")

# for testing
dlname <- "S2T"
tableNum <- c("1")

startDate <- as.POSIXct("2023-05-23 14:00:00")

for (dlname in dlnames) {
  for (tableNum in tableNums) {
    tname <- paste0(dlname, "_Table", tableNum)
    
    #read in and clean up the table
    colnames <- c("TIMESTAMP","RECORD","VWC_1_Avg","EC_1_Avg","T_1_Avg","VWC_2_Avg","EC_2_Avg","T_2_Avg","VWC_3_Avg","EC_3_Avg","T_3_Avg")
    
    t <- readr::read_csv(paste0("data-raw/", tname, ".dat"), col_names = colnames, col_types = 
                           cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                VWC_1_Avg = col_number(), 
                                EC_1_Avg = col_number(), T_1_Avg = col_number(), 
                                VWC_2_Avg = col_number(), EC_2_Avg = col_number(), 
                                T_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                                EC_3_Avg = col_number(), T_3_Avg = col_number()), 
                         skip = 4)
    t$TIMESTAMP <-  as.POSIXct(t$TIMESTAMP,"America/Denver")
    #t <- t[-1,] 
    t <- t[, -2] # remove RECORD column
    t <- t[t$TIMESTAMP >= startDate, ]
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
      
      #plot the variables
      for (varname in vars) {
        if (varname == "EC") {ylab = "Electrical conductivity \n(deciSiemens per meter)"; ylim <- c(0, 1.0)}
        if (varname == "T") {ylab = "Soil temperature (°C)"; ylim <- c(15, 35)}
        if (varname == "VWC") {ylab = "Soil temperature (°C)"; ylim <- c(0, 50)}
        date_times <- t$TIMESTAMP
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
        #        print(head(t))
        #       print(paste0("bad ", bad))
        maxy <- max(ceiling(c(y1, y2, y3)),na.rm = TRUE)
        miny <- min(floor(c(y1, y2, y3)), na.rm = TRUE)
        plotTitle <- paste0(tname, ", ", ylab)
        plotSubtitle <- paste0("start date, time: ", date_times[1], ", end date, time: ", date_times[length(date_times)], "\nmin val: ", 
                               miny, ", max val: ", maxy)
        outf <- paste0("graphics/", tname, "_", varname, ".png")
        # Generate x axis grid lines every 6 hours
        grid_lines <- seq(min(t$TIMESTAMP), max(t$TIMESTAMP), by = "6 hours")
        
        png(outf,  width = 4, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
        plot(date_times, y1, type = "l", xaxt = "n", col = "red", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, ylab = ylab, xlab = "", ylim = ylim)
        if (!bad_y2 == 1) lines(date_times, y2, type = "l", col = "green")
        if (!bad_y3 == 1) lines(date_times, y3, type = "l", col = "blue")
        axis(1, at = grid_lines,
             labels = format(grid_lines, "%m-%d %H"), cex.axis=0.7, las = 2)
        grid(nx = NA, ny = NULL)
        abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
        legend("topleft", legend = c(paste0(varname, "_1_Avg"), paste0(varname, "_2_Avg"), paste0(varname, "_3_Avg")), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
        dev.off()
      }
    }
  }
}

# read in the status file
tname_status <- paste0(dlname, "_Status")
S1M_Status <- read_csv("data-raw/",tname_status, ".dat", skip = 1)

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

