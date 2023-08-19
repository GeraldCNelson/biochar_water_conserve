library(readr)
library(tidyverse)
dlnames <- c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B", "S3T", "S3M", "S3B", "S4T", "S4M", "S4B")  # 
tableNums <- c("1", "2", "3")
tableNum <- "1"
varname <- "VWC"

# measurements of the cube around each sensor, inches to get 2 cubic feet
t6 <- 6
t12 <- 3
t18 <- 3
b6 <- 3
b12 <- 3
b18 <- 6
x <- y <- 12
area <- x * y
vol6 <- area * (t6 + b6)
vol12 <- area * (t12 + b12)
vol18 <- area * (t18 + b18)

# for testing
dlname <- "S4T"
tableNum <- c("1")

startDate <- as.POSIXct("2023-05-23 14:00:00")
startDate <- as.POSIXct("2023-07-01 01:00:00")
endDate <- as.POSIXct("2023-08-11 24:00:00")
#endDate <- Sys.time()

irrig_start_times_west <- c(
  as.POSIXct("2023-05-23 15:25:00"), 
  as.POSIXct("2023-06-01 12:45:00"),
  as.POSIXct("2023-06-26 09:31:00"),
  as.POSIXct("2023-07-17 08:04:00"),
  as.POSIXct("2023-08-05 08:48:00")
)

irrig_end_times_west <- c(
  as.POSIXct("2023-05-23 19:20:00"), 
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
  as.POSIXct("2023-07-27 05:29:00"),
  as.POSIXct("2023-08-05 11:03:00")
)

gal_applied_west <- c(145000, 67300,  89400, 120900,  98500, NA,     NA,      NA)
gal_applied_east <- c(146600, 67500, 128200, 135700, 128500, 160900, 50400, 36600)

readData_WVC <- function(dlname, tableNum) {
  varname <- "VWC"
  if (dlname %in% c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B")) { 
    west_strips <- 1 
  } else {
    west_strips <- 0
  }
  tname <- paste0(dlname, "_Table", tableNum)
  #read in and clean up the table
  colnames <- c("TIMESTAMP","RECORD","VWC_1_Avg","EC_1_Avg","T_1_Avg","VWC_2_Avg","EC_2_Avg","T_2_Avg","VWC_3_Avg","EC_3_Avg","T_3_Avg")
  
  t <- readr::read_csv(paste0("data-raw/", tname, ".dat"), locale = locale(tz = "America/Denver"), col_names = colnames, col_types = 
                         cols(TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                              VWC_1_Avg = col_number(), 
                              EC_1_Avg = col_number(), T_1_Avg = col_number(), 
                              VWC_2_Avg = col_number(), EC_2_Avg = col_number(), 
                              T_2_Avg = col_number(), VWC_3_Avg = col_number(), 
                              EC_3_Avg = col_number(), T_3_Avg = col_number()), 
                       skip = 4, na = c("", "NA", "NAN"))
  t <- t[, -2] # remove RECORD column
  t <- t[t$TIMESTAMP >= startDate, ]
  date_times <- t$TIMESTAMP
  if (nrow(t) == 0) {
    stop(paste0(tname, " has no data after the start date."))
  } else {
    
    # reorder columns for easier eyeballing
    new_col_order <- c("TIMESTAMP", "VWC_1_Avg", "VWC_2_Avg", "VWC_3_Avg", "T_1_Avg",  "T_2_Avg",  "T_3_Avg", "EC_1_Avg", "EC_2_Avg", "EC_3_Avg")
    t <- t[, new_col_order]
    varsToKeep <- c("TIMESTAMP", paste0(varname, c("_1_Avg", "_2_Avg", "_3_Avg")))
    t <- t[, (varsToKeep)]
    t <- t[t$TIMESTAMP >= startDate, ]
    print(paste0("logger: ", dlname))
    print(head(t))
    
    # water 1 cubic inch = 0.004329 US liquid gallons
    wg <- 0.004329
    t <- t |> mutate(watervol_6 = wg * VWC_1_Avg * vol6) |>
      mutate(watervol_12 = wg * VWC_2_Avg * vol12) |> 
      mutate(watervol_18 = wg * VWC_3_Avg * vol18) # in gallons
    names(t)[-1] <- paste0(dlname, "_", names(t)[-1])
    return(t)
  }
}

for (dlname in dlnames) {
  vname <- paste0("VWC_", dlname)
  assign(vname, readData_WVC(dlname, tableNum = 1))
}

# water content plots
waterContent <- function(dlname) {  
  if (dlname %in% c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B")) { 
    west_strips <- 1 
  } else {
    west_strips <- 0
  }
  
  waterVol <- get(paste0("VWC_", dlname))
  waterVol <- waterVol[waterVol$TIMESTAMP >= startDate, ]
  date_times <- waterVol$TIMESTAMP
  
  ylab = "Gallons"
  y1 = eval(parse(text = (paste0("waterVol$", dlname, "_watervol", "_6"))))
  y2 = eval(parse(text = (paste0("waterVol$", dlname, "_watervol", "_12"))))
  y3 = eval(parse(text = (paste0("waterVol$", dlname, "_watervol", "_18"))))
  df_y <- data.frame(y1, y2, y3)
  df_y$ysum <- rowSums(df_y, na.rm = FALSE)
  maxy <- max(ceiling(df_y$ysum),na.rm = TRUE)
  miny <- min(floor(c(y1, y2, y3)), na.rm = TRUE)
  ylim <- c(0, 7)
  plotTitle <- paste0(gsub("_", " ", dlname), ", ", "Gallons in sq ft column")
  plotTitle <- paste0(gsub("Table", "Table ", plotTitle))
  plotSubtitle <- paste0("start date, time: ", format(date_times[1], "%m-%d %H:%M"), ", end date, time: ", format(date_times[length(date_times)], "%m-%d %H:%M"), "\nmin val: ", 
                         miny, ", max val: ", maxy)
  outf_wc <- paste0("graphics/", dlname, "_", "waterContent", ".png")
  # Generate x axis grid lines every 12 hours
  grid_lines <- seq(min(waterVol$TIMESTAMP), max(waterVol$TIMESTAMP), by = "24 hours")
  print(outf_wc)
  png(outf_wc,  width = 4.5, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
  plot(date_times, y1, type = "l", xaxt = "n", col = "red", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, ylab = ylab, xlab = "", ylim = ylim)
  lines(date_times, y2, type = "l", col = "green")
  lines(date_times, y3, type = "l", col = "blue")
  lines(date_times, df_y$ysum, type = "l", col = "black")
  axis(1, at = grid_lines,
       labels = format(grid_lines, "%m-%d %H"), cex.axis=0.7, xpd=NA, las = 3)
  grid(nx = NA, ny = NULL)
  abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
  textY <- 1
  if (west_strips == 1) { 
    abline(v = irrig_start_times_west, col = "brown")
    abline(v = irrig_end_times_west, col = "darkgreen")
    if (varname == "T") textY <- 15
    text(irrig_start_times_west, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
    galText_west <- paste0("Gal. applied: ", gal_applied_west, "\nend")
    text(irrig_end_times_west, textY, galText_west, cex = 0.5, col = "darkgreen")
    #       text(irrig_end_times_west, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
  }
  if (west_strips == 0) {
    abline(v = irrig_start_times_east, col = "brown")
    abline(v = irrig_end_times_east, col = "darkgreen")
    if (varname == "T") textY <- 15
    text(irrig_start_times_east, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "brown")
    galText_east <- paste0("Gal. applied: ", gal_applied_east, "\nend")
    text(irrig_end_times_east, textY, galText_east, cex = 0.5, col = "darkgreen")
    #         text(irrig_end_times_east, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
  }
  legend("topleft", legend = c(paste0("water content", " 6 in"), paste0("water content", " 12 in"), paste0("water content", " 18 in"), "Sum"), text.font=3, cex = 0.7, fill = c("red", "green", "blue", "black"))
  dev.off()
}

for (dlname in dlnames) {
  waterContent(dlname)
}


#compare biochar to nonbiochar strips; 1 and 3 have biochar; 2 and 4 don't. 
# 1 and 2 get the same amount of water (more or less)
# Same thing with 3 and 4.

striplocs <- c("T", "M", "B")

for (striploc in striplocs) {
  for (EW in c("east", "west")) {
    if (EW == "east") {
      print(paste0(striploc, " ", EW))
      merged_df_b <- merge(VWC_S3T, VWC_S3M, by = "TIMESTAMP", all = TRUE) |>
        merge(VWC_S3B, all = TRUE)
      merged_df_nb <- merge(VWC_S4T, VWC_S4M, by = "TIMESTAMP", all = TRUE) |>
        merge(VWC_S4B, by = "TIMESTAMP", all = TRUE)
      comb <- merge(merged_df_b, merged_df_nb, all = TRUE)
      comb <- comb |> 
        mutate(delta_gal_T_1 = S3T_watervol_6 - S4T_watervol_6) |>
        mutate(delta_gal_T_2 = S3T_watervol_12 - S4T_watervol_12) |>
        mutate(delta_gal_T_3 = S3T_watervol_18 - S4T_watervol_18) |>
        mutate(delta_gal_M_1 = S3M_watervol_6 - S4M_watervol_6) |>
        mutate(delta_gal_M_2 = S3M_watervol_12 - S4M_watervol_12) |>
        mutate(delta_gal_M_3 = S3M_watervol_18 - S4M_watervol_18) |>
        mutate(delta_gal_B_1 = S3B_watervol_6 - S4B_watervol_6) |>
        mutate(delta_gal_B_2 = S3B_watervol_12 - S4B_watervol_12) |>
        mutate(delta_gal_B_3 = S3B_watervol_18 - S4B_watervol_18)
      write.csv(comb, "data/combined_moisture_data_east.csv")
    }
    if (EW == "west") {
      print(paste0(striploc, " ", EW))
      merged_df_b <- merge(VWC_S1T, VWC_S1M, by = "TIMESTAMP", all = TRUE) |>
        merge(VWC_S1B, all = TRUE)
      merged_df_nb <- merge(VWC_S2T, VWC_S2M, by = "TIMESTAMP", all = TRUE) |>
        merge(VWC_S2B, all = TRUE)
      comb <- merge(merged_df_b, merged_df_nb, by = "TIMESTAMP", all = TRUE)
      comb <- comb |> 
        mutate(delta_gal_T_1 = S1T_watervol_6 - S2T_watervol_6) |>
        mutate(delta_gal_T_2 = S1T_watervol_12 - S2T_watervol_12) |>
        mutate(delta_gal_T_3 = S1T_watervol_18 - S2T_watervol_18) |>
        mutate(delta_gal_M_1 = S1M_watervol_6 - S2M_watervol_6) |>
        mutate(delta_gal_M_2 = S1M_watervol_12 - S2M_watervol_12) |>
        mutate(delta_gal_M_3 = S1M_watervol_18 - S2M_watervol_18) |>
        mutate(delta_gal_B_1 = S1B_watervol_6 - S2B_watervol_6) |>
        mutate(delta_gal_B_2 = S1B_watervol_12 - S2B_watervol_12) |>
        mutate(delta_gal_B_3 = S1B_watervol_18 - S2B_watervol_18)
      write.csv(comb, "data/combined_moisture_data_west.csv")
    }
  }
}

ploter <- function(comb) {
  ylab = "Gallons"#; ylim <- c(0, 50)
  date_times <- comb$TIMESTAMP
  loggerName <- substr(names(comb)[2], 1,3)
  EW <- "east"
  if (loggerName %in% c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B")) EW = "west"
  # maxy <- max(ceiling(c(comb$delta_gal_T_1, comb$delta_gal_T_2, comb$delta_gal_T_3)),na.rm = TRUE)
  # miny <- min(floor(c(comb$delta_gal_T_1, comb$delta_gal_T_1, comb$delta_gal_T_1)), na.rm = TRUE)
  
  rnge <- range(c(comb$delta_gal_T_1, comb$delta_gal_T_2, comb$delta_gal_T_3,
                  comb$delta_gal_M_1, comb$delta_gal_M_2, comb$delta_gal_M_3,
                  comb$delta_gal_B_1, comb$delta_gal_B_2, comb$delta_gal_B_3), na.rm = T)
  if (EW == "east") {
    
    #plot the variables
    plotTitle <- "Difference in water content from 2 cubic ft, S3B (biochar) minus S4B (no biochar) "
    plotSubtitle <- paste0("start date, time: ", format(date_times[1], "%m-%d %H:%M"), ", end date, time: ", format(date_times[length(date_times)], "%m-%d %H:%M"), "\nmin val: ", rnge[1], ", max val: ", rnge[2])
    grid_lines <- seq(min(date_times), max(date_times), by = "24 hours")
    plot(date_times, delta_gal_VWC_1, type = "l", xaxt = "n", main = plotTitle,  sub = plotSubtitle, cex.sub = .7, col = "red", ylab = ylab, xlab = "")
    lines(date_times, delta_gal_VWC_2, col = "green")
    lines(date_times, delta_gal_VWC_3, col = "blue")
    axis(1, at = grid_lines,
         labels = format(grid_lines, "%m-%d %H"), cex.axis=0.7, xpd=NA, las = 3)
    grid(nx = NA, ny = NULL)
    abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
    legend("topleft", legend = c("delta VWC 6 in", "delta VWC 12 in", "delta VWC in 18 in"), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
    
    outf <- paste0("graphics/", tname, "_", varname, ".png")
    # Generate x axis grid lines every 24 hours
    grid_lines <- seq(min(t$TIMESTAMP), max(t$TIMESTAMP), by = "24 hours")
    
    png(outf,  width = 4.5, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
    
    textY <- 1
    if (west_strips == 1) { 
      abline(v = irrig_start_times_west, col = "red")
      abline(v = irrig_end_times_west, col = "darkgreen")
      if (varname == "T") textY <- 15
      text(irrig_start_times_west, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "red")
      galText_west <- paste0("Gal. applied: ", gal_applied_west, "\nend")
      text(irrig_end_times_west, textY, galText_west, cex = 0.5, col = "darkgreen")
      #       text(irrig_end_times_west, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
    }
    if (west_strips == 0) {
      abline(v = irrig_start_times_east, col = "red")
      abline(v = irrig_end_times_east, col = "darkgreen")
      if (varname == "T") textY <- 15
      text(irrig_start_times_east, textY+3.0, "Irrig.\nStart", cex = 0.5, col = "red")
      galText_east <- paste0("Gal. applied: ", gal_applied_east, "\nend")
      text(irrig_end_times_east, textY, galText_east, cex = 0.5, col = "darkgreen")
      #         text(irrig_end_times_east, textY, notes_west, cex = 0.5, col = "darkgreen", pos = 3)
      
    }
    legend("topleft", legend = c(paste0(varname, "_1_Avg"), paste0(varname, "_2_Avg"), paste0(varname, "_3_Avg")), text.font=3, cex = 0.7, fill = c("red", "green", "blue"))
    dev.off()
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
  
  save_as_docx(ft, values = NULL, path = "graphics/watercontent.docx", pr_section = sect_properties)
  
  
  