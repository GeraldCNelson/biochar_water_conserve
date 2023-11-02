# look at status data

library(readr)
dlnames <- c("S1T", "S1M", "S1B", "S2T", "S2M", "S2B", "S3T",  "S3B", "S3M", "S4T", "S4M",   "S4B")  # 
tableNums <- c("1", "2", "3")
tableNums <- c("1")
vars <- c("VWC", "EC", "T")

# for testing
dlname <- "S2T"
tableNum <- c("1")

startDate <- as.POSIXct("2023-08-01")
endDate <- as.POSIXct("2023-10-01")
# read in the status file

colsToKeep <- c("CalOffset", "RfSignalLevel", "RfRxPakBusCnt", "BattVoltage")
colsToKeep <- c("BattVoltage")

singleValueCols <- c("OSversion",  "OSDate", "OSSignature", "ProgName", "ProgSig", "PakBusAddress", "CalOffset", "RfInstalled", "RfNetAddr", "RfAddress", "RfHopSeq", "RfPwrMode", "Rf_ForceOn", "Rf_Protocol" )

for (dlname in dlnames) {
  tname_status <- paste0(dlname, "_Status")
  st <- readr::read_csv(paste0("data-raw/",tname_status, ".dat"), skip = 1, show_col_types = FALSE)
  st <- st[-(1:2),]
  st <- st[st$TIMESTAMP >= startDate, ]
  st <- st[st$TIMESTAMP <= endDate, ]
  
  st$TIMESTAMP <- as.POSIXct(st$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S") #format = "%Y-%m-%d %H:%M:%S"
  plotTitle <- paste0("Battery voltage, ", dlname)
#  plotSubtitle <- paste0("OSversion: ", unique(st$OSversion), ", ProgName: ", unique(st$ProgName), ", PakBusAddress: ", unique(st$PakBusAddress), "\nRfInstalled: ", unique(st$RfInstalled), ", RfNetAddr: ", unique(st$RfNetAddr), ", RfAddress: ", unique(st$RfAddress), ",  RfHopSeq: ", unique(st$RfHopSeq), ", RfPwrMode: ", unique(st$RfPwrMode))
  
  grid_lines <- seq(min(st$TIMESTAMP), max(st$TIMESTAMP), by = "1 day")
  
  # battery voltage
  rnge <- c(11, 14)
  outf <- paste0("graphics/status/voltage_", dlname, ".png" )
  png(outf,  width = 4.5, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
  plot(st$TIMESTAMP, st$BattVoltage, type = "l", main =   plotTitle, ylim = rnge, ylab = "Voltage", xlab = "", xaxt = "n", sub = "", cex.sub = .7)
  axis(1, at = grid_lines,
       labels = format(grid_lines, "%m-%d %H"), cex.axis=0.6, xpd=NA, las = 3)
  grid(nx = NA, ny = NULL)
  abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
  dev.off()
}

#Rf signal level, combine values from all data loggers
rm(comb)
for (dlname in dlnames) {
  tname_status <- paste0(dlname, "_Status")
  st <- readr::read_csv(paste0("data-raw/",tname_status, ".dat"), skip = 1, show_col_types = FALSE)
  st <- st[-(1:2),]
  st <- st[st$TIMESTAMP >= startDate, ]
  
  st$TIMESTAMP <- as.POSIXct(st$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
  st$TIMESTAMP <- round.POSIXt(st$TIMESTAMP, unit = "days")
  st_sub <- subset(st, select = c(TIMESTAMP, RfSignalLevel))
  st_sub <- unique(st_sub)
  names(st_sub) <- c("TIMESTAMP", paste0(dlname,"_RfSignalLevel"))
#  print(head(st_sub$TIMESTAMP))
  if (!exists("comb")) {
    comb <- st_sub
  } else {
    comb <- merge(comb, st_sub, by = "TIMESTAMP", all = TRUE)
  }
}
comb <- unique(comb)

#single values, combine values from all data loggers -----
if (exists("comb")) rm(comb)
for (dlname in dlnames) {
  tname_status <- paste0(dlname, "_Status")
  st <- readr::read_csv(paste0("data-raw/",tname_status, ".dat"), skip = 1, show_col_types = FALSE)
  st <- st[-(1:2),]
  st <- st[st$TIMESTAMP >= startDate, ]
  st$TIMESTAMP <- as.POSIXct(st$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
  st$TIMESTAMP <- round.POSIXt(st$TIMESTAMP, unit = "days")
  st_sub <- st[c("TIMESTAMP", singleValueCols)]
  st_sub <- unique(st_sub)
  st_sub$loggerName <- dlname
#  names(st_sub) <- c("TIMESTAMP", paste0(dlname, "_", singleValueCols))
  #  print(head(st_sub$TIMESTAMP))
  if (!exists("comb")) {
    comb <- st_sub
  } else {
    comb <- merge(comb, st_sub, by = c("TIMESTAMP", "loggerName", singleValueCols), all = TRUE)
  }
}
comb <- unique(comb)

write.csv(comb, "data-raw/singleValueVariables.csv")

plotTitle <- paste0("RF signal level, ", dlname)
outf <- paste0("graphics/status/RF_signallevel_", dlname, ".png" )
png(outf,  width = 4.5, height = 4, units = "in", res = 150, pointsize = 10, bg = "white")
plot(st$TIMESTAMP, st$RfSignalLevel, type = "l", main = plotTitle, , ylab = "", xlab = "", xaxt = "n", cex.sub = .7) #, sub = plotSubtitle
axis(1, at = grid_lines,
     labels = format(grid_lines, "%m-%d %H"), cex.axis=0.7, xpd=NA, las = 3)
grid(nx = NA, ny = NULL)
abline(v=grid_lines, col="lightgray", lty="dotted", lwd=par("lwd")) # x axis grid
dev.off()


# get program used
for (dlname in dlnames) {
  tname_status <- paste0(dlname, "_Status")
  st <- readr::read_csv(paste0("data-raw/",tname_status, ".dat"), skip = 1, show_col_types = FALSE)
prog <- st$ProgName
prog_unique <- unique(prog)
print(paste0("logger and program name: ", dlname, ", ", prog_unique))
}
