{
  rm(list=ls())
  cat("\014")
  t0 <- Sys.time()
  source(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/lhmpi.R", sep = ""))
  options(scipen = 999)
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  data <- readxl::read_xls(path = "ASI Final.xls")
  data$year <- format(data$year, format = "%Y")
  class(data$year) <- "Numeric"
  data <- dplyr::arrange(data,sector, year)
  
  file <- c("Apparel", "Basic Metals", "Beverage", "Chemicals", "Electrical Equipments", "Electronics",
            "Fabricated Metal Products", "Food Products", "Furniture", "Leather", "Machinery", "Machinery Equipment Repair",
            "Non-metallic mineral Products", "Other Transport Equipment", "Others", "Paper Products", "Pharma",
            "Plastics", "Print Media", "Refined Petroleum Products", "Textiles", "Tobacco Products", "Vehicles", "Wood Products")
  variables <- c("year", "sector", "GrossValueAdded", "NumberofEmployees", "FixedCapital", "NumberofFactories")
  var.columns <- c("year", "sector", "GrossValueAdded", "NumberofEmployees", "FixedCapital", "NumberofFactories")
   
  t1 <- Sys.time();print(t1 - t0)
}

################################################################################
# Parameters
################################################################################

{
  t0 <- Sys.time()
  
  mi.rts <- 'crs'; lhm.rts <- 'crs'
  mi.cv <- 'convex'; lhm.cv <- 'convex'
  
  t1 <- Sys.time();print(t1 - t0)
}

################################################################################
# Industry level Analysis
################################################################################

{
  t0 <- Sys.time()
  
  data <- data[data$sector != "Machinery Equipment Repair",]
  a_data <- dplyr::arrange(data,sector, year)
  a_data <- data.frame(a_data)
  d <- dataframe.to.array(data = a_data, year_col = "year", id_col = "sector", x_col = c(3,5, 13), y_col = 8)
  xdata <- d$xdata
  ydata <- d$ydata
  dmu <- unique(a_data$sector)
  tm <- unique(a_data$year)
  
  pi1 <- lhmi(xdata = xdata, ydata = ydata, tm = tm, dmu = dmu, rts = lhm.rts, cv = lhm.cv)
  pi2 <- mpi(xdata = xdata,ydata = ydata, tm = tm, dm = 'dea', rts = mi.rts, orientation = 'i', cv = mi.cv)
  
  columns <- unique(pi2$mi$Period)
  lhm <- pi1$lhm
  lhm.tec <- pi1$tec
  lhm.tp <- pi1$tp
  lhm.sec <- pi1$sec
  lhm.o <- pi1$lhm.o
  lhm.i <- pi1$lhm.i
  mi <- reshape(pi2$mi, idvar = "DMU", timevar = "Period", direction = "wide")
  colnames(mi) <- c("DMU", columns)
  mi$DMU <- dmu
  mi.tec <- reshape(pi2$cu, idvar = "DMU", timevar = "Period", direction = "wide")
  colnames(mi.tec) <- c("DMU", columns)
  mi.tec$DMU <- dmu
  mi.fs <- reshape(pi2$fs, idvar = "DMU", timevar = "Period", direction = "wide")
  colnames(mi.fs) <- c("DMU", columns)
  mi.fs$DMU <- dmu
  mi.sec <- reshape(pi2$sec, idvar = "DMU", timevar = "Period", direction = "wide")
  colnames(mi.sec) <- c("DMU", columns)
  mi.sec$DMU <- dmu
  
  xlsx::write.xlsx(lhm, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-TFP", append = F, row.names = F)
  xlsx::write.xlsx(lhm.tec, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-TEC", append = T, row.names = F)
  xlsx::write.xlsx(lhm.tp, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-TP", append = T, row.names = F)
  xlsx::write.xlsx(lhm.sec, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-SEC", append = T, row.names = F)
  xlsx::write.xlsx(lhm.o, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-Output", append = T, row.names = F)
  xlsx::write.xlsx(lhm.i, file = "Aggregate Productivity Indicator.xlsx", sheetName = "LHM-Input", append = T, row.names = F)
  
  xlsx::write.xlsx(mi, file = "Aggregate Productivity Indicator.xlsx", sheetName = "MI-TFP", append = T, row.names = F)
  xlsx::write.xlsx(mi.tec, file = "Aggregate Productivity Indicator.xlsx", sheetName = "MI-TEC", append = T, row.names = F)
  xlsx::write.xlsx(mi.fs, file = "Aggregate Productivity Indicator.xlsx", sheetName = "MI-TP", append = T, row.names = F)
  xlsx::write.xlsx(mi.sec, file = "Aggregate Productivity Indicator.xlsx", sheetName = "MI-SEC", append = T, row.names = F)
  
  xlsx::write.xlsx(a_data, file = "Aggregate Final Data.xlsx", sheetName = "Data", append = F, row.names = F)
  
  t1 <- Sys.time();print(t1 - t0)
}

# Plot LHM
{
  lhm.economy <- colMeans(as.matrix(lhm[,2:dim(lhm)[2]]))
  plot(y = unlist(lhm.economy), x = 2001:2018, xlim = c(2001, 2018), type = 'b', main = "LHM Productivity indicator for Maufacturing Sector in India", 
       xlab = "Year", ylab = "LHM Productivity Indicator", frame.plot = F)
}

# Plot LHM (CDF)
{
  lhm.economy.cdf <- list(); total <- 0
  for(i in 1:length(lhm.economy)){total <- total + lhm.economy[i];lhm.economy.cdf[i] <- total}
  plot(y = unlist(lhm.economy.cdf), x = c(2001:2018), xlim = c(2001,2018), type = 'b', main = "Cumulative LHM Productivity indicator for Maufacturing Sector in India", 
       xlab = "Year", ylab = "LHM Productivity Indicator", frame.plot = F)
}

for(i in 1:length(lhm$DMU)){
  lhm$cdf <- 0
  lhm[i,'cdf'] <- sum(as.numeric(lhm[i,2:19]))
}

lhm.growth <- lr(lhm$`2000-2001` ~ lhm)
