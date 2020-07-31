library(raster)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) 

#function to create raster of cellIDs and apply layer names
setNames <- function(myStack){
  names(myStack) <- c("Year", "Magnitude", "Duration", "PreValue", "SpecChg", "DSNR")
  return(myStack)
}

addCid <- function(myStack){
  cellID <- myStack[[1]]
  cellID[] <- seq(1:length(myStack[[1]]))
  myStack <- stack(myStack,cellID)
  return(myStack)
}

#function to set 0 to NA in raster
NAize <- function(x) { ifelse(x == 0, NA, x) }

myStack_NAize <- function(myStack){
  yr <- calc(myStack[[1]],NAize)
  myStack <- mask(myStack, yr)
  return(myStack)
}

tidyStack <- function(myStack, buf){
  
  crp <- extent(buf)
  myStack <- crop(myStack, crp)
  #myStack <- myStack_NAize(myStack)
  #myStack <- addCid(myStack)
    
  myStack  <- stack(myStack ,buf)
  names(myStack) <- c("Year", "Magnitude", "Duration", "PreValue", "SpecChg", "DSNR", "Paramo")
  
  return(myStack)
}


getPairs <- function(myStack, filterRaster, myWin){
  
  dat <- myStack[!is.na(filterRaster[])]
  tib <- as_tibble(dat)
  tib <- mutate(tib, Window=myWin)
}


#adapted from https://stackoverflow.com/a/49564100
quantileMaker <- function(data, calcCol, ...) {
  groupCol <- quos(...)
  calcCol <- enquo(calcCol)
  
  data %>%
    group_by(!!!groupCol) %>%
    summarise(min = min(Value), 
              q5 = quantile(Value, c(0.05), na.rm=T,names=F),
              q25 = quantile(Value, c(0.25), na.rm=T,names=F),
              med = median(Value),
              mean = mean(Value),
              q75 = quantile(Value, c(0.75), na.rm=T,names=F),
              q95 = quantile(Value, c(0.95), na.rm=T,names=F),
              max = max(Value),
              count = n())
}

#read mask data 
GISpath <- "Data/GIS/" #linux
#buf_r <- raster(paste0(GISpath,"GroundTruthingBuffer500m_mask.tif"))
#buf_r_trim <- trim(buf_r)
#plot(buf_r_trim)
#writeRaster(buf_r_trim, paste0(GISpath, "GroundTruthingBuffer500m_mask_trim.tif"))
buf_r_trim <- raster(paste0(GISpath,"GroundTruthingBuffer500m_mask_trim.tif"))


#plot(buf_r_trim)


scenario <- 1
scenario_list <- seq(from=1,to=2,by=1)

for(scenario in scenarios_list){

  raster_tmp_dir <- "/home/james/R/raster_tmp"  ## define the name of a temp directory where raster tmp files will be stored
  dir.create(raster_tmp_dir, showWarnings = F, recursive = T)  ## create the directory
  rasterOptions(tmpdir = raster_tmp_dir)  ## set raster options

  data_dir <- paste0(getwd(),"/Data/NBRanalysis/Pset",scenario)  ## define the name of directory to save results
  dir.create(data_dir, showWarnings = F, recursive=T)  ## create the directory
  
  
  start <- Sys.time()
  
  all_stack <- stack(paste0("Data/NBRanalysis/LTR_AllYear_",scenario,".tif"))
  
  #all_stack <- crop(all_stack, buf_extent)
  #yr <- calc(all_stack[[1]],NAize)
  
  #plot(yr, colNA = "red")
  
  #all_stack <- mask(all_stack, yr)
  
  all_stack <- tidyStack(all_stack,buf_r_trim)

  
  thr_stack <- stack(paste0("Data/NBRanalysis/LTR_JanApr_",scenario,".tif"))
  thr_stack <- tidyStack(thr_stack, buf_r_trim)
  
  end <- Sys.time()
  print(end - start)
  
  plot(all_stack, colNA = "red", main="All")
  #plot(thr_stack, colNA = "red", main="Third")


  #stack year layers for full vs third (four month) 
  
  #pairYears <- stack(FY1[["Year"]],TY1[["Year"]])
  #names(pairYears) <- c("Full", "Third")
  #plot(pairYears)
  
  #set true for any pixels that have identical years, otherwise false. 
  equalityChk0 <- function(x,y) { 
    ifelse(x > 2010 & y > 2010, 
           ifelse(x == y, 1, NA),NA)
  }
  pairBin <- overlay(all_stack[["Year"]],thr_stack[["Year"]], fun=equalityChk0)
  pBsum <- cellStats(pairBin, stat="sum") 
  
  #1 if a disurbance is present in both images (per pixel) in ANY year
  #disturbancePresent <- function(x,y) { 
  #  ifelse(!is.na(x) & y > !is.na(y), 1, NA)
  #}
  #pairDist <- overlay(all_stack[["Year"]],thr_stack[["Year"]], fun=disturbancePresent)
  #dPsum <- cellStats(pairDist, stat="sum") 
  
  
  start <- Sys.time()
  all_tib <- getPairs(all_stack, pairBin, "All")
  end <- Sys.time()
  print(end - start)
  
  
  start <- Sys.time()
  thr_tib <- getPairs(thr_stack, pairBin, "Third")
  end <- Sys.time()
  print(end - start)
  
  #consolidate
  AllDat <- bind_rows(all_tib, thr_tib)
  
  #set factors
  AllDat <- mutate(AllDat, 
                   Year = as_factor(Year), 
                   Window=as_factor(Window),
                   Paramo=ifelse(is.na(Paramo), 0, Paramo),
                   Paramo=as_factor(Paramo))

    
  #I need to do the summary analyses here as outputting the entire dataset is prohibitive (~250MB)
  #also do summary plots for this pset here

  AD_long <- AllDat %>%
    pivot_longer(cols= where(is.numeric),names_to="Variable", values_to="Value")
  

  start <- Sys.time()
  
  #Summarise across all groups
  YWP <- quantileMaker(data=AD_long, calcCol=Value, Year, Window, Paramo, Variable)
  write_csv(YWP, path=paste0(data_dir,"/YWP_Summary_Pset",scenario,".csv"))

  #Summarise across Window and Paramo
  WP <- quantileMaker(data=AD_long, calcCol=Value, Window, Paramo, Variable)
  write_csv(WP, path=paste0(data_dir,"/WP_Summary_Pset",scenario,".csv"))

  #Summarise across Year and Paramo
  YP <- quantileMaker(data=AD_long, calcCol=Value, Year, Paramo, Variable)
  write_csv(YP, path=paste0(data_dir,"/YP_Summary_Pset",scenario,".csv"))

  #Summarise across Year and Window
  YW <- quantileMaker(data=AD_long, calcCol=Value, Year, Window, Variable)
  write_csv(YW, path=paste0(data_dir,"/YW_Summary_Pset",scenario,".csv"))
  
  #Summarise across Window
  W <- quantileMaker(data=AD_long, calcCol=Value, Window, Variable)
  write_csv(W, path=paste0(data_dir,"/W_Summary_Pset",scenario,".csv"))
  
  #Summarise across Paramo
  P <- quantileMaker(data=AD_long, calcCol=Value, Paramo, Variable)
  write_csv(P, path=paste0(data_dir,"/P_Summary_Pset",scenario,".csv"))
  
  #Summarise across Year
  Y <- quantileMaker(data=AD_long, calcCol=Value, Year, Variable)
  write_csv(Y, path=paste0(data_dir,"/Y_Summary_Pset",scenario,".csv"))
  

  end <- Sys.time()
  print(end - start)


  #plots

  #ltrvar <- "PreValue"  

  start <- Sys.time()
  
  for(ltrvar in c("PreValue","Duration","Magnitude")){
               
    YP_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Year, y=Value, colour=Paramo)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/YP_Boxplot_",ltrvar,"_Pset",scenario,".png"),device="png")
    
    YW_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Year, y=Value, colour=Window)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/YW_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    
    PW_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Paramo, y=Value, colour=Window)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/PW_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    
    Y_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Year, y=Value)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/Y_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
  
    P_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Paramo, y=Value)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/P_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    
    W_box <- AD_long %>%
      filter(Variable == ltrvar) %>%
      ggplot(aes(x=Window, y=Value)) +
      geom_boxplot() +
      ggtitle(ltrvar)
    ggsave(paste0(data_dir,"/W_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
  }
  end <- Sys.time()
  print(end - start)
  

  print("unlink")
  ## remove the tmp dir
  unlink(raster_tmp_dir, recursive = T, force = T)
}  
  
  
  
  
  
  
  
#   
#   
# #Try using https://googledrive.tidyverse.org/ to read files
# library(googledrive)
# 
# #options(gargle_quiet = FALSE)
# #drive_auth(use_oob = TRUE)
# 
# drive_find(n_max = 30)
# 
# drive_download("~/Research/GroundTruthingBuffer500m_mask_trim.tif",
#                path = "Data/NBRanalysis/GroundTruthingBuffer500m_mask_trim.tif")
# 
# drive_download("~/Research/LTR_2020_Final/LTR_AllYear_8.tif",
#                path = "Data/NBRanalysis/LTR_AllYear_8.tif")
# 
# drive_find(pattern="LTR_2020_Final/LTR_AllYear_8.tif")$id
# 
# 
# 
# 
# #below with help from https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896/12
# #authentication "went stale" after a while and I had to restart R session. Try this fix: https://github.com/tidyverse/googledrive/issues/304
# 
# #Kris' shared folder URL
# folder_url <- "https://drive.google.com/drive/folders/1CmGmXPGjhwrSnVY0VhSPRmQ8u2xYwYHA"
# 
# folder <- drive_get(as_id(folder_url))
# 
# folder_ls <- drive_ls(folder)  #get list of all files in the shared folder
# 
# folder_ls <- drive_ls(drive_get(as_id(folder_url)))
# 
# #start scenario and file loop here
# 
# scenario_id <- 8
# window <- "AllYear"
# 
# file_name <- paste0("LTR_",window,"_",scenario_id,".tif")
# file_id <- folder_ls$id[folder_ls$name == file_name]  #get the id of the file we want (this can be pasted into a URL)
# 
# #use the id to download file from URL
# drive_download(paste0("https://drive.google.com/file/d/",file_id), 
#                path = paste0("Data/NBRanalysis/",file_name))
# 
# #delete file
# unlink(paste0("Data/NBRanalysis/",file_name))
# 
# 


