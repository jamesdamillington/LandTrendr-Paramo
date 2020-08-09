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
  return(tib)
}

getPixels <- function(myStack, myWin){
  
  print(paste0("getPixels ",myWin))
  tib <- as_tibble(myStack[])
  tib <- tib %>%
    mutate(Window=myWin)%>%
    filter(Year>2010)
  return(tib)
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

pairs <- F

#plot(buf_r_trim)


#scenario <- 1
scenario_list <- seq(from=1,to=144,by=1)
#scenario_list <- c(2,127)

for(scenario in scenario_list){

  print(scenario)
  
  start <- Sys.time()
  
  raster_tmp_dir <- "E:/raster_tmp"  ## define the name of a temp directory where raster tmp files will be stored
  dir.create(raster_tmp_dir, showWarnings = F, recursive = T)  ## create the directory
  rasterOptions(tmpdir = raster_tmp_dir)  ## set raster options

  #data_dir <- paste0(getwd(),"/Data/NBRanalysis/")
  data_dir <- paste0(getwd(),"/Data/NBRanalysis/AllPixels/Pset",scenario)  ## define the name of directory to save results
  dir.create(data_dir, showWarnings = F, recursive=T)  ## create the directory

  all_stack <- stack(paste0("Data/NBRanalysis/LTR_AllYear_",scenario,".tif"))
  
  #all_stack <- crop(all_stack, buf_extent)
  #yr <- calc(all_stack[[1]],NAize)
  
  #plot(yr, colNA = "red")
  
  #all_stack <- mask(all_stack, yr)
  
  all_stack <- tidyStack(all_stack,buf_r_trim)

  
  thr_stack <- stack(paste0("Data/NBRanalysis/LTR_JanApr_",scenario,".tif"))
  thr_stack <- tidyStack(thr_stack, buf_r_trim)
  

  
  #plot(all_stack, colNA = "red", main="All")
  #plot(thr_stack, colNA = "red", main="Third")


  #stack year layers for full vs third (four month) 
  
  if(pairs){
    
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
    
  
    all_tib <- getPairs(all_stack, pairBin, "All")
  
    thr_tib <- getPairs(thr_stack, pairBin, "Third")
  }

  if(!pairs){
    
    all_tib <- getPixels(all_stack, "All")
    
    thr_tib <- getPixels(thr_stack, "Third")
  }
    
  
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
  


  #plots

  #ltrvar <- "PreValue"  

  
  for(ltrvar in c("Magnitude","PreValue")){
    
    print(ltrvar)
              
    # YP_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Year, y=Value, colour=Paramo)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/YP_Boxplot_",ltrvar,"_Pset",scenario,".png"),device="png")
    # 
    # rm(YP_box)
    
    box_dat <- AD_long %>%
      filter(Variable == ltrvar)

    png(paste0(data_dir,"/YP_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    boxplot(Value~Year*Paramo, data=box_dat,
            col=c("lightgreen","darkgreen"),
            main=ltrvar,
            xaxt="n", 
            xlab="Year",
            frame.plot=TRUE,
            range=4)
    axis(1, at=c(1.5,5.5,9.5,13.5),labels=c("2012","2014","2016","2018"))
    legend("topright", inset=c(-0.3,0), legend=c("Outside","Inside"), fill=c("lightgreen","darkgreen"), title="Paramo")
    dev.off()

    
    # YW_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Year, y=Value, colour=Window)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/YW_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    # 
    # rm(YW_box)
    
 
    png(paste0(data_dir,"/YW_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    boxplot(Value~Year*Window, data=box_dat,
            col=c("lightgreen","darkgreen"),
            main=ltrvar,
            xaxt="n", 
            xlab="Year",
            frame.plot=TRUE,
            range=4)
    axis(1, at=c(1.5,5.5,9.5,13.5),labels=c("2012","2014","2016","2018"))
    legend("topright", inset=c(-0.3,0), legend=c("All","Third"), fill=c("lightgreen","darkgreen"), title="Window")
    dev.off()
    
    # PW_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Paramo, y=Value, colour=Window)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/PW_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    # 
    # rm(PW_box)
    
   png(paste0(data_dir,"/PW_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    boxplot(Value~Paramo*Window, data=box_dat,
            col=c("lightgreen","darkgreen"),
            main=ltrvar,
            xaxt="n", 
            xlab="Paramo",
            frame.plot=TRUE,
            range=4)
    axis(1, at=c(1.5,3.5),labels=c("Outside","Inside"))
    legend("topright", inset=c(-0.3,0), legend=c("All","Third"), fill=c("lightgreen","darkgreen"), title="Window")
    dev.off()
    
    
    # Y_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Year, y=Value)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/Y_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    # 
    # rm(Y_box)
    
    png(paste0(data_dir,"/Y_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    boxplot(Value~Year, data=box_dat,
            main=ltrvar,
            xlab="Year")
    dev.off()
    
    
     
    # P_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Paramo, y=Value)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/P_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    # 
    # rm(P_box)
    
    png(paste0(data_dir,"/P_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    boxplot(Value~Paramo, data=box_dat,
            main=ltrvar,
            xaxt="n", 
            xlab="Paramo",
            frame.plot=TRUE,
            range=4)
    axis(1,at=c(1,2),labels=c("Outside","Inside"))
    dev.off()
    
    # W_box <- AD_long %>%
    #   filter(Variable == ltrvar) %>%
    #   ggplot(aes(x=Window, y=Value)) +
    #   geom_boxplot() +
    #   ggtitle(ltrvar)
    # ggsave(paste0(data_dir,"/W_Boxplot_",ltrvar,"_Pset",scenario,".png"), device="png")
    # 
    # rm(W_box)
    
    png(paste0(data_dir,"/W_Boxplot_",ltrvar,"_Pset",scenario,"_range4.png"))
    boxplot(Value~Window, data=box_dat,
            main=ltrvar,
            xaxt="n", 
            xlab="Window",
            frame.plot=TRUE,
            range=4)
    axis(1,at=c(1,2),labels=c("All","Third"))
    dev.off()
    
  }

  

  #print("unlink")
  ## remove the tmp dir
  unlink(raster_tmp_dir, recursive = T, force = T)
  
  end <- Sys.time()
  print(end - start)
}  
  
  


