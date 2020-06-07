  
#Script to generate summary counts of cloud-free pixels for Paramo, examining different windows of time
#Analysis is by Year, StartMonth and WindowLength
#Output is counts of pixels with 0,1,2,3,4,5,6+ cloud free images in a given window, for entire images and masked to study area
#Output summary data files (csv) is window-per-line (one for entire image, one for masked study area only)
#Output also png images for all window start-length combos, and output .tif files only for windows starting in January
  
library(raster)
library(tidyverse)
library(RColorBrewer)
library(sf)

#####
#FUNCTIONS

#function to create image filename from year and month values
imageFilename <- function(yr,mon){
  
  y = yr %% 2010
  m = month.abb[mon]    #month.abb is built-in 
  fn = paste0("Clear",m,y,".tif")
  #print(fn)
  return(fn)
  
}

#function to add data for this window to the summary data tibble 
appendClearData <- function(cDt, yr, ws, wl, counts, lf){
  
  #set count variables from the frequency table
  zeros <- ones <- twos <- threes <- fours <- fives <- sixplus <- 0
  zeros <- counts[1,2]
  if(lf > 1) ones <- counts[2,2]
  if(lf > 2) twos <- counts[3,2]
  if(lf > 3) threes <- counts[4,2]
  if(lf > 4) fours <- counts[5,2]
  if(lf > 5) fives <- counts[6,2]
  if(lf > 6) sixplus <- sum(counts[7:lf,2])
  
  #append data to tibble
  cDt <- cDt %>% add_row(tibble_row(
    Year = yr,
    WindowStart = ws,
    WindowLen  = wl, 
    Count0 = zeros,
    Count1 = ones,
    Count2 = twos,
    Count3 = threes,
    Count4 = fours,
    Count5 = fives,
    Count6p = sixplus),
    Max = lf-1)
  
  #return tibble (overwrite)
  return(cDt)
}

#function to write raster to png
rasPNG <- function(ras, yr, mons) {
  
  png(filename=paste0(path,"ClearImagesTotals_",yr,"_",month.abb[head(mons,1)],"-",month.abb[tail(mons,1)],".png"))
  
  #first plot without legend
  plot(ras, breaks=mycuts, col=mypal(length(mycuts)), legend=FALSE, main=paste0(yr," ",month.abb[head(mons,1)],"-",month.abb[tail(mons,1)]))
  legend("right", legend=as.character(mycuts),fill=mypal(length(mycuts)))   #add custom legend
  plot(st_geometry(buf), border="red",add=T) #add paramo buffer area
  
  dev.off()
}


#####
#INPUTS
Years <- seq(2019,2019,1)   #list of Years to analyse
StartMonth <- seq(1,1,1)  #list of StartMonths to analyse (1 is Jan, 12 is Dec)
path <- "Data/ClearImages/" #path to data directory

#structure for output summary data
clearData_image <- tibble(
  Year = numeric(),         #year this window is in
  WindowStart = numeric(),  #month in which the window starts
  WindowLen = numeric(),    #number of months in this window
  Count0 = numeric(),       #count of pixels with 0 cloud free images
  Count1 = numeric(),       #... 1 cloud free image
  Count2 = numeric(),       #... 2 cloud free images
  Count3 = numeric(),       #... 3 cloud free images
  Count4 = numeric(),       #... 4 cloud free images
  Count5 = numeric(),       #... 5 cloud free images
  Count6p = numeric(),      #count of pixels with 6 OR MORE cloud free images
  Max = numeric()           #max number of cloud free images in this window (should be equivalent to total number of images in the window)
)

clearData_mask <- clearData_image

#read mask data 
#GISpath <- "/home/james/OneDrive/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/" #linux
GISpath <- "E:/OneDrive - King's College London/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/"  #windows
  
buf <- st_read(paste0(GISpath,"500mbufferOfComplejos/GroundTruthingBuffer500m.shp"))
buf <- st_transform(buf, "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
buf_r <- raster(paste0(GISpath,"500mbufferOfComplejos/GroundTruthingBuffer500m_mask.tif"))

#plotting parms
mycuts <- c(0,1,2,3,4,5,6,100)
mypal <- colorRampPalette(c("white","black"))



#####
#ANALYSIS
for(i in Years){
  print(paste0("Year: ",i))  #loop on years of analysis
  
  for(j in StartMonth){
    print(paste0("Start Mon: ",month.abb[j]))  #loop on StartMonth of analysis
    
    max_wl = 13 - j   #max window length possible for this StartMonth (12 for Jan, 11 for Feb, ... 1 for Dec)
    Lengths <- seq(from=1,to=max_wl,by=1)   #create a list of possible window lengths for next loop
    
    for(k in Lengths){  #loop on possible Window Lengths for this StartMonth
      print(paste0("Window Length: ",k))
      
      
      ##MEMORY MANAGEMENT see https://r-forge.r-project.org/forum/forum.php?thread_id=30946&forum_id=995&group_id=302
      raster_tmp_dir <- "raster_tmp"  ## define the name of a temp directory where raster tmp files will be stored
      dir.create(raster_tmp_dir, showWarnings = F, recursive = T)  ## create the directory
      rasterOptions(tmpdir = raster_tmp_dir)  ## set raster options
      ##
      
      Months <- seq(from=j,length.out=k)  #create list of months for this StartMonth-WindowLength (j-k) combo 
      images <- stack()   #empty stack to hold rasters read in next loop
      
      #create the stack of rasters for this window length
      #loop through months in the StartMonth-WindowLength combo
      for(l in Months){
        ras <- raster(paste0(path,imageFilename(yr=i,mon=l)))  #read raster
        images <- stack(images, ras)  #add to stack
      }
      
      images_mask <- mask(images,buf_r) #apply mask to the stack
      
      freqs <- 0  #create dummy object for frequencies from images
      freqs_mask <- 0 #create dummy object for frequencies from masked images
      
      if(k > 1) { 
        totals <- sum(images)   #if more than one image in the window, calc sum of all images in the stack
        freqs <- freq(totals)   #then calc freq on the sum 
        
        totals_mask <- sum(images_mask)    #if more than one image in the window, calc sum of all images in the stack
        freqs_mask <- freq(totals_mask)    #then calc freq on the sum 
        
        #if StartMonth is Jan, output .tif of cell sums (write full image only, can mask this later if needed in subsequent analysis)
        if(j==1) writeRaster(totals, filename=paste0(path,"ClearImagesTotals_",i,"_",month.abb[head(Months,1)],"-",month.abb[tail(Months,1)],".tif"),datatype="INT2S")
        rasPNG(ras=totals,yr=i, mons=Months)    #always write png 
        
      
      } else {
        freqs <- freq(images,merge=T)             #else, only one image in the stack, calc freq on this using merge=T
        freqs_mask <- freq(images_mask,merge=T)   #else, only one image in the stack, calc freq on this using merge=T
        
        #if StartMonth is Jan, output raster of cell sums (write full image only, can mask this later if needed in subsequent analysis)
        if(j==1) writeRaster(images, filename=paste0(path,"ClearImagesTotals_",i,"_",month.abb[head(Months,1)],"-",month.abb[tail(Months,1)],".tif"),datatype="INT2S")
        rasPNG(ras=images,yr=i, mons=Months)      #always write png 
        
      }

      lenfreq <- length(freqs[,1])  #returns max count + 1
      lenfreq_mask <- length(freqs_mask[,1])  #returns max count + 1
      
      #add data for this window start-length combo to the summary data table
      clearData_image <- appendClearData(cDt=clearData_image,yr=i, ws=j, wl=k, counts=freqs,lf=lenfreq)
      clearData_mask <- appendClearData(cDt=clearData_mask,yr=i, ws=j, wl=k, counts=freqs_mask,lf=lenfreq_mask)

      ## remove the tmp dir
      unlink(raster_tmp_dir, recursive = T, force = T)
      
    }  #end [Window] Lengths loop
  }  #end StartMonth loop
}  #end Years loop

#output summary data tables
write_csv(x=clearData_image,
          path=paste0(path,"ClearImages_ImageSummary_",head(Years,1),"-",tail(Years,1),"_",month.abb[head(StartMonth,1)],"-",month.abb[tail(StartMonth,1)],".csv"))

write_csv(x=clearData_mask,
          path=paste0(path,"ClearImages_MaskSummary_",head(Years,1),"-",tail(Years,1),"_",month.abb[head(StartMonth,1)],"-",month.abb[tail(StartMonth,1)],".csv"))

