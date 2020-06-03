  
#Script to generate summary counts of cloud-free pixels for Paramo, examining different windows of time
#Analysis is by Year, StartMonth and WindowLength
#Output is counts of pixels with 0,1,2,3,4,5,6+ cloud free images in a given window
#Output data file (csv) is window-per-line
  
library(raster)
library(tidyverse)

#function to create image filename from year and month values
imageFilename <- function(yr,mon){
  
  y = yr %% 2010
  m = month.abb[mon]    #month.abb is built-in 
  fn = paste0("Clear",m,y,".tif")
  #print(fn)
  return(fn)
  
}

#INPUTS
Years <- seq(2010,2010,1)   #list of Years to analyse
StartMonth <- seq(10,11,1)  #list of StartMonths to analyse (1 is Jan, 12 is Dec)
path <- "Data/ClearImages/" #path to data directory

#structure for output data
clearData <- tibble(
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

#start analysis loop
for(i in Years){
  print(paste0("Year: ",i))  #loop in years of analysis
  
  for(j in StartMonth){
    print(paste0("Start Mon: ",month.abb[j]))  #loop on StartMonth of analysis
    
    max_wl = 13 - j   #max window length possible for this StartMonth (12 for Jan, 11 for Feb, ... 1 for Dec)
    Lengths <- seq(from=1,to=max_wl,by=1)   #create a list of possible window lengths for next loop
    
    for(k in Lengths){
      print(paste0("Window Length: ",k))
      
      Months <- seq(from=j,length.out=k)  #create list of months for this StartMonth-WindowLength (j-k) combo 
      images <- stack()   #empty stack to hold rasters read in next loop
      
      #create the stack of rasters for this window length
      #loop through months in the StartMonth-WindowLength combo
      for(l in Months){
        ras <- raster(paste0(path,imageFilename(yr=i,mon=l)))  #read raster
        images <- stack(images, ras)  #add to stack
      }
      
      freqs <- 0  #create dummy object
      
      if(k > 1) { 
        totals <- sum(images)   #if more than one image in the window, calc sum of all images in the stack
        freqs <- freq(totals)   #then calc freq on the sum 
      } else {
        freqs <- freq(images,merge=T)   #if only one image in the stack, calc freq on this using merge=T
      }
      
      lenfreq <- length(freqs[,1])  #returns max count + 1
      
      #set count variables from the frequency table
      zeros <- ones <- twos <- threes <- fours <- fives <- sixplus <- 0
      zeros <- freqs[1,2]
      if(lenfreq > 1) ones <- freqs[2,2]
      if(lenfreq > 2) twos <- freqs[3,2]
      if(lenfreq > 3) threes <- freqs[4,2]
      if(lenfreq > 4) fours <- freqs[5,2]
      if(lenfreq > 5) fives <- freqs[6,2]
      if(lenfreq > 6) sixplus <- sum(freqs[7:lenfreq,2])
      
      #append data to tibble
      clearData <- clearData %>% add_row(tibble_row(
        Year = i,
        WindowStart = j,
        WindowLen  = k, 
        Count0 = zeros,
        Count1 = ones,
        Count2 = twos,
        Count3 = threes,
        Count4 = fours,
        Count5 = fives,
        Count6p = sixplus),
        Max = lenfreq-1)
      
    }  #end [Window] Lengths loop
  }  #end StartMonth loop
}  #end Years loop

write_csv(x=clearData,
          path=paste0(path,"ClearImagesSummary_",head(Years,1),"-",tail(Years,1),"_",month.abb[head(StartMonth,1)],"-",month.abb[tail(StartMonth,1)],".csv"))
