library(raster)

#function to create raster of cellIDs and apply layer names
cidNames <- function(myStack){
  cid <- myStack[[1]]
  cid[] <- seq(1:length(myStack[[1]]))
  myStack <- stack(myStack,cid)
  
  names(myStack) <- c("Year", "Magnitude", "Duration", "PreValue", "SpecChg", "DSNR","cellID")
  return(myStack)
}

#function to set 0 to NA in raster
NAize <- function(x) { ifelse(x == 0, NA, x) }

myStack_NAize <- function(myStack){
  myStack[["Year"]] <- calc(myStack[["Year"]],NAize)
  myStack[["Magnitude"]] <- calc(myStack[["Magnitude"]],NAize)
  myStack[["Duration"]] <- calc(myStack[["Duration"]],NAize)
  myStack[["PreValue"]] <- calc(myStack[["PreValue"]],NAize)
  return(myStack)
}


#read mask data 
#GISpath <- "/home/james/OneDrive/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/" #linux
#buf_r <- raster(paste0(GISpath,"500mbufferOfComplejos/GroundTruthingBuffer500m_mask.tif"))
#buf_r_trim <- trim(buf_r)
#plot(buf_r_trim)
#writeRaster(buf_r_trim, paste0(GISpath, "500mbufferOfComplejos/GroundTruthingBuffer500m_mask_trim.tif"))
#buf_r_trim <- raster(paste0(GISpath,"500mbufferOfComplejos/GroundTruthingBuffer500m_mask_trim.tif"))
#crp <- extent(buf_r_trim)

#crop for testing
#crp <- c(620000, 630000, 570000, 580000)

#read crop (for testing)
FY1 <- stack("Data/NBRanalysis/LTR_AllYear_1.tif")
#FY1 <- crop(FY1, crp)

#writeRaster(FY1, "Data/NBRanalysis/LTR_AllYear_1_crop.tif")
FY1 <- stack("Data/NBRanalysis/LTR_AllYear_1_crop.tif")

#cellID, names NAs
FY1 <- cidNames(FY1)
FY1 <- myStack_NAize(FY1)

plot(FY1)


#TY1 <- stack("Data/NBRanalysis/LTR_JanApr_1.tif")
#TY1 <- crop(TY1, crp)
#writeRaster(TY1, "Data/NBRanalysis/LTR_JanApr_1_crop.tif")

TY1 <- stack("Data/NBRanalysis/LTR_JanApr_1_crop.tif")

#cellID, names NAs
TY1 <- cidNames(TY1)
TY1 <- myStack_NAize(TY1)
plot(TY1)


#Try using https://googledrive.tidyverse.org/ to read files
library(googledrive)

#options(gargle_quiet = FALSE)
#drive_auth(use_oob = TRUE)

drive_find(n_max = 30)

drive_download("~/Research/GroundTruthingBuffer500m_mask_trim.tif",
               path = "Data/NBRanalysis/GroundTruthingBuffer500m_mask_trim.tif")

drive_download("~/Research/LTR_2020_Final/LTR_AllYear_8.tif",
               path = "Data/NBRanalysis/LTR_AllYear_8.tif")

drive_find(pattern="LTR_2020_Final/LTR_AllYear_8.tif")$id




#below with help from https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896/12
#authentication "went stale" after a while and I had to restart R session. Try this fix: https://github.com/tidyverse/googledrive/issues/304

#Kris' shared folder URL
folder_url <- "https://drive.google.com/drive/folders/1CmGmXPGjhwrSnVY0VhSPRmQ8u2xYwYHA"

folder <- drive_get(as_id(folder_url))

folder_ls <- drive_ls(folder)  #get list of all files in the shared folder

folder_ls <- drive_ls(drive_get(as_id(folder_url)))

#start scenario and file loop here

scenario_id <- 8
window <- "AllYear"

file_name <- paste0("LTR_",window,"_",scenario_id,".tif")
file_id <- folder_ls$id[folder_ls$name == file_name]  #get the id of the file we want (this can be pasted into a URL)

#use the id to download file from URL
drive_download(paste0("https://drive.google.com/file/d/",file_id), 
               path = paste0("Data/NBRanalysis/",file_name))

#delete file
unlink(paste0("Data/NBRanalysis/",file_name))




