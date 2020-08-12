#script to plot outputs from data created from NBR_rasterSummaries.r for FireCounts

library(readr)
library(ggplot2) 
library(dplyr)
library(wesanderson)
library(scales)

data_dir <- paste0(getwd(),"/Data/NBRanalysis/FireCounts/")  ## define the name of directory to save results

#create empty tibble to hold all data
count_tbl <- tibble(
  Year = numeric(),
  Count	= numeric(),
  Paramo = character(), 
  Window = character()
)


# #loop below used to create count_tbl (this written to file so can now ignore and read directly from file)
# #pset <- 1
# pset_list <- seq(from=1,to=144,by=1)
# for(pset in pset_list){
# 
#   d <- read_csv(paste0(data_dir,"FireCounts_Pset",pset,".csv"))
# 
#   d <- d %>%
#     select(class, value, Paramo, Window) %>%
#     rename(Year = class) %>%
#     rename(Count = value) %>%
#     mutate(Pset = pset)
# 
#   count_tbl <- bind_rows(count_tbl, d)
# 
# }
# 
# write_csv(count_tbl, paste0(data_dir,"FireCounts_AllPsets.csv"))

count_tbl <- read_csv(paste0(data_dir,"FireCounts_AllPsets.csv"))

count_tbl <- count_tbl %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(Pset_lab = formatC(Pset, width = 3, format = "d", flag = "0")) %>%
  mutate(Pset_lab = factor(Pset_lab))


#for loop across parameter sets (to split plots for legibility)
Pset_filter <- data.frame(rbind(c(0,48),c(48,96),c(96,144)))


pdf(paste0(data_dir, "FireCounts.pdf"), width=10, height=8)

#barplot by pset compare Window (all fires)
#get the data we want for this plot
plot_dat <- count_tbl %>%
  filter(Paramo == "All") %>%
  group_by(Window, Pset, Pset_lab) %>%
  summarise(Count = sum(Count))

maxcount <- ceiling(max(plot_dat$Count))
                        
#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]

  p <- plot_dat %>%
    filter(Pset > lower & Pset <= upper) %>%
    ggplot(aes(Pset_lab, Count, fill=Window)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("Fire Count") +
    xlab("Parameter Set") +
    ggtitle("Entire Region") +
    scale_y_continuous(labels = comma, limits=c(0,maxcount)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=wes_palette("Zissou1", 2, type = "discrete"))

  print(p)
} 



#barplot by pset compare Window (inside paramo)
plot_dat <- count_tbl %>%
  filter(Paramo == "Inside") %>%
  group_by(Window, Pset, Pset_lab) %>%
  summarise(Count = sum(Count))

maxcount <- ceiling(max(plot_dat$Count))

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  p <- plot_dat %>%
    filter(Pset > lower & Pset <= upper) %>%
    ggplot(aes(Pset_lab, Count, fill=Window)) +
    geom_bar(position="dodge", stat="identity") +
    ylab("Fire Count") +
    xlab("Parameter Set") +
    ggtitle("Inside Paramo") +
    scale_y_continuous(labels = comma, limits=c(0,maxcount)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=wes_palette("Zissou1", 2, type = "discrete"))

  print(p)
} 



#barplot by pset compare Window (outside paramo)
plot_dat <- count_tbl %>%
  filter(Paramo == "Outside") %>%
  group_by(Window, Pset, Pset_lab) %>%
  summarise(Count = sum(Count))

maxcount <- ceiling(max(plot_dat$Count))

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  p <- plot_dat %>%
    filter(Pset > lower & Pset <= upper) %>%
    ggplot(aes(Pset_lab, Count, fill=Window)) +
    geom_bar(position="dodge", stat="identity") +
    ylab("Fire Count") +
    xlab("Parameter Set") +
    ggtitle("Outside Paramo") +
    scale_y_continuous(labels = comma, limits=c(0,maxcount)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=wes_palette("Zissou1", 2, type = "discrete"))
  
  print(p)
}  
  


#barplot by pset compare Window (inside paramo) - facet by year
plot_dat <- count_tbl %>%
  filter(Paramo == "Inside") %>%
  group_by(Year, Window, Pset, Pset_lab, Paramo) %>%
  summarise(Count = sum(Count))

maxcount <- ceiling(max(plot_dat$Count))

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  p <- plot_dat %>%
    filter(Pset > lower & Pset <= upper) %>%
    ggplot(aes(Pset_lab, Count, fill=Window)) +
    geom_bar(position="dodge", stat="identity") +
    facet_grid(Year~.) +
    ylab("Fire Count") +
    xlab("Parameter Set") +
    ggtitle("Inside Paramo") +
    scale_y_continuous(labels = comma, limits=c(0,maxcount)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=wes_palette("Zissou1", 2, type = "discrete"))
  
  print(p)
  
}


#barplot by pset compare Window - facet by Paramo
plot_dat <- count_tbl %>%
  filter(Paramo != "All") %>%
  group_by(Window, Pset, Pset_lab, Paramo) %>%
  summarise(Count = sum(Count))

maxcount <- ceiling(max(plot_dat$Count))

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]

  p <- plot_dat %>%
    filter(Pset > lower & Pset <= upper) %>%
    ggplot(aes(Pset_lab, Count, fill=Window)) +
    geom_bar(position="dodge", stat="identity") +
    facet_grid(.~Paramo) +
    ylab("Fire Count") +
    xlab("Parameter Set") +
    scale_y_continuous(labels = comma, limits=c(0,maxcount)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=wes_palette("Zissou1", 2, type = "discrete"))
  
  print(p)
  
}

dev.off()
