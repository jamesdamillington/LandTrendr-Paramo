#script to plot outputs from data created from NBR_rasterSummaries.r for FireCounts

library(readr)
library(ggplot2) 
library(dplyr)

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

#for loop across parameter sets (to split plots for legibility)
Pset_filter <- data.frame(rbind(c(0,48),c(48,96),c(96,144)))

#row<-1
#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  #first get psets for this plot (and make pset factor)
  subset_dat <- count_tbl %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset)) 
  
  #now get the data we want for this plot
  plot_dat <- subset_dat %>%
    mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
    filter(Paramo == "All") %>%
    group_by(Window, Pset) %>%
    summarise(Count = sum(Count))

  maxcount <- ceiling(max(plot_dat$Count))

  #barplot by pset compare Window (all fires)
  p <- plot_dat %>%
    ggplot(aes(Pset, Count, fill=Window)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count ") +
    ylim(0,maxcount) +
    ggtitle("Total Region") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  print(p)
  
  
  
  plot_dat <- subset_dat %>%
    mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
    #filter(Window == "All Year") %>%
    filter(Paramo != "All") %>%
    group_by(Window, Pset, Paramo) %>%
    summarise(Count = sum(Count))

  #barplot by pset compare Window (stack by inside vs outside)
  p <- plot_dat %>%
    ggplot(aes(Window, Count, fill=Paramo)) +
    geom_bar(position="stack", stat="identity") +
    ylab("count") +
    #facet_grid(.~Pset) +
    #ylim(0,maxcount) +
    #ggtitle("Total Region") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  print(p)
  
}

# all fires, in vs out (facet on Window)
  
  