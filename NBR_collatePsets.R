#script to collate output from NBR_rasterSummaries for individual Psets into unified data files (subsequently used in NBR_plotPsets.r)

library(dplyr)
library(tibble)
library(tidyr)
library(readr)

data_dir <- paste0(getwd(),"/Data/NBRanalysis/AllPixels/")

#create empty tibbles to hold all data
#first, tibble with all common columns
stat_tbl <- tibble(
  min = numeric(),
  q5 = numeric(),
  q25 = numeric(),
  med = numeric(),
  mean = numeric(),
  q75 = numeric(),
  q95 = numeric(),
  max = numeric(),
  count = numeric())

#now indivdual tibble for each output file structure
Y_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Year = numeric(), .before="Variable") %>%
  add_column(Pset = numeric())

Y2_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Year = numeric(), .before="Variable") %>%
  add_column(Pset = numeric())

P_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Paramo = numeric(), .before="Variable") %>%
  add_column(Pset = numeric())

W_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Window = character(), .before="Variable") %>%
  add_column(Pset = numeric())

YW_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Window = character(), .before="Variable") %>%
  add_column(Year = numeric(), .before="Window") %>%
  add_column(Pset = numeric())

YP_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Paramo = numeric(), .before="Variable") %>%
  add_column(Year = numeric(), .before="Paramo") %>%
  add_column(Pset = numeric())

WP_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Paramo = numeric(), .before="Variable") %>%
  add_column(Window = character(), .before="Paramo") %>%
  add_column(Pset = numeric())

YWP_all <- stat_tbl %>%
  add_column(Variable = character(), .before="min") %>%
  add_column(Paramo = numeric(), .before="Variable") %>%
  add_column(Window = character(), .before="Paramo") %>%
  add_column(Year = numeric(), .before="Window") %>%
  add_column(Pset = numeric())


addData <- function(all, scen, scenID, dd){
  
  d <- read_csv(paste0(dd,"Pset",scenID,"/",scen,"_Summary_Pset",scenID,".csv"))
  d <- mutate(d, Pset = scenID)
  all <- bind_rows(all, d)
  
  return(all)
}


#scenario <- 1
scenario_list <- seq(from=1,to=144,by=1)
#scenario_list <- c(2,127)

for(scenario in scenario_list){
  
  ##data_dir <- paste0(getwd(),"/Data/NBRanalysis/AllPixels/Pset",scenario)  ## define the name of directory to save results
  
  Y_all <- addData(Y_all, "Y", scenario, data_dir)
  P_all <- addData(P_all, "P", scenario, data_dir)
  W_all <- addData(W_all, "W", scenario, data_dir)

  YW_all <- addData(YW_all, "YW", scenario, data_dir)
  YP_all <- addData(YP_all, "YP", scenario, data_dir)
  WP_all <- addData(WP_all, "WP", scenario, data_dir)
  
  YWP_all <- addData(YWP_all, "YWP", scenario, data_dir)

}

write_csv(Y_all, path=paste0(data_dir,"Y_Summary_all.csv"))
write_csv(P_all, path=paste0(data_dir,"P_Summary_all.csv"))
write_csv(W_all, path=paste0(data_dir,"W_Summary_all.csv"))

write_csv(YW_all, path=paste0(data_dir,"YW_Summary_all.csv"))
write_csv(YP_all, path=paste0(data_dir,"YP_Summary_all.csv"))
write_csv(WP_all, path=paste0(data_dir,"WP_Summary_all.csv"))

write_csv(YWP_all, path=paste0(data_dir,"YWP_Summary_all.csv"))
