#script to plot outputs from data created from NBR_collatePsets.r (via NBR_rasterSummaries.r)

library(readr)
library(ggplot2) 
library(dplyr)


ltrvar <- "Magnitude"
analysis <- "PairedByWindow"

data_dir <- paste0(getwd(),"/Data/NBRanalysis/",analysis,"/")  ## define the name of directory to save results


W <- read_csv(paste0(data_dir,"W_Summary_all.csv"))
YW <- read_csv(paste0(data_dir,"YW_Summary_all.csv"))
WP <- read_csv(paste0(data_dir,"WP_Summary_all.csv"))

pdf(paste0(data_dir, ltrvar, "_Window.pdf"), width=10, height=10)


#for loop across parameter sets (to split plots for legibility)
Pset_filter <- data.frame(rbind(c(0,48),c(48,96),c(96,144)))


W <- W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(mcount = count /1000000)




maxcount <- ceiling(max(W$mcount))

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]

  W_plotDat <- W %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
   
  #count plot
  p <- W_plotDat %>%
    ggplot(aes(Pset, mcount, fill=Window)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count (million)") +
    ylim(0,maxcount) +
    ggtitle(analysis) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(p)
}



minv <- min(W$q5)
maxv <- max(W$q95)
  
#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  W_plotDat <- W %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #median plot
  p <- W_plotDat %>%
    ggplot(aes(Pset, med, colour=Window)) +
    geom_pointrange(aes(ymin=q5, ymax=q95),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}


minv <- min(W$q25)
maxv <- max(W$q75)

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  W_plotDat <- W %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #mean plot
  p <- W_plotDat %>%
    ggplot(aes(Pset, mean, colour=Window)) +
    geom_pointrange(aes(ymin=q25, ymax=q75),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}


YW <- YW %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(mcount = count /1000000) 
  

maxcount <- ceiling(max(YW$mcount))

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  YW_plotDat <- YW %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #count plot
  p <- YW_plotDat %>%
    ggplot(aes(Pset, mcount, fill=Window)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count (million)") +
    ylim(0,maxcount) +
    ggtitle(analysis) +
    facet_grid(Year~.) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}




minv <- min(YW$q5)
maxv <- max(YW$q95)

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  YW_plotDat <- YW %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #median plot
  p <- YW_plotDat %>%
    ggplot(aes(Pset, med, colour=Window)) +
    geom_pointrange(aes(ymin=q5, ymax=q95),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
    facet_grid(Year~.) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}




WP <- WP %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  mutate(mcount = count /1000000)

  
maxcount <- ceiling(max(WP$mcount))  

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  WP_plotDat <- WP %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #count plot
  p <- WP_plotDat %>%
    ggplot(aes(Pset, mcount, fill=Window)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count (million)") +
    ylim(0,maxcount) +
    ggtitle(analysis) +
    facet_grid(.~Paramo) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}



minv <- min(WP$q5)
maxv <- max(WP$q95)

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  WP_plotDat <- WP %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #median plot
  p <- WP_plotDat %>%
    ggplot(aes(Pset, med, colour=Window)) +
    geom_pointrange(aes(ymin=q5, ymax=q95),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
    facet_grid(.~Paramo) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}

dev.off()





P <- read_csv(paste0(data_dir,"P_Summary_all.csv"))
YP <- read_csv(paste0(data_dir,"YP_Summary_all.csv"))

pdf(paste0(data_dir, ltrvar, "_Paramo.pdf"), width=10, height=10)



#for loop across parameter sets (to split plots for legibility)
Pset_filter <- data.frame(rbind(c(0,48),c(48,96),c(96,144)))


P <- P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  mutate(mcount = count /1000000)




maxcount <- ceiling(max(P$mcount))


#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  P_plotDat <- P %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #count plot
  p <- P_plotDat %>%
    ggplot(aes(Pset, mcount, fill=Paramo)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count (million)") +
    ylim(0,maxcount) +
    ggtitle(analysis) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(p)
}



minv <- min(P$q5)
maxv <- max(P$q95)

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  P_plotDat <- P %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #median plot
  p <- P_plotDat %>%
    ggplot(aes(Pset, med, colour=Paramo)) +
    geom_pointrange(aes(ymin=q5, ymax=q95),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}


minv <- min(P$q25)
maxv <- max(P$q75)

#loop over pset groups and plot
for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  P_plotDat <- P %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #mean plot
  p <- P_plotDat %>%
    ggplot(aes(Pset, mean, colour=Paramo)) +
    geom_pointrange(aes(ymin=q25, ymax=q75),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}


# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset <= 48) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 48 & Pset <= 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset <= 48) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, mean, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q25, ymax=q75),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 48 & Pset <= 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, mean, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q25, ymax=q75),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# P %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, mean, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q25, ymax=q75),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




YP <- YP %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  mutate(mcount = count /1000000) 


maxcount <- ceiling(max(YP$mcount))

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  YP_plotDat <- YP %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #count plot
  p <- YP_plotDat %>%
    ggplot(aes(Pset, mcount, fill=Paramo)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("count (million)") +
    ylim(0,maxcount) +
    ggtitle(analysis) +
    facet_grid(Year~.) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}




minv <- min(YP$q5)
maxv <- max(YP$q95)

for(row in 1:length(Pset_filter[,1])){
  
  lower <- Pset_filter[row,1]
  upper <- Pset_filter[row,2]
  
  YP_plotDat <- YP %>%
    filter(Pset > lower & Pset <= upper) %>%
    mutate(Pset = formatC(Pset, width = 3, format = "d", flag = "0")) %>%  #pad with preceeding 0s so that plots margins are identical
    mutate(Pset = factor(Pset))
  
  #median plot
  p <- YP_plotDat %>%
    ggplot(aes(Pset, med, colour=Paramo)) +
    geom_pointrange(aes(ymin=q5, ymax=q95),
                    position = position_dodge(width = 0.75), size=0.33) +
    ylab(ltrvar) +
    ylim(minv,maxv) +
    ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
    facet_grid(Year~.) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}





# 
# YP %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset <= 48) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   facet_grid(Year~.) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# YP %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 48 & Pset <= 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   facet_grid(Year~.) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# YP  %>%
#   filter(Variable == ltrvar) %>%
#   mutate(Paramo = as.character(Paramo)) %>%
#   mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
#   filter(Pset > 96) %>%
#   mutate(Pset = factor(Pset)) %>%
#   ggplot(aes(Pset, med, colour=Paramo)) +
#   geom_pointrange(aes(ymin=q5, ymax=q95),
#                   position = position_dodge(width = 0.75), size=0.33) +
#   ylab(ltrvar) +
#   ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
#   facet_grid(Year~.) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



dev.off()
