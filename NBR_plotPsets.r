#script to plot outputs from data created from NBR_collatePsets.r (via NBR_rasterSummaries.r)

library(readr)
library(ggplot2) 
library(dplyr)

data_dir <- paste0(getwd(),"/Data/NBRanalysis/AllPixels/")  ## define the name of directory to save results

ltrvar <- "PreValue"

W <- read_csv(paste0(data_dir,"W_Summary_all.csv"))
YW <- read_csv(paste0(data_dir,"YW_Summary_all.csv"))
WP <- read_csv(paste0(data_dir,"WP_Summary_all.csv"))

pdf(paste0(data_dir, ltrvar, "_Window.pdf"), width=10, height=10)
  
W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Window)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Window)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

W %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Window)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





YW %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

YW %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

YW %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


WP %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(.~Paramo) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

WP %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(.~Paramo) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

WP %>%
  filter(Variable == ltrvar) %>%
  mutate(Window = recode(Window, All = "All Year", Third = "Jan-Apr")) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Window)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(.~Paramo) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


dev.off()





P <- read_csv(paste0(data_dir,"P_Summary_all.csv"))
YP <- read_csv(paste0(data_dir,"YP_Summary_all.csv"))

pdf(paste0(data_dir, ltrvar, "_Paramo.pdf"), width=10, height=10)

P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Paramo)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Paramo)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

P %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, mean, colour=Paramo)) +
  geom_pointrange(aes(ymin=q25, ymax=q75),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (mean with 25th and 75th percentiles)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


YP %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset <= 48) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

YP %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 48 & Pset <= 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

YP  %>%
  filter(Variable == ltrvar) %>%
  mutate(Paramo = as.character(Paramo)) %>%
  mutate(Paramo = recode(Paramo, "0" = "Outside", "1" = "Inside")) %>%
  filter(Pset > 96) %>%
  mutate(Pset = factor(Pset)) %>%
  ggplot(aes(Pset, med, colour=Paramo)) +
  geom_pointrange(aes(ymin=q5, ymax=q95),
                  position = position_dodge(width = 0.75), size=0.33) +
  ylab(ltrvar) +
  ggtitle(paste0(ltrvar," (median with 5th and 95th percentiles)")) +
  facet_grid(Year~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



dev.off()
