setwd("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/")
# library(hydroGOF)
# library(hydroTSM)

library(ggplot2)
# library(xts)
# library(dygraphs)
# library(plotly)
library(hydroGOF)
library(dplyr)
library(tidyr)
library(lubridate)

options(stringsAsFactors = FALSE)

#####Handling Method to Get finally used data####
Full_Data <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/Full_finalData.csv")

#######Remove all the -99 from the Full_Data- change to NA##############
Hrly_Dat <- Full_Data %>% na_if(-99) #%>% 
#filter(GPM < 0) to test accuracy

FullDat_Zone <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/Full_finalDatZONE.csv")

#Implement correction of the 2 stations whose zones were changed
FullDat_Zone_crctd <- FullDat_Zone %>% 
  filter(variable == "TA00241") %>% 
  mutate(Zone = "Guinean-Savannah")

FullDat_Zone_Cmplt <- FullDat_Zone %>% 
  mutate(Zone=replace(Zone, variable=="TA00241", "Guinean-Savannah")) %>% 
  as.data.frame()


write.csv(FullDat_Zone_Cmplt, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/Full_finalDatZONE_crctd.csv")
#########################

FullDat_Zone <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/Full_finalDatZONE_crctd.csv")

HrlyAV_perZONE <- FullDat_Zone %>%    #### Gave same result as RR above
  select(timestamp..UTC : Zone) %>% 
  group_by(timestamp..UTC, Zone) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
#OLD result  #write.csv(HrlyAV_perZONE, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/HrlyAV_RnFl_perZONE.csv")
#write.csv(HrlyAV_perZONE, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/HrlyAV_RnFl_perZONE_crctd.csv")

#Get individual R values
Cor_perZone <- HrlyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSE_Hrly_perZone <- HrlyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse( . , TAHMO, use = "pairwise.complete.obs")))

MAE_Hrly_perZone <- HrlyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~mae( . , TAHMO, use = "pairwise.complete.obs")))

pBIAS_Hrly_perZone <- HrlyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~pbias( . , TAHMO, use = "pairwise.complete.obs")))

# ME_Hrly_perZone <- HrlyAV_perZONE %>% 
#   group_by(Zone) %>% 
#   summarise(across(TAHMO : GPM, ~me( . , TAHMO, use = "pairwise.complete.obs")))

#For full data####
HrlyAV_Volta <- FullDat_Zone %>%    #### Gave same result as RR above
  select(timestamp..UTC : Zone) %>% 
  group_by(timestamp..UTC) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

#Get individual R values
Cors_HrlyVolta <- HrlyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSEs_HrlyVolta <- HrlyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse(TAHMO, . , use = "pairwise.complete.obs")))

#######SCATTER PLOT ##############
##############Base R ##############
#Best in Hourly data for PERSIANN CCS
HrlyAV_SudSAH <- HrlyAV_perZONE %>% 
  #arrange() %>% 
  #select(variable, PCCS) %>% 
  filter(Zone == "Sudano-Sahelian")

#HrlyAV_SudSAH %>% select(timestamp..UTC, TAHMO, GPM) %>% 
cor(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, method = 'pearson', use = "pairwise.complete.obs")
plot(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, xlab= "RG Averaged Hourly Rainfall(mm)", 
     ylab="IMERG-E Averaged Hourly Rainfall(mm)", pch=19, col = "darkblue", cex = 0.9)
# Add fit lines
abline(lm(HrlyAV_SudSAH$TAHMO ~ HrlyAV_SudSAH$GPM), col="black") # regression line (y~x)
text(8, 20, cex = 0.7,
     "Stn: Av. of all stations\nR=0.36\nNo.Pt.:26304")



###GGPLOT2#################
library(ggplot2)
#ggplot(data = remove_missing(HrlyAV_SudSAH, na.rm = TRUE), aes(x=TAHMO, y=GPM, na.rm = TRUE)) +
#Remove outliers to plot PDIR data 
IMERG_plot_Hrly <- HrlyAV_perZONE %>% 
  mutate(TAHMO = replace(TAHMO, TAHMO > 40, NA))

IMERG <- ggplot(data = (IMERG_plot_Hrly), aes(x=TAHMO, y=GPM)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  #geom_text(aes(#label=text, 
  #angle=angle, 
  #hjust=hjust, 
  #vjust=vjust)) + 
  ylab("IMERG-E Averaged Hourly Rainfall(mm)") +
  xlab("RG Averaged Hourly Rainfall (mm)") +
  #labs(x = NULL) +
  #xlab("RG Averaged Hourly Rainfall(mm)")
  #facet_wrap(~Zone, scales = "free") + #came out portrait
  #facet_grid(Zone ~ ., scales = "free") + #plot didn't come out nice
  
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  scale_x_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) +
  scale_y_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white"))

#expand_limits(x = 45, y = 46)
IMERG
#geom_label(inherit.aes=FALSE, aes(x = 1.2, y = 32,label=paste("Stn: Av. of All Stations\n R=slope,","," ","intercept=",intercept,","," ","R^2=",R2,","," ","R^2.Adj=",R2.Adj)))

GSMaP <- ggplot(data = (HrlyAV_perZONE), aes(x=TAHMO, y=GSMaP)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("GSMaP_NRT Averaged Hourly Rainfall(mm)")+
  xlab("RG Averaged Hourly Rainfall (mm)") +
  #labs(x = NULL) +
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  #facet_grid(~Zone, space = "free", scales = "free_y") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white"))

#expand_limits(x = 45, y = 46) ##to avoid the clipping of points at edges,
#but it changed the facets from free scales to have the specified values
GSMaP

#####Plot for PCCS ####
#Remove outliers to plot PDIR data 
PCCS_plot_Hrly <- HrlyAV_perZONE %>% 
  #mutate(PDIR = na_if(PDIR, "55.411765"))
  mutate(PCCS = replace(PDIR, PDIR > 25, NA)) %>% 
  mutate(TAHMO = replace(TAHMO, TAHMO > 40, NA))

#To extend x and y limits (it does not work for reduction of limits)
PCCS_plot_Hrly <- data.table::data.table(PCCS_plot_Hrly)
PCCS_plot_Hrly[Zone == "Sudano-Sahelian", y_min := 0]
PCCS_plot_Hrly[Zone == "Sudano-Sahelian", y_max := 25]

PCCS_plot_Hrly[Zone == "Guinean-Savannah", x_min := 0]
PCCS_plot_Hrly[Zone == "Guinean-Savannah", x_max := 20]

PSNCCS <- ggplot(data = (PCCS_plot_Hrly), aes(x=TAHMO, y=PCCS)) +
  geom_point(colour = "darkblue") +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_abline(slope = 1, intercept = 0) +
  ylab("PERSIANN-CCS Averaged Hourly Rainfall(mm)") +
  #labs(x = NULL) +
  xlab("RG Averaged Hourly Rainfall(mm)") +
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white")) +
  geom_blank(aes(y = y_min, x = x_min)) +
  geom_blank(aes(y = y_max, x = x_max))
#expand_limits(x = 45, y = 46)
PSNCCS


#####Plot for PDIR ####
#Remove outliers to plot PDIR data 
PDIR_plot_Hrly <- HrlyAV_perZONE %>% 
  #mutate(PDIR = na_if(PDIR, "55.411765"))
  mutate(PDIR = replace(PDIR, PDIR > 36, NA)) %>% 
  mutate(TAHMO = replace(TAHMO, TAHMO > 40, NA))

#To re-arrage facet_wrap panels
PDIR_plot_Hrly$Zone_f = factor(PDIR_plot_Hrly$Zone, levels = c("Sudano-Sahelian", "Sudanian-Savannah", "Guinean-Savannah"))

#To extend x and y limits (it does not work for reduction of limits)
PDIR_plot_Hrly <- data.table::data.table(PDIR_plot_Hrly)
PDIR_plot_Hrly[Zone == "Sudano-Sahelian", y_min := 0]
PDIR_plot_Hrly[Zone == "Sudano-Sahelian", y_max := 25]

PDIR_plot_Hrly[Zone == "Guinean-Savannah", x_min := 0]
PDIR_plot_Hrly[Zone == "Guinean-Savannah", x_max := 20]





PDIR <- ggplot(data = (PDIR_plot_Hrly), aes(x=TAHMO, y=PDIR)) +
  geom_point(colour = "darkblue") +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_abline(slope = 1, intercept = 0) +
  ylab("PDIR_NOW Rainfall(mm)")+
  xlab("RG Averaged Hourly Rainfall(mm)")+
  #facet_wrap(~Zone, scales = "free") +
  #facet_wrap(~Zone_f, ncol = 1, scales = "free") +
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  coord_cartesian(clip = 'off')+
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) + 
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white")) +
  geom_blank(aes(y = y_min, x = x_min)) +
  geom_blank(aes(y = y_max, x = x_max))
PDIR

# library(gridExtra)
# grid.arrange(IMERG, GSMaP, PSNCCS, PDIR) #Code abruptly terminated R twice


#############DAILY COMPUTATIONS ###################
setwd("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/")
library(ggplot2)
library(hydroGOF)
library(dplyr)
library(tidyr)
library(lubridate)

options(stringsAsFactors = FALSE)

#####Handling Method for DailyAvPerZone#########

Daily_Data <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/DailyData.csv")

#####Remove all the -99 from the Full_Data- change to NA
Daily_Gd <- Daily_Data %>% na_if(-99) #%>% 

Sudano_Sahelian <- c("TA00164", "TA00163", "TA00161", "TA00160")
Sudanian_Savannah <- c("TA00170", "TA00251", "TA00249",  "TA00254",
                       "TA00165", "TA00264", "TA00467")
Guinean_Savannah <- c("TA00008", "TA00278", "TA00275", "TA00267", 
                      "TA00043", "TA00137", "TA00282", "TA00111", 
                      "TA00265", "TA00266", "TA00259", "TA00260", 
                      "TA00045", "TA00392", "TA00237", "TA00341", 
                      "TA00241")

Daily_Zone <- Daily_Gd %>% 
  na_if(-99) %>% 
  mutate(Zone = variable) %>% 
  mutate(Zone = as.character(Zone)) %>% 
  mutate(Zone = replace(Zone, Zone %in% Sudano_Sahelian, "Sudano-Sahelian")) %>% 
  mutate(Zone = replace(Zone, Zone %in% Sudanian_Savannah, "Sudanian-Savannah")) %>% 
  mutate(Zone = replace(Zone, Zone %in% Guinean_Savannah, "Guinean-Savannah"))  


DailyAV_perZONE <- Daily_Zone %>%    #### Gave same result as RR above
  #select(day : Zone) %>% 
  group_by(day, Zone) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
#OLD data  #write.csv(DailyAV_perZONE, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/DailyAV_RnFl_perZONE.csv")

write.csv(DailyAV_perZONE, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/DailyAV_RnFl_perZONE_crctd.csv")
############################

DailyAV_perZONE <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/DailyAV_RnFl_perZONE_crctd.csv")

#Get individual R values
Cor_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSE_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse( . , TAHMO, use = "pairwise.complete.obs")))

MAE_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~mae( . , TAHMO, use = "pairwise.complete.obs")))

pBIAS_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~pbias( . , TAHMO, use = "pairwise.complete.obs")))

# ME_Daily_perZone <- DailyAV_perZONE %>% 
#   group_by(Zone) %>% 
#   summarise(across(TAHMO : GPM, ~me( . , TAHMO, use = "pairwise.complete.obs")))


#For full data##########################
#############Table and stat values################
DailyAV_Volta <- Daily_Zone %>%    #### Gave same result as RR above
  #select(timestamp..UTC : Zone) %>% 
  group_by(day) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

#Get individual R values
Cors_DailyVolta <- DailyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSEs_DailyVolta <- DailyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse(TAHMO, . , use = "pairwise.complete.obs")))


#######SCATTER PLOT ##############
##############Base R ##############
#Best in Hourly data for PERSIANN CCS
HrlyAV_SudSAH <- HrlyAV_perZONE %>% 
  #arrange() %>% 
  #select(variable, PCCS) %>% 
  filter(Zone == "Sudano-Sahelian")

#HrlyAV_SudSAH %>% select(timestamp..UTC, TAHMO, GPM) %>% 
cor(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, method = 'pearson', use = "pairwise.complete.obs")
plot(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, xlab= "RG Averaged Hourly Rainfall(mm)", 
     ylab="IMERG-E Averaged Hourly Rainfall(mm)", pch=19, col = "darkblue", cex = 0.9)
# Add fit lines
abline(lm(HrlyAV_SudSAH$TAHMO ~ HrlyAV_SudSAH$GPM), col="black") # regression line (y~x)
text(8, 20, cex = 0.7,
     "Stn: Av. of all stations\nR=0.36\nNo.Pt.:26304")



###GGPLOT2#################
library(ggplot2)
#ggplot(data = remove_missing(HrlyAV_SudSAH, na.rm = TRUE), aes(x=TAHMO, y=GPM, na.rm = TRUE)) +
IMERG_plot_Daily <- DailyAV_perZONE %>% 
  mutate(TAHMO = replace(TAHMO, TAHMO > 90, NA))


IMERG <- ggplot(data = (IMERG_plot_Daily), aes(x=TAHMO, y=GPM)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("IMERG-E Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  # labs(x = NULL) +
  #xlab("RG Averaged Hourly Rainfall(mm)")
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  scale_x_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) +
  scale_y_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white"))
#expand_limits(x = 45, y = 46)
IMERG
#geom_label(inherit.aes=FALSE, aes(x = 1.2, y = 32,label=paste("Stn: Av. of All Stations\n R=slope,","," ","intercept=",intercept,","," ","R^2=",R2,","," ","R^2.Adj=",R2.Adj)))


#To extend x and y limits (it does not work for reduction of limits)
GSMaP_plot_Daily <- data.table::data.table(DailyAV_perZONE)
GSMaP_plot_Daily[Zone == "Sudano-Sahelian", y_min := 0]
GSMaP_plot_Daily[Zone == "Sudano-Sahelian", y_max := 25]

GSMaP_plot_Daily[Zone == "Guinean-Savannah", x_min := 0]
GSMaP_plot_Daily[Zone == "Guinean-Savannah", x_max := 40]


GSMaP <- ggplot(data = (GSMaP_plot_Daily), aes(x=TAHMO, y=GSMaP)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("GSMaP_NRT Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  #labs(x = NULL) +
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white")) +
  geom_blank(aes(x = x_min)) +
  geom_blank(aes(x = x_max))
#expand_limits(x = 45, y = 46) ##to avoid the clipping of points at edges,
#but it changed the facets from free scales to have the specified values
GSMaP


#To adjust x and y limits for PCCS
PCCS_plot_Daily <- DailyAV_perZONE %>% 
  mutate(PCCS = replace(PCCS, PCCS > 100, NA))

#To extend x and y limits (it does not work for reduction of limits)
PCCS_plot_Daily <- data.table::data.table(PCCS_crctd_Daily)
# PCCS_plot_Daily[Zone == "Sudano-Sahelian", y_min := 0]
# PCCS_plot_Daily[Zone == "Sudano-Sahelian", y_max := 25]

PCCS_plot_Daily[Zone == "Guinean-Savannah", x_min := 0]
PCCS_plot_Daily[Zone == "Guinean-Savannah", x_max := 50]

PCCS_plot_Daily[Zone == "Guinean-Savannah", y_min := 0]
PCCS_plot_Daily[Zone == "Guinean-Savannah", y_max := 100]

PCCS_plot_Daily[Zone == "Sudanian-Savannah", y_min := 0]
PCCS_plot_Daily[Zone == "Sudanian-Savannah", y_max := 100]

PCCS_plot_Daily[Zone == "Sudanian-Savannah", x_min := 0]
PCCS_plot_Daily[Zone == "Sudanian-Savannah", x_max := 50]



PSNCCS <- ggplot(data = (PCCS_plot_Daily), aes(x=TAHMO, y=PCCS)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("PERSIANN-CCS Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  #xlab("RG Averaged Hourly Rainfall(mm)")
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white")) +
  geom_blank(aes(x = x_min, y = y_min)) +
  geom_blank(aes(x = x_max, y = y_max))
#expand_limits(x = 45, y = 46)

PSNCCS




PDIR_plot_Daily <- DailyAV_perZONE %>% 
  mutate(TAHMO = replace(TAHMO, TAHMO > 90, NA)) %>% 
  mutate(PDIR = replace(PDIR, PDIR > 100, NA))

#To extend x and y limits (it does not work for reduction of limits)
PDIR_plot_Daily <- data.table::data.table(PDIR_plot_Daily)
PDIR_plot_Daily[Zone == "Guinean-Savannah", x_min := 0]
PDIR_plot_Daily[Zone == "Guinean-Savannah", x_max := 40]


PDIR <- ggplot(data = (PDIR_plot_Daily), aes(x=TAHMO, y=PDIR)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("PDIR_NOW Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  facet_wrap(~Zone, ncol = 1, scales = "free") +
  #facet_wrap(~Zone, scales = "free") +
  coord_cartesian(clip = 'off')+
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white")) +
  geom_blank(aes(x = x_min)) +
  geom_blank(aes(x = x_max))
PDIR

#To improve apprearance of PDIR data
#install.packages("facetscales")  #Installing didn't work
# devtools::install_github("zeehio/facetscales")
# 
# scales_y <- list(
#   `Guinean-Savannah` = scale_y_continuous(limits = c(5, 25), breaks = seq(5, 25, 5)),
#   `Sudanian-Savannah` = scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)),
#   `Sudano-Sahelian` = scale_y_continuous(limits = c(10, 20), breaks = seq(10, 20, 2))
# )

PDIR <- ggplot(data = (DailyAV_perZONE), aes(x=TAHMO, y=PDIR)) +
  geom_point(colour = "darkblue") +
  geom_abline(slope = 1, intercept = 0) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("PDIR_NOW Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  #facet_grid(~Zone, scales = "free") +
  facet_wrap(~Zone, scales = "free_x") +
  coord_cartesian(ylim = c(0, 60),  clip = 'off')+
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black",
                                        fill = "white"))
PDIR
# library(gridExtra)
# grid.arrange(IMERG, GSMaP, PSNCCS, PDIR) #Code abruptly terminated R twice


######################################MONTHLY Evaluation ###################
monthly_Perstatn <- Full_Data %>% 
  na_if(-99) %>% 
  mutate(timestamp..UTC = mdy_hm(as.character(timestamp..UTC))) %>% 
  mutate(MONTH = month(timestamp..UTC, label = TRUE)) %>% 
  mutate(YEAR = year(timestamp..UTC)) %>% 
  group_by(YEAR, MONTH, variable) %>% 
  summarise(across(.cols = 2:6, ~sum(. , na.rm = T)))#, total_precip = sum( na.rm = TRUE)))

Monthly_Zone <- monthly_Perstatn %>% 
  mutate(Zone = variable) %>% 
  mutate(Zone = as.character(Zone)) %>% 
  mutate(Zone = replace(Zone, Zone %in% Sudano_Sahelian, "Sudano-Sahelian")) %>% 
  mutate(Zone = replace(Zone, Zone %in% Sudanian_Savannah, "Sudanian-Savannah")) %>% 
  mutate(Zone = replace(Zone, Zone %in% Guinean_Savannah, "Guinean-Savannah"))  


###########Rain variation NORTH###################
Month_North <- Monthly_Zone %>% 
  filter(Zone != "Guinean-Savannah") 

Month_North_Av <- Monthly_Zone %>% 
  filter(Zone != "Guinean-Savannah") %>% 
  group_by(MONTH, YEAR) %>%
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

Month_North_Av_AllYrs <- Monthly_Zone %>% 
  filter(Zone != "Guinean-Savannah") %>% 
  group_by(MONTH) #%>%
summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

Monthly_NrthAV_MELT <- reshape::melt(as.data.frame(Month_North_Av_AllYrs))


# write.csv(Monthly_AV_AllMELT
#Plot line graph
ggplot(Monthly_AV_AllMELT, aes(x = as.factor(MONTH), y = value, group = variable, colour = variable)) +
  geom_line(size = 1)+
  theme(legend.title = element_blank())+
  geom_point() +
  xlab("Month") +
  ylab("Average Monthly Rainfall (mm/Month) - North")


##########SOUTH############## 
Month_South_Av_AllYrs <- Monthly_Zone %>% 
  filter(Zone == "Guinean-Savannah") %>% 
  group_by(MONTH, Zone) %>%
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

Monthly_SthAV_MELT <- reshape::melt(as.data.frame(Month_South_Av_AllYrs))

#Plot line graph
ggplot(Monthly_SthAV_MELT, aes(x = as.factor(MONTH), y = value, group = variable, colour = variable)) +
  geom_line(size = 1)+
  theme(legend.title = element_blank())+
  geom_point() +
  xlab("Month") +
  ylab("Average Monthly Rainfall(mm/Month) - South")



###################### ANNUAL #################
Yr_Ttl_pSttn <- Monthly_Zone %>% 
  group_by(YEAR, variable, Zone) %>%
  summarise(across(TAHMO : GPM, ~sum(., na.rm = T)))
write.csv(Yr_Ttl_pSttn, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/Yrly_ttl_perSttn.csv")

AnnualAv_pSttn <- Yr_Ttl_pSttn %>% 
  group_by(variable, Zone) %>%
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
write.csv(AnnualAv_pSttn, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/AnnualAv_perSttn.csv")

#Test
TahmoAv08 <- (Yr_Ttl_pSttn$TAHMO[1] + Yr_Ttl_pSttn$TAHMO[1+28] + Yr_Ttl_pSttn$TAHMO[1+56])/3

AnnualAv_pZone <- AnnualAv_pSttn %>% 
  group_by(Zone) %>%
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
write.csv(AnnualAv_pZone, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/AnnualAv_perZone.csv")

AnnualAv_overall <- AnnualAv_pZone %>% 
  #group_by(Zone) %>%
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
write.csv(AnnualAv_pZone, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/AnnualAv_perZone.csv")

# #AnnualpBias_pZone <- AnnualAv_pZone %>%  WRONG
#   group_by(Zone) %>% 
#   summarise(across(TAHMO : GPM, ~pbias(. , TAHMO)))

# AnnualpBias_overall <- AnnualAv_pZone %>% # WRONG
#   #group_by(Zone) %>%
#   summarise(across(TAHMO : GPM, ~pbias(. , TAHMO)))


########################################
MonthlyAV_perZONE <- Monthly_Zone %>%    #### Gave same result as RR above
  #select(day : Zone) %>% 
  group_by(MONTH, Zone) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))
#write.csv(DailyAV_perZONE, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/DailyAV_RnFl_perZONE.csv")

#Get individual R values
Cor_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSE_Daily_perZone <- DailyAV_perZONE %>% 
  group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse( . , TAHMO, use = "pairwise.complete.obs")))


#For full data##########################
#############Table and stat values################
DailyAV_Volta <- Daily_Zone %>%    #### Gave same result as RR above
  #select(timestamp..UTC : Zone) %>% 
  group_by(day) %>% 
  summarise(across(TAHMO : GPM, ~mean(., na.rm = T)))

#Get individual R values
Cors_DailyVolta <- DailyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~cor(TAHMO, . , use = "pairwise.complete.obs")))

RMSEs_DailyVolta <- DailyAV_Volta %>% 
  #group_by(Zone) %>% 
  summarise(across(TAHMO : GPM, ~rmse(TAHMO, . , use = "pairwise.complete.obs")))


#######SCATTER PLOT ##############
##############Base R ##############
#Best in Hourly data for PERSIANN CCS
HrlyAV_SudSAH <- HrlyAV_perZONE %>% 
  #arrange() %>% 
  #select(variable, PCCS) %>% 
  filter(Zone == "Sudano-Sahelian")

#HrlyAV_SudSAH %>% select(timestamp..UTC, TAHMO, GPM) %>% 
cor(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, method = 'pearson', use = "pairwise.complete.obs")
plot(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, xlab= "RG Averaged Hourly Rainfall(mm)", 
     ylab="IMERG-E Averaged Hourly Rainfall(mm)", pch=19, col = "darkblue", cex = 0.9)
# Add fit lines
abline(lm(HrlyAV_SudSAH$TAHMO ~ HrlyAV_SudSAH$GPM), col="black") # regression line (y~x)
text(8, 20, cex = 0.7,
     "Stn: Av. of all stations\nR=0.36\nNo.Pt.:26304")



###GGPLOT2#################
library(ggplot2)
#ggplot(data = remove_missing(HrlyAV_SudSAH, na.rm = TRUE), aes(x=TAHMO, y=GPM, na.rm = TRUE)) +
IMERG <- ggplot(data = (DailyAV_perZONE), aes(x=TAHMO, y=GPM)) +
  geom_point(colour = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("IMERG-E Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  # labs(x = NULL) +
  #xlab("RG Averaged Hourly Rainfall(mm)")
  facet_grid(~Zone, scales = "free") +
  scale_x_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) +
  scale_y_continuous(#breaks=c(0, 0.5, 1), 
    expand=c(0, 0)) 
#expand_limits(x = 45, y = 46)
IMERG
#geom_label(inherit.aes=FALSE, aes(x = 1.2, y = 32,label=paste("Stn: Av. of All Stations\n R=slope,","," ","intercept=",intercept,","," ","R^2=",R2,","," ","R^2.Adj=",R2.Adj)))

GSMaP <- ggplot(data = (DailyAV_perZONE), aes(x=TAHMO, y=GSMaP)) +
  geom_point(colour = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("GSMaP_NRT Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  #labs(x = NULL) +
  facet_grid(~Zone, scales = "free") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) 
#expand_limits(x = 45, y = 46) ##to avoid the clipping of points at edges,
#but it changed the facets from free scales to have the specified values
GSMaP

PSNCCS <- ggplot(data = (DailyAV_perZONE), aes(x=TAHMO, y=PCCS)) +
  geom_point(colour = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("PERSIANN-CCS Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  #xlab("RG Averaged Hourly Rainfall(mm)")
  facet_grid(~Zone, scales = "free") +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0))
#expand_limits(x = 45, y = 46)
PSNCCS

PDIR <- ggplot(data = (DailyAV_perZONE), aes(x=TAHMO, y=PDIR)) +
  geom_point(colour = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  ylab("PDIR_NOW Averaged Daily Rainfall(mm)")+
  xlab("RG Averaged Daily Rainfall(mm)")+
  facet_grid(~Zone, scales = "free") +
  #coord_cartesian(clip = 'off')+
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0))
PDIR

# library(gridExtra)
# grid.arrange(IMERG, GSMaP, PSNCCS, PDIR) #Code abruptly terminated R twice




##########################
sum(is.na(HrlyAV_SudSAH))
arr = HrlyAV_SudSAH[-which(is.na(HrlyAV_SudSAH)),]

cor(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, method = 'pearson', use = "pairwise.complete.obs")
plot(HrlyAV_SudSAH$TAHMO, HrlyAV_SudSAH$GPM, xlab= "RG Averaged Hourly Rainfall(mm)", ylab="IMERG-E Averaged Hourly Rainfall(mm)", pch=14, col = "blue")
# Add fit lines
abline(lm(HrlyAV_SudSAH$TAHMO ~ HrlyAV_SudSAH$GPM), col="black") # regression line (y~x)
text(20, 7, "Stn: Av. of all stations\nR=0.36\nNo.Pt.:26304")





# main="Station with Best Performance for Persiann CCS",
# 
# TAHMO_Hrly <- Hrly_Dat %>% 
#   arrange() %>% 
#   select(variable, TAHMO) %>% 
#   filter(variable == "TA00165")

