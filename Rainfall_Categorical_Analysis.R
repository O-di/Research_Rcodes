setwd("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/PERSIANN-CCS")

require(tidyr)
library(dplyr)
library(ModelMetrics)
library(lubridate)

library(hydroGOF) #Stats for hydrology package

##########Handling Method###############################
# TAHMO <- read.csv(file = "results/TAHMO_sorted.csv")
# PCCS <- read.csv("results/PSN_sorted.csv")
# PDIR <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/PDIR_Now/PDIR_final.csv")
# GSMaP18 <- read.csv("D:/GSMap/NewDwnloads2018/GSMaP2018.csv")
# GSMaP19 <- read.csv("D:/GSMap/2019/GSMaP2019.csv")
# GSMaP20 <- read.csv("D:/GSMap/2020/GSMaP2020.csv")
# 
# # GSMaP <- gtools::smartbind(GSMaP18, GSMaP19, GSMaP20)
# # write.csv(GSMaP, "D:/GSMap/GSMaP_data.csv")
# 
# GSMaP <- read.csv("D:/GSMap/GSMaP_data.csv")
# 
# TAHMO.1 <- TAHMO[,-(1:2)]
# PDIR.1 <- PDIR[-(26305),-(1)] # to ensure 23hr of Dec. 31st, 2020 is included
# 
# TAHM_melt <- reshape::melt(TAHMO.1)
# PDIR_melt <- reshape::melt(PDIR.1)
# PCCS_melt <- reshape::melt(PCCS)
# GSMaP_melt <- reshape::melt(GSMaP)
# write.csv(PDIR_melt, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/PDIR_melt.csv")
# 
# 
# colnames(TAHM_melt) <- c("timestamp..UTC","variable" ,"TAHMO")
# colnames(PDIR_melt) <- c("timestamp..UTC","variable" ,"PDIR")
# colnames(PCCS_melt) <- c("timestamp..UTC","variable" ,"PCCS")
# colnames(GSMaP_melt) <- c("timestamp..UTC","variable" ,"GSMaP")
# 
# 
# PptData_Full <- merge(x = PCCS$timestamp..UTC, y = PDIR.1, all = T)#, sort = F)
# 
# write.csv(PptData_Full, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/PptData_Full.csv")
# 
# Data_Full <- cbind(TAHM_melt, PCCS_melt, GSMaP_melt, PDIR_melt)
# write.csv(Data_Full, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/Data_Full.csv")

#############################################
Full_Data <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/FullData_sorted.csv")

#### Add GPM data ########################
# GPM18M <- read.csv("GPM18Melt.csv")
# GPM19M <- read.csv("GPM19Melt.csv")
# GPM20M <- read.csv("GPM20Melt.csv")
# 
# GPM_melt <- rbind(GPM18M[,-1], GPM19M[,-1], GPM20M[,-1])
# colnames(GPM_melt) <- c("timestamp..UTC","variable" ,"GPM")

# write.csv(GPM_melt, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/GPM_melt_Sorted.csv")
GPM_dat <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/GPM_melt_Sorted.csv")
#Full_4SREs <- cbind(Full_Data, GPM_dat$GPM)

# Ctg_Anal <- Full_Data %>% 
#   mutate(Stat_PCCS = PCCS) %>% 
#   mutate(Stat_PCCS = replace(Stat_PCCS, TAHMO >= 0.02 & Stat_PCCS >= 0.02, "hit")) %>% 
#   mutate(Stat_PCCS = replace(Stat_PCCS, TAHMO >= 0.02 & Stat_PCCS < 0.02, "miss")) %>% 
#   mutate(Stat_PCCS = replace(Stat_PCCS, TAHMO < 0.02 & Stat_PCCS < 0.02, "CN")) %>% 
#   mutate(Stat_PCCS = replace(Stat_PCCS, TAHMO < 0.02 & Stat_PCCS >= 0.02, "FA")) %>% 
#   filter(Stat_PCCS == "FA" )
#   

#POD <- function(x,H = "hit", M = "miss"){H/ (H + M)}

####################################
Ctg_Anal_Fin <- Full_Data %>% 
  na_if(-99) %>% 
  mutate(PCCS_Stat = PCCS) %>% 
  mutate(PCCS_Stat = replace(PCCS_Stat, TAHMO >= 0.02 & PCCS >= 0.02, "hit")) %>% 
  mutate(PCCS_Stat = replace(PCCS_Stat, TAHMO >= 0.02 & PCCS < 0.02, "miss")) %>% 
  mutate(PCCS_Stat = replace(PCCS_Stat, TAHMO < 0.02 & PCCS < 0.02, "CN")) %>% 
  mutate(PCCS_Stat = replace(PCCS_Stat, TAHMO < 0.02 & PCCS >= 0.02, "FA")) %>% 
  
  mutate(PDIR_Stat = PDIR) %>% 
  mutate(PDIR_Stat = replace(PDIR_Stat, TAHMO >= 0.02 & PDIR >= 0.02, "hit")) %>% 
  mutate(PDIR_Stat = replace(PDIR_Stat, TAHMO >= 0.02 & PDIR < 0.02, "miss")) %>% 
  mutate(PDIR_Stat = replace(PDIR_Stat, TAHMO < 0.02 & PDIR < 0.02, "CN")) %>% 
  mutate(PDIR_Stat = replace(PDIR_Stat, TAHMO < 0.02 & PDIR >= 0.02, "FA")) %>% 
  
  mutate(GSMap_Stat = GSMaP) %>% 
  mutate(GSMap_Stat = replace(GSMap_Stat, TAHMO >= 0.02 & GSMaP >= 0.02, "hit")) %>% 
  mutate(GSMap_Stat = replace(GSMap_Stat, TAHMO >= 0.02 & GSMaP < 0.02, "miss")) %>% 
  mutate(GSMap_Stat = replace(GSMap_Stat, TAHMO < 0.02 & GSMaP < 0.02, "CN")) %>% 
  mutate(GSMap_Stat = replace(GSMap_Stat, TAHMO < 0.02 & GSMaP >= 0.02, "FA")) %>% 
  
  mutate(GPM_Stat = GPM) %>% 
  #mutate(GPM_Stat = replace(GPM_Stat, TAHMO == NA | GPM == NA, NA)) %>% 
  mutate(GPM_Stat = replace(GPM_Stat, TAHMO >= 0.02 & GPM >= 0.02, "hit")) %>% 
  mutate(GPM_Stat = replace(GPM_Stat, TAHMO >= 0.02 & GPM < 0.02, "miss")) %>% 
  mutate(GPM_Stat = replace(GPM_Stat, TAHMO < 0.02 & GPM < 0.02, "CN")) %>% 
  mutate(GPM_Stat = replace(GPM_Stat, TAHMO < 0.02 & GPM >= 0.02, "FA"))


# g <- Ctg_Anal %>% 
#   filter(PCCS_Stat == -99) #%>% 
#   na_if(-99)

################### For Polar Diagrams/ month for each stat ##############################

Full_Data <- read.csv("D:/Research_proper/OBJECTIVE 1/Downloaded_Data/FullData_sorted.csv")  

Full_Data <- Full_Data %>% 
  mutate(timestamp = as.character(timestamp..UTC)) %>% 
  mutate(timestamp = mdy_hm(timestamp..UTC)) %>%
  mutate(Dat_MONTH = month(timestamp, label = T))


polar_plot <- Full_Data %>%    #### Gave same result as RR above
  select(TAHMO, PCCS, Dat_MONTH) %>% 
  group_by(Dat_MONTH) %>% 
  summarise(cor = cor(TAHMO, PCCS, use = "pairwise.complete.obs"))

require(ggplot2)
plot <- ggplot2::ggplot(polar_plot, aes(Dat_MONTH, cor, fill = Dat_MONTH)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  scale_y_continuous(breaks = 0:nlevels(polar_plot$Dat_MONTH)) +
  theme_gray() #+
theme(axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank())

plot

plot + coord_polar() 
######################

RR_funct <- function(x = Full_Data[3], y){
  for (y in ncol(Full_Data[4:7])) {
    RR <- cor(x, y, use = "pairwise.complete.obs")
    RR
  }
}

Q <- apply(Full_Data[, 4:7], 2, RR_funct)
head(Q)

RR_PCCS <- gof(Full_Data[4], Full_Data[3])

Cor_coeff <- function(x, y){
  
}

#HydGOF_Cor2 = matrix(,nrow = 28, ncol = 20)

for (i in 1:ncol(TAHMO.1)) {
  
  XXX = data.frame(TAHMO.1[,i],PDIR[,i])
  XXX = XXX[complete.cases(XXX),]
  hit = XXX %>% 
    filter(XXX[,1] >= 0.02 & XXX[,2] >= 0.02)
  
  HydGOF_Cor2[i,] = gof(XXX[,1],XXX[,2])
  
}


#HydGOF_Cor2 = matrix(,nrow = 28, ncol = 20)

for (i in ncol(Full_Data[4:7])) {
  
  XXX = data.frame(Full_Data[,3],Full_Data[,i])
  XXX = XXX[complete.cases(XXX),]
  #hit = XXX %>% 
  #filter(XXX[,1] >= 0.02 & XXX[,2] >= 0.02)
  
  HydGOF_Cor2[i,] = gof(XXX[,1],XXX[,2])
  
}

for (i in ncol(Full_Data[4:7])) {
  
  # XXX = data.frame(Full_Data[,3],Full_Data[,i])
  # XXX = XXX[complete.cases(XXX),]
  #hit = XXX %>% 
  #filter(XXX[,1] >= 0.02 & XXX[,2] >= 0.02)
  
  HydGOF_Cor2[i,] = gof(Full_Data$TAHMO, Full_Data[,i], na.rm = T)
  
}

##################For individual columns - stats calc ################  
hits_NA = sum(Ctg_Anal_Fin$PCCS_Stat == "hit", na.rm = T)
miss_NA = sum(Ctg_Anal_Fin$PCCS_Stat == "miss", na.rm = T)
FA = sum(Ctg_Anal$PCCS_Stat == "FA", na.rm = T)
CN_NA = sum(Ctg_Anal_Fin$PCCS_Stat == "CN", na.rm = T)

POD_CCS = hits/(hits + miss)
FAR_CCS = FA/(hits + FA)



################ several columns stats calc ###########################
CAT_stats = function(x){
  hits = sum(x == "hit", na.rm = T)
  miss = sum(x == "miss", na.rm = T)
  FA = sum(x == "FA", na.rm = T)
  
  POD = hits/(hits + miss)
  FAR = FA/(hits + FA)
  CSI = hits/(hits + FA + miss)
  
  return(c(POD = POD, FAR = FAR, CSI = CSI))
}

Categ_stats <- apply(Ctg_Anal_Fin[, 8:11], 2, CAT_stats)
head(Q)

write.csv(Categ_stats, "D:/Research_proper/OBJECTIVE 1/Downloaded_Data/RESULTS/Categ_Analysis.csv")

#### Why we have several values of PCCS_stat that were not converted to hit nor miss, 
#is because TAHMO had NA values there
inspect <- (unique(Ctg_Anal$PCCS_Stat))

inspect_NA <- (unique(Ctg_Anal_Fin$PCCS_Stat)) #-99 has been changed to NA here

trial2 <- Ctg_Anal_Fin %>% 
  select(TAHMO, PCCS_Stat) %>% 
  filter(PCCS_Stat %in% inspect_NA[6:68])

# hit <- for(i in 1:nrow()){
#   TAHMO >= 0.02 & PSNCCS >= 0.02
#   mutate(hit = )
# }