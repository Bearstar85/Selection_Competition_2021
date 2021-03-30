#Lets clear the objects
rm(list=ls())
getwd()
#and set the work directory to source file
setwd("~/Documents/GitHub/Andersson_etal_2021/06_MetalMonitoring")
dir()

#Load packages (not all essential)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(lubridate) # useful for working with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(dplyr)
library(broom)
library(ggpubr)
library(gridExtra)
library(rstatix)
library(tibble)

#There are 3 databases that we need to merge, trim stations from, and compute stats on

#Read in the data and basic QC####
ICES <- read.table("Data/ContaminantsInSeawater_CU.csv", header = TRUE, sep = ";", dec = ".")
head(ICES)
sapply(ICES, class)
ICES$STATN <- as.factor(as.character(ICES$STATN))

SLU <- read.table("Data/20200623_slu_CUaar.csv", header = TRUE, sep = ";", dec = ",")
head(SLU)
sapply(SLU, class)
SLU$Stationsnamn <- as.factor(as.character(SLU$Stationsnamn))

IVL <- read.table("Data/2020623_IVLallSweden.csv", header = TRUE, sep = ";", dec = ".")
head(IVL)
sapply(IVL, class)
IVL$Value <- as.character(as.factor(IVL$Value))
IVL$Value <- as.numeric(as.character(IVL$Value))
IVL$Provplats <- as.factor(as.character(IVL$Provplats))
IVL$Länskod <- as.factor(as.character(IVL$Länskod))
IVL$Enhet <- as.factor(as.character(IVL$Enhet))

#We have a total of 122 + 11 + 285 sites in all datasets
levels(ICES$STATN)
levels(SLU$Stationsnamn)
levels(IVL$Provplats)

#Trimming sites and samples####

###
#ICES
###

#So there are some differences based in MATRX (AF=after filtration, BF = before filtration, and WT = water(unknown filtration?))
ggplot(ICES, aes(ICES$STATN, log10(ICES$Value))) +
  geom_boxplot(mapping = aes(color = MATRX)) + #mapping = aes(color = Metal), size = 4
  scale_color_manual(values=c("Black", "Red", "Blue"))

#But most of the problem is that ul/kg data is extremely high and likley corresponds to sediment/biota or filtere product (which should not be in this database).
ggplot(ICES, aes(ICES$STATN, log10(ICES$Value))) +
  geom_boxplot(mapping = aes(color = MUNIT)) + #mapping = aes(color = Metal), size = 4
  scale_color_manual(values=c("Black", "Red", "Blue"))       

#In ICES the ul/kg is extremely high and likley corresponds to sediment/biota or filtere product
#I remove these datapoints aswell as empty rows
levels(ICES$MATRX)
ICESt <- subset.data.frame(ICES, grepl("ug/l", ICES$MUNIT))
ICESt <- ICESt %>% drop_na(Value)
#Lets also remove filtered samples
#ICESt <- subset.data.frame(ICESt, grepl("BF|WT", ICESt$MATRX))
dim(ICES)
dim(ICESt)

#The newly downloaded ICES file (2020-06-23) has the same number of observations as in 2018
#And the ug/kg data is still there
ICESall <- read.table("Data/ContaminantsInSeawater_ICES.csv", header = TRUE, sep = ";", dec = ".")
ICESCu <- subset.data.frame(ICESall, grepl("CU", ICESall$PARAM))
ICESCu <- subset.data.frame(ICESCu, grepl("ug/l", ICESCu$MUNIT))
ICESCu <- ICESCu %>% drop_na(Value)
dim(ICESt)
dim(ICESCu)

#Data looks more even now
ggplot(ICESt, aes(ICESt$STATN, (ICESt$Value))) +
  geom_boxplot(mapping = aes())  #mapping = aes(color = Metal), size = 4

#min, max and median value for Cu (uM)
min(ICESCu$Value)/63.5
median(ICESCu$Value)/63.5
max(ICESCu$Value)/63.5

#Lets make some observations on Cd as well

ICESCd <- subset.data.frame(ICESall, grepl("CD", ICESall$PARAM))
ICESCd <- subset.data.frame(ICESCd, grepl("ug/l", ICESCd$MUNIT))
ICESCd <- ICESCd %>% drop_na(Value)

ggplot(ICESCd, aes(ICESCd$STATN, (ICESCd$Value))) +
  geom_boxplot(mapping = aes())  #mapping = aes(color = Metal), size = 4

#min, max and median value for Cd (uM) are all very low (1000x lower than EC50s)
min(ICESCd$Value)/112.8
median(ICESCd$Value)/112.8
max(ICESCd$Value)/112.8

class(ICESCd$Value)
sapply(ICESCd, class)

###
#IVL
###

#First we need to confine data to sites around the Baltic coast of Southern Sweden
# Gotland (I), Blekinge (K) Skane (M), Kalmar(H), Sormland (D) och Stockholm (AB) 
levels(IVL$Länskod)

S <- c("I", "K", "M", "H", "D", "AB")
IVLt <- IVL[IVL$Länskod %in% S,]

#There are some different units in IVL dataset, lets standardize
levels(IVLt$Enhet)

IVLmg <- IVLt[IVLt$Enhet %in% "mg/l",]
IVLng <- IVLt[IVLt$Enhet %in% "ng/l",]
IVLug <- IVLt[IVLt$Enhet %in% "µg/l",]


IVLmg$Value <- IVLmg$Value*1000
IVLmg$Enhet <- recode_factor(IVLmg$Enhet, "mg/l" = "µg/l")
IVLng$Value <- IVLng$Value/1000
IVLng$Enhet <- recode_factor(IVLng$Enhet, "ng/l" = "µg/l")

IVLt2 <- rbind(IVLmg, IVLng, IVLug)

ggplot(IVLt2, aes(IVLt2$Provplats, (IVLt$Value))) +
  geom_boxplot(mapping = aes())  #mapping = aes(color = Metal), size = 4

###
#SLU
###

#This data was constraint based on creating an area that covered the eastern baltic coast of Sweden below Stockholm (in the URL, see READme file).
#Data looks good as it is
ggplot(SLU, aes(SLU$Stationsnamn, (SLU$Value))) +
  geom_boxplot(mapping = aes())  #mapping = aes(color = Metal), size = 4

#Transform units####

#Change ug/l to uM
ICESt$Value <- ICESt$Value/63.5
ICESt$MUNIT <- recode_factor(ICESt$MUNIT, "ug/l" = "uM")

SLU$Value <- SLU$Value/63.5
SLU$Unit <- recode_factor(SLU$Unit, "µg/l" = "uM")

IVLt2$Value <- IVLt2$Value/63.5
IVLt2$Enhet <- recode_factor(IVLt2$Enhet, "µg/l" = "uM")


#Trimming of irrelevant sites and observations####
levels(IVLt$Provplats)

#Lets remove non-seawater or rivermouth sites from IVL
#Flodmynningar only in SLU: SLU <- SLU[SLU$Projekt %in%  "Flodmynningar",]
levels(SLU$Stationsnamn)
J <- c("Botorpström Brunnsö", "Emån Emsfors", "Ljungbyån Ljungbyholm",
       "Nyköpingsån Spånga", "Storåns Utl", "Strömsrum", "Stora Binga")
SLUt <- SLU[SLU$Stationsnamn %in% J,]

#Rivermouths: Include: "Storåns Utl" Remove: "Alsterån Getebro"
#2 km from coast: 
#Include: "Storåns Utl", "Strömsrum", "Stora Binga" 
#Remove: "Alsterån Getebro" 

#The metadata is very incomplete for IVL so I did a manual selection

#Rivermouths: Include: Remove: "Hagaviken Stockholm"
#2 km from coast: Include: "Vesan", "Åbyån" Remove: "Hagaviken Stockholm"

I <- c("Edeboviken 1 Hallstavik", "Edeboviken 2 Hallstavik", "Edeboviken 3 Hallstavik",
       "Edeboviken 4 Hallstavik", "Emån utlopp, Ems herrgård", "Gothemsån utlopp Y07",
       "Hanveden", "Linnéaholm", "Öresund Malmö hamn", "Visby ARV", "Vesan", "Åbyån")
IVLt2 <- IVLt2[IVLt2$Provplats %in% I,]

#For the Map figure, Lets reduce to the max value for each of the sites
dim(ICESt)
ICEStmax <- ICESt %>% group_by(STATN) %>% slice(which.max(Value))
dim(ICEStmax)
#122 sites

IVLt2max <- IVLt2 %>% group_by(Provplats) %>% slice(which.max(Value))
dim(IVLt2max)
#12 sites

SLUtmax <- SLUt %>% group_by(Stationsnamn) %>% slice(which.max(Value))
dim(SLUtmax)
#7 sites

#Output of data for GIS Map figure####
write.table(SLUtmax, file = "Output/SLU.csv", row.names=FALSE, sep = ",")
write.table(ICEStmax, file = "Output/ICES.csv", row.names=FALSE, sep = ",")
write.table(IVLt2max, file = "Output/IVL.csv", row.names=FALSE, sep = ",")
write.table(ICESlist, file = "Output/ICEShighStation.csv", row.names=FALSE, sep = ",")

#Stats and data analysis####

#some descriptive statistics

#N observations
N <- nrow(ICESt)+nrow(IVLt2)+nrow(SLUt)
#N Number of locals
L <- nrow(ICEStmax)+nrow(IVLt2max)+nrow(SLUtmax)

#Avarage observations per local
N/L

#List ICES sites with >EC05 values
ICESlist <- filter(ICEStmax, Value > 0.264877)

#Sites with values above 0.1

filter(ICEStmax, Value > 0.1)
filter(SLUtmax, Value > 0.1)
filter(IVLt2max, Value > 0.1)

#Lets identify all stations with a value above the EC05 of RO5AC without EDTA
#EC50: 0.264877 S.E.  0.028666

filter(ICEStmax, Value > 0.264877)

#7 stations
ICESthigh <- ICESt[ICESt$STATN %in% c("3ZP", "4ZP", "K41", "OM225103", "OMMVGB19", "OMMVKHM", "T18P"),]
ICESfig <- ICESthigh %>% select(STATN, Value)
ICESfig$Station <- ICESfig$STATN

#Summary stats on sites
StatsICESthigh <- ddply(ICESthigh, "STATN", summarise,
                     median = median(Value), max = max(Value),
                     min = min(Value),
                     N = length(Value))

#Month of max value
ICEShighMax <- ICEStmax[ICEStmax$STATN %in% c("3ZP", "4ZP", "K41", "OM225103", "OMMVGB19", "OMMVKHM", "T18P"),]

#Repeat for SLU
filter(SLUtmax, Value > 0.264877)
#1 station
SLUthigh <- SLUt[SLUt$Stationsnamn %in% "Nyköpingsån Spånga",]
SLUfig <- SLUthigh %>% select(Stationsnamn, Value)
SLUfig$Station <- SLUthigh$Stationsnamn
filter(IVLt2max, Value > 0.264877)

#Summary stats on sites
StatsSLUthigh <- ddply(SLUthigh, "Stationsnamn", summarise,
                        median = median(Value), max = max(Value),
                        min = min(Value),
                        N = length(Value))

#And IVL

#1 station
IVLt2high <- IVLt2[IVLt2$Provplats %in% "Linnéaholm",]
IVLt2fig <- IVLt2high %>% select(Provplats, Value)
IVLt2fig$Station <- IVLt2fig$Provplats

AvaragesRFU <- ddply(RFU, c("Metal", "t"), summarise,
                     mean = mean(RFU), sd = sd(RFU))

#and bind it all
#Allhigh <- bind_rows(ICESthigh %>% select(STATN, Value), SLUthigh %>% select(Stationsnamn, Value), IVLt2high %>% select(Provplats, Value))
Allhigh <- bind_rows(ICESfig, SLUfig, IVLt2fig)

#Figures####
dim(IVLt2)
levels(IVLt2$Provplats)

#All stations
FigA <- ggplot(ICESt, aes(ICESt$STATN, (ICESt$Value))) +
  geom_boxplot(mapping = aes()) + #mapping = aes(color = Metal), size = 4
  theme(axis.text.x = element_text(angle = 55, hjust = 1))  #control tilt

FigB <- ggplot(IVLt2, aes(IVLt2$Provplats, (IVLt2$Value))) +
  geom_boxplot(mapping = aes()) +  #mapping = aes(color = Metal), size = 4
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

FigC <-ggplot(SLUt, aes(SLUt$Stationsnamn, (SLUt$Value))) +
  geom_boxplot(mapping = aes()) +  #mapping = aes(color = Metal), size = 4
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggarrange(FigA, FigB, FigC, nrow = 3,
          labels = c("A", "B", "C"), widths=c(0.5,5))

dev.copy(pdf, "Output/BoxplotsAllstations.pdf")
dev.off()

#Stations with "one or more high values"

#Lets set the 0 values to lowest reported value as detection limit for log plotting
dlimit <- min(Allhigh[,2][which(Allhigh[,2]>0)])
dlimit
Allhigh$Value <- (ifelse(Allhigh$Value < dlimit, (dlimit), Allhigh$Value))
?geom_boxplot
#Stations with "one or more high values"
FigHigh <- ggplot(Allhigh, aes(Station, Value)) +
  geom_boxplot(mapping = aes()) + #mapping = aes(color = Metal), size = 4
  #scale_color_manual(values=c("#000000", "#828282", "#FDCC65", "#B85633")) +
  coord_cartesian(ylim=c(dlimit, 2), expand = F) + #ylim=c(-10000,+10000)
  geom_hline(yintercept = 0.264877, color = "Red") +
  geom_hline(yintercept = 0.211628, color = "Red", linetype="dashed") +
  geom_hline(yintercept = 0.331526, color = "Red", linetype="dashed") +
  #scale_y_discrete(limits=c(0,0.1,0.2,0.3,0.4,0.5, 0.6, 0.7)) +
  #scale_fill_manual(values=c("#000000", "#828282", "#FDCC65", "#B85633")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Station",  y=expression("Cu concentration"~(mu*M)), title = "") +
  scale_y_continuous(trans = "log10") +
  theme(plot.title = element_text(vjust = - 14, hjust = 0.04)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(text=(element_text(size=20))) +
  theme(axis.text=(element_text(size=20))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.5) +
  theme(legend.position = c("top"),
        #legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"))

FigHigh
dev.copy(pdf, "Output/BoxplotsHighStations.pdf")
dev.off()

#Lets add the cumulative frequency plot
AllCuData <- c(ICESt$Value, IVLt2$Value, SLUt$Value)
count(AllCuData > 0.264877)
count(AllCuData > 0.211628)
count(AllCuData > 0.1)

FigCum <- ggplot()+aes(AllCuData) +
  stat_ecdf(geom = "step", pad = FALSE) +
  #coord_cartesian(ylim=c(dlimit, 2), expand = F) + #ylim=c(-10000,+10000)
  geom_vline(xintercept = 0.264877, color = "Red") +
  geom_vline(xintercept = 0.211628, color = "Red", linetype="dashed") +
  geom_vline(xintercept = 0.331526, color = "Red", linetype="dashed") +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x=expression("Cu concentration"~(mu*M)),  y="Cumulative probability", title = "") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(plot.title = element_text(vjust = - 14, hjust = 0.04)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(text=(element_text(size=20))) +
  theme(axis.text=(element_text(size=20))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=1)
 
print(FigCum)
dev.copy(pdf, "Output/FigCum.pdf")
dev.off()                