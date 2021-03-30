#Lets clear old objects
rm(list=ls())
getwd()
#and set the work directory in case we have moved around or opened another project
setwd("~/Documents/R/Andersson_etal_2021/04_SelectionModels/ModelResults/Cd")
dir()

#Load packages and data (not adapted)
library(reshape2)
library(devtools)
library(scales)
library(ggplot2)
library(devtools)
library(tidyverse)

#This whole script is a bit messy and basically only imports, values, sums up or culcuates averages and ratios from models generated in earlier scripts. Several input files.
#Note the script analyzed both the observed (allias Parallel) and DRC predicted inhibitions. Only the DRC predicted was included in the manuscript 

#Import data (a bit messy)
myData <- read.csv("Input/PRoutcomeCd.csv", header=FALSE)
head(myData)
myData2 <- read.csv2("Input/CompExpResults.csv", header=FALSE)
sapply(myData2, class)
myData2$V3 <- as.numeric(as.character(myData2$V3))
myData2$V4 <- as.numeric(as.character(myData2$V4))

myData3 <- rbind(myData2, myData)
colnames(myData3) <- c("Index", "Metal", "t", "Area")

#Lets get the Selection model into one the dataframe####
#It needs to be summed up and modified a bit

#First drc bases
myData4 <- read.csv("Input/StrainMdrc_Cd.csv", header=FALSE)
colnames(myData4) <- c("Strain", "Metal", "t", "Area")

#Combine TB strains biomass(Area)
TB_strain <- subset.data.frame(myData4, grepl("TB", myData4$Strain))
TB_strain2 <- t(dcast(data = TB_strain,formula = Strain~t, fun.aggregate = sum,value.var = c("Area"), head = FALSE))
colnames(TB_strain2) <- as.character(TB_strain2[1,])
TB_strain2 <- TB_strain2[-1, ]
class(TB_strain2) <- "numeric"
dim(TB_strain2)
TB_strain2_v <- rowSums(TB_strain2)

#Combine SM strains biomass(Area)
SM_strain <- subset.data.frame(myData4, grepl("SM", myData4$Strain))
SM_strain2 <- t(dcast(data = SM_strain,formula = Strain~t, fun.aggregate = sum,value.var = c("Area"), head = FALSE))
colnames(SM_strain2) <- as.character(SM_strain2[1,])
SM_strain2 <- SM_strain2[-1, ]
class(SM_strain2) <- "numeric"
dim(SM_strain2)
SM_strain2_v <- rowSums(SM_strain2)

#Compute ratios and make dataframe to match other models
SelectionM_ratios <- SM_strain2_v/TB_strain2_v
n = 101
Model <- rep("Selection DRC", each = n)
Metal <- rep("Cd", each = n)
t <- seq(0, 10, by=0.1)

SelectionM <- as.data.frame(cbind(Model, Metal, t, SelectionM_ratios))
colnames(SelectionM) <- c("Model", "Metal", "t", "Ratio")

#Making strain specific ratios (4 x 4 comparison)
SM_16 <- subset.data.frame(myData4, grepl("SM_GP2-4_16", myData4$Strain))
SM_13 <- subset.data.frame(myData4, grepl("SM_GP2-4_13", myData4$Strain))
SM_19 <- subset.data.frame(myData4, grepl("SM_GP2-4_19", myData4$Strain))
SM_20 <- subset.data.frame(myData4, grepl("SM_GP2-4_20", myData4$Strain))
TB_9 <- subset.data.frame(myData4, grepl("TB_GP2-4_9", myData4$Strain))
TB_11 <- subset.data.frame(myData4, grepl("TB_GP2-4_11", myData4$Strain))
TB_13 <- subset.data.frame(myData4, grepl("TB_GP2-4_13", myData4$Strain))
TB_16 <- subset.data.frame(myData4, grepl("TB_GP2-4_16", myData4$Strain))

SM_16_TB_9 <- SM_16$Area/TB_9$Area
SM_16_TB_11 <- SM_16$Area/TB_11$Area
SM_16_TB_13 <- SM_16$Area/TB_13$Area
SM_16_TB_16 <- SM_16$Area/TB_16$Area

SM_13_TB_9 <- SM_13$Area/TB_9$Area
SM_13_TB_11 <- SM_13$Area/TB_11$Area
SM_13_TB_13 <- SM_13$Area/TB_13$Area
SM_13_TB_16 <- SM_13$Area/TB_16$Area

SM_19_TB_9 <- SM_19$Area/TB_9$Area
SM_19_TB_11 <- SM_19$Area/TB_11$Area
SM_19_TB_13 <- SM_19$Area/TB_13$Area
SM_19_TB_16 <- SM_19$Area/TB_16$Area

SM_20_TB_9 <- SM_20$Area/TB_9$Area
SM_20_TB_11 <- SM_20$Area/TB_11$Area
SM_20_TB_13 <- SM_20$Area/TB_13$Area
SM_20_TB_16 <- SM_20$Area/TB_16$Area

Allstrain <- as.data.frame(cbind(t, SM_16_TB_9, SM_16_TB_11, SM_16_TB_13, SM_16_TB_16,
                                 SM_13_TB_9, SM_13_TB_11, SM_13_TB_13, SM_13_TB_16,
                                 SM_19_TB_9, SM_19_TB_11, SM_19_TB_13, SM_19_TB_16,
                                 SM_20_TB_9, SM_20_TB_11, SM_20_TB_13, SM_20_TB_16))

Allstrain <- melt(Allstrain, id.var = "t")
#Done!

#Repeat for parallel bases model####

myData5 <- read.csv("Input/StrainMO_Cd.csv", header=FALSE)
colnames(myData5) <- c("Strain", "Metal", "t", "Area")

#Combine TB strains biomass(Area)
TB_strain3 <- subset.data.frame(myData5, grepl("TB", myData5$Strain))
TB_strain4 <- t(dcast(data = TB_strain3,formula = Strain~t, fun.aggregate = sum,value.var = c("Area"), head = FALSE))
colnames(TB_strain4) <- as.character(TB_strain4[1,])
TB_strain4 <- TB_strain4[-1, ]
class(TB_strain4) <- "numeric"
dim(TB_strain4)
TB_strain4_v <- rowSums(TB_strain4)

#Combine SM strains biomass(Area)
SM_strain3 <- subset.data.frame(myData5, grepl("SM", myData5$Strain))
SM_strain4 <- t(dcast(data = SM_strain3,formula = Strain~t, fun.aggregate = sum,value.var = c("Area"), head = FALSE))
colnames(SM_strain4) <- as.character(SM_strain4[1,])
SM_strain4 <- SM_strain4[-1, ]
class(SM_strain4) <- "numeric"
dim(SM_strain4)
SM_strain4_v <- rowSums(SM_strain4)

#Compute ratios and make dataframe to match other models
SelectionMO_ratios <- SM_strain4_v/TB_strain4_v
n = 101
Model <- rep("Selection Parallel", each = n)
Metal <- rep("Cd", each = n)
t <- seq(0, 10, by=0.1)

SelectionMO <- as.data.frame(cbind(Model, Metal, t, SelectionMO_ratios))
colnames(SelectionMO) <- c("Model", "Metal", "t", "Ratio")


#Now calculate prediction using species averages####

#Grab only Cd data

Cd <- subset.data.frame(myData3, grepl("Cd", myData3$Metal))

#Plot data to make sure it looks ok
ggplot(data = Cd, aes(x = t, y = Area)) +
  geom_point() + facet_grid(Index ~ .) +
  scale_y_continuous(trans = "log10") #change the scale on y axis

#Then we need to make ratios for the two species...

Cd <- separate(data = Cd, col = Index, into =c("Species", "Model"), sep = "_", remove = TRUE, convert = FALSE)
Cd

Cd_SM <- subset.data.frame(Cd, grepl("SM", Cd$Species))
Cd_TB <- subset.data.frame(Cd, grepl("TB", Cd$Species))

#and add up all vector models####

#This is dangerous and risk of misalignment must be doubled-checked (e.g. last plot)
Cd_ratios <- cbind(Cd_SM, Cd_TB)
colnames(Cd_ratios) <- c("Species_SM", "Model_SM", "Metal_SM", "t_SM", "Area_SM", "Species_TB", "Model_TB", "Metal_TB", "t_TB", "Area_TB")
plot(Cd_ratios$t_SM~Cd_ratios$t_TB)

#Looks ok, now we can calculate biomass ratios
Cd_ratios$Ratio <- Cd_ratios$Area_SM/Cd_ratios$Area_TB
Cd_ratios

#lets remove the Control (growth) and change names of Models, and add selection model

#REMOVING "PRuO", PRuO = "Mean Parallel", PRuP = "Mean DRC"
Keep <- c("Exp")
Cd_ratios2 <- filter(Cd_ratios, Model_SM %in% Keep)
Cd_ratios2$Model_SM <- recode_factor(Cd_ratios2$Model_SM, 
                                     Exp = "Observation")
Cd_ratios2$Species_SM <- NULL
Cd_ratios2$Area_SM <- NULL
Cd_ratios2$Species_TB <- NULL
Cd_ratios2$Model_TB <- NULL
Cd_ratios2$Metal_TB <- NULL
Cd_ratios2$t_TB <- NULL
Cd_ratios2$Area_TB <- NULL

colnames(Cd_ratios2) <- c("Model", "Metal", "t", "Ratio")

#REMOVING SelectionMO from plotting
Cd_ratios3 <- rbind(Cd_ratios2, SelectionM)
Cd_ratios3$t <- as.numeric(as.character(Cd_ratios3$t))
Cd_ratios3$Ratio <- as.numeric(as.character(Cd_ratios3$Ratio))

Fig3.C <- ggplot(Cd_ratios3, aes(t, Ratio)) +
  geom_line(data=Allstrain, aes(x=t, y=value, linetype = variable, colour = "Strain-by-strain")) +
  guides(colour=guide_legend(ncol=1), linetype=FALSE) +
  #geom_point(mapping = aes(color = Model, shape = Model), size = 0) +
  stat_smooth(mapping = aes(colour = Model), size = 1.5, method = 'lm', formula = y ~ poly(x,2), se= TRUE) + #Fits polynomal function to data (can be changed to lm: https://plotly.com/ggplot2/stat_smooth/) and https://stackoverflow.com/questions/31829528/specify-regression-line-intercept-r-ggplot2
  #scale_color_brewer(palette = "Paired") +
  geom_point(data = subset(Cd_ratios3, Model == "Observation"), shape = 1, size = 4) + 
  #geom_smooth(mapping = aes(x = t_SM, y = (Ratio), color = Model_SM), method = "auto") +
  scale_color_manual(values=c("#000000", "#FDCC65", "#cccccc"), aesthetics = c("colour", "fill"), guide=FALSE) +
  #scale_shape_manual(values=c(2)) +
  #geom_line(mapping = aes(t_SM ~ Ratio, color = Model_SM)) +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(10, 1, 1)) + #Size of symbols
  #scale_linetype_manual(values=c(1,3,1)) +
  #geom_errorbar(aes(ymin = X95L, ymax = X95H, width=.2, color = EC), position = position_dodge(0.3)) +
  coord_cartesian(xlim=c(-0.1, 10.1), ylim=c(0.00005, 20000), expand = F) + #ylim=c(-10000,+10000)
  scale_x_discrete(limits=c(0,2,4,6,8,10)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + #change the scale on y axis
  annotation_logticks(sides = "l") + # adds linier tickmarks
  #coord_cartesian(ylim=c(-4,4), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  #scale_fill_manual(values=c("#000000", "#000000", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  #theme_classic() +
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Time (days)", y=("Relative biomass (SM/TB)"), title = "C) Cd") +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.1)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=30)) +
  theme(text=(element_text(size=30))) +
  theme(axis.text=(element_text(size=30))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=1) +
  #stat_cor(aes(color = scientific_name), label.x = 3)
  theme(plot.margin=unit(c(0,1,0,0.2),"cm")) +
  theme(legend.position = "none", panel.background = element_blank())

print(Fig3.C)

dev.copy(pdf, "Results/Fig3C.pdf")
dev.off()

##Strain selection graphs#####

#assemble matrices, melt for plotting, with all strains change in relative biomass.
StrainsO <- cbind(SM_strain4, TB_strain4)
StrainsDrc <- cbind(SM_strain2, TB_strain2)

longDataO <-melt(StrainsO)
longDataDrc <-melt(StrainsDrc)
sapply(longDataDrc, class)

#Change names of TB_GP2-4_9 and SM_GP2-4_16 to TB_GP2-4_09 and SM_GP2-4_06
longDataO$Var2 <- recode_factor(longDataO$Var2, 
                                "TB_GP2-4_9" = "TB_GP2-4_09", "SM_GP2-4_16" = "SM_GP2-4_06")
longDataO$Var2 <- as.character(as.factor(longDataO$Var2))

longDataDrc$Var2 <- recode_factor(longDataDrc$Var2, 
                                  "TB_GP2-4_9" = "TB_GP2-4_09", "SM_GP2-4_16" = "SM_GP2-4_06")
longDataDrc$Var2 <- as.character(as.factor(longDataDrc$Var2))


FigDRC.C <- ggplot(data = longDataDrc, aes(x = Var1, y = value, fill=Var2)) + geom_area(position='fill') +
  labs (x="Time (days)", y=("Relative biomass"), title = "C) Cd") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.04)) +
  coord_cartesian(xlim=c(0, 10), ylim=c(0, 1), expand = F) + #ylim=c(-10000,+10000)
  scale_color_manual(values=c("#00FF00", "#00C800", "#007800", "#003C00", "#508CFF",  "#2846FF", "#1E3296",  "#141964"), aesthetics = c("colour", "fill")) +
  scale_x_discrete(limits=c(0,2,4,6,8,10)) +
  panel_border(colour = "black", size = 1) +
  background_grid(major = "none", minor = "none") +# and a border around each panel
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=18)) +
  theme(text=(element_text(size=30))) +
  theme(axis.text=(element_text(size=30))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=1.1) +
  #stat_cor(aes(color = scientific_name), label.x = 3)
  theme(legend.position = "none", panel.background = element_blank())

FigDRC.C

dev.copy(pdf, "Results/FigDRC.C.pdf")
dev.off()

FigO.C <- ggplot(data = longDataO, aes(x = Var1, y = value, fill=Var2)) + geom_area(position='fill') +
  labs (x="Time (days)", y=("Relative biomass"), title = "C) Cd") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.04)) +
  coord_cartesian(xlim=c(0, 10), ylim=c(0, 1), expand = F) + #ylim=c(-10000,+10000)
  scale_color_manual(values=c("#00FF00", "#00C800", "#007800", "#003C00", "#508CFF",  "#2846FF", "#1E3296",  "#141964"), aesthetics = c("colour", "fill")) +
  scale_x_discrete(limits=c(0,2,4,6,8,10)) +
  panel_border(colour = "black", size = 1) +
  background_grid(major = "none", minor = "none") +# and a border around each panel
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=18)) +
  theme(text=(element_text(size=30))) +
  theme(axis.text=(element_text(size=30))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=1.1) +
  #stat_cor(aes(color = scientific_name), label.x = 3)
  theme(legend.position = "none", panel.background = element_blank())

FigO.C

dev.copy(pdf, "Results/FigO.C.pdf")
dev.off()


#Output data####
#Lets output the growth rate estimates from the selection models so we can plot that for all metals later

#Sum strain Areal increase vs t here

#We have vectors for t and Area separate in selection models so lets use that

#Drc models first, we can use dplyr's lag function in the equation

RateSMdrc <- as.data.frame(cbind(t, rep("SM", each = 101), rep("Cd", each = 101), (log(SM_strain2_v/lag(SM_strain2_v))/(t - lag(t)))))
RateTBdrc <- as.data.frame(cbind(t, rep("TB", each = 101), rep("Cd", each = 101), (log(TB_strain2_v/lag(TB_strain2_v))/(t - lag(t)))))                         
RatePredDrc <- as.data.frame(rbind(RateSMdrc, RateTBdrc))
row.names(RatePredDrc) = NULL
RatePredDrc$t <- as.numeric(as.character(RatePredDrc$t))
RatePredDrc$V4 <- as.numeric(as.character(RatePredDrc$V4))
RatePredDrc$V2 <- as.factor(as.character(RatePredDrc$V2))
plot(x = RatePredDrc$t, y = RatePredDrc$V4, col=RatePredDrc$V2)

write.table(RatePredDrc, file = "Results/RatePredDrc.csv", sep = ",", col.names = FALSE, qmethod = "double")

#Now Observed models model
RateSMO <- as.data.frame(cbind(t, rep("SM", each = 101), rep("Cd", each = 101), (log(SM_strain4_v/lag(SM_strain4_v))/(t - lag(t)))))
RateTBO <- as.data.frame(cbind(t, rep("TB", each = 101), rep("Cd", each = 101), (log(TB_strain4_v/lag(TB_strain4_v))/(t - lag(t)))))                       
RatePredO <- as.data.frame(rbind(RateSMO, RateTBO))
row.names(RatePredO) = NULL
RatePredO$t <- as.numeric(as.character(RatePredO$t))
RatePredO$V4 <- as.numeric(as.character(RatePredO$V4))
RatePredO$V2 <- as.factor(as.character(RatePredO$V2))
plot(x = RatePredO$t, y = RatePredO$V4, col=RatePredO$V2)

write.table(RatePredO, file = "Results/RatePredO.csv", sep = ",", col.names = FALSE, qmethod = "double")
