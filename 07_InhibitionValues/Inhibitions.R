#Lets clear old objects
rm(list=ls())
getwd()
#and set the work directory in case we have moved around or opened another project
setwd("~/Documents/R/Andersson_etal_2021/07_InhibitionValues")
dir()

#Load packages and data (not adapted)
library(reshape2)
library(multtest) 
library(lattice)
library(devtools)
library(ggbiplot)
library(plyr)
library(scales)
library(ggplot2)
library(devtools)
library(grid)
#library(gplots)
library(tidyverse)
library(lubridate) # useful for working with dates
library(cowplot) # useul for combining multiple plots
library(ggthemes)
library(broom)
library(ggpubr)
library(gridExtra)


#Figures####
#Note, the statistical analysis of this data is performed when it was computed (see 04_SelectionModels)

#Import data
myData2 <- read.csv2("Inhibition.csv", header=TRUE)
head(myData2)
myData2
sapply(myData2, class)

#Make some list for grepping data

#Change relevant data to numeric 
for(i in c(4:10: ncol(myData2))) {
  myData2[,i] <- as.numeric(as.character(myData2[,i]))
}

myData2$Strain <- as.character(as.factor(myData2$Strain))

#Lets grep Ag, Cd and Cu for stats
Control2 <- subset.data.frame(myData2, grepl("Control", myData2$Metal))
Ag2 <- subset.data.frame(myData2, grepl("Ag", myData2$Metal))
Cd2 <- subset.data.frame(myData2, grepl("Cd", myData2$Metal))
Cu2 <- subset.data.frame(myData2, grepl("Cu", myData2$Metal))
Ag2

Fig2.A <- ggplot(Control2, aes(Strain, Inhibition)) +
  geom_point(mapping = aes(shape = Exp), size = 3, position = position_dodge(0)) + #position = position_dodge(0.3), size = Species, fill = EC
  #scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 4, 2), labels=c("Observed", "Pre-culture", "Predicted (DRC)")) +
  #geom_hline(yintercept = 0.046, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = Low, ymax = High, width=.1), position = position_dodge(-1)) + #position = position_dodge(0.3
  #scale_y_continuous(trans = "log10") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(-0.1,2), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression(paste(~mu* " "(day^{-1}))), title = "A) Control") +
  theme(plot.title = element_text(vjust = - 6, hjust = 0.05)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank(), panel.background = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.25) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = c(0.15,0.3),
        #legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 10, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"))


print(Fig2.A)

dev.copy(pdf, "Fig2A.pdf")
dev.off()

Fig2.B <- ggplot(Ag2, aes(Strain, Inhibition)) +
  geom_point(mapping = aes(shape = Exp), size = 3, position = position_dodge(0)) + #position = position_dodge(0.3), size = Species, fill = EC
  #scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  #geom_hline(yintercept = 0.046, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = Low, ymax = High, width=.1), position = position_dodge(-1)) + #position = position_dodge(0.3
  #scale_y_continuous(trans = "log10") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(-0.1,2), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression(paste("Inhibition (fraction "~mu*")")), title = "B) Ag") +
  theme(plot.title = element_text(vjust = - 6, hjust = 0.05)) +
  theme(panel.spacing = unit(.1, "lines")) +
  theme(legend.title=element_blank(), panel.background = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.25) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())


print(Fig2.B)

dev.copy(pdf, "Fig2B.pdf")
dev.off()

Fig2.C <- ggplot(Cd2, aes(Strain, Inhibition)) +
  geom_point(mapping = aes(shape = Exp), size = 3, position = position_dodge(0)) + #position = position_dodge(0.3), size = Species, fill = EC
  #scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  #geom_hline(yintercept = 0.046, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = Low, ymax = High, width=.1), position = position_dodge(-1)) + #position = position_dodge(0.3
  #scale_y_continuous(trans = "log10") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(-0.1,2), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression(paste("Inhibition (fraction "~mu*")")), title = "C) Cd") +
  theme(plot.title = element_text(vjust = - 6, hjust = 0.05)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank(), panel.background = element_blank()) +
  theme(legend.text=element_text(size=12)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.25) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())


print(Fig2.C)

dev.copy(pdf, "Fig2C.pdf")
dev.off()

Fig2.D <- ggplot(Cu2, aes(Strain, Inhibition)) +
  geom_point(mapping = aes(shape = Exp), size = 3, position = position_dodge(0)) + #position = position_dodge(0.3), size = Species, fill = EC
  #scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  #geom_hline(yintercept = 0.046, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = Low, ymax = High, width=.1), position = position_dodge(-1)) + #position = position_dodge(0.3
  #scale_y_continuous(trans = "log10") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(-0.1,2), expand = F) + #changes the y axis
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) + #adjust text tilt and possition
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  #theme_classic() +
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression(paste("Inhibition (fraction "~mu*")")), title = "D) Cu") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.02)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  #theme(legend.title=element_blank()) +
  #theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  #theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.25) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())


print(Fig2.D)

dev.copy(pdf, "Fig2D.pdf")
dev.off()