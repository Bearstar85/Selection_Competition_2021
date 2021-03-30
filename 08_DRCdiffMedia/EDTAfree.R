## Houskeeping
rm(list=ls()) #remove ALL objects 
Sys.setenv(LANG = "en") #Let's keep stuff in English
Sys.setlocale("LC_ALL","English")
graphics.off()
cat("\014") # clear console window prior to new runekeeping
setwd("~/Documents/R/Andersson_etal_2021/08_DRCdiffMedia")
#packages & functions

library(drc)
library(ggplot2)

#First analyse standard F/2 media####
myData3= read.csv("Input/NewMedia.csv", header=TRUE)
myData3

#First look at the imported data
plot(myData3$Concentration, myData3$Inhibition) #$says which speific columns you will use

#Transform Concentration from numeric to absolut Cu concentration (removed +1.3 for new water)
myData3$Concentration <- (ifelse(myData3$Concentration > 0, myData3$Concentration*5*0.6285, myData3$Concentration))
plot(myData3$Concentration, myData3$Inhibition)

#drm = dose-response model for error plots (change model as needed)
fitData3= subset(myData3, myData3$Concentration >=0)
bestNewMedia.WB = drm(Inhibition~Concentration, data = fitData3, fct = W2.2())

#Then the EDTA free media####
myData2= read.csv("Input/EDTAfree.csv", header=TRUE)
myData2

#First look at the imported data
plot(myData2$Concentration, myData2$Inhibition) #$says which speific columns you will use

#Transform Concentration from numeric to absolut Cu concentration
myData2$Concentration <- (ifelse(myData2$Concentration > 0, myData2$Concentration*5*0.6285, myData2$Concentration))
plot(myData2$Concentration, myData2$Inhibition) #$says which speific columns you will use

#drm = dose-response model for error plots (change model as needed)
fitData2= subset(myData2, myData2$Concentration >=0)
bestEDTAfree.WB = drm(Inhibition~Concentration, data = fitData2, fct = W2.2())


#Make double analysis for plot####
myData= read.csv("Input/RO5_EDTAfree.csv", header=TRUE)
myData
#First look at the imported data
plot(myData$Concentration, myData$Inhibition) #$says which speific columns you will use

#Transform Concentration from numeric to absolut Cu concentration (new media means that the 1.3 baseline is removed)
myData$Concentration <- (ifelse(myData$Concentration > 0, myData$Concentration*5*0.6285, myData$Concentration))
plot(myData3$Concentration)

#Fit the data using the Weibull1 model, fix the lower and upper limits
fitData= subset(myData, myData$Concentration >=0)
multi.m1 <- drm(Inhibition~Concentration, data = fitData, Strain, fct = W2.2())

#Choose colors to doubble plots
#Check ot this link for col funktion https://www.statmethods.net/advgraphs/parameters.html
#And http://research.stowers.org/mcm/efg/R/Color/Chart/

#generate RGB code for Green=SM
rgb(0, 128, 0, maxColorValue=255)

#RGB Blue=TB
rgb(40, 70, 255, maxColorValue=255)

#RGB Red=TB
rgb(128, 0, 0, maxColorValue=255)

#Now i am working on a script to make nice looking plots with aid of:
#https://www.rdocumentation.org/packages/drc/versions/3.0-1/topics/plot.drc

#YES it makes a new window for each graph but whatever
dev.off()
dev.new(width=2.5, height=2.5)
par(mar=c(7,7,5,5))
plot(multi.m1,
     type = "all",
     col=c("#7F7F7F", "#DC0000"), 
     broken = TRUE, bp = 0.1, #this controls where the axis is cut
     #bcontrol = (bp = 0.5, factor = "defult", style = "gap", width = "0.02"), #this controls how the cut is made but not working yet
     conName = "Control",
     cex =  2, cex.axis=2, lty=c(1, 2), lwd=2, #size of: symbols, axis numbers, lines type, line thickness,  stuff
     xlim = c(0,14), #Changes,  must be the same in the row below  
     ylim =c(-0.2,1.2),
     xt = c(0, 0.3, 0.9, 2.7, 8.1), #Specify x numbers
     yt = c(-0.2, 0.2, 0.6, 1), #Specify y number
     xlab = expression("Cu concentration (" *mu ~ "M)"), line = 5, cex.lab =2,
     ylab = "Inhibition of growth rate",
     legend = TRUE, legendPos = c(9, 1.1), legendText=c("Standard", "EDTAfree"), cex.legend = 1.5, lwd.legend = 1.75,
     main = title(main = "A", adj = 0.05, line = -2.5, cex.main = 2))
plot(bestNewMedia.WB, col = "#7F7F7F", type = "confidence", xlim = c(0,10), lty=c(0), add = TRUE)
plot(bestEDTAfree.WB, col = "#DC0000", type = "confidence", xlim = c(0,10), lty=c(0), add = TRUE)
dev.copy(pdf, "Results/FigEDTA.pdf")
dev.off()

sink("Results/ECvaluesEDTA.txt")

#report the EC50
ED(bestNewMedia.WB, respLev = 0.5, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay
#report the EC05
ED(bestNewMedia.WB, respLev = 0.05, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay
#report the EC95
ED(bestNewMedia.WB, respLev = 0.95, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay

#report the EC50
ED(bestEDTAfree.WB, respLev = 0.5, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay
#report the EC05
ED(bestEDTAfree.WB, respLev = 0.05, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay
#report the EC95
ED(bestEDTAfree.WB, respLev = 0.95, type="absolute", interval="tfls") #to get ED help file put ? in front of ED and run. 
PLay = c(0.1, 0.5, 0.7, 0.9)
PLay

sink()