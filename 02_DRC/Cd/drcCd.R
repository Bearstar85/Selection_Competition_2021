#Lets clear old objects
rm(list=ls())
getwd()
#and set the work directory in case we have moved around or opened another project
setwd("~/Documents/R/Andersson_etal_2021/02_DRC/Cd")
dir()

#packages & functions
library(drc)
library(ggplot2)
library(staplr)
library(dplyr)
sessionInfo()

#Data loading####
#first create individual .csv files based on .csv containing data of all strains
all = read.csv2("Doseresponse_all.csv")
all$Concentration <- as.numeric(as.character(all$Concentration))
all$Inhibition <- as.numeric(as.character(all$Inhibition))
all$Strain <- as.factor(as.character(all$Strain))
sapply(all, class)

#grep the Cd data only
Cd <- subset.data.frame(all, grepl("Cd", all$Metal))

#Transform Concentration from numeric to absolute Cd concentration
Cd$Concentration <- (ifelse(Cd$Concentration > 0, Cd$Concentration*0.6759+0.1274, Cd$Concentration))
plot(Cd$Concentration)

#analysis####
for (name in levels(Cd$Strain)){
  #Subset data based on Running.ID
  tmp <- subset(Cd,Strain==name)
  #Create a new filename for each strain - the folder 'Files' should already exist
  fn <- paste("Files/Doseresponse_",gsub(' ','',name), ".csv", sep='')
  #Save the CSV file containing separate expenses data for each strain
  write.table(tmp,fn,row.names=FALSE, sep=",")
}

#generate list of strains
f <- levels(Cd$Strain)
f

#1. Looped response prediction ####
for (i in f) {
  # define name of Input and Output 
  input=paste("Files/Doseresponse_",i,".csv", sep="")
  output=paste("PR/PR_5.51?M_",i,".csv", sep="")
  myData= read.csv(input, header=TRUE)
  
  #drm = dose-response model for error plots (change fct model as needed, fitData removes any controls not properly labeled 0)
  fitData= subset(myData, myData$Concentration >=0)
  bestDoseresponse.WB = drm(Inhibition~Concentration, data = fitData, fct = W1.2())
  
  
  # predict response at specific concentration c(x) with 95% confidence
  Response= predict(bestDoseresponse.WB, data.frame(dose=5.508530537, CURVE= "1"), interval = "confidence")
  Response2 = t(Response)
  
  #export result
  write.table(Response2, file = output, sep=",",  col.names=FALSE, row.names= i)
  
}

# 2. Looped effective concentrations (e.g. EC50)####
for (i in f) {
  
  # define name of Input and Output 
  input=paste("Files/Doseresponse_",i,".csv", sep="")
  output50=paste("EC/EC50_",i,".csv", sep="")
  output5=paste("EC/EC5_",i,".csv", sep="")
  output95=paste("EC/EC95_",i,".csv", sep="")
  
  #Import data
  myData2= read.csv(input, header=TRUE)
  
  #drm = dose-response model for error plots (change fct model as needed, fitData removes any controls not properly labeled 0)
  fitData2= subset(myData2, myData2$Concentration >=0)
  bestDoseresponse.WB = drm(Inhibition~Concentration, data = fitData2, fct = W1.2())
  
  #report the EC50 (log-normal 95% conf. intervals)
  EC50=ED(bestDoseresponse.WB, respLev = 0.5, type="absolute", interval="tfls")
  
  #report the EC05 (log-normal 95% conf. intervals)
  EC5=ED(bestDoseresponse.WB, respLev = 0.05, type="absolute", interval="tfls")
  
  #report the EC95 (log-normal 95% conf. intervals)
  EC95=ED(bestDoseresponse.WB, respLev = 0.95, type="absolute", interval="tfls")
  
  name50= paste(i,"0.5")
  name5= paste(i,"0.05")
  name95= paste(i,"0.95")
  
  write.table(EC50, file= output50, col.names = FALSE, row.names = name50)
  write.table(EC5, file= output5, col.names = FALSE, row.names = name5)
  write.table(EC95, file= output95, col.names = FALSE, row.names = name95)
  
}

#3. Plot dose-respons curves####

for (i in f) {
  
  input=paste("Files/Doseresponse_",i,".csv", sep="")
  output=paste("Plots/Fig_",i,".pdf", sep="")
  
  #Import data
  myData2= read.csv(input, header=TRUE)
  myData2
  
  #First look at the imported data
  plot(myData2$Concentration, myData2$Inhibition) #$says which speific columns you will use
  
  #drm = dose-response model for error plots (change model as needed)
  fitData2= subset(myData2, myData2$Concentration >=0)
  bestDoseresponse_GP.WB = drm(Inhibition~Concentration, data = fitData2, fct = W1.2())
  
  #Now i am working on a script to make nice looking plots with aid of:
  #https://www.rdocumentation.org/packages/drc/versions/3.0-1/topics/plot.drc
  
  dev.off()
  dev.new(width=2.5, height=2.5)
  par(mar=c(7,7,5,5))
  plot(bestDoseresponse_GP.WB,
       type = "all",
       col=c("#008000", "#800000"), 
       broken = TRUE, bp = 1,   #this controls where the axis is cut
       bcontrol = list(factor = 1.2), #= "defult", style = "gap", width = "0.02"), #this controls how the cut is made but not working yet
       conName = "Control",
       cex =  2, cex.axis=2, lty=c(1, 2), lwd=1, #size of: symbols, axis numbers, lines type, line thickness,  stuff
       xlim = c(0,32), #Changes,  must be the same in the row below  
       ylim =c(-0.5,1.2),
       xt = c(0, 2, 4, 8, 16), #Specify x numbers
       yt = c(-0.5, 0, 0.5, 1), #Specify y number
       xlab = expression("Cd concentration (" *mu ~ "M)"), line = 5, cex.lab =2,
       ylab = "Inhibition of growth rate",
       #legend = TRUE, legendPos = c(9, 1), legendText=c(i), cex.legend = 1.75, lwd.legend = 2,
       #legend = FALSE, legendPos = c(1.75, 1), legendText=c("1.5h", "3h", "5h"), cex.legend = 2, lwd.legend = 2,
       main = title(main = c(i), adj = 0.05, line = -2.5, cex.main = 2))
  #plot(bestDoseresponse_all.WB, col = "#800000", type = "confidence", xlim = c(7,18), lty=c(0), add = TRUE)
  plot(bestDoseresponse_GP.WB, col = "#008000", type = "confidence", xlim = c(1,16), lty=c(0), add = TRUE)
  dev.copy(pdf, output)
  dev.off()
  

}

#colors
#TB = "#2846FF"
#SM = "#008000"