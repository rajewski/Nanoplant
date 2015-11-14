#Read in the file ignoring the second line of units
library(data.table)

#Enter the names of the control and nanoceria files in quotes separated by commas
controls <- rbind("Ctrl2_light_ci.csv", "Ctrl3_light_ci.csv")
nanoceria <- rbind("nc2_light_ci.csv", "nc3_light_ci.csv")

#Get the headers from the first file on the list
header <- scan(controls[1], nlines=1,sep=";",quote="\"", what=character())
units <- c("yyyy-mm-dd hh:mm:ss", scan(controls[1], nlines=1, skip=1, sep=";", what=character(), encoding="latin1")[3:48])

#read in all the control files
data <- rbindlist(lapply(controls, fread, header=F, skip=2, col.names=header, colClasses=c(V1="POSIXct", V2="POSIXct"), encoding="Latin-1", data.table=FALSE))

#Convert the two time columns to something that R can understand
data$DateTime <- as.POSIXct(paste(data$Date, data$Time))
data <- within(data, rm(Date))
data <- within(data, rm(Time))

#Maybe use the from before and just assume that it's always both curves and that the batch
# is the first sample?
#Create subsets of the data depending on what was recorded
#Make a sorted matrix to name and define the subsets for later curves
Value <- rbind(cbind(rep("LC"), which(data$Comment %in% "Light Curve")), cbind(rep("CC"),which(data$Comment %in% "CO2 Curve")),cbind("END",nrow(data)+1))
Value<-Value[rank(as.numeric(Value[,2])),]
Value<-Value[rank(as.numeric(Value[,2])),]  #Why do i have to do this twice???

#Loop through the locations of the CO2 and Light curves and put them into separate data sets
for (i in 1:((length(Value)/2)-1)) {
  assign(paste("Ctrl_", Value[i,1],ceiling(i/2), sep=""), data[Value[i,2]:(as.numeric(Value[i+1,2])-1),])
}

#Do it all again for the nanoceria datasets
data <- rbindlist(lapply(nanoceria, fread, header=F, skip=2, col.names=header, colClasses=c(V1="Date"), encoding="Latin-1", data.table=FALSE))
data$DateTime <- as.POSIXlt(paste(data$Date, data$Time))
data <- within(data, rm(Date))
data <- within(data, rm(Time))
Value <- rbind(cbind(rep("LC"), which(data$Comment %in% "Light Curve")), cbind(rep("CC"),which(data$Comment %in% "CO2 Curve")),cbind("END",nrow(data)+1))
Value<-Value[rank(as.numeric(Value[,2])),]
Value<-Value[rank(as.numeric(Value[,2])),]  #Why do i have to do this twice???
for (i in 1:((length(Value)/2)-1)) {
  assign(paste("NC_", Value[i,1],ceiling(i/2), sep=""), data[Value[i,2]:(as.numeric(Value[i+1,2])-1),])
  rm(i)
}

#Clean up your mess
rm(nanoceria)
rm(controls)
rm(data)

#Create bins for the given CO2 level
#####Change the break points if we measure at new levels
co2breaks <- c(0, 51, 110, 210, 310, 420, 620, 820, 1020)
CO2Curve$co2bins <- cut(CO2Curve$CO2abs,breaks=co2breaks, labels=c("50","100", "200", "300", "400", "600", "800", "1000"))
rm(co2breaks)

#Create bins for the PAR levels
#####Same as above, change this if you change the measurements
lightbreaks <- c(0, 10, 60, 110, 160, 210, 310, 410, 610, 910, 1210, 1610, 2010)
LightCurve$PARbin <- cut(LightCurve$PARtop, breaks=lightbreaks, labels=c("0","50", "100", "150", "200", "300", "400", "600", "900", "1200", "1600", "2000"))
rm(lightbreaks)

#Some Plotting Examples
attach(CO2Curve)

#Plot the time against the carbon assimilation
plot(Time,A)
plot(Time,A,xlim=rev(range(Time))) #same but reversed time to show a 'typical' curve

#Example of how to narrow onto a time point
plot(Time,A, xlim=as.POSIXct(c("2015-11-10 16:31:00","2015-11-10 16:43:00")))

#Plot the potentially more informative ci against the assimilation
plot(ci,A, xlab="Intracellular [CO2]", ylab="Carbon Assimilation")
title(main="Carbon Assimilation Curve \n (Control Plant)")

#Examples of plotting a graph with both control and exp plant
#Note: this requires that you have two data sets imported, this will NOT work as a basic sript
#In these examples the data frames above are given the prefix "Ctl" for control plants and "NC" for nanoceria plants


#Plot A vs ci
plot(CtlCO2Curve$ci,CtlCO2Curve$A, xlab="Intracellular [CO2]", ylab="Carbon Assimilation", pch=1, col="black")
par(new=T)
plot(NCCO2Curve$ci,NCCO2Curve$A, pch=1, col="red",axes=F, ylab="", xlab="")
title(main="Carbon Assimilation Curve")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#plot A vs PAR
plot(CtlLightCurve$PARtop,CtlLightCurve$A, xlab="PAR", ylab="Carbon Assimilation", pch=1, col="black")
par(new=T)
plot(NCLightCurve$PARtop,NCLightCurve$A, pch=1, col="red",axes=F, ylab="", xlab="")
title(main="Carbon Assimilation Curve")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#Plot Yield vs PAR
plot(CtlLightCurve$PARtop,CtlLightCurve$Yield, xlab="PAR", ylab="Yield", pch=1, col="black")
par(new=T)
plot(NCLightCurve$PARtop,NCLightCurve$Yield, pch=1, col="red",axes=F, ylab="", xlab="")
title(main="Yield vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#qp vs PAR
plot(CtlLightCurve$PARtop,CtlLightCurve$qP, xlab="PAR", ylab="qp", pch=1, col="black")
par(new=T)
plot(NCLightCurve$PARtop,NCLightCurve$qP, pch=1, col="red",axes=F, ylab="", xlab="")
title(main="qP vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)




