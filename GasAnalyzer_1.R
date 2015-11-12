#Read in the file ignoring the second line of units
file <-"NC3_light_ci.csv"
seperator<- ","
header <- scan(file, nlines=1,sep=seperator, quote="\"", what=character())
data <- read.csv(file, header=F, skip=2,colClasses=c("Date"="character","Time"="character"), col.names=header, sep=seperator)
units <- scan(file, nlines=1, skip=1, sep=seperator, quote="\"", what=character(), encoding="latin1")

rm(header)

#Convert the two time columns to something that R can understand
data <- cbind(Time=as.POSIXlt(paste(data$Date, data$Time)), subset(data[c(-1,-2)]))

#Create subsets of the data depending on what was recorded
if(" Light Curve" %in% data$Comment & " CO2 Curve" %in% data$Comment){
  #Make a sorted matrix to name and define the subsets for later curves
  Value <- cbind(c("LightCurve","CO2Curve","END"),c(match(" Light Curve", data$Comment),match(" CO2 Curve", data$Comment),dim(data)[1]))
  Value <- Value[rank(as.numeric(Value[,2])),]
  assign(Value[1,1], data[Value[1,2]:(as.numeric(Value[2,2])-1),])
  assign(Value[2,1], data[Value[2,2]:Value[3,2],])
  rm(Value)
}
if(xor(" Light Curve" %in% data$Comment," CO2 Curve" %in% data$Comment)){
  #Make a sorted matrix to name and define the subsets for later curves
  Value <- cbind(c("LightCurve","CO2Curve","END"),c(match(" Light Curve", data$Comment, nomatch=0), match(" CO2 Curve", data$Comment, nomatch=0),dim(data)[1]))
  Value <- Value[rank(as.numeric(Value[,2])),]
  assign(Value[2,1], data[Value[2,2]:Value[3,2],])
  rm(Value)
}

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




