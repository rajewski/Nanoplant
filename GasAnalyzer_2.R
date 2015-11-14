#Read in the file ignoring the second line of units
library(data.table)

#Enter the names of the control and nanoceria files in quotes separated by commas
controls <- rbind("Ctrl1_light_ci.csv","Ctrl2_light_ci.csv", "Ctrl3_light_ci.csv")
nanoceria <- rbind("nc1_light_ci.csv","nc2_light_ci.csv", "nc3_light_ci.csv")

#Combine Get a piared list of all files and clean up
allfiles <- rbind(controls, nanoceria)
rm(controls)
rm(nanoceria)

#Get the units from the first file on the list
units <- c("yyyy-mm-dd hh:mm:ss", scan(allfiles[1], nlines=1, skip=1, sep=";", what=character(), encoding="latin1")[3:48])

#read in all the control files
data <- rbindlist(lapply(allfiles, fread, header=F, skip=2, col.names=scan(allfiles[1], nlines=1,sep=";",quote="\"", what=character()), colClasses=c(V1="POSIXct", V2="POSIXct"), encoding="Latin-1", data.table=FALSE))

#Convert the two time columns to something that R can understand
data$DateTime <- as.POSIXct(paste(data$Date, data$Time))
data <- within(data, rm(Date))
data <- within(data, rm(Time))

#Add on the names of the samples
data <- cbind(data, File = as.vector(rep(sub("_.*$","",allfiles), sapply(lapply(allfiles, fread, skip=1), nrow))))
data$Type <- substring(data$File,1, nchar(data$File)-1)
data$Rep <- substring(data$File, nchar(data$File), nchar(data$File))
data <- within(data, rm(File))

#Make a sorted matrix to name and define the subsets for later curves
Value <- rbind(cbind(rep("LC"), which(data$Comment %in% "Light Curve")), cbind(rep("CC"),which(data$Comment %in% "CO2 Curve")),cbind("END",nrow(data)+1))
Value<-Value[sort.list(as.numeric(Value[,2])),]

#Delete the initial Zero point to get this to align
#Add in what type of curve it is
data<-data[-1,]
data<-cbind(data,Curve = rep(Value[1:12,1],as.vector(as.numeric(Value[-1,2])-as.numeric(Value[1:12,2]))))
rm(Value)

attach(data)

#Plot the ci against the assimilation
plot(ci[Curve=="CC"], A[Curve=="CC"], xlab="Intracellular [CO2]", ylab="Carbon Assimilation", col=factor(Type))
title(main="Carbon Assimilation Curve")


#Plot A vs ci
plot(ci[Type=="Ctrl" & Curve=="CC"], A[Type=="Ctrl" & Curve=="CC"], xlab="Intracellular [CO2]", ylab="Carbon Assimilation", col="red", ylim=c(0,40), xlim=c(0,750))
par(new=T)
plot(ci[Type=="nc" & Curve=="CC"], A[Type=="nc" & Curve=="CC"], xlab="", ylab="", axes=F, col="black", ylim=c(0,40), xlim=c(0,750))
title(main="Carbon Assimilation Curve")
legend("bottomright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#plot A vs PAR
plot(PARtop[Type=="Ctrl" & Curve=="LC"],A[Type=="Ctrl" & Curve=="LC"], xlab="PAR", ylab="Carbon Assimilation", pch=1, col="black", xlim=c(0,2000), ylim=c(0,35))
par(new=T)
plot(PARtop[Type=="nc" & Curve=="LC"],A[Type=="nc" & Curve=="LC"], pch=1, col="red",axes=F, ylab="", xlab="", xlim=c(0,2000), ylim=c(0,35))
title(main="Carbon Assimilation Curve")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#Plot Yield vs PAR
plot(PARtop[Type=="Ctrl" & Curve=="LC"],Yield[Type=="Ctrl" & Curve=="LC"], xlab="PAR", ylab="Yield", pch=1, col="black", xlim=c(0,2000), ylim=c(0,.5))
par(new=T)
plot(PARtop[Type=="nc" & Curve=="LC"],Yield[Type=="nc" & Curve=="LC"], pch=1, col="red",axes=F, ylab="", xlab="", xlim=c(0,2000), ylim=c(0,.5))
title(main="Yield vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)

#qp vs PAR
plot(PARtop[Type=="Ctrl" & Curve=="LC"],qP[Type=="Ctrl" & Curve=="LC"], xlab="PAR", ylab="qp", pch=1, col="black", xlim=c(0,2000), ylim=c(0,1))
par(new=T)
plot(PARtop[Type=="nc" & Curve=="LC"],qP[Type=="nc" & Curve=="LC"], pch=1, col="red",axes=F, ylab="", xlab="", xlim=c(0,2000), ylim=c(0,1))
title(main="qP vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)
