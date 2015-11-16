#Lets load some libraries well need later
library(data.table)
#prettier graphs can be made with the ggplot2 package
library(ggplot2)
library(broom)

#Enter the names of the control and nanoceria files in order in quotes separated by commas
controls <- rbind("Ctrl1_light_ci.csv","Ctrl2_light_ci.csv", "Ctrl3_light_ci.csv")
nanoceria <- rbind("nc1_light_ci.csv","nc2_light_ci.csv", "nc3_light_ci.csv")

#Combine Get a piared list of all files and clean up
allfiles <- rbind(controls, nanoceria)
rm(controls)
rm(nanoceria)

#Get the units from the first file on the list
units <- c("yyyy-mm-dd hh:mm:ss", scan(allfiles[1], nlines=1, skip=1, sep=";", what=character(), encoding="latin1")[3:48])

#read in all the control files
data <- rbindlist(lapply(allfiles, fread, header=FALSE, skip=2, col.names=scan(allfiles[1], nlines=1,sep=";",quote="\"", what=character()), colClasses=c(V1="POSIXct", V2="POSIXct"), encoding="Latin-1", data.table=FALSE))

#Convert the two time columns to something that R can plot
data$DateTime <- as.POSIXct(paste(data$Date, data$Time))
data <- within(data, rm(Date))
data <- within(data, rm(Time))

#Add on the names of the sample types and rep numbers
data <- cbind(data, File = as.vector(rep(sub("_.*$","",allfiles), sapply(lapply(allfiles, fread, skip=1), nrow))))
data$Type <- substring(data$File,1, nchar(data$File)-1)
data$Rep <- substring(data$File, nchar(data$File), nchar(data$File))
data <- within(data, rm(File))
rm(allfiles)

#Make a sorted matrix to help name and define the subsets for the curves
Value <- rbind(cbind(rep("LC"), which(data$Comment %in% "Light Curve")), cbind(rep("CC"),which(data$Comment %in% "CO2 Curve")),cbind("END",nrow(data)+1))
Value<-Value[sort.list(as.numeric(Value[,2])),]

#Delete the initial Zero point to get this to align
#Add in what type of curve it is
data<-data[-1,]
data<-cbind(data,Curve = rep(Value[1:12,1],as.vector(as.numeric(Value[-1,2])-as.numeric(Value[1:12,2]))))
rm(Value)

#Add in columns to bin the levels of light or CO2
data$CO2bins <- cut(data$CO2abs,breaks=c(0, 51, 110, 210, 310, 420, 620, 820, 1020), labels=c("50","100", "200", "300", "400", "600", "800", "1000"), include.lowest=TRUE)
data$PARbin <- cut(data$PARtop, breaks=c(0, 10, 60, 110, 160, 210, 310, 410, 610, 910, 1210, 1610, 2010), labels=c("0","50", "100", "150", "200", "300", "400", "600", "900", "1200", "1600", "2000"), include.lowest=TRUE)

#Code to deal with the stabilization of the PAR at high light
#Create a table of the values we want to remove
dat<-rbindlist(by(data[(PARbin ==2000) & (Curve=="LC"),], list(data[(PARbin ==2000) & (Curve=="LC"),]$Rep, data[(PARbin ==2000) & (Curve=="LC"),]$Type), head,-6))
dat <- data.table(dat, key="DateTime")
#Use a data.table trick to remove the intersect of these two data tables
#Since this is the only unique value for each row, lets make it our key
data <- data.table(data, key="DateTime")
data <-data[!dat]
rm(dat)
attach(data)

#Let's just do a quick two-tailed t-test to see if the Fv/Fm is different between the samples
#The Fv/Fm has the accents around it so that R won't interpret the / as a literal division
t<-tidy(t.test(`Fv/Fm`[(PARbin=="0") & (!is.na(Yield)) & (Type=="nc")], `Fv/Fm`[(PARbin=="0") & (!is.na(Yield)) & (Type=="Ctrl")]))

#Do a quick boxplot with ggplot of the Fv/Fm
ggplot(data[(PARbin=="0") & (!is.na(Yield)),], aes(x=Type, y=`Fv/Fm`)) +
  geom_boxplot(aes(fill=Type)) +
  ylab("") +
  xlab(paste("P-value:",as.character(round(t[5],4)))) +
  ggtitle("Fv/Fm") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) 
  

######Start Graphing!!!
##Let's make some plots of the raw data
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
plot(PARtop[Type=="nc" & Curve=="LC"],qP[Type=="nc" & Curve=="LC"], pch=1, col="red",axes=FALSE, ylab="", xlab="", xlim=c(0,2000), ylim=c(0,1))
title(main="qP vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.2,yjust=0)



##Instead of calculating stats for every graph, let's make a summary table for light curves and use that for later plotting
#Add yield
LCaggr <- cbind(aggregate(Yield~Type*PARbin, data, FUN=mean), YieldSD=aggregate(Yield~Type*PARbin, data, FUN=sd)[,3])
#Add qP
LCaggr <- cbind(LCaggr, qP=aggregate(qP~Type*PARbin, data, mean)[,3], qpSD=aggregate(qP~Type*PARbin,data,sd)[,3])
#Add A filtered for light curves only
LCaggr <- cbind(LCaggr, A=aggregate(A~Type*PARbin, data[(Curve=="LC"),], mean)[,3],ASD=aggregate(A~Type*PARbin, data[(Curve=="LC"),], sd)[,3])
#Add number of A measurements
LCaggr <- cbind(LCaggr, An=aggregate(A~Type*PARbin, data[(Curve=="LC"),], length)[,3])
#Add in the Fv/Fm measurement
#It's measured twice per plant, so the !is.na bit filters it so you don't get duplicates
LCaggr <- cbind(LCaggr, `Fv/Fm`=aggregate(`Fv/Fm`~Type*PARbin, data[!is.na(Yield)], mean)[,3])
LCaggr <- cbind(LCaggr, `Fv/FmSD`=aggregate(`Fv/Fm`~Type*PARbin, data[!is.na(Yield)], sd)[,3])
#Add in the stomatal conductance (GH20)
LCaggr <- cbind(LCaggr, GH2O=aggregate(GH2O~Type*PARbin, data[(Curve=="LC"),], mean)[,3])
LCaggr <- cbind(LCaggr, GH2OSD=aggregate(GH2O~Type*PARbin, data[(Curve=="LC"),], sd)[,3])
#Add n
LCaggr <- cbind(LCaggr, n=aggregate(Yield~Type*PARbin, data, length)[,3])

#Since we'll be dealing more with this aggregated data, lets attach the LCaggr dataset instead
detach(data)
attach(LCaggr)


#This will jitter overlapping error bars
pd <- position_dodge(20)

#use ggplot to get all these things fine tuned
ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=GH2O, colour=Type, group=Type)) +
  xlab("PAR") +
  geom_errorbar(aes(ymin=GH2O-(GH2OSD/sqrt(n)), ymax=GH2O+(GH2OSD/sqrt(n))), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  ggtitle("Stomatal Conductance")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 
