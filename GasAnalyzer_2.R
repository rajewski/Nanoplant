<<<<<<< HEAD
#Read in the file ignoring the second line of units
library(data.table)
=======
#Lets load some libraries well need later
<<<<<<< HEAD
library(data.table) #to organize data better
library(ggplot2) #to make pretty graphs
library(broom) #to make tidy stats outputs
library(lattice) #better organized plots
library(chron) #work with dates
=======
library(data.table)
#prettier graphs can be made with the ggplot2 package
library(ggplot2)
library(broom)
>>>>>>> 8c527ef22acec1bfd79b98eed959d653d0954d8f
>>>>>>> c9ca5fcabb34efea2c6565047af8e096d6a572aa

#Enter the names of the control and nanoceria files in order in quotes separated by commas
controls <- rbind("Ctrl1_light_ci.csv","Ctrl2_light_ci.csv", "Ctrl3_light_ci.csv", "Ctrl4_light_ci.csv", "Ctrl5_light_ci.csv", "Ctrl6_light_ci.csv")
nanoceria <- rbind("NC1_light_ci.csv","NC2_light_ci.csv", "NC3_light_ci.csv", "NC4_light_ci.csv", "NC5_light_ci.csv", "NC6_light_ci.csv")

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

##Add in what type of curve it is
#Delete the initial Zero point to get this to align
data<-data[-1,]
#Use the value matrix to add a column where the curve name is repeated
#calculate the number of times to repeat by substrcting the positions of the curve names in the data data frame
data<-cbind(data,Curve = rep(Value[1:(length(Value[,1])-1),1],as.vector(as.numeric(Value[-1,2])-as.numeric(Value[1:(length(Value[,1])-1),2]))))
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
<<<<<<< HEAD

=======
<<<<<<< HEAD

attach(data)

=======
>>>>>>> c9ca5fcabb34efea2c6565047af8e096d6a572aa
attach(data)
######Start Graphing!!!

#Let's just do a quick two-tailed t-test to see if the Fv/Fm is different between the samples
#The Fv/Fm has the accents around it so that R won't interpret the / as a literal division
t<-tidy(t.test(`Fv/Fm`[(PARbin=="0") & (!is.na(Yield)) & (Type=="NC")], `Fv/Fm`[(PARbin=="0") & (!is.na(Yield)) & (Type=="Ctrl")]))

#Do a quick boxplot with ggplot of the Fv/Fm
ggplot(data[(PARbin=="0") & (!is.na(Yield)),], aes(x=Type, y=`Fv/Fm`)) +
  geom_boxplot(aes(fill=Type)) +
  ylab("") +
  xlab(paste("P-value:",as.character(round(t[5],4)))) +
  ggtitle("Fv/Fm") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) 
  

<<<<<<< HEAD
=======
######Start Graphing!!!
>>>>>>> 8c527ef22acec1bfd79b98eed959d653d0954d8f
>>>>>>> c9ca5fcabb34efea2c6565047af8e096d6a572aa
##Let's make some plots of the raw data
#Plot the ci against the assimilation
plot(ci[Curve=="CC"], A[Curve=="CC"], xlab="Intracellular [CO2]", ylab="Carbon Assimilation", col=factor(Type))
title(main="Assimilation Curve")
legend("bottomright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n", x.intersp=.3, y.intersp=.5, yjust=0)

#Plot PAR vs Assimilation
plot(PARtop[Curve=="LC"], A[Curve=="LC"], xlab="PAR", ylab="Carbon Assimilation", col=factor(Type))
title(main="Carbon Assimilation Curve")
legend("bottomright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.5,yjust=0)

#Plot Yield vs PAR
plot(PARtop[Curve=="LC"], Yield[Curve=="LC"], xlab="PAR", ylab="Yield", col=factor(Type))
title(main="Yield vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.5,yjust=0)

#qp vs PAR
plot(PARtop[Curve=="LC"], qP[Curve=="LC"], xlab="PAR", ylab="qP", col=factor(Type))
title(main="qP vs. PAR")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.5,yjust=0)

<<<<<<< HEAD
#GH2O vs PAR
plot(PARtop[Curve=="LC"], GH2O[Curve=="LC"], xlab="PAR", ylab="GH2O", col=factor(Type))
title(main="Stomatal Conductance")
legend("topright",c("Control","Nanoceria"),pch=1, col=c("black","red"),bty="n",x.intersp=.3, y.intersp=.5,yjust=0)
=======
<<<<<<< HEAD
##Those were great, but now lets start dealing with averaged data
#Plot the averaged values for the Yield vs. PAR
with(aggregate(Yield~PARbin*Type, data, mean), plot(as.numeric(levels(PARbin))[PARbin],Yield, xlab="PAR",col=factor(Type)))
title(main="Yield vs. PAR")

##Instead of calculating stats for every graph, let's make a summary table for light curves
=======
>>>>>>> c9ca5fcabb34efea2c6565047af8e096d6a572aa


#use ggplot to get all these things fine tuned
#Plot the stomatal conductance
ggplot(data[Curve=="LC"], aes(x=as.numeric(as.character(PARbin)), y=GH2O, colour=Type, group=Type)) +
  xlab("PAR") +
  geom_point(position=pd) +
  geom_line(position=pd) +
  facet_grid(.~Rep) +
  ggtitle("Stomatal Conductance")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 


##Those were great, but now lets start dealing with averaged data
#Plot the averaged values for the Yield vs. PAR
with(aggregate(Yield~PARbin*Type, data, mean), plot(as.numeric(levels(PARbin))[PARbin],Yield, xlab="PAR",col=factor(Type)))
title(main="Yield vs. PAR")

##Instead of calculating stats for every graph, let's make a summary table for light curves and use that for later plotting
>>>>>>> 8c527ef22acec1bfd79b98eed959d653d0954d8f
#Add yield
LCaggr <- cbind(aggregate(Yield~Type*PARbin, data, FUN=mean), YieldSD=aggregate(Yield~Type*PARbin, data, FUN=sd)[,3])
#Add qP
LCaggr <- cbind(LCaggr, qP=aggregate(qP~Type*PARbin, data, mean)[,3], qpSD=aggregate(qP~Type*PARbin,data,sd)[,3])
#Add A filtered for light curves only
LCaggr <- cbind(LCaggr, A=aggregate(A~Type*PARbin, data[(Curve=="LC"),], mean)[,3],ASD=aggregate(A~Type*PARbin, data[(Curve=="LC"),], sd)[,3])
#Add number of A measurements
LCaggr <- cbind(LCaggr, An=aggregate(A~Type*PARbin, data[(Curve=="LC"),], length)[,3])
<<<<<<< HEAD
#Add n
LCaggr <- cbind(LCaggr, n=aggregate(Yield~Type*PARbin, data, length)[,3])
=======
#Add in the Fv/Fm measurement
#It's measured twice per plant, so the !is.na bit filters it so you don't get duplicates
LCaggr <- cbind(LCaggr, `Fv/Fm`=aggregate(`Fv/Fm`~Type*PARbin, data[!is.na(Yield)], mean)[,3])
LCaggr <- cbind(LCaggr, `Fv/FmSD`=aggregate(`Fv/Fm`~Type*PARbin, data[!is.na(Yield)], sd)[,3])
#Add in the stomatal conductance (GH20)
LCaggr <- cbind(LCaggr, GH2O=aggregate(GH2O~Type*PARbin, data[(Curve=="LC"),], mean)[,3])
LCaggr <- cbind(LCaggr, GH2OSD=aggregate(GH2O~Type*PARbin, data[(Curve=="LC"),], sd)[,3])
#Add n
LCaggr <- cbind(LCaggr, n=aggregate(Yield~Type*PARbin, data, length)[,3])
write.csv(LCaggr, file="Light Curve Summary Data.csv", row.names=FALSE)
#Since we'll be dealing more with this aggregated data, lets attach the LCaggr dataset instead
detach(data)
attach(LCaggr)

#This will jitter overlapping error bars
pd <- position_dodge(20)

#use ggplot to get all these things fine tuned
#Plot the stomatal conductance
ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=GH2O, colour=Type, group=Type)) +
  xlab("PAR") +
  geom_errorbar(aes(ymin=GH2O-GH2OSD, ymax=GH2O+GH2OSD), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  facet_grid(.~Rep) +
  ggtitle("Stomatal Conductance")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 
<<<<<<< HEAD


#Let's use lattice to get some nice trellis graphs by rep
#make a scale for the color that matches
my.settings <- list(superpose.symbol=list(col=c("black", "red"), fill=c("black", "red")))
RepAxPAR <- xyplot(A~PARtop|Rep, groups=Type, par.settings=my.settings, auto.key=TRUE, data=data[(Curve=="LC")], main="Assimilation by Rep")
RepYieldxPAR <- xyplot(Yield~PARtop|Rep, groups=Type, par.settings=my.settings, auto.key=TRUE, data=data[(Curve=="LC")], main="Yield by Rep")
RepGH2OxPAR <- xyplot(GH2O~PARtop|Rep, groups=Type, par.settings=my.settings, auto.key=TRUE, data=data[(Curve=="LC")], main="Conductance by Rep")
RepqPxPAR <- xyplot(qP~PARtop|Rep, groups=Type, par.settings=my.settings, auto.key=TRUE, data=data[(Curve=="LC")], main="qP by Rep")

#ok now switch to the aggregated data
detach(data)
attach(LCaggr)

TypeGH2OxPAR <- ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=GH2O, colour=Type, group=Type)) +
  xlab("PAR") +
  geom_errorbar(aes(ymin=GH2O-GH2OSD, ymax=GH2O+GH2OSD), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  ggtitle("Stomatal Conductance")+
  scale_colour_manual(values = c("black","red"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 

TypeAxPAR <- ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=A, colour=Type, group=Type)) +
  xlab("PAR") +
  ylab("Assimilation") +
  geom_errorbar(aes(ymin=A-ASD, ymax=A+ASD), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  ggtitle("Assimilation vs PAR")+
  scale_colour_manual(values = c("black","red"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 

TypeYieldxPAR <- ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=Yield, colour=Type, group=Type)) +
  xlab("PAR") +
  ylab("Yield") +
  geom_errorbar(aes(ymin=Yield-YieldSD, ymax=Yield+YieldSD), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  ggtitle("Yield vs PAR")+
  scale_colour_manual(values = c("black","red"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 

TypeqPxPAR <- ggplot(LCaggr, aes(x=as.numeric(as.character(PARbin)), y=qP, colour=Type, group=Type)) +
  xlab("PAR") +
  ylab("qP") +
  geom_errorbar(aes(ymin=qP-qpSD, ymax=qP+qpSD), width=20, position=pd) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  ggtitle("qP vs PAR")+
  scale_colour_manual(values = c("black","red"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line("lightgray"),
        panel.background = element_blank()) 

grid.arrange(RepYieldxPAR, TypeYieldxPAR, ncol=2)
grid.arrange(RepqPxPAR, TypeqPxPAR, ncol=2)
grid.arrange(RepAxPAR, TypeAxPAR, ncol=2)
grid.arrange(RepGH2OxPAR, TypeGH2OxPAR, ncol=2)


###Example to change color of lattice graphs
my.settings <- list(superpose.symbol=list(col=c("black", "red"), fill=c("black", "red")))
=======
>>>>>>> 8c527ef22acec1bfd79b98eed959d653d0954d8f
>>>>>>> c9ca5fcabb34efea2c6565047af8e096d6a572aa
