#proces sobre poisons crisis i derivats
library(ggplot2)
setwd('/home/toni/Projectes/R/simulaBolsa/')
#dadesBolsa <- read.csv(file="data/GSPC.csv", header=TRUE, sep=",")
#plot(y = dadesBolsa$Open,x = dadesBolsa$Date)



#faig un resum del que vull fer amb un exemple tonto
#molaria agafar de guia https://www.yardeni.com/pub/sp500corrbear.pdf

#per tenir les dates be
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# dadesBolsa <- read.csv(file="data/pujadesSP.csv", header=TRUE, sep=";")
dadesBolsa <- read.csv(file="data/baixadesSP.csv", header=TRUE, sep=";")
dadesBolsa$TroughDate<-as.Date(dadesBolsa$TroughDate,"%m/%d/%Y")
dadesBolsa$PeakDate<-as.Date(dadesBolsa$PeakDate,"%m/%d/%Y")
dadesBolsa$PercentLoss<-as.numeric(dadesBolsa$PercentLoss)
# dadesBolsa$PercentLoss2<-ifelse(dadesBolsa$PercentLoss)


# plot(x = dadesBolsa$Trough.Date,y = dadesBolsa$Trough.Price)
ggplot(dadesBolsa, aes(x=PeakDate,y = TroughPrice))+geom_line() 
ggplot(dadesBolsa, aes(x=TroughDate,y = TroughPrice))+geom_line() 
    

#En quan a la freq
for(i in 1:length(dadesBolsa$PeakDate)){
    dadesBolsa$crisis[i]<-dadesBolsa$PeakDate[i+1]-dadesBolsa$PeakDate[i]   
    dadesBolsa$Gain[i]<-dadesBolsa$PeakPrice[i+1]-dadesBolsa$TroughPrice[i] 
    dadesBolsa$periodePujades[i]<-dadesBolsa$TroughDate[i+1]-dadesBolsa$PeakDate[i] 
}


frecuCris<-mean(dadesBolsa$crisis[1:51])

hist(dadesBolsa$crisis,breaks = 15)
# 
# simu<-rpois(n = 1000, lambda = frecuCris)
# hist(simu,breaks = 30)

simu<-append(rnorm(n = 350,mean = 250,sd = 150),rnorm(n = 150,mean = 750,sd=200))
hist(simu,breaks = 30)


ggplot(subset(dadesBolsa[1:51,], PercentLoss<20),aes(crisis,fill=factor(PercentLoss)))+geom_histogram(binwidth = 365/4)
ggplot(subset(dadesBolsa[1:51,], PercentLoss>20),aes(crisis,fill=factor(PercentLoss)))+geom_histogram(binwidth = 365/4)
ggplot(subset(dadesBolsa[1:51,], PercentLoss<15),aes(x=crisis,NumberDays))+geom_point(aes(col=PercentLoss,size=PeakDate))
ggplot(subset(dadesBolsa[1:51,], PercentLoss>15),aes(x=crisis,NumberDays))+geom_point(aes(col=PercentLoss,size=PeakDate))
#En quan a la durada
hist(dadesBolsa$NumberDays,breaks = 15)

#En quan a la intensitat...
plot(y=dadesBolsa$PercentLoss,x=dadesBolsa$crisis)
plot(y=dadesBolsa$PercentLoss,x=dadesBolsa$NumberDays)
plot(y=dadesBolsa$PercentLoss,x=dadesBolsa$periodePujades)
plot(y=dadesBolsa$Gain,x=dadesBolsa$periodePujades)
ggplot(subset(dadesBolsa[1:51,], PercentLoss>15),aes(x=crisis,NumberDays))+geom_point(aes(col=PercentLoss,size=Gain))
ggplot(subset(dadesBolsa[1:51,]),aes(x=crisis,periodePujades))+geom_point(aes(col=PercentLoss,size=Gain))

