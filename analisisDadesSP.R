#proces sobre poisons crisis i derivats
library(ggplot2)
library(lubridate)
library(dplyr)
setwd('/home/toni/Projectes/R/simulaBolsa/')
#dadesBolsa <- read.csv(file="data/GSPC.csv", header=TRUE, sep=",")
#plot(y = dadesBolsa$Open,x = dadesBolsa$Date)



#dades agafades de http://www.cboe.com/micro/buywrite/dailypricehistory.xls

#per tenir les dates be
# Sys.setlocale("LC_TIME", "en_US.UTF-8")

# dadesBolsa <- read.csv(file="data/pujadesSP.csv", header=TRUE, sep=";")
dadesBolsa <- read.csv(file="data/dailypricehistorySP500.csv", header=TRUE, sep=";")

dadesBolsa$DATE<-as.Date(dadesBolsa$DATE,"%d/%m/%Y")
dadesBolsa$SP500<-as.numeric(dadesBolsa$SP500)
dadesBolsa$VAR<-as.numeric(dadesBolsa$VAR)
ggplot(dadesBolsa, aes(VAR)) + geom_density()
ggplot(dadesBolsa, aes(DATE,SP500)) + geom_line()

#te mes tendencia a pujades fortes que a baixades fortes
dadesBolsa[dadesBolsa$VAR>=8,]
dadesBolsa[dadesBolsa$VAR<=-8,]

#summary
summary(dadesBolsa$VAR)
var(dadesBolsa$VAR)
sd(dadesBolsa$VAR)
#passo a mensual
dadesBolsa$month <-month(dadesBolsa$DATE)
dadesBolsa$year <-year(dadesBolsa$DATE)
dadesBolsaMensual<-dadesBolsa%>%group_by(year,month) %>%summarise(n(),SP500=first(SP500),DATE=first(DATE))

#####Ojo que esta malament la diferencia
dadesBolsaMensual$VAR<-0
for(i in 1:(nrow(dadesBolsaMensual)-1)){
    dadesBolsaMensual$VAR[i+1] <- 100*(dadesBolsaMensual$SP500[i+1]-dadesBolsaMensual$SP500[i])/dadesBolsaMensual$SP500[i+1]
}


#torno a dibuixar
ggplot(dadesBolsaMensual, aes(VAR)) + geom_density()
ggplot(dadesBolsaMensual, aes(DATE,SP500)) + geom_line()

dadesBolsaMensual[dadesBolsaMensual$VAR>=8,]
dadesBolsaMensual[dadesBolsaMensual$VAR<=-8,]

