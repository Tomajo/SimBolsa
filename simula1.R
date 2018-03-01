library(dplyr)
library(rethinking)
set.seed(12)



simulaPut<-function(strike, valorPutOrigin,valorIndexOrigen,tendenciaMercat,lamb,mesosPut,mitja,sig){
    #simulo probabilitat de crack amb Poisson
    mesoscrack<-rpois(n = 1000000,lambda = lamb)
    
    #simulo baixada en % del crack
    intensitat<-rnorm(n = 1000000,mean = mitja,sd = sig)
    
    
    #calculo el valor del index quan peta, segueixo tendencia
    valorIndexActual<-valorIndexOrigen*(1+(mesoscrack*tendenciaMercat/100))
    
    #calculo la revalorització en funció del la magnitud del crack
    valorFinIndex<-valorIndexActual*(intensitat)
    valorFinIndex<-ifelse(valorFinIndex>0,valorFinIndex,0)
    
    #calculo benefici
    benefici<-(ifelse((strike-valorFinIndex)>0,strike-valorFinIndex,0))*100 - valorPutOrigin
    # benefici<-ifelse(benefici>0,benefici,0)
    # summary(benefici)
    # benefici<-benefici[benefici>0]
    
    # dens(benefici[benefici>0])
    return(summary(benefici))
    
    
}
#Tesla
# simulaPut(strike=230,valorPutOrigin=200,valorIndexOrigen=250,tendenciaMercat=1,lamb=24,mesosPut=6,mitja=1-0.5,sig=0.2)
#DIA
simulaPut(strike=197,valorPutOrigin=367,valorIndexOrigen=250,tendenciaMercat=1,lamb=24,mesosPut=11,mitja=1-0.3,sig=0.1)
# benefici
