#Histogramas
x <- c(27772, 19014, 14286, 11651, 12911, 7464, 7000, 6418, 
                                4571, 5891, 5012, 6207, 7893, 6947, 7536, 10129)
aux = min(x)+ (max(x) - min(x))*(0:6)/6

d = hist(x, breaks=aux,right=T, ylab="FREQUÊNCIA ABSOLUTA",
                   main="DESMATAMENTO EM KM2", xlab="KM2", col="#0E4C3E",
                                           ylim=c(0,12),axes=F, xlim=c(0,30000))
axis(1, c(0,aux),round(c(0,aux),1))
axis(2)
text(d$mids,d$counts+0.5, paste(round(d$density*(max(x) - min(x))/6*100,2), 
                                                                   "%", sep=""))

d = hist(x, breaks=aux,right=T, prob=T, ylab="DENSIDADE DE FREQUÊNCIA",
                      main="DESMATAMENTO EM KM2", xlab="KM2",col="#0E4C3E",
                                       ylim=c(0,0.0002),axes=F, xlim=c(0,30000))
axis(1, c(0,aux),round(c(0,aux),1))
axis(2)
text(d$mids,d$density+0.5e-5, paste(round(d$density*(max(x) - min(x))/6*100,2),
                                                                   "%", sep=""))
