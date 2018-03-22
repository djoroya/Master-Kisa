
t=seq(-pi,pi,0.01)
xlim=1.5
ylim=0.5
# Curva parametrizada según Wikipedia
x = (sqrt(2)*cos(t))/(sin(t)**2+1)
y = (sqrt(2)*cos(t)*sin(t))/(sin(t)**2+1)
pdf("Ejercicio-2.2-Jesus-Oroya.pdf",width=9, height=5.5)
plot(x,y,xlim=c(-xlim,xlim),ylim=c(-ylim,ylim),axes=FALSE,
    main="Lemniscata",xlab=" ",ylab=" ",type="l")
lines(x,y,col="red",lwd=5)
dev.off()


pdf("Ejercicio-2.1-Jesus-Oroya.pdf",width=13, height=7.5)
titulo="Evolución del precio del cobre ($) \n en el periodo 1800-1997"
ylim=c(67,475)
xlim=c(1800,1997)
valores <- read.table(file="9.4.DAT")
cobre = cbind(1800:1997,valores[[2]])
par(cex.main=1.3,col.main="blue",col.lab="red",bty="l",las=1,font.main=4,cex.lab=1.1)
plot(cobre,type="l",col=c("red"),xlab="Año",ylab="",xlim=xlim,ylim=ylim,xaxt='n',yaxt='n')
mtext(2,text="Precio($) ",col="red",cex=1.1)
axis(1, at=seq(1800,2000,(2000-1800)/8))
axis(2, at=seq(67,475,(475-67)/3))
title(titulo, line = 0)
dev.off()

