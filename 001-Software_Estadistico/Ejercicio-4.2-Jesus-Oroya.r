# Ejercicio 4.1

# Exportamos la libreria MASS, necesaria para tener la data.frame
# biopsy.

library(MASS)
head(biopsy)

# # =================
# # Preprocesamiento.
# # =================
#
#
# 1. ¿Cuáles son los objetos a los que les faltan datos?
#-------------------------------------------------------------------------
#
# 	Definimos la variable ID_NA, para guardar los indices de filas que 
# 	tiene algun dato no informado
#
ID_NA <- c()

for (fila in 1:length(biopsy$ID)){
    if ('NA' %in% biopsy[fila,]){
        ID_NA <- c(ID_NA,fila)
    }
}
#
#	Los objetos que les faltan datos son:
#
biopsy[ID_NA,]
#
# 2. Define otro data.frame con los objetos que tienen los datos completos.
#--------------------------------------------------------------------------
#
biopsy_noNA <- biopsy[-ID_NA,]
#
head(biopsy_noNA)
#
#
# # =================================================================================
# # =================================================================================
# # Análisis descriptivo univariante
# # =================================================================================
# # =================================================================================
#
# 1. ¿Cuántas modalidades tiene cada una de las variables?
#----------------------------------------------------------
#
#	Imprimimos cuantos distintos valores pueden tomar cada una de las variables 
#
lenb <- length(biopsy_noNA)
for (col in 1:lenb){
    cat(names(biopsy_noNA)[col],"-",length(unique(biopsy_noNA[,col])),"\n")
}
#
#	Podemos ver que las variables V1-V8 tomas 10 distintos valores.
#	La variable V9 toma 9 distintos valores, mientras que la variable
#	class solo toma 2. 
#	Por otra parte, como el ID es simplemente identificativo para cada objeto
#	podemos saber que tenemos 630 objetos.
#
#
# 2.¿Cuál es el tipo de gráfico más apropiado para representar la variabilidad
#  	de las 9 variables?
#------------------------------------------------------------------------------
#
#	La variabilidad de las nueves variables se puede ver en un histograma. 
# 	Reliazaremos esta gráficas en el punto 5.
#
#
#
# 3. Todas las variables, salvo una, son cuantitativas de carácter ordinal. 
#	 Calcula los estadísticos que creas oportuno para describir su tendencia 
#	 central y su dispersión.
#------------------------------------------------------------------------------
#
# Aplicando la funcción mean y sd al objeto biopsy_noNA.  
# 
mean <- apply(as.matrix(biopsy_noNA[,2:10]),2,mean)
sd   <- apply(as.matrix(biopsy_noNA[,2:10]),2,sd)
estadisticos <- cbind(mean,sd)
estadisticos
#
#
# 4. Describe la variable cualitativa.
#------------------------------------------------------------------------------
#
summary(biopsy_noNA[11])
#
# 
# 5. Haz comentarios sobre el grado de variabilidad de las variables.
#------------------------------------------------------------------------------
#
# Primero hacemos las gráficas con ayuda de la información obtenida en 
# este apartado
#
pdf("Jesus-Oroya-Ejercicio-4.2-Analisis-Univariante-5.pdf")
for (var in 2:10){
    hist(unlist(biopsy_noNA[var]),xlab=names(biopsy_noNA[var]),
         main=paste("Histograma de ",names(biopsy_noNA)[var]),
         freq = FALSE,ylim=c(0,1.1))
    lines(density(unlist(biopsy_noNA[var]),kernel = c("gaussian")), 
    	col = "red", lty = 2, lwd = 3)
    tex <- paste("Media",round(estadisticos[var-1,1],2),"\n"
                 ,"Desvisación",round(estadisticos[var-1,2],2))                
    text(5,0.8,tex)
}
dev.off()
# 
# Podemos ver que en general las dispersión de las variables es grande
# En general la desviación es alrededor de 3, con excepción de V9 que 
# tiene el valor 1.73.
#
#
# # =================================================================================
# # =================================================================================
# # Análisis descriptivo bivariante. Asociaciones entre las variables cuantitativas.
# # =================================================================================
# # =================================================================================
#
#
# 1. Representa gráficamente las relaciones entre las variables cuantitativas ordinales.
#------------------------------------------------------------------------------

#
pdf("Jesus-Oroya-Ejercicio-4.2-Analisis-Univariante-relaciones_V1-V9.pdf")
#
grid <- combn(c(2:10),2)
#
for (fila in 1:length(grid[1,])){
    plot(biopsy_noNA[,grid[1,fila]],biopsy_noNA[,grid[2,fila]],
         xlab=names(biopsy_noNA)[grid[1,fila]],
         ylab=names(biopsy_noNA)[grid[2,fila]]
        )
}

dev.off()
#
# 2. Calcula las correlaciones entre las variables cuantitativas ordinales (ten en cuenta 
#	 que hay una variable con datos faltantes).
#------------------------------------------------------------------------------
#
grid <- t(grid <- combn(c(2:10),2))
correlation <- function(v){cor(biopsy_noNA[,v[1]],biopsy_noNA[,v[2]], method="pearson")}
df.corr <- cbind(grid,apply(grid,1,FUN=correlation))
df.corr.names <- cbind(names(biopsy)[df.corr[,1]],names(biopsy)[df.corr[,2]],df.corr[,3])
#
head(df.corr.names)
#
tail(df.corr.names)
#
# 3. ¿Cuál son las dos variables cuantitativas más correladas?
#------------------------------------------------------------------------------
# 
df.corr.names[sort(df.corr.names[,3],index.return =TRUE,decreasing=TRUE )$ix,][1,]
#
#
# # =================================================================================
# # =================================================================================
# # Análisis descriptivo bivariante. Asociaciones entre las variables cuantitativas 
# # 	y la variable cualitativa 
# # =================================================================================
# # =================================================================================
#
#
# 1. Representa gráficamente las relaciones entre las variables cuantitativas y 
# 	 la variable cualitativa.
#------------------------------------------------------------------------------
#
pdf("Jesus-Oroya-Ejercicio-4.2-Analisis-Univariante-relaciones_histo-cuanti-cuali.pdf")
for (index in 2:10){
    class <- as.factor(biopsy_noNA[,11])
    v <- biopsy_noNA[,index]
    prop.table(table(class,v))
    barplot(prop.table(table(class,v)),beside=T,col=c("white","red"),
           xlab=names(biopsy_noNA)[index],ylab="Densidad",ylim=c(0,0.7))
    legend("topright", c("benign", "malignant"), col = c("white", "red"),
            lty = c(-1, -1), pch = c(1, 1),
            bg = "gray")
}
dev.off()
#
# 2. Calcula las correlaciones entre las variables cuantitativas y las variable 
# 	 cualitativa (ten en cuenta que hay una variable con datos faltantes).
#
#------------------------------------------------------------------------------
#
# Definimos razon.cor
razon.cor <- function(index){
    x <- biopsy_noNA[,index]
    factor <- biopsy_noNA[,11]
    niv <- levels(factor)
    numniv <- length(niv)
    SSB <- 0
     for(i in 1:numniv){
       xx <- x[factor==niv[i]]
       nxx <- length(xx)
       SSB <- SSB + nxx*(mean(xx)-mean(x))^2
     }
     SST <- (length(x)-1)*var(x)
     eta2 <-      SSB/SST
     return(eta2)
}
#
#
grid <- t(t(2:10))
df.corr <- cbind(grid,apply(grid,1,razon.cor))
df.corr.names <- cbind(names(biopsy)[df.corr[,1]],df.corr[,2])

#
# 3. ¿Con qué variable cuantitativa está más correlacionada la variable cualitativa?
#
#------------------------------------------------------------------------------
#
index <- sort(df.corr.names[,2],decreasing=TRUE,index.return = TRUE)
df.corr.names[index$ix[1],]
