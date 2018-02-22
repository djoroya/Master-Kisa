
library(bnlearn)
library(gRbase)
library(RBGL)
rm(list=ls())
graphics.off()
dag.study<-dag("1","2","7",c("3","1","2"),c("4","2","7"),c("5","3","4"),c("6","3"))
plot(dag.study)
# _____________________________________________________________________________________________
# Problema 3.1 Empleando las funciones que se han presentado en el tutorial, implementar una 
# funcion que implemente el metodo de la d-separaci ́on para leer independencias condicionadas
# a partir del grafo
# _____________________________________________________________________________________________
# Implementación de d-separation
d.separation = function (dag,x1,x2,c){
  set <- c(c,x1,x2)
  dag.ancestral<- ancestralGraph(set=set,object=dag.study)
  moral.graph<-moralize(dag.ancestral)
  boolean <- separates(x1,x2,c,moral.graph)
  return(boolean)
}
# _____________________________________________________________________________________________
# Problema 3.2 Dado el DAG de la Figura 2:
# _____________________________________________________________________________________________
# 
# ---------------------------------------------------------
# Comprobar si se verifican las independencias (1; 4|5), 
# (1; 5|3, 4), (6; 7|3), (1; 7|5), (1; 4|6) y (1; 4|2, 3)
# ----------------------------------------------------------
# (1; 4|5)
d.separation(dag.study,"1","4","5")
# (1; 5|3, 4)
d.separation(dag.study,"1","5",c("3","4"))
#(6; 7|3)
d.separation(dag.study,"6","7",c("3"))
# (1; 7|5)
d.separation(dag.study,"1","7",c("5"))
# (1; 4|6)  
d.separation(dag.study,"1","4",c("6"))
# (1; 4|2, 3)
d.separation(dag.study,"1","4",c("2","3"))
#
# ---------------------------------------------------------
# Que conjunto(s) de variables C verifican (1; 7|C ) y 
# (1; 5|C )
# ---------------------------------------------------------
#
set.search<-function(dag.study,vars){
  list <- graph::nodes(dag.study) 
  for (c in vars){
    list <- list[list != c]
  }
  result <- list()
  index = 0
  for (iter in 1:length(list)){
    conjunto <- combn(list,iter)
    boolean.list <- apply(conjunto,2,function (x) d.separation(dag.study,vars[1],vars[2],x))
    index=index+1
    result[[index]]=conjunto[,boolean.list==TRUE]
  }
  return(result)
}
# (1; 7|C ) -
vars <- c("1","7")
set.1.7 <-set.search(dag.study,vars)
vars <- c("1","5")
set.1.5 <-set.search(dag.study,vars)
