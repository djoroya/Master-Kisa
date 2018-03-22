library(bnlearn)
library(gRain)
library(Rgraphviz)
library(ggplot2)
library(foreign)

rm(list = ls())
car<-read.arff("car.arff")
mushroom<-read.arff("mushroom.arff")
summary(car)
summary(mushroom)


## ----data_loading,prompt=TRUE,message=FALSE,cache=FALSE------------------
# net_bn<-hc(car)
# net_graphnel<-as(amat(net_bn),"graphNEL")
# net_grain<-grain(net_graphnel,data=car)
# summary(net_grain)
# 
# 
# ## ----manual_cration,prompt=TRUE,message=FALSE----------------------------
# yn<-c("NO","SI")
# s <- cptable(~S, values=c(50,50),levels=yn)
# j <- cptable(~J, values=c(25,75),levels=yn)
# c.sj <- cptable(~C+S+J, values=c(80,20,50,50,50,50,10,90),levels=yn)
# a.sc <- cptable(~A|S:C, values=c(95,5,50,50,25,75,5,95),levels=yn)
# plist<-compileCPT(list(s,j,c.sj,a.sc))
# net.study<-grain(plist)
# 
# 
# ## ----marginal_A,prompt=TRUE,message=FALSE--------------------------------
# querygrain(net.study,nodes="A",type="marginal")
# 
# 
# ## ----example_mush,prompt=TRUE,message=FALSE,fig.path="./figures/",fig.keep='all',fig.show='hide',tidy=FALSE----
# bn_mush<-hc(mushroom[,1:10])
# net_mush<-grain(as(amat(bn_mush),"graphNEL"),
#                 data=mushroom[,1:10],smooth=1/dim(mushroom)[1])
# plot(net_mush)
# 
# 
# ## ----example_mush_2,prompt=TRUE,message=FALSE,fig.path="./figures/",fig.keep='all',fig.show='hide'----
# net_mush_moral<-moralize(net_mush$dag)
# net_mush_triang<-triangulate(net_mush_moral)
# plot(net_mush_moral)
# plot(net_mush_triang)
# 
# 
# ## ----rip_ordering,prompt=TRUE,message=FALSE,eval=FALSE-------------------
# ## rip(net_mush_triang)
# ## plot(rip(net_mush_triang))
# 
# 
# ## ----compilation,prompt=TRUE,message=FALSE-------------------------------
# net_mush_compiled<-compile(net_mush)
# net_mush$isCompiled
# net_mush_compiled$isCompiled
# summary(net_mush_compiled)
# 
# 
# ## ----propagation,prompt=TRUE,message=FALSE-------------------------------
# net_mush_propagated<-propagate(net_mush_compiled)
# net_mush$isPropagated
# net_mush_propagated$isPropagated
# summary(net_mush_propagated)
# 
# 
# ## ----Using evidence,prompt=TRUE,message=FALSE----------------------------
# net_mush_propagated_ev<-setFinding(net_mush_propagated,nodes="BRUISES",states="t", propagate=F)
# net_mush_propagated_ev<-setEvidence(net_mush_propagated_ev,nodes="GILL_COL",states="n", propagate=F)
# net_mush_propagated_ev<-propagate(net_mush_propagated_ev)
# querygrain(net_mush_propagated,nodes="EDIBILITY",type="marginal")
# querygrain(net_mush_propagated_ev,nodes="EDIBILITY",type="marginal")
# 
# 
# ## ----Using evidence 2,prompt=TRUE,message=FALSE--------------------------
# net_mush_propagated_ev2<-retractEvidence(net_mush_propagated_ev,nodes=c("BRUISES","GILL_COL"), propagate=F)
# net_mush_propagated_ev2<-setEvidence(net_mush_propagated_ev2,nodes=c("CAP_COL","CAP_SHAPE"),states=c("w","b"), propagate=T)
# querygrain(net_mush_propagated_ev,nodes="EDIBILITY",type="marginal")
# querygrain(net_mush_propagated_ev2,nodes="EDIBILITY",type="marginal")
# 
# 
# ## ----marginal_A_exact,prompt=TRUE,message=FALSE--------------------------
# querygrain(net.study,nodes="A",type="marginal")
# 
# 
# ## ----echo=FALSE----------------------------------------------------------
# set.seed(666)
# 
# 
# ## ----marginal_A_aprox,prompt=TRUE,message=FALSE--------------------------
# samp<-simulate(net.study,nsim=10)
# summary(samp$A)/10
# querygrain(net.study,nodes="A",type="marginal")
# 
# 
# ## ----marginal_A_aprox_2,prompt=TRUE,message=FALSE,cache=TRUE-------------
# aprox_marginal<-function(net,node,nsamples){
#   samp<-simulate(net,nsim=nsamples)
#   summary(samp[,node])/nsamples
# }
# querygrain(net_mush_propagated,nodes="cap-surface",type="marginal")
# aprox_marginal(net_mush,"cap-surface",10)
# aprox_marginal(net_mush,"cap-surface",10)
# aprox_marginal(net_mush,"cap-surface",10)
# aprox_marginal(net_mush,"cap-surface",1000)
# 
# 
# ## ----bias_var_marginal_aprox,prompt=TRUE,message=FALSE,fig.path="./figures/",fig.keep='all',fig.show='hide',cache=TRUE,tidy=FALSE----
# df<-data.frame()
# node<-"gill-size"
# marg_exacta<-querygrain(net_mush,nodes=node,type="marginal")[[1]][1]
# 
# for (i in seq(10,210,20)){
#   aux<-vector()
#   for (rep in 1:10){
#     aux<-c(aux,aprox_marginal(net_mush,node,i)[1])
#   }
#   df<-rbind(df,data.frame(size=i,min=min(aux),max=max(aux)))
# }
# 
# ggplot(df,aes(x=size,ymin=min,ymax=max)) + 
#   geom_ribbon(fill="slategray1") + 
#   geom_line(aes(y=min),col="darkgray",size=1.5) + 
#   geom_line(aes(y=max),col="darkgray",col="darkgray",size=1.5) + 
#   geom_abline(intercept=marg_exacta,slope=0) + 
#   labs(x="No. de muestras",y="Envolvente de las estimaciones") + 
#   annotate("text", x=25,y=marg_exacta+0.01,label="Valor exacto")
# 
# 
