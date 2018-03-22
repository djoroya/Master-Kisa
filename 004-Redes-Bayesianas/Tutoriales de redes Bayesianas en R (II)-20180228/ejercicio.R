rm(list = ls())
yn<-c("NO","SI")
dev.off()
c <- cptable(~C, values=c(25,75),levels=yn)
a.c <- cptable(~A|C, values=c(25,75,25,75),levels=yn)
s.c <- cptable(~S|C, values=c(25,75,25,75),levels=yn)

plist<-compileCPT(list(c,a.c,s.c))
net.study<-grain(plist)
plot.new()
plot(net.study)

get_conditional_marginal <- function(net,node,evidence_nodes,evidence,numSamples){
  index <-0
  len_evidences <- length(evidence)
  df <- simulate(net,nsim=1)
  while(index < numSamples){
    samp<-simulate(net,nsim=1)
    if (sum(samp[evidence_nodes]==evidence) == len_evidences){
      index = index + 1
      df = rbind(df,samp)
    }
  }
  return(summary(df[-1,node])/numSamples)  
}

result <- get_conditional_marginal(net.study,"A",c("C"),c("SI"),20)
result
