
####################################

#rm(list=ls())
library(ggplot2)
library(scales)
library(gridExtra)
library(glmmTMB)
plogis_trans <- function() {
  trans_new("plogis", transform = plogis, inverse = qlogis)
}
probit_trans <- function() {
  trans_new("probit", transform = pnorm, inverse = qnorm)
}

####################################

# simulation for false positives

trueLF = "probit"
usedLF = "logit"
niter = 200

N = 200
k = 15

b0 = 0.5
b1 = 1
b2 = -1

pval = rep(NA,niter)
for(i in 1:niter){
  id = rep(1:N,each=k*2)
  rInt = rep(rnorm(N,0,0.5),each=k*2)
  group = rep(0:1,each=k*N)
  condition = rep(0:1,each=k,times=N)
  
  yLin = b0 + b1*group + b2*condition
  if(trueLF=="logit") yProb = plogis(yLin)
  if(trueLF=="probit") yProb = pnorm(yLin)
  y = rbinom(length(yLin),1,yProb)
  df = data.frame(y,id,group,condition)
  
  fit = glmmTMB(y ~ group*condition, data=df, family=binomial(link=usedLF))
  pval[i] = summary(fit)$coefficients$cond["group:condition","Pr(>|z|)"]
}
mean(pval<0.05)

group = as.factor(group)
condition = as.factor(condition)
ggplot(data=data.frame(),aes(x=condition,y=yProb,group=group,color=group,shape=group,linetype=group))+
  geom_point(size=3)+
  geom_line(linewidth=1)

####################################

# just plot 

b0 = 0.5
b1 = 1
b2 = -1
s1 = expand.grid(group=c(0,1),condition=c(0,1))
s1$yLin = b0 + b1*s1$group + b2*s1$condition
s1$prob = pnorm(s1$yLin)
s1$group = as.factor(s1$group)
s1$condition = as.factor(s1$condition)
lines_at = seq(-5,5,.5)
fs = 30
gg = ggplot(s1,aes(x=condition,y=prob,group=group,shape=group,color=group,linetype=group)) +
  geom_point(size=5) +
  geom_line(linewidth=1.5)
gg1 = gg + 
  geom_hline(yintercept=pnorm(lines_at),color="darkgray") +
  ggtitle("link='probit',\n no interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))
gg2 = gg + 
  geom_hline(yintercept=plogis(lines_at),color="darkgray") +
  ggtitle("link='logit',\n negative interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))
gg3 = gg + 
  geom_hline(yintercept=0.5+(lines_at/10),color="darkgray") +
  ggtitle("link='identity',\n positive interaction") + 
  theme(text=element_text(size=fs), title=element_text(size=fs*.7), axis.title=element_text(size=fs))
grid.arrange(gg1,gg2,gg3,ncol=3)

####################################

