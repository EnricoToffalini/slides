
################################

library(ggplot2)
library(effects)

################################

# category group conditions

N = 1e4
conditionA = rbinom(N,1,.5)
conditionB = rbinom(N,1,.5)
group = paste(paste0("A",conditionA),paste0("B",conditionB))

b0 = 0.5
b1 = 0.5
b2 = 1

yLin = b0 + conditionA*b1 + conditionB*b2
shape=5
y = rgamma(N,shape=shape,scale=exp(yLin)/shape)
df = data.frame(y,conditionA,conditionB,group)
df$group = as.factor(df$group)
df$conditionA = as.factor(df$conditionA)
df$conditionB = as.factor(df$conditionB)
ggplot(df)+geom_density(aes(x=y,group=group,color=group),linewidth=1)

fit = glm(y~conditionA*conditionB,data=df,family=gaussian(link="log"))
summary(fit)

fit_log = glm(y~conditionA*conditionB,data=df,family=Gamma(link="log"))
fit_id = glm(y~conditionA*conditionB,data=df,family=Gamma(link="identity"))

nd <- expand.grid(conditionA = c("0","1"), conditionB = c("0","1"))
nd$p_log <- predict(fit_log, newdata = nd, type = "response")
nd$p_id <- predict(fit_id, newdata = nd, type = "response")

summary(fit)

eff = data.frame(allEffects(fit)$"conditionA:conditionB")
ggplot(eff,aes(x=conditionA,y=fit,group=conditionB,color=conditionB,shape=conditionB))+
  geom_point(size=4)+geom_line(linewidth=1)+geom_errorbar(aes(ymin=lower,ymax=upper),width=0)

df$sim = simulate(fit)$sim_1
ggplot(df)+
  geom_density(aes(x=y,group=group,color=group),linewidth=1)+
  geom_density(aes(x=sim,group=group,fill=group),color=NA,alpha=.4)

################################

# continuous predictor

N = 1e4
b0 = 0.5
b1 = 1
b2 = 0.5

group = rbinom(N,1,.5)
x = runif(N,0.5,3.5)
yLin = b0 + b1*group + b2*x
shape = 5
y = rgamma(N,shape=shape,scale=exp(yLin)/shape)
df = data.frame(y,x,group)
df$group = as.factor(df$group)

fit = glm(y~group*x,data=df,family=Gamma(link="identity"))
#fit = glm(y~group*x,data=df,family=Gamma(link="log"))
summary(fit)

eff = data.frame(allEffects(fit,xlevels=list(x=seq(0.5,3.5,.1)))$"group:x")
ggplot(eff,aes(x=x,group=group,color=group,y=fit))+
  geom_point(data=df,aes(x=x,y=y),size=1,alpha=.2)+
  geom_line(linewidth=1)+
  coord_cartesian(ylim=c(min(df$y),quantile(df$y,.999)))

df$sim = simulate(fit)$sim_1
ggplot(df)+
  geom_density(aes(x=y,group=group,color=group),linewidth=1)+
  geom_density(aes(x=sim,group=group,fill=group),color=NA,alpha=.4)
# plot(df$y, df$sim)

####

N = 1e4
b0 = 0.5
b1 = 1
b2 = 0.5

niter = 10
deltaLog = rep(NA,niter)
deltaId = rep(NA,niter)
for(i in 1:niter){
  group = rbinom(N,1,.5)
  x = runif(N,0.5,3.5)
  yLin = b0 + b1*group + b2*x
  shape = 5
  y = rgamma(N,shape=shape,scale=yLin/shape)
  df = data.frame(y,x,group)
  df$group = as.factor(df$group)
  
  fitId = glm(y~group*x,data=df,family=Gamma(link="identity"))
  fitLog = glm(y~group*x,data=df,family=Gamma(link="log"))
  deltaLog[i] = cv.glm(df,fitLog,K=10)$delta[2]
  deltaId[i] = cv.glm(df,fitId,K=10)$delta[2]
}
hist(deltaLog-deltaId)

################################


