
####################################

# import, prepare, select

library(ggplot2)
library(ggtext)
library(readxl)
library(dplyr)
library(pROC)
typ = read.csv("data/datasetTipicoSimulato.csv")
typ$IAGQI = typ$IAG - typ$QI_tot
ndd = data.frame(read_excel("data/datasetClinico.xlsx"))
ndd = ndd[ndd$QI_tot>=70,]
ndd = ndd[ndd$IAG>=40&ndd$IAG<=160&ndd$QI_tot>=40&ndd$QI_tot<=160&!is.na(ndd$IAG)&!is.na(ndd$QI_tot),]
ndd = ndd[ndd$includi==1,]
ndd$IAGQI = ndd$IAG - ndd$QI_tot
df = bind_rows(typ, ndd)
df$tipo = ifelse(df$dataset%in%c("DSA","ADHD"),"NDD","TYP")
df$tipo01 = ifelse(df$tipo=="NDD",1,0)

# descriptives

quantile(typ$IAGQI,probs=c(.05,.10,.25,.50,.75,.90,.95)) # -9 to 9; -7 and 7; -4 to 4; 
quantile(ndd$IAGQI,probs=c(.05,.10,.25,.50,.75,.90,.95)) # -3 to 16; -1 and 14; 3 to 10 

mean(ndd$IAGQI[ndd$genere=="M"],na.rm=T)
mean(ndd$IAGQI[ndd$genere=="F"],na.rm=T)

####################################

# logistic regression
fit = glm(tipo01 ~ IAGQI, data=df, family=binomial(link="logit"))
summary(fit)
b0 = fit$coefficients[1]
b1 = fit$coefficients[2]

# plot
IAGQI = seq(-15,20,.5)
probs = data.frame(expand.grid(IAGQI=IAGQI, base=qlogis(c(.05,.50))))
probs$Contesto = ifelse(probs$base==qlogis(.05),"Scuola/Generale \n(base: 5%)","Centro clinico \n(base: 50%)")
probs$avg = ifelse(probs$base==qlogis(.05),0*.95+6.5*.05,0*.5+6.5*.5)
probs$prob = plogis(probs$base + (probs$IAGQI-probs$avg)*b1)

ggplot(probs,aes(x=IAGQI,y=prob))+
  theme_bw()+theme(panel.border = element_blank())+
  geom_vline(xintercept=c(0,7),size=0.8,color=c("blue","red"))+
  geom_ribbon(aes(xmin=-4,xmax=4),color=NA,linetype=0,alpha=.1,fill="blue")+
  geom_ribbon(aes(xmin=-7,xmax=7),color=NA,linetype=0,alpha=.1,fill="blue")+
  geom_ribbon(aes(xmin=-9,xmax=9),color=NA,linetype=0,alpha=.1,fill="blue")+
  geom_ribbon(aes(xmin=3,xmax=10),color=NA,linetype=0,alpha=.1,fill="red")+
  geom_ribbon(aes(xmin=-1,xmax=14),color=NA,linetype=0,alpha=.1,fill="red")+
  geom_ribbon(aes(xmin=-3,xmax=16),color=NA,linetype=0,alpha=.1,fill="red")+
  scale_x_continuous(
    breaks = seq(-100,100,5),
    sec.axis = dup_axis(
      name="",
      breaks = c(-9,-3,0,7,9,16),
      labels = c("<span style='color:blue;'>-9</span>",
                 "<span style='color:red;'>-3</span>",
                 "<span style='color:blue;'>0</span>",
                 "<span style='color:red;'>7</span>",
                 "<span style='color:blue;'>9</span>",
                 "<span style='color:red;'>16</span>")
      )
  )+
  geom_line(linewidth=1.5,aes(group=Contesto,linetype=Contesto))+
  scale_linetype_manual(values=c(2,1))+
  theme(text=element_text(size=22),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        legend.key.size=unit(1.5,"cm"),
        legend.key.height = unit(1.5, "cm"),
        axis.text.x = element_markdown(size=23))+
  scale_y_continuous(breaks=seq(0,1,.1),
                     labels=paste0(seq(0,100,10),"%"))+
  xlab("IAG - QI tot")+
  ylab("Probabilità diagnosi")


####################################

# histogram for gender

plothist = function(data=NA){
  dx = data
  dx$genere = ifelse(dx$genere=="F","Femmine (DSA)",ifelse(dx$genere=="M","Maschi (DSA)",NA))
  gg = ggplot(dx)+
    geom_histogram(aes(x=Discrepanza,y=after_stat(density)),
                   binwidth=1,size=0.8,color="black",alpha=.5)+
    geom_density(aes(x=Discrepanza,y=after_stat(density)),
                 binwidth=1,size=1.5,color="black",alpha=.5)+
    # geom_histogram(aes(x=Discrepanza,y=after_stat(count),fill=Direzione),
    #                binwidth=1,color="black",alpha=.5)+
    #  scale_fill_manual(values=c("red","white","darkgreen"))+
    geom_vline(xintercept=0,size=1.5,linetype=2)+
    theme(text=element_text(size=30),axis.text.y=element_blank(),
          legend.title=element_text(size=15),legend.text=element_text(size=13),
          strip.text.x=element_text(size=32))+
    xlab("Discrepanza (IAG-QIT)")+ylab("Densità dei casi")+
    scale_x_continuous(breaks=seq(-100,100,5))
  return(gg)
}

####################################

