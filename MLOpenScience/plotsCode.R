
####################################

rm(list=ls())
library(ggplot2)
library(readxl)
df = data.frame(read_excel("_Download PDF/MemoryLearning20102024.xlsx"))

#### 

## DOWNLOADED OR NOT

df$type = ""
df$downloaded = 0
for(i in 1:nrow(df)){
  if(paste0(df$ID[i],".pdf")%in%dir("_Download PDF/pdfs/")){
    df$type[i] = "downloaded"
    df$downloaded[i] = 1
  }else{
    df$type[i] = "not downloaded"
  }
}

tab = data.frame(table(year=df$Year,type=df$type))
tab$year = as.numeric(as.character(tab$year))
tab$type = factor(tab$type,levels=c("not downloaded","downloaded"))

(ggDownloaded = ggplot(tab,aes(x=year,y=Freq,fill=type))+
  theme_bw()+
  scale_x_continuous(breaks=seq(2010,2024,2))+
  scale_y_continuous(breaks=seq(0,100,5))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("#AAAAAA","deepskyblue4"))+
  theme(text=element_text(size=20),legend.title=element_blank())
)

paste(df$ID[df$type=="not downloaded"]," - ",df$Title[df$type=="not downloaded"])

####################################

## PREREGISTERED OR NOT

dfx = df[df$downloaded == 1,]

dfx$type = ""
for(i in 1:nrow(dfx)){
  if(is.na(dfx$Preregistered[i])){
    dfx$type[i] = "Not preregistered (undeclared)"
  } else if(dfx$Preregistered[i]==0){
    dfx$type[i] = "Not preregistered (declared)"
  } else if(dfx$Preregistered[i]==1) dfx$type[i] = "Preregistered"
}

tab = data.frame(table(year=dfx$Year,type=dfx$type))
tab$year = as.numeric(as.character(tab$year))
tab$type = factor(tab$type,levels=c("Not preregistered (undeclared)",
                                    "Not preregistered (declared)",
                                    "Preregistered"))

(ggPreregistered = ggplot(tab,aes(x=year,y=Freq,fill=type))+
  ggtitle("PREREGISTRATIONS")+
  theme_bw()+
  scale_x_continuous(breaks=seq(2010,2024,2))+
  scale_y_continuous(breaks=seq(0,100,5))+
  geom_bar(position="stack",stat="identity")+
    scale_fill_manual(values=c("#999999","orange","darkgreen"),
                      labels=c("Not preregistered \n(undeclared)",
                               "Not preregistered \n(declared)",
                               "Preregistered \n "))+
  theme(text=element_text(size=22),legend.title=element_blank())
)
(ggPreregisteredProp = ggplot(tab,aes(x=year,y=Freq,fill=type))+
    ggtitle("PREREGISTRATIONS")+
    theme_bw()+
    scale_x_continuous(breaks=seq(2010,2024,2))+
    geom_area(position="fill")+
    ylab("Proportion")+
    geom_hline(yintercept=0.07,linewidth=0.8,color="white",linetype=2)+
    annotate("text",y=0.11,x=2013,label="average field 2022",color="white",size=5)+
    scale_fill_manual(values=c("#999999","orange","darkgreen"),
                      labels=c("Not preregistered \n(undeclared)",
                               "Not preregistered \n(declared)",
                               "Preregistered \n "))+
    theme(text=element_text(size=22),legend.title=element_blank())
)

####################################

## OPEN DATA OR NOT

dfx = df[df$downloaded == 1,]

dfx$type = ""
for(i in 1:nrow(dfx)){
  if(is.na(dfx$OpenData[i])){
    dfx$type[i] = "No open data"
  } else if(dfx$OpenData[i]!="1" & grepl("1",dfx$OpenData[i])){
    dfx$type[i] = "Yes but with issues"
  } else if(dfx$OpenData[i]==1){
    dfx$type[i] = "Yes, open data"
  } else if(dfx$OpenData[i]==0){
    dfx$type[i] = "No, well justified"
  } else {
    dfx$type[i] = "No open data"
  }
}

tab = data.frame(table(year=dfx$Year,type=dfx$type))
tab$year = as.numeric(as.character(tab$year))
tab$type = factor(tab$type,levels=c("No open data",
                                    "No, well justified",
                                    "Yes but with issues",
                                    "Yes, open data"))

(ggOpenData = ggplot(tab,aes(x=year,y=Freq,fill=type))+
  ggtitle("OPEN DATA")+
  theme_bw()+
  scale_x_continuous(breaks=seq(2010,2024,2))+
  scale_y_continuous(breaks=seq(0,100,5))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("#999999","darksalmon","cadetblue1","deepskyblue2"))+
  theme(text=element_text(size=22),legend.title=element_blank())
)
(ggOpenDataProp = ggplot(tab,aes(x=year,y=Freq,fill=type))+
  ggtitle("OPEN DATA")+
  theme_bw()+
  scale_x_continuous(breaks=seq(2010,2024,2))+
  geom_area(position="fill")+
  ylab("Proportion")+
  geom_hline(yintercept=0.14,linewidth=0.8,color="white",linetype=2)+
  annotate("text",y=0.18,x=2013,label="average field 2022",color="white",size=5)+
  scale_fill_manual(values=c("#999999","darksalmon","cadetblue1","deepskyblue2"))+
  theme(text=element_text(size=22),legend.title=element_blank())
)

####################################

## OPEN CODE OR NOT

dfx = df[df$downloaded == 1,]

dfx$type = ""
for(i in 1:nrow(dfx)){
  if(is.na(dfx$OpenCode[i])){
    dfx$type[i] = "No open code"
  } else if(dfx$OpenCode[i]==1){
    dfx$type[i] = "Yes, open code"
  } else {
    dfx$type[i] = "No open code"
  }
}

tab = data.frame(table(year=dfx$Year,type=dfx$type))
tab$year = as.numeric(as.character(tab$year))
tab$type = factor(tab$type,levels=c("No open code",
                                    "Yes, open code"))

(ggOpenCode = ggplot(tab,aes(x=year,y=Freq,fill=type))+
  ggtitle("OPEN CODE")+
  theme_bw()+
  scale_x_continuous(breaks=seq(2010,2024,2))+
  scale_y_continuous(breaks=seq(0,100,5))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("#999999","darkviolet"))+
  theme(text=element_text(size=22),legend.title=element_blank())
)
(ggOpenCodeProp = ggplot(tab,aes(x=year,y=Freq,fill=type))+
    ggtitle("OPEN CODE")+
    theme_bw()+
    scale_x_continuous(breaks=seq(2010,2024,2))+
    geom_area(position="fill")+
    ylab("Proportion")+
    geom_hline(yintercept=0.085,linewidth=0.8,color="white",linetype=2)+
    annotate("text",y=0.125,x=2013,label="average field 2022",color="white",size=5)+
    scale_fill_manual(values=c("#999999","darkviolet"))+
    theme(text=element_text(size=22),legend.title=element_blank())
)
####################################

save(list=ls()[grepl("gg",ls())],file="plots.RData")

####################################

