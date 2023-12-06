

# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(raster)
library(terra)
library(sf)
library(patchwork)
library(heatwaveR)

setwd("C:/Emperors/Sea-Ice-on-Emperor-Penguin-Colonies/")


bear<-readRDS("Bear_Peninsula.Rds")
brown<-readRDS("Brownson_Island.Rds")
bryan<-readRDS("Bryan_Coast.Rds")
cape<-readRDS("Cape_Gates.Rds")
novi<-readRDS("Noville_Peninsula.Rds")

pfro<-readRDS("Pfrogner_Point.Rds")
roth<-readRDS("Rothschild_Island.Rds")
smyl<-readRDS("Smyley_Island.Rds")
thur<-readRDS("Thurston_Glacier.Rds")
verd<-readRDS("Verdi_Inlet.Rds")



bear$site=c("BEAR")
brown$site=c("BROW")
bryan$site=c("BRYA")
cape$site=c("CAPE")
novi$site=c("NOVI")

pfro$site=c("PFRO")
roth$site=c("ROTH")
smyl$site=c("SMYL")
thur$site=c("THUR")
verd$site=c("VERD")


all<-rbind(bear,brown,bryan,cape,novi,pfro,roth,smyl,thur,verd)

all$Year<-year(all$t)
all$quarter<-quarter(all$t)
all$Season[all$quarter=="1"]<-"JFM"
all$Season[all$quarter=="2"]<-"AMJ"
all$Season[all$quarter=="3"]<-"JAS"
all$Season[all$quarter=="4"]<-"OND"



allm<-plyr::ddply(all, c("Year","Season","site"), summarise,
                  mean=na.omit(mean(sicMean)),
                  min=na.omit(mean(sicMin)),
                  max=na.omit(mean(sicMax)),
                  sd=na.omit(mean(sicSD)))
summary(allm)

ggplot(subset(allm,Season=="JFM"), aes(Year,min))+
  geom_smooth(se=F)+
  geom_point()+theme_bw()+ggtitle("a. Summer")+ylab("Sea Ice cover")+
  xlim(2010,2023)+
  
  ggplot(subset(allm,Season=="AMJ"), aes(Year,min))+
  geom_smooth(se=F)+
  geom_point()+theme_bw()+ggtitle("b. Autumn")+ylab("Sea Ice cover")+
  xlim(2010,2023)+
  
  ggplot(subset(allm,Season=="JAS"), aes(Year,min))+
  geom_smooth(se=F)+
  geom_point(method="lm",se=F)+theme_bw()+ggtitle("c. Winter")+ylab("Sea Ice cover")+
  xlim(2010,2023)+
  
  ggplot(subset(allm,Season=="OND"), aes(Year,min))+
  geom_smooth(se=F)+
  geom_point()+theme_bw()+ggtitle("d. Spring")+ylab("Sea Ice cover")+
  xlim(2010,2023)





# SICMin thresholds

sicMean01 <- ts2clm(data = bear, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)
sicMean02 <- ts2clm(data = brown, 
                    y = sicMean, climatologyPeriod = c("1986-01-01", "2023-05-30"), pctile = 25)
sicMean03 <- ts2clm(data = bryan, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)

sicMean04 <- ts2clm(data = cape, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)
sicMean05 <- ts2clm(data = novi, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)
sicMean06 <- ts2clm(data = pfro, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)

sicMean07 <- ts2clm(data = roth, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)

sicMean08 <- ts2clm(data = smyl, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)
sicMean09 <- ts2clm(data = thur, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)
sicMean10 <- ts2clm(data = verd, 
                    y = sicMean, climatologyPeriod = c("1985-01-01", "2023-05-30"), pctile = 25)



#DETECT alldfS


sic01 <- detect_event(data = sicMean01, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic02 <- detect_event(data = sicMean02, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic03 <- detect_event(data = sicMean03, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic04 <- detect_event(data = sicMean04, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic05 <- detect_event(data = sicMean05, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic06 <- detect_event(data = sicMean06, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic07 <- detect_event(data = sicMean07, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic08 <- detect_event(data = sicMean08, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic09 <- detect_event(data = sicMean09, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 
sic10 <- detect_event(data = sicMean10, y = sicMean,minDuration = 3,maxGap = 7,coldSpells = T) 


sdf01<-data.frame(sic01$climatology,site_id=c("BEAR")) 
sdf02<-data.frame(sic02$climatology,site_id=c("BSON")) 
sdf03<-data.frame(sic03$climatology,site_id=c("BRYA")) 
sdf04<-data.frame(sic04$climatology,site_id=c("GATE")) 
sdf05<-data.frame(sic05$climatology,site_id=c("NOVI")) 

sdf06<-data.frame(sic06$climatology,site_id=c("PFRO")) 
sdf07<-data.frame(sic07$climatology,site_id=c("ROTS")) 
sdf08<-data.frame(sic08$climatology,site_id=c("SMYL")) 
sdf09<-data.frame(sic09$climatology,site_id=c("THUR")) 
sdf10<-data.frame(sic10$climatology,site_id=c("VERD")) 



clim<-rbind(sdf01,sdf02,sdf03,sdf04,sdf05,
            sdf06,sdf07,sdf08,sdf09,sdf10)

clim$diff = clim$thresh - clim$seas
clim$thresh_2x = clim$thresh + clim$diff
clim$thresh_3x = clim$thresh_2x + clim$diff
clim$thresh_4x = clim$thresh_3x + clim$diff

write.csv(clim,"ClimatologyData.csv")


head(sdf06)

summary(sdf06$t)

sdf06$month<-month(sdf06$t)
sdf06$day<-day(sdf06$t)

sdf05$month<-month(sdf05$t)
sdf05$day<-day(sdf05$t)


# 01 may data 

may04<-subset(sdf06,month=="5" & day=="4")

may04.2023<-subset(sdf06,t=='2023-05-04')


apr29<-subset(sdf05,month=="4" & day=="29")
apr29.2023<-subset(sdf05,t=='2023-04-29')



t.test(may04$sicMean, mu=may04.2023$thresh,alternative = "greater")
t.test(may04$sicMean, mu=may04.2023$sicMean,alternative = "greater")

t.test(apr29$sicMean, mu=apr29.2023$thresh,alternative = "greater")
t.test(apr29$sicMean, mu=apr29.2023$sicMean,alternative = "greater")


summary(t1)


ggplot()+
  stat_density(data = may04, aes(x=sicMean),geom = "area",
               position = "stack",bw = "nrd0", adjust = 1.1, kernel = "gaussian",
               n = 512, trim = FALSE, na.rm = FALSE, show.legend = NA,
               inherit.aes = TRUE,fill="deepskyblue4")+xlim(0.5,1.12)+
  theme_bw()+
  geom_vline(xintercept = may04.2023$sicMean,linetype="dashed",color="red",linewidth=1)+
  geom_vline(xintercept = may04.2023$thresh,linetype="dotted",color="orange2",linewidth=1)+
  geom_vline(xintercept = may04.2023$seas,linetype="solid",color="grey50",linewidth=1)+
  xlab("Sea ice cover mean")+ggtitle(label="a. Pfrogner Point 04 May 2023")+
annotate("text",x=1,y=4,label="threshold, t=4.53, P=2.973e-05",size=2.5,color="orange2")+
  annotate("text",x=1,y=3.75,label="observed, t=13.22, P=6.859e-16",size=2.5,color="red")+
  
ggplot()+
  stat_density(data = apr29, aes(x=sicMean),geom = "area",
               position = "stack",bw = "nrd0", adjust = 1.1, kernel = "gaussian",
               n = 512, trim = FALSE, na.rm = FALSE, show.legend = NA,
               inherit.aes = TRUE,fill="deepskyblue4")+xlim(0.3,1.12)+
  theme_bw()+
  geom_vline(xintercept = apr29.2023$sicMean,linetype="dashed",color="red",linewidth=1)+
  geom_vline(xintercept = apr29.2023$thresh,linetype="dotted",color="orange2",linewidth=1)+
  geom_vline(xintercept = apr29.2023$seas,linetype="solid",color="grey50",linewidth=1)+
  xlab("Sea ice cover mean")+ggtitle(label="b. Noville Peninsula 29 April 2023")+
  annotate("text",x=1,y=3.75,label="threshold, t=5.41, P=1.94e-06",size=2.5,color="orange2")+
  annotate("text",x=1,y=3.5,label="observed, t=11.11, P=1.2e-13",size=2.5,color="red")




climS<-merge(clim,sites,by="site_id")


head(climS)

# Set category fill colours
fillColCat2 <- c(
  "Strong" = "#ff6900",
  "Moderate" = "#ffc866",
  "Normal" = "#85B7CC"
)

lineColCat2 <- c(
  "Sea Ice" = "black",
  "Climatology" = "blue3",
  "Threshold" = "orange3",
  "2x Threshold" = "red3"
)

climS$diff = climS$thresh - climS$seas
climS$thresh_2x = climS$thresh + climS$diff
climS$thresh_3x = climS$thresh_2x + climS$diff
climS$thresh_4x = climS$thresh_3x + climS$diff

sites$site_name

climS$site_id<-factor(climS$site_id,levels=c("ROTS","VERD","SMYL","BRYA","PFRO","NOVI",
                                             "BSON","BEAR","GATE","THUR"))


climS$site_name<-factor(climS$site_name,levels=c("Rothschild Island","Verdi Inlet",
                                                 "Smyley","Bryan Coast","Pfrogner Point",
                                                 "Noville Peninsula", "Brownson Islands",
                                                 "Bear Peninsula","Cape Gates",
                                                 "Thurston Glacier, Mount Siple"))

head(climS)

