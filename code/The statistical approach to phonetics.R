rm(list=ls())
library(tidyverse);library(emmeans);library(effects)
library(lme4);library(lmerTest);library(do)
library(splitstackshape)
#Set path
setwd("/Users/zhouxingwei/Desktop/R学习/朱磊 2019170901024 大作业/代码结果")

#Data preparation
#Operate on the first file
DFD <- read.csv("Deferential_F0_Dur.csv", fileEncoding = 'GBK')
view(DFD)
DFD$FileName1 <- DFD$FileName
DFD$FileName1 <- Replace(DFD$FileName1, from="F",to="F_")
DFD$FileName1 <- Replace(DFD$FileName1, from="M",to="M_")
DFD <- cSplit(DFD, "FileName1","_")
names(DFD)
colnames(DFD)[33:36]=c("gender","speaker","item","attitude")
DFD$speaker <- str_c(DFD$gender,DFD$speaker)
DFD <- DFD[complete.cases(DFD),]
DFD_new <- DFD %>% group_by(gender,attitude,speaker,item) %>%
            summarize(meanf0=mean(SylMeanPitch),
            articulationRate=n()/sum(SylDuration),
            f0Span=0.95*max(SylMaxPitch)-1.05*min(SylMinPitch),
            N=n())
view(DFD_new)

#-----------------------------------------------------------------
#Operate on the second file
DID <- read.csv("Deferential_Intensity_Dur.csv")
DID$fileName2 <- DID$fileName
DID$fileName2 <- Replace(DID$fileName2, from="F",to="F_")
DID$fileName2 <- Replace(DID$fileName2, from="M",to="M_")
DID <- cSplit(DID, "fileName2","_")
names(DID)
colnames(DID)[15:18]=c("gender","speaker","item","attitude")
DID$speaker <- str_c(DID$gender,DID$speaker)
unique(DID$name)
DID <- filter(DID, name %in% 
                c("dan4","shi","wo","shen1","ti3",
                    "bu","tai","shu1","fu2","tu2",
                    "guan3","li","hao3","xiang4","mei2",
                    "you","zhei4","ben3","shu","zhe4","xian4",
                    "zai4","dian3","ji2","shi4","jin1",
                    "tian1","bi4","xu1","gei3","ta1","da","dian4",
                  "hua4","zhu4","yi4","dao","ge","tong1","zhi1","dao4",
                    "hui4","di4","ming2","bai","ji1","zhong1","de",
                  "yuan2","li3","zhao3","le","hen3","jiu3","dou1",
                    "zen3","me","yong4","you3","tin,g1","qing1","chu",
                  "shou1","na","feng","you2","jian4","qi","di1","nei4",
                    "wang3","zhan4","fou3","ne2","zong","ji3","mo4","du1"))
DID$meanIntensity <- as.numeric(DID$meanIntensity) 
DID_new <- DID %>% group_by(gender,attitude,speaker,item) %>% 
  summarise(meanIntensity=mean(meanIntensity),N=n())
view(DID_new)

#-----------------------------------------------------------------
#Operate on the third file
DVQ <- read.csv("Deferential_voiceQuality.csv")
DVQ$Filename3 <- DVQ$Filename
DVQ$Filename3 <- Replace(DVQ$Filename3, from="F",to="F_")
DVQ$Filename3 <- Replace(DVQ$Filename3, from="M",to="M_")
DVQ <- cSplit(DVQ, "Filename3","_")
names(DVQ)
colnames(DVQ)[17:20]=c("gender","speaker","item","attitude")
DVQ$speaker <- str_c(DVQ$gender,DVQ$speaker)
unique(DVQ$Segment.label)
DVQ <- filter(DVQ, Segment.label %in%
                c("an4","i","wo","en1","i3",
                  "u","ai","u1","u2","uan3",
                  "l","ao3","iang4","ei2","you",
                  "ei4","en3","e4","ian4","ai4",
                  "ian3","i2","i4","in1","ian1",
                  "ei3","a1","a","ua4","u4","yi4",
                  "ao","e","ong1","i1","ao4","ui4",
                  "ing2","yuan2","iu3","ou1","yong4",
                  "you3","ing1","eng","you2","wang3",
                  "ou3","e2","ong","o4"))
DVQ_new <- DVQ %>% group_by(gender,attitude,speaker,item) %>% 
                    summarise(H1_H2=mean(H1.H2.1),N=n())
view(DVQ_new)

#---------------------------------------------------------------------------
#Data merge
#Create a common ID column
DFD_new$ID <- paste0(DFD_new$speaker,"_",DFD_new$item,"_",DFD_new$attitude)
DVQ_new$ID <- paste0(DVQ_new$speaker,"_",DVQ_new$item,"_",DVQ_new$attitude)
DID_new$ID <- paste0(DID_new$speaker,"_",DID_new$item,"_",DID_new$attitude)
#Merge by ID
demo1 <- merge(DFD_new,DVQ_new,by="ID")
demo2 <- merge(demo1, DID_new, by="ID")
#Merge by ID
view(demo2)
names(demo2)
demo2 <- demo2[,-c(10:13,16:19)]
view(demo2)
#Rename the desired column
names(demo2)
colnames(demo2)[c(2:5)]=c("gender","attitude","speaker","item")
write.csv(demo2,"demo2.csv")


demo2 <- read.csv("demo2.csv")
names(demo2)
#Extract columns containing meanf0 only
demo3 <- demo2[,c(2:7)]       

#Separate Def and Non
demo3 <- Replace(demo3,from="_Def", to="")
demo3 <- Replace(demo3,from="_Non", to="")
demo3_wide <- demo3 %>% pivot_wider(values_from = meanf0,names_from = attitude) 
# Calculate the difference between def and non
str(demo3_wide)
demo3_wide$Def <- as.numeric(demo3_wide$Def)
demo3_wide$Non <- as.numeric(demo3_wide$Non)
demo3_diff <- mutate(demo3_wide,f0difference=Def-Non) 
nrow(demo3_diff) #250
#Set effective significant digit
options(digits = 1)

#merge demo3 and demo3_diff
names(demo3)
names(demo3_diff)
demo3$speaker_item <- paste0(demo3$speaker, "_", demo3$item)
demo3_diff$speaker_item <- paste0(demo3_diff$speaker,"_",demo3_diff$item)
demo4 <- merge(demo3,demo3_diff, by = "speaker_item")
nrow(demo4) #calculate the number of the row

#Restore variables' name
names(demo4)
demo4 <- demo4[,-c(8:13)]
colnames(demo4)[c(2,3,5,6)]=c("ID","gender","speaker","item")

#Generate f0Change
demo4 <- mutate(demo4, f0Change=ifelse(f0difference> 5,"f0Higher",
                                ifelse(f0difference< -5,"f0Lower",
                                ifelse(f0difference<= 5 & f0difference>= -5, 
                                       "f0Similar",""))))
names(demo4)
names(demo2)
demo4$ID <- paste0(demo4$ID,"_",demo4$attitude)
demo5 <- merge(demo4,demo2, by = "ID")
view(demo5)
names(demo5)
demo5 <- demo5[,-c(1:2,10:15)]
colnames(demo5)[1:5]=c("gender","attitude","speaker","item","meanf0")
research_data <- demo5
view(research_data)
write.csv(research_data,"research_data.csv")

#-----------------------------------------------------------------------

#Statistical analysis
RD <- read.csv("research_data.csv")
view(RD)
#Descriptive statistics

#DST = descriptive statistics table
DST <- RD %>% group_by(gender,attitude) %>% 
              summarise(M_f0=mean(meanf0),
                        SD_f0=sd(meanf0), 
                        N_f0=n(),
                        M_f0Span=mean(f0Span),
                        SD_f0Span=sd(f0Span), 
                        N_f0Span=n(),
                        M_Intensity=mean(meanIntensity),
                        SD_Intensity=sd(meanIntensity), 
                        N_Intensity=n(),
                        M_articulationRate=mean(articulationRate),
                        SD_articulationRate=sd(articulationRate), 
                        N_articulationRate=n(),
                        M_H1_H2=mean(H1_H2),
                        SD_H1_H2=sd(H1_H2),
                        N_H1_H2=n())
view(DST)

#boxplot
library(devEMF)
#attitude
emf("A_meanf0_boxplot.emf", width=5.5, height=3)
boxplot(meanf0~attitude+gender, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average F0 (Hz)",
        main="Avarage F0 of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("A_f0Span_boxplot.emf", width=5.5, height=3)
boxplot(f0Span~attitude+gender, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average Span (ms)",
        main="Avarage Span of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("A_meanIntensity_boxplot.emf", width=5.5, height=3)
boxplot(meanIntensity~attitude+gender, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average Intensity (db)",
        main="Avarage Intensity of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("A_articulationRate_boxplot.emf", width=5.5, height=3)
boxplot(articulationRate~attitude+gender, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average articulation rate (n/ms)",
        main="Avarage articulation rate of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("A_H1-H2_boxplot.emf", width=5.5, height=3)
boxplot(H1_H2~attitude+gender, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average H1-H2 (Hz)",
        main="Avarage H1-H2 of Deferential and Neutral Mandarin Speech")
dev.off()

#gender
emf("G_meanf0_boxplot.emf", width=5.5, height=3)
boxplot(meanf0~gender+attitude, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average F0 (Hz)",
        main="Avarage F0 of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("G_f0Span_boxplot.emf", width=5.5, height=3)
boxplot(f0Span~gender+attitude, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average Span (ms)",
        main="Avarage Span of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("G_meanIntensity_boxplot.emf", width=5.5, height=3)
boxplot(meanIntensity~gender+attitude, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average Intensity (db)",
        main="Avarage Intensity of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("G_articulationRate_boxplot.emf", width=5.5, height=3)
boxplot(articulationRate~gender+attitude, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average articulation rate (n/ms)",
        main="Avarage articulation rate of Deferential and Neutral Mandarin Speech")
dev.off() 
emf("G_H1-H2_boxplot.emf", width=5.5, height=3)
boxplot(H1_H2~gender+attitude, RD,
        col=c("coral1","deepskyblue"),
        notch=T,names=c("Deferential","Neutral","Deferential","Neutral"),
        xlab="Attitude", ylab="Average H1-H2 (Hz)",
        main="Avarage H1-H2 of Deferential and Neutral Mandarin Speech")
dev.off()


#Inferential statistics

fit_1 <- lmer(meanf0 ~ attitude * gender + 
              (1 + attitude|speaker)+(1 + attitude|item), 
              data = RD, REML = F)
fit_2 <- lmer(meanf0 ~ attitude + gender + 
              (1 + attitude|speaker)+(1 + attitude |item), 
              data = RD, REML = F)
anova(fit_1, fit_2)
fit_3 <- lmer(meanf0 ~ attitude + gender + 
              (1 + 1|speaker)+(1 + attitude |item), 
              data = RD, REML = F)
fit_4 <- lmer(meanf0 ~ attitude + gender + 
              (1 + attitude|speaker)+(1 + 1|item), 
              data= RD, REML = F)
anova(fit_2, fit_4)
AIC(fit_1, fit_3)
fit_5 <- lmer(meanf0 ~ gender + (1 + attitude|speaker)+
                (1 + 1|item), data= RD, REML = F)
anova(fit_4, fit_5)
fit.f0 <- fit_5
summary(fit.f0)
fit.span <- lmer(f0Span ~ gender + (1 + attitude|speaker)+
                (1 + 1|item), data= RD, REML = F)
summary(fit.span)
fit.intensity <- lmer(meanIntensity ~ gender + (1 + attitude|speaker)+
                        (1 + 1|item), data= RD, REML = F)
summary(fit.intensity)
fit.AR <- lmer(articulationRate ~ gender + (1 + attitude|speaker)+
                 (1 + 1|item), data= RD, REML = F)
summary(fit.AR)
fit.H1_H2 <- lmer(H1_H2 ~ gender + (1 + attitude|speaker)+
                    (1 + 1|item), data= RD, REML = F)
summary(fit.H1_H2)
#Visualization
plot(allEffects(fit.f0))
plot(allEffects(fit.span))
plot(allEffects(fit.intensity))
plot(allEffects(fit.AR))
plot(allEffects(fit.H1_H2))


RD_1 <- mutate(RD,num_attitude=ifelse(attitude=="Def",1,0))
fit.full <- glm(num_attitude ~ meanf0 + f0Span + meanIntensity + articulationRate + H1_H2,
                family = binomial(), data = RD_1)
summary(fit.full)
library(car)
par(mfrow=c(2,2))
plot(fit.full)

fit.reduced <- glm(num_attitude ~ meanIntensity + articulationRate+ H1_H2,
                   family = binomial(), data = RD_1)
summary(fit.reduced)
plot(fit.reduced)

save.image("朱磊 大作业.RData")

