# SFB Experiment 2
# Pam Fuhrmeister, Audrey Bürki, 2020

# load packages----
x <- c("tidyverse","cowplot","lme4","afex")
lapply(x, FUN = function(X) {
  do.call("require", list(X)) 
})

# Reanalysis of Experiment 1 individual differences----
setwd("/Users/pamelafuhrmeister/Dropbox/Postdoc_projects/SFB/Experiment_2/Data")

df <- read.csv("Results_PWI_EEG_20200526.csv", sep=";")

df$Span_Mean_c <- df$Span_Mean-mean(df$Span_Mean)
df$CTET_Hit_c <- df$CTET_Hit-mean(df$CTET_Hit)
df$Slope_Simon_c <- df$Slope_Simon-mean(df$Slope_Simon)
df$Slope_Flanker_c <- df$Slope_Flanker-mean(df$Slope_Flanker)

summary(df)
df$Accuracy <- as.factor(as.character(df$Response_Trial))
df$Participant_ID <- as.factor(as.character(df$Participant_ID))

df$Trial.position <- as.numeric(as.character(df$Picture_ID_Multipic))
df$Picture  <- as.factor(as.character(df$Trigger_Picture ))

df$Comment <- as.factor(as.character(df$Response_Trial))
df$Condition <- as.factor(as.character(df$Trigger_Condition))
df$Condition <- plyr::mapvalues(df$Condition, from = c("101", "102", "103","104","105"), to = c("baseline","phono.rel", "phono.unr", "sem.rel", "sem.unr"))
df$Condition <- as.factor(as.character(df$Condition))

nrow(df)

table(df$Response_Trial)

df2b <- df[df$Response_Trial=="correct",]
nrow(df2b)
nrow(df2b)-nrow(df)

nrow(df) - nrow(df2b)

df2b <- df2b[df2b$RT < 2301,]

df3 <- df2b[!is.na(df2b$RT),]


library(MASS)
ginv2 <- function(x) #Function to inverse Matrix (see Schad et al., https://arxiv.org/abs/1807.10451)
  fractions(provideDimnames(ginv(x),base=dimnames(x)[2:1]))

#define contrast matrix
contrasts(df3$Condition)
hM <- cbind(c.gen_int1=c(F1=-1,F2=0, F3 = 1, F4 = 0,  F5 =0), c.gen_int2=c(F1=-1,F2=0, F3 = 0, F4 = 0,  F5 =1), c.phon =c(F1=0,F2=1, F3 = -1, F4 = 0,  F5 =0),c.sem= c(F1=0,F2=0, F3 = 0, F4 = 1,  F5 =-1))
cM <- ginv2(t(hM))
contrasts(df3$Condition) <- cM
contrasts(df3$Condition)

m0 <- lmer(RT~(Condition)+(1|Picture)+(1| Participant_ID), data=df3)
mat_trt <- model.matrix(m0)
head(mat_trt)
df3$c.gen.int1 <- mat_trt[,2]
df3$c.gen.int2 <- mat_trt[,3]
df3$c.phon<- mat_trt[,4]
df3$c.sem <- mat_trt[,5]

detach(package:MASS)
#check means
m <- lm(RT~c.gen.int1+c.gen.int2+c.phon+c.sem, data=df3)
summary(m)


m1 <- lmer(log(RT) ~ (c.gen.int1 + c.gen.int2 + c.phon + c.sem) * (Slope_Flanker_c + Slope_Simon_c + CTET_Hit_c + Span_Mean_c) + (1 + c.gen.int1 + c.gen.int2 + c.phon+c.sem||Picture) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Participant_ID), data = df3)
summary(m1) 

m1b <- lmer(log(RT) ~ (c.gen.int1 + c.gen.int2 + c.phon + c.sem) * (Slope_Flanker_c + Slope_Simon_c + CTET_Hit_c + Span_Mean_c) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Picture) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Participant_ID), data = df3[abs(scale(resid(m1)))<2.5,])
print(summary(m1b, corr=F))

m2 <- lmer(RT ~ (c.gen.int1 + c.gen.int2 + c.phon + c.sem) * (Slope_Flanker_c + Slope_Simon_c + CTET_Hit_c + Span_Mean_c) + (1 + c.gen.int1 + c.gen.int2 + c.phon+c.sem||Picture) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Participant_ID), data = df3)
summary(m2) 

m2b <- lmer(RT ~ (c.gen.int1 + c.gen.int2 + c.phon + c.sem) * (Slope_Flanker_c + Slope_Simon_c + CTET_Hit_c + Span_Mean_c) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Picture) + (1 + c.gen.int1 + c.gen.int2 + c.phon + c.sem||Participant_ID), data = df3[abs(scale(resid(m2)))<2.5,])
print(summary(m2b, corr=F))


# Experiment 2----
setwd("/Users/pamelafuhrmeister/Dropbox/Postdoc_projects/SFB/Experiment_2/Data")

df <- read.csv("Experiment_2_data.csv", sep=",")

df$Span_Mean_c <- df$Mean_Span_PCU-mean(df$Mean_Span_PCU)
df$CTET_Hit_c <- df$CTET_Hit_rate-mean(df$CTET_Hit_rate)
df$Slope_Simon_c <- df$Simon_Slope-mean(df$Simon_Slope)
df$Slope_Flanker_c <- df$Flanker_Slope-mean(df$Flanker_Slope)

summary(df)
df$Accuracy <- as.factor(as.character(df$Response_Trial))
df$Participant_ID <- as.factor(as.character(df$Participant_ID))

df$Picture  <- as.factor(as.character(df$Picture_ID))

df$Comment <- as.factor(as.character(df$Response_Trial))
df$Condition <- as.factor(as.character(df$Condition_ID))
df$Condition <- plyr::mapvalues(df$Condition, from = c("9001", "9002", "9003","9004","9005"), to = c("baseline","phono.rel", "phono.unr", "sem.rel", "sem.unr"))
df$Condition <- as.factor(as.character(df$Condition))

nrow(df)

table(df$Response_Trial)

df1 <- df %>%
  filter(Response_Trial == "correct", RT != "NA")

nrow(df1)
nrow(df) - nrow(df1)


library(MASS)
ginv2 <- function(x) #Function to inverse Matrix (see Schad et al., https://arxiv.org/abs/1807.10451)
  fractions(provideDimnames(ginv(x),base=dimnames(x)[2:1]))
#define contrast matrice

#define contrast matrix
contrasts(df1$Condition)
hM <- cbind(c.gen_int1=c(F1=-1,F2=0, F3 = 1, F4 = 0,  F5 =0), c.gen_int2=c(F1=-1,F2=0, F3 = 0, F4 = 0,  F5 =1), c.phon =c(F1=0,F2=1, F3 = -1, F4 = 0,  F5 =0),c.sem= c(F1=0,F2=0, F3 = 0, F4 = 1,  F5 =-1))
cM <- ginv2(t(hM))
contrasts(df1$Condition) <- cM
contrasts(df1$Condition)

m0 <- lmer(RT~(Condition)+(1|Picture)+(1| Participant_ID), data=df1)
mat_trt <- model.matrix(m0)
head(mat_trt)
df1$c.gen.int1 <- mat_trt[,2]
df1$c.gen.int2 <- mat_trt[,3]
df1$c.phon<- mat_trt[,4]
df1$c.sem <- mat_trt[,5]

detach(package:MASS)

#check means
m <- lm(RT~c.gen.int1+c.gen.int2+c.phon+c.sem, data=df1)
summary(m)

m1 <- lmer(log(RT)~(c.gen.int1 +c.gen.int2+ c.phon+c.sem)*(Slope_Flanker_c+Slope_Simon_c+CTET_Hit_c+Span_Mean_c) +(1 +c.gen.int1+c.gen.int2 + c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+ c.phon+c.sem||Participant_ID), data = df1)
summary(m1)

m1b <- lmer(log(RT)~(c.gen.int1 +c.gen.int2+ c.phon+c.sem)*(Slope_Flanker_c+Slope_Simon_c+CTET_Hit_c+Span_Mean_c) +(1+c.gen.int1+c.gen.int2 + c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+ c.phon+c.sem||Participant_ID), data = df1[abs(scale(resid(m1)))<2.5,])
print(summary(m1b, corr=F))

m2 <- lmer(RT~(c.gen.int1 +c.gen.int2+ c.phon+c.sem)*(Slope_Flanker_c+Slope_Simon_c+CTET_Hit_c+Span_Mean_c) +(1 +c.gen.int1+c.gen.int2 + c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+ c.phon+c.sem||Participant_ID), data = df1)
summary(m2)

m2b <- lmer(RT~(c.gen.int1 +c.gen.int2+ c.phon+c.sem)*(Slope_Flanker_c+Slope_Simon_c+CTET_Hit_c+Span_Mean_c) +(1+c.gen.int1+c.gen.int2 + c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+ c.phon+c.sem||Participant_ID), data = df1[abs(scale(resid(m1)))<2.5,])
print(summary(m2b, corr=F))


# slope of PWI task----
# df2 <- df1 %>%
#   select(Participant_ID, Condition, RT) %>%
#   filter(Condition %in% c("sem.unr", "sem.rel")) %>%
#   mutate(compatible = ifelse(Condition == "sem.unr", "c", "i"))
# 
# inhib <- inhib.delta(rt = df2$RT, comp = df2$compatible, sujet = df2$Participant_ID, 
#                       cond = NA, dquantile = 5, type = 7)
# 
# inhib_sem_all <- inhib[2]$delta.slope

# df4 <- df1 %>%
#   select(Participant_ID, Condition, RT) %>%
#   filter(Condition %in% c("sem.unr", "sem.rel")) %>%
#   group_by(Participant_ID, Condition) %>%
#   mutate(quantile = ntile(RT, 5)) %>%
#   group_by(Participant_ID, Condition, quantile) %>%
#   summarize(mean_quantile = mean(RT)) %>%
#   filter(quantile > 3)
# 
# df5 <- df4 %>%
#   pivot_wider(names_from = c(Condition, quantile), values_from = mean_quantile) %>%
#   mutate(delta_5 = sem.rel_5 - sem.unr_5) %>%
#   mutate(delta_4 = sem.rel_4 - sem.unr_4) %>%
#   mutate(mean_5 = ((sem.rel_5 + sem.unr_5)/2)) %>%
#   mutate(mean_4 = ((sem.rel_4 + sem.unr_4)/2)) %>%
#   group_by(Participant_ID) %>%
#   summarize(slope_sem = (delta_5 - delta_4)/(mean_5 - mean_4))
# 
# # df6 <- merge(df3, df5, by = "Participant_ID")

df2 <- droplevels(subset(df1, Condition %in% c("sem.rel", "sem.unr")))

# make data set to add to meta-analysis
SFB_Exp_2 <- df2 %>%
  filter(Type_Speed == 3001) %>%
  select(Participant_ID, Picture, Condition, RT, Trial_Nbr)
SFB_Exp_2$Study.ID <- "Fuhrmeister2.unpublished"
SFB_Exp_2$SOA <- 0
SFB_Exp_2$Language <- "German"

SFB_Exp_2 <- SFB_Exp_2[,c(6,1:5,7:8)]
colnames(SFB_Exp_2) <- colnames(all)
write.csv(SFB_Exp_2, "../../Meta-analysis/Data/SFB_Exp2.csv")

df2$Speed <- ifelse(df2$Type_Speed == 3001, "normal", "fast")

# all trials
contrasts(df2$Condition) <- c(-.5,.5)
m2 <- lmer(log(RT)~Condition*Slope_PWI_Sem_All_Speed +(1+Condition+Slope_PWI_Sem_All_Speed||Picture)+(1+Condition+Slope_PWI_Sem_All_Speed||Participant_ID), data = df2)
summary(m2)

m2b <- lmer(log(RT)~Condition*Slope_PWI_Sem_All_Speed +(1+Condition+Slope_PWI_Sem_All_Speed||Picture)+(1+Condition+Slope_PWI_Sem_All_Speed||Participant_ID), data = df2[abs(scale(resid(m2)))<2.5,])
print(summary(m2b, corr=F))

m3 <- lmer(RT~Condition*Slope_PWI_Sem_All_Speed +(1+Condition+Slope_PWI_Sem_All_Speed||Picture)+(1+Condition+Slope_PWI_Sem_All_Speed||Participant_ID), data = df2)
summary(m3)

m3b <- lmer(RT~Condition*Slope_PWI_Sem_All_Speed +(1+Condition+Slope_PWI_Sem_All_Speed||Picture)+(1+Condition+Slope_PWI_Sem_All_Speed||Participant_ID), data = df2[abs(scale(resid(m3)))<2.5,])
print(summary(m3b, corr=F))

df2a <- df2 %>%
  select(Participant_ID, Condition, RT, Slope_PWI_Sem_All_Speed) %>%
  group_by(Participant_ID, Condition, Slope_PWI_Sem_All_Speed) %>%
  summarize(mean = mean(RT)) %>%
  pivot_wider(names_from = Condition, values_from = mean) %>%
  mutate(sem_int = sem.rel-sem.unr)

ggplot(df2a, aes(Slope_PWI_Sem_All_Speed, sem_int)) +
  geom_point() +
  geom_smooth(method = "lm")

# fast trials
df3 <- droplevels(subset(df2, Speed == "fast"))
contrasts(df3$Condition) <- c(-.5,.5)

m4 <- lmer(log(RT)~Condition*Slope_PWI_Sem_Fast +(1+Condition+Slope_PWI_Sem_Fast||Picture)+(1+Condition+Slope_PWI_Sem_Fast||Participant_ID), data = df3)
summary(m4)

m4b <- lmer(log(RT)~Condition*Slope_PWI_Sem_Fast +(1+Condition+Slope_PWI_Sem_Fast||Picture)+(1+Condition+Slope_PWI_Sem_Fast||Participant_ID), data = df3[abs(scale(resid(m4)))<2.5,])
print(summary(m4b, corr=F))

m5 <- lmer(RT~Condition*Slope_PWI_Sem_Fast +(1+Condition+Slope_PWI_Sem_Fast||Picture)+(1+Condition+Slope_PWI_Sem_Fast||Participant_ID), data = df3)
summary(m5)

m5b <- lmer(RT~Condition*Slope_PWI_Sem_Fast +(1+Condition+Slope_PWI_Sem_Fast||Picture)+(1+Condition+Slope_PWI_Sem_Fast||Participant_ID), data = df3[abs(scale(resid(m5)))<2.5,])
print(summary(m5b, corr=F))

df3a <- df3 %>%
  select(Participant_ID, Condition, RT, Slope_PWI_Sem_Fast) %>%
  group_by(Participant_ID, Condition, Slope_PWI_Sem_Fast) %>%
  summarize(mean = mean(RT)) %>%
  pivot_wider(names_from = Condition, values_from = mean) %>%
  mutate(sem_int = sem.rel-sem.unr)

ggplot(df3a, aes(Slope_PWI_Sem_Fast, sem_int)) +
  geom_point() +
  geom_smooth(method = "lm")

# slow trials
df4 <- droplevels(subset(df2, Speed == "normal"))
contrasts(df4$Condition) <- c(-.5,.5)

m6 <- lmer(log(RT)~Condition*Slope_PWI_Sem_Constant +(1+Condition+Slope_PWI_Sem_Constant||Picture)+(1+Condition+Slope_PWI_Sem_Constant||Participant_ID), data = df4)
summary(m6)

m6b <- lmer(log(RT)~Condition*Slope_PWI_Sem_Constant +(1+Condition+Slope_PWI_Sem_Constant||Picture)+(1+Condition+Slope_PWI_Sem_Constant||Participant_ID), data = df4[abs(scale(resid(m6)))<2.5,])
print(summary(m6b, corr=F))

m7 <- lmer(RT~Condition*Slope_PWI_Sem_Constant +(1+Condition+Slope_PWI_Sem_Constant||Picture)+(1+Condition+Slope_PWI_Sem_Constant||Participant_ID), data = df4)
summary(m7)

m7b <- lmer(RT~Condition*Slope_PWI_Sem_Constant +(1+Condition+Slope_PWI_Sem_Constant||Picture)+(1+Condition+Slope_PWI_Sem_Constant||Participant_ID), data = df4[abs(scale(resid(m7)))<2.5,])
print(summary(m7b, corr=F))

df4a <- df4 %>%
  select(Participant_ID, Condition, RT, Slope_PWI_Sem_Constant) %>%
  group_by(Participant_ID, Condition, Slope_PWI_Sem_Constant) %>%
  summarize(mean = mean(RT)) %>%
  pivot_wider(names_from = Condition, values_from = mean) %>%
  mutate(sem_int = sem.rel-sem.unr)

ggplot(df4a, aes(Slope_PWI_Sem_Constant, sem_int)) +
  geom_point() +
  geom_smooth(method = "lm")


# coefficient of variation
m_cv <- lm(RT ~ Condition*Speed, data = df5)

df5$resid <- resid(m_cv)

df5 <- df5 %>%
  group_by(Participant_ID) %>%
  mutate(var = abs(mean(resid)))

df5$Speed <- as.factor(df5$Speed)
contrasts(df5$Speed) <- c(-.5,.5)

m2_cv <- lmer(RT~Speed/(Condition*var) +(1+Speed/(Condition*var)||Picture)+(1+Speed/(Condition*var)||Participant_ID), data = df5)
summary(m2_cv)

m2b_cv <- lmer(RT~Speed/(Condition*var) +(1+Speed/(Condition*var)||Picture)+(1+Speed/(Condition*var)||Participant_ID), data = df5[abs(scale(resid(m2_cv)))<2.5,])
print(summary(m2b_cv, corr=F))

df3 <- df2 %>%
  select(Participant_ID, Condition, Speed, RT, cv) %>%
  group_by(Participant_ID, Condition, Speed, cv) %>%
  summarize(mean = mean(RT)) %>%
  pivot_wider(names_from = Condition, values_from = mean) %>%
  mutate(sem_int = sem.rel-sem.unr)

ggplot(df3, aes(cv, sem_int)) +
  facet_wrap(~Speed) +
  geom_point() +
  geom_smooth(method = "lm")


# recall task----
range(df2$D_prime_Recall_Participant)
hist(df2$D_prime_Recall_Participant)
m_recall <- lmer(log(RT)~(c.gen.int1+c.gen.int2+c.phon+c.sem)*D_prime_Recall_Participant + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3)
summary(m_recall)

mb_recall <- lmer(log(RT)~(c.gen.int1+c.gen.int2+c.phon+c.sem)*D_prime_Recall_Participant + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall, corr=F))

m_recall_raw <- lmer(RT~(c.gen.int1+c.gen.int2+c.phon+c.sem)*D_prime_Recall_Participant + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3)
summary(m_recall_raw)

mb_recall_raw <- lmer(RT~(c.gen.int1+c.gen.int2+c.phon+c.sem)*D_prime_Recall_Participant + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall_raw, corr=F))

# with accuracy instead of d'
df3 <- df3 %>%
  group_by(Participant_ID) %>%
  mutate(recall_distractor = mean(ACC_Recall_Distractor, na.rm=TRUE))

m_recall <- lmer(log(RT)~(c.gen.int1+c.gen.int2+c.phon+c.sem)*recall_distractor + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3)
summary(m_recall)

mb_recall <- lmer(log(RT)~(c.gen.int1+c.gen.int2+c.phon+c.sem)*recall_distractor + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall, corr=F))

m_recall_raw <- lmer(RT~(c.gen.int1+c.gen.int2+c.phon+c.sem)*recall_distractor + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3)
summary(m_recall_raw)

mb_recall_raw <- lmer(RT~(c.gen.int1+c.gen.int2+c.phon+c.sem)*recall_distractor + (1+c.gen.int1+c.gen.int2+c.phon+c.sem||Picture)+(1+c.gen.int1+c.gen.int2+c.phon+c.sem||Participant_ID), data = df3[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall_raw, corr=F))

# with accuracy for distractor on each trial as a predictor
df4 <- droplevels(subset(df3, Condition %in% c("sem.rel", "sem.unr", "phono.rel", "phono.unr")))

library(MASS)
ginv2 <- function(x) #Function to inverse Matrix (see Schad et al., https://arxiv.org/abs/1807.10451)
  fractions(provideDimnames(ginv(x),base=dimnames(x)[2:1]))

#define contrast matrix
contrasts(df4$Condition)
hM <- cbind(c.phon=c(F1=1,F2=-1,F3 = 0,F4 = 0),c.sem=c(F1=0,F2=0,F3 = 1,F4 = -1))
cM <- ginv2(t(hM))
contrasts(df4$Condition) <- cM
contrasts(df4$Condition)

m0 <- lmer(RT~(Condition)+(1|Picture)+(1| Participant_ID), data=df4)
mat_trt <- model.matrix(m0)
head(mat_trt)
df4$c.phon <- mat_trt[,2]
df4$c.sem <- mat_trt[,3]

detach(package:MASS)

#check means
m <- lm(RT~c.phon+c.sem, data=df4)
summary(m)

mean(df4$RT)
dif_sem <- mean(df4[df4$Condition == "sem.rel",]$RT) - mean(df4[df4$Condition == "sem.unr",]$RT)
dif_phono <- mean(df4[df4$Condition == "phono.rel",]$RT) - mean(df4[df4$Condition == "phono.unr",]$RT)

m_recall <- lmer(log(RT)~(c.phon+c.sem)*ACC_Recall_Distractor + (1+c.phon+c.sem||Picture)+(1+c.phon+c.sem||Participant_ID), data = df4)
summary(m_recall)

mb_recall <- lmer(log(RT)~(c.phon+c.sem)*ACC_Recall_Distractor + (1+c.phon+c.sem||Picture)+(1+c.phon+c.sem||Participant_ID), data = df4[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall, corr=F))

m_recall_raw <- lmer(RT~(c.phon+c.sem)*ACC_Recall_Distractor + (1+c.phon+c.sem||Picture)+(1+c.phon+c.sem||Participant_ID), data = df4)
summary(m_recall_raw)

mb_recall_raw <- lmer(RT~(c.phon+c.sem)*ACC_Recall_Distractor + (1+c.phon+c.sem||Picture)+(1+c.phon+c.sem||Participant_ID), data = df4[abs(scale(resid(m_recall)))<2.5,])
print(summary(mb_recall_raw, corr=F))

# pilot data for reliability paper----
df1$Speed <- ifelse(df1$Type_Speed == 3001, "normal", "fast")

df2 <- droplevels(subset(df1, Speed == "fast"))
df2 <- df2 %>%
  filter(Type_Presentation %in% c(4002, 4004)) %>%
  filter(Condition == "baseline")

df3 <- df2 %>%
  group_by(Participant_ID) %>%
  mutate(TrialNum = row_number()) %>%
  mutate(Group = ifelse(TrialNum %% 2 == 0, "even", "odd")) %>%
  select(Participant_ID, RT, Group)

df4 <- df3 %>%
  group_by(Participant_ID, Group) %>%
  summarize(mean_RT = mean(RT))

df5 <- pivot_wider(df4, names_from = Group, values_from = mean_RT)
cor.test(df5$even, df5$odd)

ggplot(df5, aes(x = even, y = odd)) + 
  geom_point() +
  geom_smooth(method = "lm")

df6 <- df2 %>%
  group_by(Participant_ID) %>%
  summarize(mean_RT = mean(RT))

hist(df6$mean_RT)
