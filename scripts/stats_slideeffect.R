# SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Perception 2 - like Free Classification/Results/R/Data")


# READ IN PACKAGES
library(lme4)


# READ IN DATA AND ORGANIZE
data = read.table("data_full.txt", header=T, sep="\t")
data = subset(data, Condition=="A_E")

# Modify main effects
contrasts(data$Inst_Lg) = c(-0.5, 0.5)
data$Inst_LgContrast <- contrasts(data$Inst_Lg)[,1][as.numeric(data$Inst_Lg)]

contrasts(data$Slide) = c(1, 2, 3, 4)
data$SlideContrast <- contrasts(data$Slide)[,1][as.numeric(data$Slide)]


# RUN LMERS
# LMER for main effects
data_full.lmer = glmer(Error ~ Inst_LgContrast * SlideContrast + (1+SlideContrast|Subject) + (1|Sound.File), family="binomial", data=data, REML=F)

# Effect of language of instruction
data_noinstlg.lmer = glmer(Error ~ Inst_LgContrast * SlideContrast - Inst_LgContrast + (1+SlideContrast|Subject) + (1|Sound.File), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noinstlg.lmer)

# Effect of slide number
data_noslide.lmer = glmer(Error ~ Inst_LgContrast * SlideContrast - SlideContrast + (1+SlideContrast|Subject) + (1|Sound.File), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noslide.lmer)

# Interaction of language of instruction and slide number
data_noinstlgslideint.lmer = glmer(Error ~ Inst_LgContrast * SlideContrast - Inst_LgContrast:SlideContrast + (1+SlideContrast|Subject) + (1|Sound.File), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noinstlgslideint.lmer)


# ADDITIONAL REGRESSIONS TO TEST INTERACTION
data_e = subset(data, Inst_Lg=="E")
data_s = subset(data, Inst_Lg=="S")

# English
data_e_full.glm = glm(Error ~ SlideContrast, data=data_e, family="binomial")
summary(data_e_full.glm)

# Spanish
data_s_full.glm = glm(Error ~ SlideContrast, data=data_s, family="binomial")
summary(data_s_full.glm)


# EXTRA LMERS
data_e_full.lmer = glmer(Error ~ SlideContrast + (1+SlideContrast|Subject), family="binomial", data=data_e, REML=F)

data_s_full.lmer = glmer(Error ~ SlideContrast + (1+SlideContrast|Subject), family="binomial", data=data_s, REML=F)

data_s_noslide.lmer = glmer(Error ~ SlideContrast - SlideContrast + (1+SlideContrast|Subject), family="binomial", data=data_s, REML=F)

anova(data_s_full.lmer, data_s_noslide.lmer)


















