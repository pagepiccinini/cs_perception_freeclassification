setwd("~/Desktop/Experiments/CS E-S Perception 2 - like Free Classification/Results/R/Data")

	# READ IN PACKAGES
library(lme4)

	# READ IN DATA AND ORGANIZE
data = read.table("data_full.txt", header=T, sep="\t")
data = subset(data, Condition=="A_E")

	# MODIFY MAIN EFFECTS
contrasts(data$Inst_Lg) = c(-0.5, 0.5)
data$Inst_LgContrast <- contrasts(data$Inst_Lg)[,1][as.numeric(data$Inst_Lg)]

contrasts(data$Language) = c(-0.5, 0.5)
data$LanguageContrast <- contrasts(data$Language)[,1][as.numeric(data$Language)]

	# LMER TO TEST MAIN EFFECTS
data_full.lmer = glmer(Error ~ Inst_LgContrast * LanguageContrast + (1+LanguageContrast|Subject), family="binomial", data=data, REML=F)

	# LMER WITHOUT LANGUAGE OF INSTRUCTION
data_noinstlg.lmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - Inst_LgContrast + (1+LanguageContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noinstlg.lmer)

	# LMER WITHOUT LANGUAGE
data_nolanguage.lmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - LanguageContrast + (1+LanguageContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_nolanguage.lmer)

	# LMER WITHOUT LANGUAGE OF INSTRUCTION X LANGUAGE
data_noinstlglanguageint.lmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - Inst_LgContrast:LanguageContrast + (1+LanguageContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noinstlglanguageint.lmer)


















