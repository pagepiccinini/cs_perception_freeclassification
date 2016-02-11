## READ IN DATA ####
source("scripts/cs_perception_freeclassification_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
data_stats = data_clean %>%
  # Contrast code language of instruction (baseline set to English)
  mutate(Inst_LgContrast = ifelse(Inst_Lg == "E", -0.5, 0.5)) %>%
  # Contrast code language of stimuli (baseline set to English)
  mutate(LanguageContrast = ifelse(Language == "E", -0.5, 0.5))


## BUILD MODELS FOR ALL DATA ACROSS SLIDES ####
# Full model
all.glmer = glmer(Error ~ Inst_LgContrast * LanguageContrast +
                     (1+LanguageContrast|Subject), family = "binomial", data = data_stats)

all.glmer_sum = summary(all.glmer)

# Test for effect of language of instruction
all_noinstlg.glmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - Inst_LgContrast +
                             (1+LanguageContrast|Subject), family = "binomial", data = data_stats)

all_noinstlg.anova = anova(all.glmer, all_noinstlg.glmer)

# Test for effect of language of stimuli
all_nolanguage.glmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - LanguageContrast +
                             (1+LanguageContrast|Subject), family = "binomial", data = data_stats)

all_nolanguage.anova = anova(all.glmer, all_nolanguage.glmer)

# Test for interaction of language of instruction x language of stimuli
all_noinstlgxlanguage.glmer = glmer(Error ~ Inst_LgContrast * LanguageContrast - Inst_LgContrast:LanguageContrast +
                             (1+LanguageContrast|Subject), family = "binomial", data = data_stats)

all_noinstlgxlanguage.anova = anova(all.glmer, all_noinstlgxlanguage.glmer)
