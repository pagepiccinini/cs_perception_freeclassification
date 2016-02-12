## READ IN DATA ####
source("scripts/cs_perception_freeclassification_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
data_stats = data_clean %>%
  # Contrast code language of instruction (baseline set to English)
  mutate(Inst_LgContrast = ifelse(Inst_Lg == "E", -0.5, 0.5)) %>%
  # Contrast code language of stimuli (baseline set to English)
  mutate(LanguageContrast = ifelse(Language == "E", -0.5, 0.5)) %>%
  # Make slide number numeric
  mutate(SlideNum = as.numeric(Slide))


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


## BUILD MODELS FOR DATA BY SLIDE ####
# Full model
byslide.glmer = glmer(Error ~ Inst_LgContrast * SlideNum +
                        (1+SlideNum|Subject) +
                        (1|Sound.File), family = "binomial", data = data_stats)

byslide.glmer_sum = summary(byslide.glmer)

# Test for effect of language of instruction
byslide_noinstlg.glmer = glmer(Error ~ Inst_LgContrast * SlideNum - Inst_LgContrast +
                                 (1+SlideNum|Subject) +
                                 (1|Sound.File), family = "binomial", data = data_stats)

byslide_noinstlg.anova = anova(byslide.glmer, byslide_noinstlg.glmer)

# Test for effect of slide number
byslide_noslide.glmer = glmer(Error ~ Inst_LgContrast * SlideNum - SlideNum +
                                 (1+SlideNum|Subject) +
                                 (1|Sound.File), family = "binomial", data = data_stats)

byslide_noslide.anova = anova(byslide.glmer, byslide_noslide.glmer)

# Test for interaction of language of instruction x slide number
byslide_noinstlgxslide.glmer = glmer(Error ~ Inst_LgContrast * SlideNum - Inst_LgContrast:SlideNum +
                                 (1+SlideNum|Subject) +
                                 (1|Sound.File), family = "binomial", data = data_stats)

byslide_noinstlgxslide.anova = anova(byslide.glmer, byslide_noinstlgxslide.glmer)


## FOLLOW UP SIMPLE REGRESSIONS TO TEST INTERACTION OF LANGUAGE OF INSTRUCTION X SLIDE NUMBER ####
# Organize data
data_eng_stats = data_stats %>%
  filter(Inst_Lg == "E") %>%
  mutate(Inst_Lg = factor(Inst_Lg))

data_sp_stats = data_stats %>%
  filter(Inst_Lg == "S") %>%
  mutate(Inst_Lg = factor(Inst_Lg))

# Run regressions
eng.glm = glm(Error ~ SlideNum, family = "binomial", data = data_eng_stats)
eng.glm_sum = summary(eng.glm)

sp.glm = glm(Error ~ SlideNum, family = "binomial", data=data_sp_stats)
sp.glm_sum = summary(sp.glm)
