## READ IN DATA ####
source("scripts/cs_perception_freeclassification_cleaning.R")


## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## ORGANIZE DATA ####
# Fix labels for figures
data_figs = data_clean %>%
  mutate(Inst_Lg = factor(Inst_Lg, levels = c("E", "S"), labels = c("English", "Spanish"))) %>%
  mutate(Language = factor(Language, levels = c("E", "S"), labels = c("English", "Spanish"))) %>%
  mutate(Slide = factor(Slide, levels = c("A", "B", "C", "D"), labels = c("Slide 1 (Eng.)", "Slide 2 (Sp.)", "Slide 3 (Eng.)", "Slide 4 (Sp.)")))

# Make data for figures collapsing across slides
data_all_figs = data_figs %>%
  group_by(Subject, Inst_Lg, Language) %>%
  summarise(Perc_Corr = mean(Error) * 100) %>%
  ungroup()

# Make data for figures separted by slides
data_byslide_figs = data_figs %>%
  group_by(Subject, Inst_Lg, Language, Slide) %>%
  summarise(Perc_Corr = mean(Error) * 100) %>%
  ungroup()


## MAKE COLORS ####
cols = brewer.pal(5, "PRGn")


## MAKE BOXPLOT ####
# All data across slides
all.plot = ggplot(data_all_figs, aes(factor(Inst_Lg), Perc_Corr)) +
  geom_boxplot(aes(fill=factor(Language))) +
  scale_y_continuous(limits=c(0,100)) +
  geom_hline(yintercept=50) +
  ggtitle("Percent Correct by\nLanguage of Instruction and Stimuli") +
  xlab("Language of Instruction") +
  ylab("Percent correct") +
  scale_fill_manual(name="Language of Stimuli", values=c(cols[5], cols[1])) +
  theme_bw() +
  theme(text=element_text(size=18),
        axis.line = element_line(color = "black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour="white", fill="white"),
        legend.position="top")

pdf("figures/all.pdf")
all.plot
dev.off()

# Data by slide
byslide.plot = ggplot(data_byslide_figs, aes(factor(Inst_Lg), Perc_Corr)) +
  geom_boxplot(aes(fill=factor(Slide))) +
  scale_y_continuous(limits=c(0,100)) +
  geom_hline(yintercept=50) +
  ggtitle("Percent Correct by\nLanguage of Instruction and Slide") +
  xlab("Language of Instruction") +
  ylab("Percent correct") +
  scale_fill_manual(name="", values=c(cols[5], cols[1], cols[4], cols[2])) +
  theme_bw() +
  theme(text=element_text(size=18),
        axis.line = element_line(color = "black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour="white", fill="white"),
        legend.position="top")

pdf("figures/byslide.pdf")
byslide.plot
dev.off()










