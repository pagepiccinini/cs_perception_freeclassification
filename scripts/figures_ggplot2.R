## LOAD IN PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## READ IN DATA AND ORGANIZE ####
data = read.table("data/data_full.txt", header=T, sep="\t")
data = subset(data, Condition=="A_E")

data2 = aggregate(data$Error, by=list(data$Subject, data$Inst_Lg, data$Language), FUN=mean)
	colnames(data2)[1] = "Subject"
	colnames(data2)[2] = "Inst_Lg"
	colnames(data2)[3] = "Language"
	colnames(data2)[4] = "Error"
data2$Error = data2$Error * 100

data2$Inst_Lg = factor(data2$Inst_Lg, labels=c("English", "Spanish"))
data2$Language = factor(data2$Language, labels=c("English", "Spanish"))


## SET COLORS
cols = brewer.pal(5, "PRGn")


## MAKE BOXPLOT ####
summary_plot <- ggplot(data2, aes(factor(Inst_Lg), Error)) +
	geom_boxplot(aes(fill=factor(Language))) +
	ggtitle("Percent Correct by\nLanguage of Instruction and Stimuli") +
	xlab("Language of Instruction") +
	ylab("Percent correct") +
	scale_y_continuous(limits=c(0,100)) +
	geom_hline(yintercept=50) +
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

pdf("figures/summary.pdf")
summary_plot
dev.off()










