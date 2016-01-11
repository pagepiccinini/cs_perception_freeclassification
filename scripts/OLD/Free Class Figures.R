	# Read in libraries
library(ggplot2)
library(plyr)

	# Read in data files
english <- read.table("english.txt", header=T, sep="\t")
spanish <- read.table("spanish.txt", header=T, sep="\t")

palette = c("grey30", "grey")

# MAKE PLOTS

	# English
		# Summary
		
e_summ.plot <- qplot(summ_labels, e_summ_values, geom="bar", position="dodge", xlab="Stimuli language", ylab = "Average percent correct", main = "English Instructions", fill = c("E", "S"), ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("english_summ.pdf")
e_summ.plot + geom_linerange(aes(ymax = e_summ_values+e_summ_sd, ymin=e_summ_values-e_summ_sd), position=dodge)+theme_bw() + opts(legend.position="none") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + opts(plot.title = theme_text(size = 18))
	dev.off()

		# By Slide
		
		
e_slide.plot <- qplot(slide_labels, e_slide_values, geom="bar", position="dodge", xlab="Slide number", ylab = "Average percent correct", main = "English Instructions", fill = c("E", "S", "E", "S"), ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("english_slides.pdf")
e_slide.plot + geom_linerange(aes(ymax = e_slide_values+e_slide_sd, ymin=e_slide_values-e_slide_sd), position=dodge)+theme_bw() + opts(legend.position="none") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) 
	dev.off()

	# Spanish

		# Summary
		
s_summ.plot <- qplot(labels, values, geom="bar", position="dodge", xlab="Stimuli language", ylab = "Average percent correct", main = "Spanish Instructions", fill = c("E", "S"), ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("spanish_summ.pdf")
s_summ.plot + geom_linerange(aes(ymax = values+stdev, ymin=values-stdev), position=dodge)+theme_bw() + opts(legend.position="none") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) + opts(plot.title = theme_text(size = 18))
	dev.off()

		# By Slide
		
		
s_slide.plot <- qplot(slide_labels, s_slide_values, geom="bar", position="dodge", xlab="Slide number", ylab = "Average percent correct", main = "Spanish Instructions", fill = c("E", "S", "E", "S"), ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("spanish_slides.pdf")
s_slide.plot + geom_linerange(aes(ymax = s_slide_values+s_slide_sd, ymin=s_slide_values-s_slide_sd), position=dodge)+theme_bw() + opts(legend.position="none") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()) 
	dev.off()
	
	


