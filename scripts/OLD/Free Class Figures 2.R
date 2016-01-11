slides = read.table("slides.txt", header=T, sep="\t")

e_slides <- slides[slides$Instruction=="English",]
s_slides <- slides[slides$Instruction=="Spanish",]


		# English
		
e_slide.plot <- qplot(slide_labels, e_slides$Mean, fill=factor(e_slides$Stimuli), geom="bar", position="dodge", xlab="Slide number", ylab = "Average percent correct", main = "English Instructions", ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("english_slides.pdf")
e_slide.plot + geom_linerange(aes(ymax = e_slides$Mean+e_slides$StDev, ymin=e_slides$Mean-e_slides$StDev), position=dodge)+theme_bw() + opts(legend.position="top") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())  + opts(plot.title = theme_text(size = 18))
	dev.off()
	
		# Spanish
		
s_slide.plot <- qplot(slide_labels, s_slides$Mean, fill=factor(s_slides$Stimuli), geom="bar", position="dodge", xlab="Slide number", ylab = "Average percent correct", main = "Spanish Instructions", ylim = c(0, 100))

dodge <- position_dodge(width=0.9)

	pdf("spanish_slides.pdf")
s_slide.plot + geom_linerange(aes(ymax = s_slides$Mean+s_slides$StDev, ymin=s_slides$Mean-s_slides$StDev), position=dodge)+theme_bw() + opts(legend.position="top") + scale_fill_manual(values=palette) + opts(legend.title=theme_blank()) + geom_hline(yintercept=50, lwd=1) + opts(panel.background = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank())  + opts(plot.title = theme_text(size = 18))
	dev.off()
	
	
	
	
	
	
	
	
	
	
	