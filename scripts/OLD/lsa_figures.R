# Totals

ae_e_perc = c(57.5, 51.5)
ae_s_perc = c(61.5, 54.5)


	pdf("../Figures/Experiment 2/totals_englishinst.pdf")
barplot(ae_e_perc, col=c("blue", "darkgreen"),ylim=c(0,100), xaxt="n", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, ylab="Average Percent Correct", xlab="Stimuli language", main = "English Instructions")
axis(1, at=c(0.75,1.9), labels=c("begins in English", "begins in Spanish"), cex.lab=1.5,cex.axis=1.5)
#legend("topleft",legend=c("English", "Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
abline(h=50, lwd=4)
	dev.off()
	
	pdf("../Figures/Experiment 2/totals_spanishinst.pdf")
barplot(ae_s_perc, col=c("blue", "darkgreen"),ylim=c(0,100), xaxt="n", cex.main=1.7,cex.lab=1.5,cex.axis=1.5, ylab="Average Percent Correct", xlab="Stimuli language", main = "Spanish Instructions")
axis(1, at=c(0.75,1.9), labels=c("begins in English", "begins in Spanish"), cex.lab=1.5,cex.axis=1.5)
#legend("topleft",legend=c("English", "Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
abline(h=50, lwd=4)
	dev.off()


# By slide
	
ae_e_slides_perc = c(58, 49, 57,  54)
ae_s_slides_perc = c(68, 58, 55, 51)	

	pdf("../Figures/Experiment 2/english_slides.pdf")
barplot(ae_e_slides_perc,col=c("blue", "darkgreen"),ylim=c(0,100), main="English Instructions by Slide", ylab="Average Percent Correct", xlab="Slide number", xaxt="n", cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
axis(1, at=c(0.75, 1.9, 3.1, 4.3), labels=c("Slide 1", "Slide 2", "Slide 3", "Slide 4"), cex.lab=1.5,cex.axis=1.5)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
abline(h=50, lwd=4)
	dev.off()
	
	pdf("../Figures/Experiment 2/spanish_slides.pdf")
barplot(ae_s_slides_perc,col=c("blue", "darkgreen"),ylim=c(0,100), main="Spanish Instructions by Slide", ylab="Average Percent Correct", xlab="Slide number", xaxt="n", cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
axis(1, at=c(0.75, 1.9, 3.1, 4.3), labels=c("Slide 1", "Slide 2", "Slide 3", "Slide 4"), cex.lab=1.5,cex.axis=1.5)
legend("topleft",legend=c("begins in English", "begins in Spanish"), fill=c("blue", "darkgreen"), cex=1.5, bty="n")
abline(h=50, lwd=4)
	dev.off()


library(languageR)
library(lme4)

data = read.table("fc_data.txt", header=T, sep="\t")
	
	
	
fc.lmer = lmer(Error ~ Language*Inst_Lg + (1|Subject), family="binomial", data=data_sub)
fc.lmer