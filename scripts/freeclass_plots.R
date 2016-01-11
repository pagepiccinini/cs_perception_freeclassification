setwd("~/Desktop/Experiments/CS E-S Perception 2 - like Free Classification/Results/R/Data")

freeclass_plot = read.table("freeclass_plot.txt", header=T, sep="\t")

		# Grouped barplot for full data set
data <- tapply(freeclass_plot$Correct, list(freeclass_plot$Language, freeclass_plot$Lg_Mode), sum)
labels=c("English", "Spanish")

freeclass_plot_x = c(1.5, 2.5, 4.5, 5.5)
freeclass_plot_y = c(freeclass_plot$Correct)
freeclass_plot_sd = c(freeclass_plot$St_Error)
freeclass_plot_sd_upper = c(freeclass_plot_y+freeclass_plot_sd)
freeclass_plot_sd_lower = c(freeclass_plot_y-freeclass_plot_sd)

x = c(0, 8)
y = c(50, 50)

	pdf("../Figures/fc_full.pdf")

barplot(data ,beside=T,col=c("grey18","white"), main="Percent Correct by Language of Stimuli\nand Instruction", xlab="Language of Instruction",ylab="Average percent correct", ylim=c(0,100), cex.main=2,cex.lab=1.5,cex.axis=1.5, cex.names=1.5)

errbar(freeclass_plot_x, freeclass_plot_y, freeclass_plot_sd_lower, freeclass_plot_sd_upper, add=T, lty=1, lwd=2)

points(y ~ x, type="l", lwd=2)

legend("topright", labels, fill=c("grey18","white"), cex=1.2, box.lwd=0, title="Language of Stimuli")

	dev.off()
