freeclass_slides_plot = read.table("freeclass_slides_plot.txt", header=T, sep="\t")

		# Grouped barplot for full data set
data <- tapply(freeclass_slides_plot$Correct, list(freeclass_slides_plot$Slide, freeclass_slides_plot$Lg_Mode), sum)
labels=c("English", "Spanish")

freeclass_slides_plot_x = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5)
freeclass_slides_plot_y = c(freeclass_slides_plot$Correct)
freeclass_slides_plot_sd = c(freeclass_slides_plot$St_Error)
freeclass_slides_plot_sd_upper = c(freeclass_slides_plot_y+freeclass_slides_plot_sd)
freeclass_slides_plot_sd_lower = c(freeclass_slides_plot_y-freeclass_slides_plot_sd)

x_slides = c(0, 16)
y_slides = c(50, 50)

	pdf("../Figures/fc_slides.pdf")

barplot(data ,beside=T,col=c("grey18","white"), main="Percent Correct by Slides\nand Language of Instruction", xlab="Language of Instruction",ylab="Average percent correct", ylim=c(0,100), cex.main=1.7,cex.lab=1.5,cex.axis=1.5)

errbar(freeclass_slides_plot_x, freeclass_slides_plot_y, freeclass_slides_plot_sd_lower, freeclass_slides_plot_sd_upper, add=T, lty=1, lwd=2)

points(y_slides ~ x_slides, type="l", lwd=2)

legend("topright", labels, fill=c("grey18","white"), cex=1.2, box.lwd=0, title="Language of Stimuli")

	dev.off()
