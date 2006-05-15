x <- runif(100)
summary(x)

pdf("testbild.pdf")
hist(x)
dev.off()
