library(foreign)

belady <- read.arff("/home/dave/Git/PM/P1/effortEstimation/Belady/belady.arff")

xtrain <- log(belady$size)
ytrain <- log(belady$effort)

lmbelady <- lm(ytrain ~ xtrain)
plot(xtrain, ytrain)

abline(lmbelady, lwd=2, col="blue")
