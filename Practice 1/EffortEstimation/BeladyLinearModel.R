library(foreign)

belady <- read.arff("/home/dave/Git/PM/P1/effortEstimation/Belady/belady.arff")

samplesize <- floor(0.66*nrow(belady))
set.seed(012)

train_idx <- sample(seq_len(nrow(belady)), size = samplesize)
belady_train <- belady[train_idx, ]
belady_test <- belady[-train_idx, ]

par(mfrow=c(1,1))

xtrain <- log(belady$size)
ytrain <- log(belady$effort)

lmbelady <- lm(ytrain ~ xtrain)

b0_bel <- lmbelady$coefficients[1]
b1_bel <- lmbelady$coefficients[2]
res <- signif(residuals(lmbelady), 5)

xtest <- belady_test$size
ytest <- belady_test$effort
pre_bel <- exp(b0_bel+b1_bel*log(xtest))

plot(xtest, ytest)
curve(exp(b0_bel+b1_bel*log(x)), from=0, to=300, add=TRUE, col="blue", lwd=2)
segments(xtest, ytest, xtest, pre_bel, col="red")
