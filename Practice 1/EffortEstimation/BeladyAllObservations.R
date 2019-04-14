library(foreign)

belady <- read.arff("/home/dave/Git/PM/P1/effortEstimation/Belady/belady.arff")

samplesize <- floor(0.66*nrow(belady))
set.seed(012)

train_idx <- sample(seq_len(nrow(belady)), size = samplesize)
belady_train <- belady[train_idx, ]
belady_test <- belady[-train_idx, ]

par(mfrow=c(1,1))

effort_belady <- belady$effort
size_belady <- belady$size

lmbelady <- lm(effort_belady ~ size_belady)
plot(size_belady, effort_belady)
abline(lmbelady, lwd=3, col="blue")

res <- signif(residuals(lmbelady), 5)
predicted <- predict(lmbelady)

level_pred <- 0.25
lowpred <- effort_belady*(1 - level_pred)
uppred <- effort_belady*(1 + level_pred)
predict_inrange <- predicted <= uppred & predicted >= lowpred
Lpred <- sum(predict_inrange)/length(predict_inrange)

Lpred

segments(size_belady, lowpred, size_belady, uppred, col="red", lwd=3)
