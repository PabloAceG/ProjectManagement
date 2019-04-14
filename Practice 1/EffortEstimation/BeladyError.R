library(foreign)
 
gm_mean = function(x, na.rm=TRUE)
{
     exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}
 
## Get data from file.
 
belady <- read.arff("/home/dave/Git/PM/P1/effortEstimation/Belady/belady.arff")
samplesize <- floor(0.66*nrow(belady))
set.seed(012)
 
train_idx <- sample(seq_len(nrow(belady)), size = samplesize)
 
## Separate data into train and test data using the 1:3 proportion.
belady_train <- belady[train_idx, ]
belady_test <- belady[-train_idx, ]
 
## Analyze training data.
logbelady_size <- log(belady_train$size)
logbelady_effort <- log(belady_train$effort)
linmodel_logbelady_train <- lm(logbelady_effort ~ logbelady_size)
 
 
b0 <- linmodel_logbelady_train$coefficients[1]
b1 <- linmodel_logbelady_train$coefficients[2]
 
belady_size_test <- belady_test$size
realEffort <- belady_test$effort
predicted <- exp(b0+b1*log(belady_size_test))
 
err <- realEffort - predicted
ae <- abs(err)
hist(ae, main="Absolute Error in Belady Test Data")

gmar <- gm_mean(ae)

level_pred <- 0.25
lowpred <- realEffort*(1-level_pred)
uppred <- realEffort*(1+level_pred)

pred <- predicted <= uppred & predicted >= lowpred
Lpred <- sum(pred)/length(pred)
Lpred
gmar
