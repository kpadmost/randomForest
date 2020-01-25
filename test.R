library(rpart)
library(mowRandomForest)

mykip <- data.frame(kyphosis)
names(mykip) <- casefold(names(mykip)) #remove mixed case

f <- mowRandomForest(kyphosis ~ age + number + start, mykip, ntrees = 500, minsplit=6)
p <- predict(f, mykip)
a <- as.numeric(mykip$kyphosis)
print(paste('eq: ', which(p == a)))
print(paste('neq:', which(p != a)))
