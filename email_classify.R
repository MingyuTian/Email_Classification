#load
setwd('G:/')
load('trainvariables.rda')
load('testData.rda')
load('blindTestData.rda')

#Regression Trees
library(rpart)
rt.isSpam = rpart(isSpam ~., data = trainVariables, method = 'class')
plot(rt.isSpam)
text(rt.isSpam, offset = 0.3, cex = 0.5, col = 'blue')
#on training data
rt.predict1 = predict(rt.isSpam, trainVariables, type = 'class')
table(rt.predict1, trainVariables$isSpam)
i = which(rt.predict1 == trainVariables$isSpam)
summary(trainVariables[i,])
summary(trainVariables[-i,])
#on test data
rt.predict = predict(rt.isSpam, testVariables, type = 'class')
table(rt.predict, testVariables$isSpam)
treemis = which(rt.predict != testVariables$isSpam)
#on blind data
rt.blind = predict(rt.isSpam, blindTestVariables, type = 'class')
save(rt.blind, file = 'treeverify.rda')
####################################################################################
#cross validation
trainVariables1 = scale(trainVariables) #scale
set.seed(1)
index = sample(1:6540, 6540)
divides = vector('list', 5)
for(i in 1:5){
  divides[[i]] = index[(1308*i - 1307):(1308*i)]
}
dist1 = vector('list', 5)
dist2 = vector('list', 5)
dist3 = vector('list', 5)
dist4 = vector('list', 5)
for(l in 1:5){
  test = trainVariables[divides[[l]],]
  train = trainVariables[-divides[[l]],]
  testdata = trainVariables1[divides[[l]],]
  training = trainVariables1[-divides[[l]],]
  testdata1 = testdata[,-30]
  training1 = training[,-30]
  alldata = rbind(testdata1, training1)
  dist1[[l]] = as.matrix(dist(alldata))[1:nrow(testdata1), -(1:nrow(testdata1))]
  dist2[[l]] = as.matrix(dist(rbind(test,train)[,-30]))[1:nrow(testdata1), -(1:nrow(testdata1))]
  dist3[[l]] = as.matrix(dist(alldata, 
                              method = 'manhattan'))[1:nrow(testdata1), -(1:nrow(testdata1))]
  dist4[[l]] = as.matrix(dist(alldata, method = 'minkowski'
                              , p = 3))[1:nrow(testdata1), -(1:nrow(testdata1))]
} 
#plot to compare
knnerror = function(dists){
  kall = vector('list', 4)
  for(k in 1:length(dists)){
    train = trainVariables[-divides[[k]],]
    test = trainVariables[divides[[k]],]
    kall[[k]] = sapply(1:20, function(i) kpercent(dists[[k]], i, train, test))
  }
  return((kall[[1]]+ kall[[2]] + kall[[3]] + kall[[4]])/4)
}
#knn function
myknn = function(distance, k){
  a = matrix( , nrow(distance), k)
  for(i in 1:nrow(distance)){
    a[i,] = order(distance[i,])[1:k]
  }
  return(a)
}

#percent of verify with k
kpercent = function(distance, k, train, test){
  l = myknn(distance, k)
  verifyk = vector('logical', nrow(test))
  for(i in 1:nrow(test)){
    if(mean(train$isSpam[l[i,]]) >= 0.5) verifyk[i] = TRUE
  }
  percent = sum(verifyk == test$isSpam)/nrow(test)
  return(percent)
}

#table of it
tableit = function(distance, k, train, test){
  l = myknn(distance, k)
  verifyk = vector('logical', nrow(test))
  for(i in 1:nrow(test)){
    if(mean(train$isSpam[l[i,]]) >= 0.5) verifyk[i] = TRUE
  }
  return(table(verifyk, test$isSpam))
}
#error rate
error1 = knnerror(dist1)
error2 = knnerror(dist2)
error3 = knnerror(dist3)
error4 = knnerror(dist4)

#plot 
plot(1 - error1, col = 'red', main = 'mean error rate of four distances',
     type = 'l', xlab = 'k', ylab = 'error rate', ylim = c(0,0.23), pch = 0)
points(1 - error2, col = 'blue', type = 'l', pch = 2)
points(1 - error3, col = 'green', type = 'l', pch = 3)
points(1 - error4, col = 'yellow', type = 'l', pch = 4)
legend('bottomright', title='distance method', 
       c('Euclidean', 'No scale', 'Manhattan', 'Minkowski'),
       col=c('red', 'blue', 'green', 'yellow'), pch = c(1,1,1,1), cex = 0.5)

#confusion matrixs for the distance we choose
lapply(1:5, function(i) tableit(dist3[[i]], 4, train = trainVariables[-divides[[i]],],
                                test = trainVariables[divides[[i]],]))

#summary of one cross validation
l = myknn(dist3[[1]], 4)
verify1 = vector('logical', nrow(trainVariables[divides[[1]],]))
for(i in 1:nrow(trainVariables[-divides[[1]],])){
  if(mean(trainVariables[-divides[[1]],]$isSpam[l[i,]]) >= 0.5) verify1[i] = TRUE
}
i = which(verify1 == trainVariables[divides[[1]],]$isSpam)
summary(trainVariables[-divides[[1]],][i,])
summary(trainVariables[-divides[[1]],][-i,])

#use the model on known isSPam test data
test = testVariables
train = trainVariables
all = rbind(test,train)
scaleall = scale(all[,-30])
distance = as.matrix(dist(scaleall, 
                            method = 'manhattan'))[1:nrow(test), -(1:nrow(test))]
kpercent(distance, 4, train, test)
tableit(distance, 4, train, test)
verify2 = vector('logical', nrow(test))
l = myknn(distance, 4)
for(i in 1:nrow(test)){
  if(mean(train$isSpam[l[i,]]) >= 0.5) verify2[i] = TRUE
}
knnmis = which(verify2 != test$isSpam)

#test on blind test data
test = blindTestVariables
train = trainVariables
train1 = trainVariables[,-30]
all = rbind(test,train1)
scaleall = scale(all)
distance = as.matrix(dist(scaleall, 
                          method = 'manhattan'))[1:nrow(test), -(1:nrow(test))]
l = myknn(distance,1)
verify1 = vector('logical', nrow(test))
for(i in 1:nrow(test)){
  if(mean(train$isSpam[l[i,]]) >= 0.5) verify1[i] = TRUE
}
save(verify1, file = 'knnverify.rda')

#explore the intersect of two methods
intersect(knnmis,treemis)