library(caret)
data(tecator)

#calculate the mean and variance of each column of endpoints
apply(absorp,  2,  mean)
apply(absorp,  2,  var)

pc=prcomp(absorp,  scale = TRUE)

print( pc$center )
print( pc$scale )



biplot(pc,  scale = 0)

#calculate the largest 6 eigenvalues percentage
pc_var = pc$sdev^2
pve = pc_var/sum(pc_var)*100
head(pve,10)
plot( pve, xlim=c(0,40), type='b', pch=16, xlab='number of components', ylab='percent of total variation' )
# 98.626192582  0.969705229  0.279324276  0.114429868  0.006460911  0.002624591
#only the first dimension is effective
#we use the third attribute in the endpoints
set.seed(0)

Training=createDataPartition(endpoints[, 3], p = 0.75, list= FALSE)

absorbtrain=absorp[Training,]
absorbtest=absorp[-Training,]

proteintrain =endpoints[ Training, 3]
proteintest = endpoints[-Training,3]


control = trainControl(method = "repeatedcv", repeats = 5)


#linear model
set.seed(1)
lm=train(x = absorbtrain, y = proteintrain, method = "lm", trControl = trainControl(method = "repeatedcv", repeats = 5))
mean(proteintrain)

#rlm 
rlm = train( x = absorbtrain, y = proteintrain, method="rlm", preProcess=c("pca"), trControl=trainControl(method="repeatedcv",repeats=5) )

#pcr
set.seed(1)
PCR=train(x = absorbtrain, y = proteintrain,  method = "pcr", preProcess=c("pca"),trControl = control, tuneLength = 25)
#pls
set.seed(1)
PLS=train(x = absorbtrain, y = proteintrain, method = "pls",trControl=trainControl(method="repeatedcv",repeats=5), preProcess = c("center", "scale"),tuneLength = 25)
#PCR

PCR <- train(x = absorbtrain, y = proteintrain,method = "pcr", trControl = trainControl(method="repeatedcv",repeats=5), tuneLength = 25)

comps <- rbind(PLS$results, PCR$results)
comps$Model <- rep(c("PLS", "PCR"), each = 25)
a=comps[1:25,1:6]
b=comps[26:50,1:6]

plot(a$ncomp,a$RMSE,col="blue",type="l")
par(new=TRUE)
plot(b$ncomp,b$RMSE,col="red",type="l")
legend(col=c("red","blue"))
set.seed(1)
#enet
ENet = train(x = absorbtrain, y = proteintrain,method = "enet",trControl = trainControl(method="repeatedcv",repeats=5), preProcess = c("center", "scale"),tuneGrid = expand.grid(lambda = c(0, .001, .01, .1, 1), fraction = seq(0.05, 1, length = 20)))
plot(ENet)
