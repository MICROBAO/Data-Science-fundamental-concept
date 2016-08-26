library(caret)
library(AppliedPredictiveModeling)
data(permeability)


zero = nearZeroVar( fingerprints )
fingerprintsnonzero = fingerprints[,-zero]

Training = createDataPartition( permeability, p=0.75 )
fingerprintstrain = fingerprintsnonzero [Training$Resample1,]
permeabilitytrain =permeability[Training$Resample1,]

fingerprintstest=fingerprintsnonzero [-Training$Resample1,]
permeabilitytest=permeability[-Training$Resample1,]

hist(permeabilitytrain,breaks=50)
hist(log2(permeabilitytrain),breaks=50)
set.seed(3)
control=trainControl(method = "LGOCV")
plsTune =train(x = fingerprintstrain, y =log10(permeabilitytrain) , method = "pls",tuneGrid = expand.grid(ncomp = 1:15),trControl = control)
plsTune

ypredict = predict( plsTune, newdata=fingerprintstest )
rsquared_pls = cor(ypredict,permeabilitytest,method="pearson")^2
rmse_pls = sqrt( mean( (ypredict-permeabilitytest)^2 ) )


set.seed(3)
lm = train( fingerprintstrain, permeabilitytrain, method="lm", preProcess=c("center","scale"), trControl=trainControl(method="repeatedcv",repeats=5) )
ypredict_lm = predict( lm, newdata=fingerprintstest )
r2_lm = cor(ypredict_lm,permeabilitytest,method="pearson")^2
rmse_lm = sqrt( mean( (ypredict_lm-permeabilitytest)^2 ) )


set.seed(3)
rlm = train( fingerprintstrain, permeabilitytrain, method="rlm", preProcess=c("pca"), trControl=trainControl(method="repeatedcv",repeats=5) )
ypredict_rlm = predict( rlm, newdata=fingerprintstest )
r2_rlm = cor(ypredict_rlm,permeabilitytest,method="pearson")^2
rmse_rlm = sqrt( mean( (ypredict_rlm-permeabilitytest)^2 ) )


set.seed(3)
PCR=train(fingerprintstrain, permeabilitytrain,  method = "pcr", preProcess=c("pca"),trControl = control, tuneLength = 25)

set.seed(3)

enet = train( fingerprintstrain, permeabilitytrain, method="enet",      tuneGrid = expand.grid(lambda = c(0, .05, .1), fraction = seq(0.05, 1, length = 20)), trControl=trainControl(method="repeatedcv",repeats=5) )
plot(enet)
