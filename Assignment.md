---
title: "AssignmentPML"
output: html_document
---

 

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
testing=read.csv("pml-testing.csv")
training=read.csv("pml-training.csv")
```

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 
####Imputing data

```r
z=c()
for (i in 1:ncol(training))
  z[i]=length(which(is.na(training[,i])))
col=which(z>0)
training[,-col]->training
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv
```

```
##                         freqRatio percentUnique zeroVar   nzv
## X                           1.000     100.00000   FALSE FALSE
## user_name                   1.101       0.03058   FALSE FALSE
## raw_timestamp_part_1        1.000       4.26562   FALSE FALSE
## raw_timestamp_part_2        1.000      85.53155   FALSE FALSE
## cvtd_timestamp              1.001       0.10193   FALSE FALSE
## new_window                 47.330       0.01019   FALSE  TRUE
## num_window                  1.000       4.37264   FALSE FALSE
## roll_belt                   1.102       6.77811   FALSE FALSE
## pitch_belt                  1.036       9.37723   FALSE FALSE
## yaw_belt                    1.058       9.97350   FALSE FALSE
## total_accel_belt            1.063       0.14779   FALSE FALSE
## kurtosis_roll_belt       1921.600       2.02324   FALSE  TRUE
## kurtosis_picth_belt       600.500       1.61553   FALSE  TRUE
## kurtosis_yaw_belt          47.330       0.01019   FALSE  TRUE
## skewness_roll_belt       2135.111       2.01305   FALSE  TRUE
## skewness_roll_belt.1      600.500       1.72256   FALSE  TRUE
## skewness_yaw_belt          47.330       0.01019   FALSE  TRUE
## max_yaw_belt              640.533       0.34655   FALSE  TRUE
## min_yaw_belt              640.533       0.34655   FALSE  TRUE
## amplitude_yaw_belt         50.042       0.02039   FALSE  TRUE
## gyros_belt_x                1.059       0.71348   FALSE FALSE
## gyros_belt_y                1.144       0.35165   FALSE FALSE
## gyros_belt_z                1.066       0.86128   FALSE FALSE
## accel_belt_x                1.055       0.83580   FALSE FALSE
## accel_belt_y                1.114       0.72877   FALSE FALSE
## accel_belt_z                1.079       1.52380   FALSE FALSE
## magnet_belt_x               1.090       1.66650   FALSE FALSE
## magnet_belt_y               1.100       1.51870   FALSE FALSE
## magnet_belt_z               1.006       2.32902   FALSE FALSE
## roll_arm                   52.338      13.52563   FALSE FALSE
## pitch_arm                  87.256      15.73234   FALSE FALSE
## yaw_arm                    33.029      14.65702   FALSE FALSE
## total_accel_arm             1.025       0.33636   FALSE FALSE
## gyros_arm_x                 1.016       3.27693   FALSE FALSE
## gyros_arm_y                 1.454       1.91622   FALSE FALSE
## gyros_arm_z                 1.111       1.26389   FALSE FALSE
## accel_arm_x                 1.017       3.95984   FALSE FALSE
## accel_arm_y                 1.140       2.73672   FALSE FALSE
## accel_arm_z                 1.128       4.03629   FALSE FALSE
## magnet_arm_x                1.000       6.82397   FALSE FALSE
## magnet_arm_y                1.057       4.44399   FALSE FALSE
## magnet_arm_z                1.036       6.44685   FALSE FALSE
## kurtosis_roll_arm         246.359       1.68179   FALSE  TRUE
## kurtosis_picth_arm        240.200       1.67159   FALSE  TRUE
## kurtosis_yaw_arm         1746.909       2.01305   FALSE  TRUE
## skewness_roll_arm         249.558       1.68688   FALSE  TRUE
## skewness_pitch_arm        240.200       1.67159   FALSE  TRUE
## skewness_yaw_arm         1746.909       2.01305   FALSE  TRUE
## roll_dumbbell               1.022      83.78351   FALSE FALSE
## pitch_dumbbell              2.277      81.22516   FALSE FALSE
## yaw_dumbbell                1.132      83.14137   FALSE FALSE
## kurtosis_roll_dumbbell   3843.200       2.02834   FALSE  TRUE
## kurtosis_picth_dumbbell  9608.000       2.04362   FALSE  TRUE
## kurtosis_yaw_dumbbell      47.330       0.01019   FALSE  TRUE
## skewness_roll_dumbbell   4804.000       2.04362   FALSE  TRUE
## skewness_pitch_dumbbell  9608.000       2.04872   FALSE  TRUE
## skewness_yaw_dumbbell      47.330       0.01019   FALSE  TRUE
## max_yaw_dumbbell          960.800       0.37203   FALSE  TRUE
## min_yaw_dumbbell          960.800       0.37203   FALSE  TRUE
## amplitude_yaw_dumbbell     47.920       0.01529   FALSE  TRUE
## total_accel_dumbbell        1.073       0.21914   FALSE FALSE
## gyros_dumbbell_x            1.003       1.22821   FALSE FALSE
## gyros_dumbbell_y            1.265       1.41678   FALSE FALSE
## gyros_dumbbell_z            1.060       1.04984   FALSE FALSE
## accel_dumbbell_x            1.018       2.16594   FALSE FALSE
## accel_dumbbell_y            1.053       2.37489   FALSE FALSE
## accel_dumbbell_z            1.133       2.08949   FALSE FALSE
## magnet_dumbbell_x           1.098       5.74865   FALSE FALSE
## magnet_dumbbell_y           1.198       4.30129   FALSE FALSE
## magnet_dumbbell_z           1.021       3.44511   FALSE FALSE
## roll_forearm               11.589      11.08959   FALSE FALSE
## pitch_forearm              65.983      14.85577   FALSE FALSE
## yaw_forearm                15.323      10.14677   FALSE FALSE
## kurtosis_roll_forearm     228.762       1.64102   FALSE  TRUE
## kurtosis_picth_forearm    226.071       1.64611   FALSE  TRUE
## kurtosis_yaw_forearm       47.330       0.01019   FALSE  TRUE
## skewness_roll_forearm     231.518       1.64611   FALSE  TRUE
## skewness_pitch_forearm    226.071       1.62573   FALSE  TRUE
## skewness_yaw_forearm       47.330       0.01019   FALSE  TRUE
## max_yaw_forearm           228.762       0.22933   FALSE  TRUE
## min_yaw_forearm           228.762       0.22933   FALSE  TRUE
## amplitude_yaw_forearm      59.677       0.01529   FALSE  TRUE
## total_accel_forearm         1.129       0.35674   FALSE FALSE
## gyros_forearm_x             1.059       1.51870   FALSE FALSE
## gyros_forearm_y             1.037       3.77637   FALSE FALSE
## gyros_forearm_z             1.123       1.56457   FALSE FALSE
## accel_forearm_x             1.126       4.04648   FALSE FALSE
## accel_forearm_y             1.059       5.11161   FALSE FALSE
## accel_forearm_z             1.006       2.95587   FALSE FALSE
## magnet_forearm_x            1.012       7.76679   FALSE FALSE
## magnet_forearm_y            1.247       9.54031   FALSE FALSE
## magnet_forearm_z            1.000       8.57711   FALSE FALSE
## classe                      1.470       0.02548   FALSE FALSE
```

```r
which(nsv[,4]==TRUE)->drop


c(1,2,drop)->drop
length(drop)
```

```
## [1] 36
```

```r
training[,-drop]->trainingR
trainingR[,-3]->trainingR
set.seed(10)
 
inTrain <- createDataPartition(y=trainingR$classe,
                              p=0.3, list=FALSE)
training=trainingR[inTrain,]
testing=trainingR[-inTrain,]
model <- train( classe ~ .,method="rf",data=training,prox=TRUE)
```

```
## 1 package is needed for this model and is not installed. (randomForest). Would you like to try to install it now?
```

```
## Error:
```

```r
selected=rownames(varImp(model)[[1]])[order(-varImp(model)[[1]][,1])][1:10]
```

```
## Error: object 'model' not found
```

```r
training[,rownames(varImp(model)[[1]])[1:10]]
```

```
## Error: object 'model' not found
```

```r
 training[,v]
```

```
## Error: object 'v' not found
```

```r
pred=predict (model, training[,-ncol(training)], na.action = na.pass)
```

```
## Error: object 'model' not found
```

```r
pred
```

```
## Error: object 'pred' not found
```

```r
table(pred,training$classe)
```

```
## Error: object 'pred' not found
```

```r
pred=predict (model, testing[,-ncol(training)], na.action = na.pass)
```

```
## Error: object 'model' not found
```

```r
pred
```

```
## Error: object 'pred' not found
```

```r
table(pred,testing$classe)
```

```
## Error: object 'pred' not found
```

```r
predictions <- predict(modFit,newdata=trainingR[inTrain,])
```

```
## Error: object 'modFit' not found
```

```r
confusionMatrix(predictions,training$classe)
```

```
## Error: object 'predictions' not found
```

```r
model <- train(classe ~ .,method="rf",data=training[,selected],prox=TRUE)
```

```
## Error: object 'selected' not found
```

