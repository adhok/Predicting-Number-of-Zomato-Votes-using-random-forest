---
title: "A model to predict the number of Votes"
author: "Pradeep Adhokshaja"
date: "6/25/2018"
output: html_document
---




## Problem statement

In this post, we will try to build a model for getting the number of Votes for restaurants in New Delhi.


## Libraries and Data Preparation


In this kernel, we will try to build a model to predict the number of Votes a restaurant would get based on chosen features.In this section of the code, we remove the columns that might not be of much value to us. These include columns that talk about the location , locality and the cuisine that is served.


```r
library(ggplot2)
library(tidyverse)

zomato = read.csv('zomato.csv')

zomato_nd = zomato %>% filter(Country.Code==1) %>% filter(City=='New Delhi')



zomato_nd_predict = zomato_nd %>% select(-Restaurant.ID,-Restaurant.Name,-Country.Code,-City,-Address,
                                          -Locality,-Locality.Verbose,-Longitude,-Latitude,-Cuisines,-Currency,-Switch.to.order.menu)
```



## A look into the data

Now that we have removed the variables that might not add much information to our model, let us take a look that the resulting dataset.




```r
zomato_nd_predict %>% str()
```

```
## 'data.frame':	5473 obs. of  9 variables:
##  $ Average.Cost.for.two: int  500 350 1500 500 500 400 400 450 300 1500 ...
##  $ Has.Table.booking   : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 1 1 1 1 2 ...
##  $ Has.Online.delivery : Factor w/ 2 levels "No","Yes": 1 2 2 1 2 1 2 2 2 2 ...
##  $ Is.delivering.now   : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Price.range         : int  2 1 3 2 2 1 1 1 1 3 ...
##  $ Aggregate.rating    : num  0 3.2 3.4 2.6 3.4 3.1 2.7 2.6 3.7 3.5 ...
##  $ Rating.color        : Factor w/ 6 levels "Dark Green","Green",..: 5 3 3 3 3 3 3 3 6 6 ...
##  $ Rating.text         : Factor w/ 6 levels "Average","Excellent",..: 4 1 1 1 1 1 1 1 3 3 ...
##  $ Votes               : int  2 46 45 11 238 8 64 113 66 141 ...
```

There seems to be three variables that point to the same thing. These are `Aggregate.rating` , `Rating.color` and `Rating.text`.

Before we remove two of these, let's take a look at them.


```r
p = zomato_nd_predict %>%
  select(Aggregate.rating,Rating.color,Rating.text) %>%
  ggplot(aes(x=Aggregate.rating))+geom_density()+facet_grid(~Rating.color+Rating.text)

ggsave(p,'variable_dustributions_number_of_votes_0.png')
```

```
## Error: `device` must be NULL, a string or a function.
```

From the above density plots we see that, `Rating.color`, `Rating.text` and the `Aggregate.rating` are related to each other. Keeping all three would not add much to the model that we are trying to build. So let's remove `Rating.text` and `Rating.color`.


```r
zomato_nd_predict = zomato_nd_predict %>% select(-Rating.color,-Rating.text)
```

## Distributions of Variables


Before we start building any model, we need to identify the problem we are trying to solve. The number of votes would be real valued. This would then call for building a regression model. But before that, let's  take a look at the distributions of the chosen variables.




```r
p = zomato_nd_predict %>%
  select_if(is.numeric) %>%
  tidyr::gather(type,value,1:4) %>%
  ggplot(aes(x=value))+geom_histogram()+facet_grid(~type,scales = 'free')+
  theme(panel.background = element_blank())
ggsave(p,'variable_dustributions_number_of_votes_1.png')
```

```
## Error: `device` must be NULL, a string or a function.
```

All of the variables are highly skewed in nature. This can add biases to the regression model that we build. One way to counter this is to transform the variables so that they have a nearly normal distribution.



### Data transformation


To transform the data that have highly biased, we firstly remove the variables that are outliers.

These removals include

* `Aggregate.rating` that have a 0
* All restuarants that have an average cost of two of over Rs2000






```r
p= zomato_nd_predict%>%
  select_if(is.numeric) %>%
  filter(Aggregate.rating>2,Average.Cost.for.two<2000) %>%
  #mutate(Average.Cost.for.two=log(Average.Cost.for.two+0.001),Votes=log(Votes)) %>%
  tidyr::gather(type,value,1:4) %>%
  ggplot(aes(x=value))+geom_histogram()+facet_grid(~type,scales = 'free')

ggsave(p,'variable_dustributions_number_of_votes_2.png')
```

```
## Error: `device` must be NULL, a string or a function.
```


There is a slight change in the distributions. But we can do more. The variable responsible for the average cost for two, is still a little bit skewed. On way way to curb this would be to use the log transformation. Also, the number of votes is skewed as well.



```r
p = zomato_nd_predict%>%
  select_if(is.numeric) %>%
  filter(Aggregate.rating>2,Average.Cost.for.two<2000) %>%
  mutate(Average.Cost.for.two=log(Average.Cost.for.two+0.001),Votes=log(Votes)) %>%
  tidyr::gather(type,value,1:4) %>%
  ggplot(aes(x=value))+geom_histogram()+facet_grid(~type,scales = 'free')

ggsave(p,'variable_dustributions_number_of_votes_3.png')
```

```
## Error: `device` must be NULL, a string or a function.
```

We see much more improvement in the normality of the variables.


We apply these transformations to the original data set and look for the amount of data that we have lost.




```r
zomato_nd_predict_new = zomato_nd_predict %>%
  filter(Aggregate.rating>2,Average.Cost.for.two<2000) %>%
  mutate(Average.Cost.for.two=log(Average.Cost.for.two+0.001),Votes=log(Votes))


print(1-(nrow(zomato_nd_predict_new)/nrow(zomato_nd_predict)))
```

```
## [1] 0.2994701
```
We have lost about 30% of the data to build our regression model.


## Simple linear regression model



```r
train = sample(1:nrow(zomato_nd_predict_new),nrow(zomato_nd_predict_new)*(3/4))
lm1 = lm(Votes~.,data=zomato_nd_predict_new[train,])
summary(lm1)
```

```
## 
## Call:
## lm(formula = Votes ~ ., data = zomato_nd_predict_new[train, ])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8678 -0.7537 -0.0342  0.6713  4.3795 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -4.30406    0.32382 -13.292  < 2e-16 ***
## Average.Cost.for.two    0.37853    0.05934   6.380 2.06e-10 ***
## Has.Table.bookingYes    0.07238    0.07134   1.015    0.310    
## Has.Online.deliveryYes  0.53706    0.04178  12.855  < 2e-16 ***
## Is.delivering.nowYes    0.03230    0.27665   0.117    0.907    
## Price.range             0.32557    0.05572   5.843 5.72e-09 ***
## Aggregate.rating        1.53368    0.04423  34.673  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.027 on 2868 degrees of freedom
## Multiple R-squared:  0.4972,	Adjusted R-squared:  0.4962 
## F-statistic: 472.7 on 6 and 2868 DF,  p-value: < 2.2e-16
```

```r
test_data = zomato_nd_predict_new[-train,]
test_data = test_data %>% select(-Votes)

prediction_lm = predict(lm1,test_data)
mean((zomato_nd_predict_new[-train,]$Votes-prediction_lm)^2)
```

```
## [1] 1.158523
```

From the above results, we find that the simple linear model explains around 48% of variance in the data. This is a fairly poor model. One way to improve this situation is to use ensemble learning methods like random forests. The RMSE for the test data is around 1.1585233.



## Random Forest

A random forest is a supervised machine learning algorithm. As the same suggests, this algorithm builds an ensemble of decision trees and takes the average of the predictions of all these trees. This is averaging of the predcitions of many models is called `bagging`. The random forest in each iteration, finds the best feature from a randomly selected set of features.

Here, we will be using the `caret` package to build our model. This package allows us to conduct cross validation using a few lines of code.

The random forest algorithm follows a standard recursive partitioning algorithm that performs splits in each tree by choosing a random `mtry` variables from the given set of variables. In each split, this algorithm chooses a different set of `mtry` variables. Here we choose `mtry` to be the squared root of the number of features available.



```r
library(caret)
control = trainControl(method="repeatedcv", number=10, repeats=3)
seed = 7
metric = "RMSE"
set.seed(seed)
mtry = round(zomato_nd_predict_new %>% ncol() %>% sqrt())
tunegrid = expand.grid(.mtry=mtry)
rf_default = train(Votes~., data=zomato_nd_predict_new, method="rf", metric="RMSE", tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)
```

```
## Random Forest 
## 
## 3834 samples
##    6 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 3451, 3451, 3450, 3450, 3452, 3450, ... 
## Resampling results:
## 
##   RMSE       Rsquared   MAE      
##   0.9159788  0.5997693  0.7181354
## 
## Tuning parameter 'mtry' was held constant at a value of 3
```

The R squared metric seems to have improved a little.In this section, we have done a 10 fold cross validation 3 times. The original data is randomly partitioned into 10 equal sized subsamples. One subsample is chosen as the validation data for testing the model and the remaining are used as the training data.This process is then repeated 3 times. These 3 results are then used to produce the final estimation. This technique will allow us to assess the performance of the model on a more generalized data.



### Parameters in the random forest

We have only introduced one parameter in the randomforest. That's the `mtry` parameter. The other one is the `ntree`.This is the number of trees to grow. A larger number of trees produces more stable models, but require more memory and a longer run time.
To choose the optimal `mtry` and `ntree` parameters, we will use a custom modeling function. This modeling function was defined in [this post in Machine Learning Mastery](https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/)


```r
library(randomForest)
customRF = list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters = data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid = function(x, y, len = NULL, search = "grid") {}
customRF$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)

customRF$prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")
customRF$sort = function(x) x[order(x[,1]),]
customRF$levels = function(x) x$classes
```

After this definition, we then apply the function to the caret train module. We will choose different  `ntree` and `mtry`  and iterate through these.


```r
control = trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid = expand.grid(.mtry=c(1:6), .ntree=c(100,200,500,1000,1100,1500,2000))
set.seed(seed)
custom = train(Votes~., data=zomato_nd_predict_new, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
custom$bestTune
```

```
##    mtry ntree
## 19    3  1100
```

This process does take some time. The optimal `mtry` here is 3 and the optimal number of trees is 1100.



## Saving the Model

After we have created the optimal random forest model, we can save it as an `RDS` for future use without going through the entire process again. The model can be loaded again using the `read.RDS` function.



```r
saveRDS(custom, file="delhi_votes.rds")
model =readRDS("delhi_votes.rds")
```

After we have read this model, let's try do apply this model to the training set.

## Mean Squared Error with the training set

Now that we have the model, we can compare the RMSE of the random forest model, with the RMSE for the linear model.



```r
prediction = predict(model,test_data)

mean((zomato_nd_predict_new[-train,]$Votes-prediction)^2)
```

```
## [1] 0.6745926
```


The RMSE has reduced a lot from 1.1585233 to 0.6745926: a ~40% decrease!

## Libraries and their versions


```r
sessionInfo()
```

```
## R version 3.4.0 (2017-04-21)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.13.3
## 
## Matrix products: default
## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-14 caret_6.0-78        lattice_0.20-35    
##  [4] bindrcpp_0.2        dplyr_0.7.4         purrr_0.2.4        
##  [7] readr_1.1.1         tidyr_0.7.2         tibble_1.4.2       
## [10] tidyverse_1.1.1     ggplot2_2.2.1.9000 
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.3.1         ddalpha_1.3.1.1    sfsmisc_1.1-1     
##  [4] jsonlite_1.5       splines_3.4.0      foreach_1.4.4     
##  [7] prodlim_1.6.1      modelr_0.1.1       assertthat_0.2.0  
## [10] stats4_3.4.0       DRR_0.0.3          cellranger_1.1.0  
## [13] robustbase_0.92-8  ipred_0.9-6        pillar_1.2.1      
## [16] glue_1.2.0         rvest_0.3.2        colorspace_1.3-2  
## [19] recipes_0.1.2      Matrix_1.2-9       plyr_1.8.4        
## [22] psych_1.7.8        timeDate_3042.101  pkgconfig_2.0.1   
## [25] CVST_0.2-1         broom_0.4.2        haven_1.1.0       
## [28] scales_0.5.0.9000  gower_0.1.2        lava_1.6          
## [31] withr_2.1.2        nnet_7.3-12        lazyeval_0.2.1    
## [34] mnormt_1.5-5       survival_2.41-3    magrittr_1.5      
## [37] readxl_1.0.0       evaluate_0.10.1    nlme_3.1-131      
## [40] MASS_7.3-47        forcats_0.2.0      xml2_1.1.1        
## [43] dimRed_0.1.0       foreign_0.8-67     class_7.3-14      
## [46] tools_3.4.0        hms_0.3            stringr_1.3.0     
## [49] kernlab_0.9-25     munsell_0.4.3      compiler_3.4.0    
## [52] RcppRoll_0.2.2     rlang_0.2.0.9001   grid_3.4.0        
## [55] iterators_1.0.9    labeling_0.3       gtable_0.2.0      
## [58] ModelMetrics_1.1.0 codetools_0.2-15   reshape2_1.4.3    
## [61] R6_2.2.2           lubridate_1.7.1    knitr_1.18        
## [64] bindr_0.1          stringi_1.1.7      parallel_3.4.0    
## [67] Rcpp_0.12.16       rpart_4.1-11       DEoptimR_1.0-8    
## [70] tidyselect_0.2.2
```

## Resources

[Machine Learning Mastery](https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/)

[Medium Article on Random Forests](https://towardsdatascience.com/the-random-forest-algorithm-d457d499ffcd)
