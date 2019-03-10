Homework 4: Bags, Forests, Boosts, oh my
================
Komal Suchak
3/9/2019

Problem 1
---------

Problem 7 from Chapter 8 in the text. To be specific, please use a sequence of `ntree` from 25 to 500 in steps of 25 and `mtry` from 3 to 9 for by 1.

In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500. Create a plot displaying the test error resulting from random forests on this data set for a more comprehensive range of values for mtry and ntree. You can model your plot after Figure 8.10. Describe the results obtained.

Answer 1
--------

``` r
set.seed(1234)

for (k in 1:20){
  inTraining <- createDataPartition(Boston$medv, p = .75, list = F)
  train_boston <- Boston[inTraining, ]
  test_boston <- Boston[-inTraining, ]
  mtry <- c(3:9)
  ntree <- seq(25, 500, len = 20)
  results <- tibble(trial = rep(NA, 140),
  mtry = rep(NA, 140),
  ntree = rep(NA, 140),
  mse = rep(NA, 140)) 
  for(i in 1:7){
    cat(sprintf('Trial: %s, mtry: %s --- %s\n', k, mtry[i], Sys.time()))
    for(j in 1:20){ 
      rf_train <- randomForest(medv ~ .,
                               data = train_boston,
                               mtry = mtry[i],
                               ntree = ntree[j])
      mse <- mean((predict(rf_train, newdata = test_boston) - test_boston$medv)^2)
      results[(i-1)*20 + j, ] <- c(k, mtry[i], ntree[j], mse)
    }
  }
  if(exists("results_total")){
  results_total <- bind_rows(results_total, results)
  }
  else(
  results_total <- results
  )
}
```

    ## Trial: 1, mtry: 3 --- 2019-03-10 11:48:28
    ## Trial: 1, mtry: 4 --- 2019-03-10 11:48:32
    ## Trial: 1, mtry: 5 --- 2019-03-10 11:48:38
    ## Trial: 1, mtry: 6 --- 2019-03-10 11:48:45
    ## Trial: 1, mtry: 7 --- 2019-03-10 11:48:52
    ## Trial: 1, mtry: 8 --- 2019-03-10 11:49:01
    ## Trial: 1, mtry: 9 --- 2019-03-10 11:49:10
    ## Trial: 2, mtry: 3 --- 2019-03-10 11:49:21
    ## Trial: 2, mtry: 4 --- 2019-03-10 11:49:25
    ## Trial: 2, mtry: 5 --- 2019-03-10 11:49:31
    ## Trial: 2, mtry: 6 --- 2019-03-10 11:49:37
    ## Trial: 2, mtry: 7 --- 2019-03-10 11:49:45
    ## Trial: 2, mtry: 8 --- 2019-03-10 11:49:53
    ## Trial: 2, mtry: 9 --- 2019-03-10 11:50:03
    ## Trial: 3, mtry: 3 --- 2019-03-10 11:50:13
    ## Trial: 3, mtry: 4 --- 2019-03-10 11:50:18
    ## Trial: 3, mtry: 5 --- 2019-03-10 11:50:23
    ## Trial: 3, mtry: 6 --- 2019-03-10 11:50:30
    ## Trial: 3, mtry: 7 --- 2019-03-10 11:50:37
    ## Trial: 3, mtry: 8 --- 2019-03-10 11:50:46
    ## Trial: 3, mtry: 9 --- 2019-03-10 11:50:55
    ## Trial: 4, mtry: 3 --- 2019-03-10 11:51:05
    ## Trial: 4, mtry: 4 --- 2019-03-10 11:51:10
    ## Trial: 4, mtry: 5 --- 2019-03-10 11:51:16
    ## Trial: 4, mtry: 6 --- 2019-03-10 11:51:22
    ## Trial: 4, mtry: 7 --- 2019-03-10 11:51:30
    ## Trial: 4, mtry: 8 --- 2019-03-10 11:51:38
    ## Trial: 4, mtry: 9 --- 2019-03-10 11:51:48
    ## Trial: 5, mtry: 3 --- 2019-03-10 11:51:58
    ## Trial: 5, mtry: 4 --- 2019-03-10 11:52:03
    ## Trial: 5, mtry: 5 --- 2019-03-10 11:52:08
    ## Trial: 5, mtry: 6 --- 2019-03-10 11:52:15
    ## Trial: 5, mtry: 7 --- 2019-03-10 11:52:23
    ## Trial: 5, mtry: 8 --- 2019-03-10 11:52:31
    ## Trial: 5, mtry: 9 --- 2019-03-10 11:52:40
    ## Trial: 6, mtry: 3 --- 2019-03-10 11:52:50
    ## Trial: 6, mtry: 4 --- 2019-03-10 11:52:55
    ## Trial: 6, mtry: 5 --- 2019-03-10 11:53:01
    ## Trial: 6, mtry: 6 --- 2019-03-10 11:53:07
    ## Trial: 6, mtry: 7 --- 2019-03-10 11:53:15
    ## Trial: 6, mtry: 8 --- 2019-03-10 11:53:23
    ## Trial: 6, mtry: 9 --- 2019-03-10 11:53:33
    ## Trial: 7, mtry: 3 --- 2019-03-10 11:53:43
    ## Trial: 7, mtry: 4 --- 2019-03-10 11:53:48
    ## Trial: 7, mtry: 5 --- 2019-03-10 11:53:53
    ## Trial: 7, mtry: 6 --- 2019-03-10 11:54:00
    ## Trial: 7, mtry: 7 --- 2019-03-10 11:54:07
    ## Trial: 7, mtry: 8 --- 2019-03-10 11:54:15
    ## Trial: 7, mtry: 9 --- 2019-03-10 11:54:25
    ## Trial: 8, mtry: 3 --- 2019-03-10 11:54:35
    ## Trial: 8, mtry: 4 --- 2019-03-10 11:54:40
    ## Trial: 8, mtry: 5 --- 2019-03-10 11:54:45
    ## Trial: 8, mtry: 6 --- 2019-03-10 11:54:52
    ## Trial: 8, mtry: 7 --- 2019-03-10 11:54:59
    ## Trial: 8, mtry: 8 --- 2019-03-10 11:55:07
    ## Trial: 8, mtry: 9 --- 2019-03-10 11:55:16
    ## Trial: 9, mtry: 3 --- 2019-03-10 11:55:27
    ## Trial: 9, mtry: 4 --- 2019-03-10 11:55:31
    ## Trial: 9, mtry: 5 --- 2019-03-10 11:55:37
    ## Trial: 9, mtry: 6 --- 2019-03-10 11:55:43
    ## Trial: 9, mtry: 7 --- 2019-03-10 11:55:50
    ## Trial: 9, mtry: 8 --- 2019-03-10 11:55:59
    ## Trial: 9, mtry: 9 --- 2019-03-10 11:56:08
    ## Trial: 10, mtry: 3 --- 2019-03-10 11:56:18
    ## Trial: 10, mtry: 4 --- 2019-03-10 11:56:22
    ## Trial: 10, mtry: 5 --- 2019-03-10 11:56:28
    ## Trial: 10, mtry: 6 --- 2019-03-10 11:56:34
    ## Trial: 10, mtry: 7 --- 2019-03-10 11:56:42
    ## Trial: 10, mtry: 8 --- 2019-03-10 11:56:50
    ## Trial: 10, mtry: 9 --- 2019-03-10 11:57:00
    ## Trial: 11, mtry: 3 --- 2019-03-10 11:57:10
    ## Trial: 11, mtry: 4 --- 2019-03-10 11:57:14
    ## Trial: 11, mtry: 5 --- 2019-03-10 11:57:20
    ## Trial: 11, mtry: 6 --- 2019-03-10 11:57:27
    ## Trial: 11, mtry: 7 --- 2019-03-10 11:57:34
    ## Trial: 11, mtry: 8 --- 2019-03-10 11:57:43
    ## Trial: 11, mtry: 9 --- 2019-03-10 11:57:52
    ## Trial: 12, mtry: 3 --- 2019-03-10 11:58:02
    ## Trial: 12, mtry: 4 --- 2019-03-10 11:58:07
    ## Trial: 12, mtry: 5 --- 2019-03-10 11:58:12
    ## Trial: 12, mtry: 6 --- 2019-03-10 11:58:19
    ## Trial: 12, mtry: 7 --- 2019-03-10 11:58:27
    ## Trial: 12, mtry: 8 --- 2019-03-10 11:58:35
    ## Trial: 12, mtry: 9 --- 2019-03-10 11:58:44
    ## Trial: 13, mtry: 3 --- 2019-03-10 11:58:54
    ## Trial: 13, mtry: 4 --- 2019-03-10 11:58:59
    ## Trial: 13, mtry: 5 --- 2019-03-10 11:59:05
    ## Trial: 13, mtry: 6 --- 2019-03-10 11:59:12
    ## Trial: 13, mtry: 7 --- 2019-03-10 11:59:19
    ## Trial: 13, mtry: 8 --- 2019-03-10 11:59:27
    ## Trial: 13, mtry: 9 --- 2019-03-10 11:59:36
    ## Trial: 14, mtry: 3 --- 2019-03-10 11:59:47
    ## Trial: 14, mtry: 4 --- 2019-03-10 11:59:51
    ## Trial: 14, mtry: 5 --- 2019-03-10 11:59:57
    ## Trial: 14, mtry: 6 --- 2019-03-10 12:00:03
    ## Trial: 14, mtry: 7 --- 2019-03-10 12:00:10
    ## Trial: 14, mtry: 8 --- 2019-03-10 12:00:19
    ## Trial: 14, mtry: 9 --- 2019-03-10 12:00:28
    ## Trial: 15, mtry: 3 --- 2019-03-10 12:00:38
    ## Trial: 15, mtry: 4 --- 2019-03-10 12:00:43
    ## Trial: 15, mtry: 5 --- 2019-03-10 12:00:48
    ## Trial: 15, mtry: 6 --- 2019-03-10 12:00:54
    ## Trial: 15, mtry: 7 --- 2019-03-10 12:01:02
    ## Trial: 15, mtry: 8 --- 2019-03-10 12:01:10
    ## Trial: 15, mtry: 9 --- 2019-03-10 12:01:19
    ## Trial: 16, mtry: 3 --- 2019-03-10 12:01:30
    ## Trial: 16, mtry: 4 --- 2019-03-10 12:01:35
    ## Trial: 16, mtry: 5 --- 2019-03-10 12:01:40
    ## Trial: 16, mtry: 6 --- 2019-03-10 12:01:47
    ## Trial: 16, mtry: 7 --- 2019-03-10 12:01:54
    ## Trial: 16, mtry: 8 --- 2019-03-10 12:02:03
    ## Trial: 16, mtry: 9 --- 2019-03-10 12:02:12
    ## Trial: 17, mtry: 3 --- 2019-03-10 12:02:23
    ## Trial: 17, mtry: 4 --- 2019-03-10 12:02:27
    ## Trial: 17, mtry: 5 --- 2019-03-10 12:02:33
    ## Trial: 17, mtry: 6 --- 2019-03-10 12:02:39
    ## Trial: 17, mtry: 7 --- 2019-03-10 12:02:46
    ## Trial: 17, mtry: 8 --- 2019-03-10 12:02:55
    ## Trial: 17, mtry: 9 --- 2019-03-10 12:03:04
    ## Trial: 18, mtry: 3 --- 2019-03-10 12:03:14
    ## Trial: 18, mtry: 4 --- 2019-03-10 12:03:19
    ## Trial: 18, mtry: 5 --- 2019-03-10 12:03:25
    ## Trial: 18, mtry: 6 --- 2019-03-10 12:03:31
    ## Trial: 18, mtry: 7 --- 2019-03-10 12:03:39
    ## Trial: 18, mtry: 8 --- 2019-03-10 12:03:47
    ## Trial: 18, mtry: 9 --- 2019-03-10 12:03:56
    ## Trial: 19, mtry: 3 --- 2019-03-10 12:04:07
    ## Trial: 19, mtry: 4 --- 2019-03-10 12:04:11
    ## Trial: 19, mtry: 5 --- 2019-03-10 12:04:17
    ## Trial: 19, mtry: 6 --- 2019-03-10 12:04:23
    ## Trial: 19, mtry: 7 --- 2019-03-10 12:04:31
    ## Trial: 19, mtry: 8 --- 2019-03-10 12:04:39
    ## Trial: 19, mtry: 9 --- 2019-03-10 12:04:48
    ## Trial: 20, mtry: 3 --- 2019-03-10 12:04:59
    ## Trial: 20, mtry: 4 --- 2019-03-10 12:05:03
    ## Trial: 20, mtry: 5 --- 2019-03-10 12:05:09
    ## Trial: 20, mtry: 6 --- 2019-03-10 12:05:16
    ## Trial: 20, mtry: 7 --- 2019-03-10 12:05:23
    ## Trial: 20, mtry: 8 --- 2019-03-10 12:05:32
    ## Trial: 20, mtry: 9 --- 2019-03-10 12:05:41

Problem 2
---------

Problem 8 from Chapter 8 in the text. Set your seed with 9823 and split into train/test using 50% of your data in each split. In addition to parts (a) - (e), do the following:

1.  Fit a gradient-boosted tree to the training data and report the estimated test MSE.
2.  Fit a multiple regression model to the training data and report the estimated test MSE
3.  Summarize your results.

Answer 2
--------

#### Part a

``` r
set.seed(9823)
train <- sample(1:nrow(Carseats), nrow(Carseats), nrow(Carseats)/2)
train_carseats <- Carseats[train, ]
test_carseats <- Carseats[-train, ]
```

#### Part b

``` r
#Regression Tree
carseats_tree <- tree(Sales ~ ., data = train_carseats)
summary(carseats_tree)
```

    ## 
    ## Regression tree:
    ## tree(formula = Sales ~ ., data = train_carseats)
    ## Variables actually used in tree construction:
    ## [1] "ShelveLoc"   "Price"       "Advertising" "Education"   "CompPrice"  
    ## [6] "Income"      "Age"        
    ## Number of terminal nodes:  17 
    ## Residual mean deviance:  2.085 = 798.7 / 383 
    ## Distribution of residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -4.71900 -0.87300 -0.04756  0.00000  1.06300  4.46600

``` r
plot(carseats_tree)
text(carseats_tree, pretty = 0)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
pred_carseats <- predict(carseats_tree, newdata = test_carseats)
test_sales <- Carseats[-train, "Sales"]
mean((pred_carseats-test_sales)^2)
```

    ## [1] 4.847714

#### Part c

``` r
croVal_carseats <- cv.tree(carseats_tree)
plot(croVal_carseats$size, croVal_carseats$dev, type = "b")
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
tree_min <- which.min(croVal_carseats$dev)

carseat_prune <- prune.tree(carseats_tree, best = 5)
plot(carseat_prune)
text(carseat_prune,pretty = 0)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
predict_carseats <- predict(carseats_tree, newdata = test_carseats)
plot(predict_carseats, test_sales)
abline(0, 1)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
mean((predict_carseats-test_sales)^2)
```

    ## [1] 4.847714

#### Part d

``` r
carseats_bag <- randomForest(Sales ~ ., data = train_carseats, mtry = 10, ntree = 25, importance = TRUE)
predict_bag <- predict(carseats_bag, newdata = test_carseats)
mean((predict_bag-test_sales)^2)
```

    ## [1] 2.93594

``` r
importance(carseats_bag)
```

    ##               %IncMSE IncNodePurity
    ## CompPrice   16.317107    384.976793
    ## Income       7.021871    179.151792
    ## Advertising 10.305496    251.643197
    ## Population   3.034718    105.111986
    ## Price       24.053031    878.733533
    ## ShelveLoc   18.780270    698.770705
    ## Age          7.351579    219.862453
    ## Education    4.631847    113.053308
    ## Urban        1.328569      8.352597
    ## US           3.205622     17.100850

#### Part e

``` r
carseats_RF <- randomForest(Sales ~ ., data = train_carseats, mtry = 3, importance = TRUE)
predict_RF <- predict(carseats_RF, newdata = test_carseats)
mean((predict_RF - test_sales)^2)
```

    ## [1] 2.949677

``` r
importance(carseats_RF)
```

    ##              %IncMSE IncNodePurity
    ## CompPrice   40.41487     314.47115
    ## Income      31.29056     270.25897
    ## Advertising 32.33394     281.44223
    ## Population  22.98604     194.93760
    ## Price       59.32788     695.64025
    ## ShelveLoc   58.98360     627.98256
    ## Age         34.67376     302.59348
    ## Education   24.73089     140.91316
    ## Urban       10.25157      28.32842
    ## US          12.75097      40.40530

#### Additional Part

``` r
#Gradient-Boosted Tree
train_carseats_gbm <- gbm(Sales ~ . ,data = train_carseats, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

summary(train_carseats_gbm)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-7-1.png)

    ##                     var    rel.inf
    ## Price             Price 33.7943852
    ## ShelveLoc     ShelveLoc 27.7870527
    ## CompPrice     CompPrice 14.4247681
    ## Advertising Advertising  9.4248547
    ## Age                 Age  8.4260023
    ## Income           Income  4.6145911
    ## Education     Education  0.6423640
    ## Population   Population  0.6065766
    ## US                   US  0.1906127
    ## Urban             Urban  0.0887926

``` r
par(mfrow = c(1,2))
plot(train_carseats_gbm, i = "Price")
plot(train_carseats_gbm, i = "ShelveLoc")
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
test_gbm_pred <- predict(train_carseats_gbm, newdata = test_carseats, n.trees = 5000)
mean((test_gbm_pred-test_sales)^2)
```

    ## [1] 1.959018

Summary: Of all the models ran here, the boosted-gradient model has the lowest MSE.
