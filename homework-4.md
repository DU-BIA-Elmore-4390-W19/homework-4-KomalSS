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
?randomForest
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
      rf_train1 <- randomForest(medv ~ .,
                               data = train_boston,
                               mtry = mtry[i],
                               ntree = ntree[j])
      mse <- mean((predict(rf_train1, newdata = test_boston) - test_boston$medv)^2)
      results[(i-1)*20 + j, ] <- c(k, mtry[i], ntree[j], mse)
    }
  }
  if(exists("results_total")){
  results_total <- bind_rows(results_total, results)
  }
  else(
  results_total <- results
  )
  #plot(1:500, rf_train1$mse, type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
}
```

    ## Trial: 1, mtry: 3 --- 2019-03-10 17:32:10
    ## Trial: 1, mtry: 4 --- 2019-03-10 17:32:15
    ## Trial: 1, mtry: 5 --- 2019-03-10 17:32:21
    ## Trial: 1, mtry: 6 --- 2019-03-10 17:32:27
    ## Trial: 1, mtry: 7 --- 2019-03-10 17:32:35
    ## Trial: 1, mtry: 8 --- 2019-03-10 17:32:43
    ## Trial: 1, mtry: 9 --- 2019-03-10 17:32:52
    ## Trial: 2, mtry: 3 --- 2019-03-10 17:33:02
    ## Trial: 2, mtry: 4 --- 2019-03-10 17:33:07
    ## Trial: 2, mtry: 5 --- 2019-03-10 17:33:13
    ## Trial: 2, mtry: 6 --- 2019-03-10 17:33:19
    ## Trial: 2, mtry: 7 --- 2019-03-10 17:33:27
    ## Trial: 2, mtry: 8 --- 2019-03-10 17:33:35
    ## Trial: 2, mtry: 9 --- 2019-03-10 17:33:45
    ## Trial: 3, mtry: 3 --- 2019-03-10 17:33:55
    ## Trial: 3, mtry: 4 --- 2019-03-10 17:34:00
    ## Trial: 3, mtry: 5 --- 2019-03-10 17:34:05
    ## Trial: 3, mtry: 6 --- 2019-03-10 17:34:12
    ## Trial: 3, mtry: 7 --- 2019-03-10 17:34:19
    ## Trial: 3, mtry: 8 --- 2019-03-10 17:34:28
    ## Trial: 3, mtry: 9 --- 2019-03-10 17:34:37
    ## Trial: 4, mtry: 3 --- 2019-03-10 17:34:47
    ## Trial: 4, mtry: 4 --- 2019-03-10 17:34:52
    ## Trial: 4, mtry: 5 --- 2019-03-10 17:34:58
    ## Trial: 4, mtry: 6 --- 2019-03-10 17:35:04
    ## Trial: 4, mtry: 7 --- 2019-03-10 17:35:12
    ## Trial: 4, mtry: 8 --- 2019-03-10 17:35:20
    ## Trial: 4, mtry: 9 --- 2019-03-10 17:35:30
    ## Trial: 5, mtry: 3 --- 2019-03-10 17:35:40
    ## Trial: 5, mtry: 4 --- 2019-03-10 17:35:44
    ## Trial: 5, mtry: 5 --- 2019-03-10 17:35:50
    ## Trial: 5, mtry: 6 --- 2019-03-10 17:35:57
    ## Trial: 5, mtry: 7 --- 2019-03-10 17:36:04
    ## Trial: 5, mtry: 8 --- 2019-03-10 17:36:13
    ## Trial: 5, mtry: 9 --- 2019-03-10 17:36:22
    ## Trial: 6, mtry: 3 --- 2019-03-10 17:36:32
    ## Trial: 6, mtry: 4 --- 2019-03-10 17:36:37
    ## Trial: 6, mtry: 5 --- 2019-03-10 17:36:43
    ## Trial: 6, mtry: 6 --- 2019-03-10 17:36:49
    ## Trial: 6, mtry: 7 --- 2019-03-10 17:36:57
    ## Trial: 6, mtry: 8 --- 2019-03-10 17:37:06
    ## Trial: 6, mtry: 9 --- 2019-03-10 17:37:15
    ## Trial: 7, mtry: 3 --- 2019-03-10 17:37:25
    ## Trial: 7, mtry: 4 --- 2019-03-10 17:37:30
    ## Trial: 7, mtry: 5 --- 2019-03-10 17:37:36
    ## Trial: 7, mtry: 6 --- 2019-03-10 17:37:42
    ## Trial: 7, mtry: 7 --- 2019-03-10 17:37:50
    ## Trial: 7, mtry: 8 --- 2019-03-10 17:37:58
    ## Trial: 7, mtry: 9 --- 2019-03-10 17:38:08
    ## Trial: 8, mtry: 3 --- 2019-03-10 17:38:18
    ## Trial: 8, mtry: 4 --- 2019-03-10 17:38:22
    ## Trial: 8, mtry: 5 --- 2019-03-10 17:38:28
    ## Trial: 8, mtry: 6 --- 2019-03-10 17:38:34
    ## Trial: 8, mtry: 7 --- 2019-03-10 17:38:42
    ## Trial: 8, mtry: 8 --- 2019-03-10 17:38:51
    ## Trial: 8, mtry: 9 --- 2019-03-10 17:39:00
    ## Trial: 9, mtry: 3 --- 2019-03-10 17:39:10
    ## Trial: 9, mtry: 4 --- 2019-03-10 17:39:15
    ## Trial: 9, mtry: 5 --- 2019-03-10 17:39:20
    ## Trial: 9, mtry: 6 --- 2019-03-10 17:39:27
    ## Trial: 9, mtry: 7 --- 2019-03-10 17:39:34
    ## Trial: 9, mtry: 8 --- 2019-03-10 17:39:43
    ## Trial: 9, mtry: 9 --- 2019-03-10 17:39:52
    ## Trial: 10, mtry: 3 --- 2019-03-10 17:40:02
    ## Trial: 10, mtry: 4 --- 2019-03-10 17:40:07
    ## Trial: 10, mtry: 5 --- 2019-03-10 17:40:12
    ## Trial: 10, mtry: 6 --- 2019-03-10 17:40:19
    ## Trial: 10, mtry: 7 --- 2019-03-10 17:40:26
    ## Trial: 10, mtry: 8 --- 2019-03-10 17:40:35
    ## Trial: 10, mtry: 9 --- 2019-03-10 17:40:44
    ## Trial: 11, mtry: 3 --- 2019-03-10 17:40:55
    ## Trial: 11, mtry: 4 --- 2019-03-10 17:40:59
    ## Trial: 11, mtry: 5 --- 2019-03-10 17:41:05
    ## Trial: 11, mtry: 6 --- 2019-03-10 17:41:11
    ## Trial: 11, mtry: 7 --- 2019-03-10 17:41:19
    ## Trial: 11, mtry: 8 --- 2019-03-10 17:41:27
    ## Trial: 11, mtry: 9 --- 2019-03-10 17:41:37
    ## Trial: 12, mtry: 3 --- 2019-03-10 17:41:47
    ## Trial: 12, mtry: 4 --- 2019-03-10 17:41:52
    ## Trial: 12, mtry: 5 --- 2019-03-10 17:41:58
    ## Trial: 12, mtry: 6 --- 2019-03-10 17:42:04
    ## Trial: 12, mtry: 7 --- 2019-03-10 17:42:11
    ## Trial: 12, mtry: 8 --- 2019-03-10 17:42:20
    ## Trial: 12, mtry: 9 --- 2019-03-10 17:42:29
    ## Trial: 13, mtry: 3 --- 2019-03-10 17:42:39
    ## Trial: 13, mtry: 4 --- 2019-03-10 17:42:44
    ## Trial: 13, mtry: 5 --- 2019-03-10 17:42:50
    ## Trial: 13, mtry: 6 --- 2019-03-10 17:42:56
    ## Trial: 13, mtry: 7 --- 2019-03-10 17:43:04
    ## Trial: 13, mtry: 8 --- 2019-03-10 17:43:12
    ## Trial: 13, mtry: 9 --- 2019-03-10 17:43:21
    ## Trial: 14, mtry: 3 --- 2019-03-10 17:43:32
    ## Trial: 14, mtry: 4 --- 2019-03-10 17:43:36
    ## Trial: 14, mtry: 5 --- 2019-03-10 17:43:42
    ## Trial: 14, mtry: 6 --- 2019-03-10 17:43:48
    ## Trial: 14, mtry: 7 --- 2019-03-10 17:43:55
    ## Trial: 14, mtry: 8 --- 2019-03-10 17:44:04
    ## Trial: 14, mtry: 9 --- 2019-03-10 17:44:13
    ## Trial: 15, mtry: 3 --- 2019-03-10 17:44:23
    ## Trial: 15, mtry: 4 --- 2019-03-10 17:44:28
    ## Trial: 15, mtry: 5 --- 2019-03-10 17:44:33
    ## Trial: 15, mtry: 6 --- 2019-03-10 17:44:40
    ## Trial: 15, mtry: 7 --- 2019-03-10 17:44:47
    ## Trial: 15, mtry: 8 --- 2019-03-10 17:44:56
    ## Trial: 15, mtry: 9 --- 2019-03-10 17:45:05
    ## Trial: 16, mtry: 3 --- 2019-03-10 17:45:16
    ## Trial: 16, mtry: 4 --- 2019-03-10 17:45:20
    ## Trial: 16, mtry: 5 --- 2019-03-10 17:45:26
    ## Trial: 16, mtry: 6 --- 2019-03-10 17:45:33
    ## Trial: 16, mtry: 7 --- 2019-03-10 17:45:40
    ## Trial: 16, mtry: 8 --- 2019-03-10 17:45:49
    ## Trial: 16, mtry: 9 --- 2019-03-10 17:45:58
    ## Trial: 17, mtry: 3 --- 2019-03-10 17:46:09
    ## Trial: 17, mtry: 4 --- 2019-03-10 17:46:13
    ## Trial: 17, mtry: 5 --- 2019-03-10 17:46:19
    ## Trial: 17, mtry: 6 --- 2019-03-10 17:46:25
    ## Trial: 17, mtry: 7 --- 2019-03-10 17:46:33
    ## Trial: 17, mtry: 8 --- 2019-03-10 17:46:41
    ## Trial: 17, mtry: 9 --- 2019-03-10 17:46:50
    ## Trial: 18, mtry: 3 --- 2019-03-10 17:47:01
    ## Trial: 18, mtry: 4 --- 2019-03-10 17:47:05
    ## Trial: 18, mtry: 5 --- 2019-03-10 17:47:11
    ## Trial: 18, mtry: 6 --- 2019-03-10 17:47:17
    ## Trial: 18, mtry: 7 --- 2019-03-10 17:47:25
    ## Trial: 18, mtry: 8 --- 2019-03-10 17:47:33
    ## Trial: 18, mtry: 9 --- 2019-03-10 17:47:43
    ## Trial: 19, mtry: 3 --- 2019-03-10 17:47:53
    ## Trial: 19, mtry: 4 --- 2019-03-10 17:47:57
    ## Trial: 19, mtry: 5 --- 2019-03-10 17:48:03
    ## Trial: 19, mtry: 6 --- 2019-03-10 17:48:10
    ## Trial: 19, mtry: 7 --- 2019-03-10 17:48:17
    ## Trial: 19, mtry: 8 --- 2019-03-10 17:48:26
    ## Trial: 19, mtry: 9 --- 2019-03-10 17:48:35
    ## Trial: 20, mtry: 3 --- 2019-03-10 17:48:45
    ## Trial: 20, mtry: 4 --- 2019-03-10 17:48:50
    ## Trial: 20, mtry: 5 --- 2019-03-10 17:48:55
    ## Trial: 20, mtry: 6 --- 2019-03-10 17:49:02
    ## Trial: 20, mtry: 7 --- 2019-03-10 17:49:09
    ## Trial: 20, mtry: 8 --- 2019-03-10 17:49:18
    ## Trial: 20, mtry: 9 --- 2019-03-10 17:49:27

``` r
plot(1:500, rf_train1$mse, type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-1-1.png)

As you can see from the plot the Test MSE decreases as the number of trees increase.

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
