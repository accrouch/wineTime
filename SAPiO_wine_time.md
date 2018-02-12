SAP.iO Data Science Challenge
================
kian kamyab
February 12, 2018

*What makes a wine 'good'?*
---------------------------

I love wine. I first fell in love with Grüner Veltliner. It screams summer. It makes you want to swim in the Danube, even in the winter. That doesn't mean that you should.

We're interested in what makes a good wine. Let's find out. On its face (or bottle's label?) this is a classification problem. Since we're interested in the *what* of wine quality, I'll avoid less interpretable models that might do quite well at predicting wine quality--like support vector machines or neural nets.

Here's an outline to my approach in getting us from grape to glass--

-   Uncorking the data - exploratory analysis
    -   What measures do we have?
    -   What do they look like (distributions, missingness)
    -   How do they relate to each other (correlations)
-   Decanting the data - feature engineering
-   See, sniff, & sip - modeling
    -   Random Forest
-   Next pour - if time were as plentiful as 2-buck-chuck

Uncorking the data
------------------

*each day now the cork more sweetly leaves the bottle* (Robert Lowell)

### What measures do we have?

I like to know the names of what we're working with:

``` r
names(sapWineTime)
```

    ##  [1] "type"                 "fixed.acidity"        "volatile.acidity"    
    ##  [4] "citric.acid"          "astringency.rating"   "residual.sugar"      
    ##  [7] "chlorides"            "free.sulfur.dioxide"  "total.sulfur.dioxide"
    ## [10] "density"              "pH"                   "sulphates"           
    ## [13] "alcohol"              "vintage"              "quality"

And their basic summary stats:

``` r
summary(sapWineTime)
```

    ##     type      fixed.acidity    volatile.acidity  citric.acid    
    ##  red  :1599   Min.   : 3.800   Min.   :0.0800   Min.   :0.0000  
    ##  white:4898   1st Qu.: 6.400   1st Qu.:0.2300   1st Qu.:0.2500  
    ##               Median : 7.000   Median :0.2900   Median :0.3100  
    ##               Mean   : 7.215   Mean   :0.3391   Mean   :0.3186  
    ##               3rd Qu.: 7.700   3rd Qu.:0.4000   3rd Qu.:0.3900  
    ##               Max.   :15.900   Max.   :1.5800   Max.   :1.6600  
    ##                                NA's   :299                      
    ##  astringency.rating residual.sugar     chlorides       free.sulfur.dioxide
    ##  Min.   :0.410      Min.   : 0.600   Min.   :0.00900   Min.   :  1.00     
    ##  1st Qu.:0.670      1st Qu.: 1.800   1st Qu.:0.03800   1st Qu.: 17.00     
    ##  Median :0.730      Median : 3.000   Median :0.04700   Median : 29.00     
    ##  Mean   :0.751      Mean   : 5.459   Mean   :0.05603   Mean   : 30.53     
    ##  3rd Qu.:0.800      3rd Qu.: 8.100   3rd Qu.:0.06500   3rd Qu.: 41.00     
    ##  Max.   :1.620      Max.   :65.800   Max.   :0.61100   Max.   :289.00     
    ##  NA's   :332        NA's   :2364                                          
    ##  total.sulfur.dioxide    density             pH          sulphates     
    ##  Min.   :  6.0        Min.   :0.9871   Min.   :2.720   Min.   :0.2200  
    ##  1st Qu.: 77.0        1st Qu.:0.9923   1st Qu.:3.110   1st Qu.:0.4300  
    ##  Median :118.0        Median :0.9949   Median :3.210   Median :0.5100  
    ##  Mean   :115.7        Mean   :0.9947   Mean   :3.218   Mean   :0.5313  
    ##  3rd Qu.:156.0        3rd Qu.:0.9970   3rd Qu.:3.320   3rd Qu.:0.6000  
    ##  Max.   :440.0        Max.   :1.0390   Max.   :4.010   Max.   :2.0000  
    ##                                        NA's   :62                      
    ##     alcohol         vintage        quality     
    ##  Min.   : 8.00   Min.   :2001   Min.   :3.000  
    ##  1st Qu.: 9.50   1st Qu.:2004   1st Qu.:5.000  
    ##  Median :10.30   Median :2005   Median :6.000  
    ##  Mean   :10.49   Mean   :2005   Mean   :5.818  
    ##  3rd Qu.:11.30   3rd Qu.:2007   3rd Qu.:6.000  
    ##  Max.   :14.90   Max.   :2008   Max.   :9.000  
    ##                  NA's   :69

### What do these measures look like?

#### Distributions

Let's take a look at the distribution of our continuous variables. I identify the continuous variables as those measures with more than 10 unique levels.

``` r
for (i in names(sapWineTime[,(apply(sapWineTime, 2, function(x) length(unique(x))) > 10)])) {
  print(i)
  plot <- ggplot(sapWineTime, 
                 aes(get(i), fill = type)) +
            geom_histogram() +
            theme_solarized_2() +
            theme(plot.title = element_text(hjust = 0.5), 
                  legend.position = 'bottom') +
            labs(x = paste(i), 
                 title = paste('Distribution of ', i)) +
            facet_wrap(~type)
  print(plot)
}
```

    ## [1] "fixed.acidity"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-1.png" style="display: block; margin: auto;" />

    ## [1] "volatile.acidity"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-2.png" style="display: block; margin: auto;" />

    ## [1] "citric.acid"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-3.png" style="display: block; margin: auto;" />

    ## [1] "astringency.rating"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-4.png" style="display: block; margin: auto;" />

    ## [1] "residual.sugar"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-5.png" style="display: block; margin: auto;" />

    ## [1] "chlorides"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-6.png" style="display: block; margin: auto;" />

    ## [1] "free.sulfur.dioxide"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-7.png" style="display: block; margin: auto;" />

    ## [1] "total.sulfur.dioxide"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-8.png" style="display: block; margin: auto;" />

    ## [1] "density"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-9.png" style="display: block; margin: auto;" />

    ## [1] "pH"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-10.png" style="display: block; margin: auto;" />

    ## [1] "sulphates"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-11.png" style="display: block; margin: auto;" />

    ## [1] "alcohol"

<img src="SAPiO_wine_time_files/figure-markdown_github/uncorking the data - distributions-12.png" style="display: block; margin: auto;" />

These distributions don't look too worrisome. Some make complete sense. In my time working behind a bar and learning about the production of wine, the right-skewed distributions of `citric.acid`, `free.sulfur.dioxide`, and `total.sulfur.dioxide` in red wines and `residual.sugar` in both red and white wines make contextual sense.

#### Missingness

Missingness is always an issue with real data. Hopefully it's missing at random. I'm primarily interested in whether missing data is related at all with wine type or quality scores. Let's do a few things to quickly explore whether missingness is in fact random:

-   Examine overall counts of missing values by type and type + quality
-   Visual inspection
-   Look at t-tests of missingness by type, ANOVA results of missingess by quality rating, and ANOVA results of missigness by type and quality rating

``` r
# here i'm creating a summary data frame aggregated to the type and type+quality-level 
# with a count of missing values and proportion of observations with missing values
# for each of the physicochemical variables

# type-level missingness
#

sapWineTimeMissOverall <- data.frame(aggregate(. ~ type, data = sapWineTime, 
                     function(x) {sum(is.na(x))/length(x)},
                     na.action = NULL))

sapWineTimeMissOverall <- melt(sapWineTimeMissOverall,
                               id.vars = 'type',
                               value.name = 'percMissing')

sapWineTimeMissOverall <- sapWineTimeMissOverall %>%
  filter(percMissing > 0)

# type+quality-level missingness
#

sapWineTimeMiss <- data.frame(aggregate(. ~ type + quality, 
                                        data = sapWineTime, 
                                        function(x) {sum(is.na(x))/length(x)}, 
                                        na.action = NULL))

sapWineTimeMiss <- melt(sapWineTimeMiss, 
                        id.vars = c('type', 'quality'), 
                        value.name = 'percMissing')

sapWineTimeMiss <- cbind(sapWineTimeMiss, 
                  melt(aggregate(. ~ type + quality, 
                                 data = sapWineTime, 
                                 function(x) {sum(is.na(x))}, 
                                 na.action = NULL),
                       id.vars = c('type', 'quality'), 
                       value.name = 'countMissing')[4])

sapWineTimeMiss <- sapWineTimeMiss %>%
  filter(countMissing > 0)

# creating plot object using the data frames created above
#

missTypePlot <- 
  ggplot(sapWineTimeMissOverall, 
         aes(x = variable, y = percMissing, fill = type)) +
  geom_bar(stat = 'identity', 
           position = 'dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = 'Percent Missingness by Wine Type', 
       y = 'Percent Missing') +
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5))

missTypeQualMissPlot <- 
  ggplot(sapWineTimeMiss, 
         aes(x = quality, y = percMissing, fill = type)) +
  geom_bar(stat = 'identity', 
           position = 'dodge') +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(3, 9, 1)) +
  labs(title = 'Percent Missingness by Wine Type & Quality Rating', 
       y = 'Percent Missing') +
  facet_wrap(~variable, 
             ncol = 1) + 
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
```

``` r
missTypePlot
```

<img src="SAPiO_wine_time_files/figure-markdown_github/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
missTypeQualMissPlot
```

<img src="SAPiO_wine_time_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

For the most part, it looks like variables are missing at random when we stratify by wine type and quality scores. There are a couple of worrisome spots, like the proportion of 3-star white wines missing `volatile.acidity` and `astringency.rating`, or 9-star white wines missing `residual.sugar`, but those cells are relatively small and we can likely combine them with other star ratings when we get to classification models.

The more concerning issue is the proportion of observations that are missing `residual.sugar` (~36% of all observations). We have a few choices about how to deal with this: (1) impute the missing values, (2) drop the feature from any modeling, (3) buy the 2,364 bottles with missing values and measure `residual.sugar` ourselves. I like that idea.

Before we decide anything, let's just take a quick look at whether missingness is occurring at random:

``` r
# testing for missingness at random by creating a sparse matrix of indicators for (not) missingness
# and binding type and quality in order to run some t-tests and anovas
#

sapWineTimeMAR <- cbind(sapWineTime[, c('type', 'quality')], 
              sapply(sapWineTime[, as.character(unique(sapWineTimeMiss$variable))], 
                     function(x) ifelse(is.na(x), 1, 0)))

for (i in names(sapWineTimeMAR[ , 3:dim(sapWineTimeMAR)[2]])) {
  print(paste('******************  ', i, '  ******************'))
  print(paste('t-test p-value :', t.test(get(i) ~ type, data = sapWineTimeMAR)$p.value))
  print(summary(aov(get(i) ~ quality, data = sapWineTimeMAR)))
  print(summary(aov(get(i) ~ type*quality, data = sapWineTimeMAR)))
}
```

    ## [1] "******************   volatile.acidity   ******************"
    ## [1] "t-test p-value : 0.467368526620972"
    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## quality        1   0.11  0.1133   2.582  0.108
    ## Residuals   6495 285.13  0.0439               
    ##                Df Sum Sq Mean Sq F value Pr(>F)
    ## type            1   0.02 0.02430   0.554  0.457
    ## quality         1   0.10 0.10262   2.338  0.126
    ## type:quality    1   0.11 0.11083   2.525  0.112
    ## Residuals    6493 285.00 0.04389               
    ## [1] "******************   astringency.rating   ******************"
    ## [1] "t-test p-value : 0.530357469563102"
    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## quality        1      0 0.00107   0.022  0.882
    ## Residuals   6495    315 0.04850               
    ##                Df Sum Sq Mean Sq F value Pr(>F)  
    ## type            1   0.02 0.01840   0.379 0.5379  
    ## quality         1   0.00 0.00028   0.006 0.9399  
    ## type:quality    1   0.17 0.16925   3.490 0.0618 .
    ## Residuals    6493 314.85 0.04849                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## [1] "******************   residual.sugar   ******************"
    ## [1] "t-test p-value : 0.667650024775958"
    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## quality        1      0 0.01092   0.047  0.828
    ## Residuals   6495   1504 0.23154               
    ##                Df Sum Sq Mean Sq F value Pr(>F)
    ## type            1    0.0  0.0429   0.185  0.667
    ## quality         1    0.0  0.0169   0.073  0.787
    ## type:quality    1    0.3  0.3345   1.444  0.229
    ## Residuals    6493 1503.4  0.2315               
    ## [1] "******************   pH   ******************"
    ## [1] "t-test p-value : 0.128430040544234"
    ##               Df Sum Sq  Mean Sq F value Pr(>F)
    ## quality        1   0.01 0.009169    0.97  0.325
    ## Residuals   6495  61.40 0.009453               
    ##                Df Sum Sq  Mean Sq F value Pr(>F)  
    ## type            1   0.03 0.027341   2.893  0.089 .
    ## quality         1   0.01 0.005863   0.620  0.431  
    ## type:quality    1   0.00 0.003889   0.411  0.521  
    ## Residuals    6493  61.37 0.009452                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## [1] "******************   vintage   ******************"
    ## [1] "t-test p-value : 0.995931142411669"
    ##               Df Sum Sq  Mean Sq F value Pr(>F)
    ## quality        1   0.00 0.000474   0.045  0.832
    ## Residuals   6495  68.27 0.010511               
    ##                Df Sum Sq  Mean Sq F value Pr(>F)
    ## type            1   0.00 0.000000   0.000  0.996
    ## quality         1   0.00 0.000483   0.046  0.830
    ## type:quality    1   0.00 0.000024   0.002  0.962
    ## Residuals    6493  68.27 0.010514

It looks like values are missing at random. For the sake of time, we're going to either drop `residual.sugar` from analyses (if it's highly correlated with another variable) or coerce it to be a categorical variable and introduce a "missing" category.

### How do measures relate?

I have a very strong suspicion that the physicochemical characteristics of wine are correlated and these correlations differ by wine type. I'll take a visual approach in assessing these correlations. This will help me do three things:

1.  What measures seem to be related with quality?
2.  Do some measures tell me the same information?
3.  Is there a reasonable (and preliminary) case to stratify my analyses by wine type?

#### Plotting correlations

I plot correlations by creating correlation matrices, shaping them into easily plottable (tidy) data frames, and using those data frames to create tile heat maps.

``` r
# creating correlation matrices from data, dropping type as i'll create separate matrices for red and white wines
wineCorMat <- round(cor(select(sapWineTime, -type), 
                        use = 'na.or.complete'),
                    2)
wineCorMatWhite <- round(cor(split(select(sapWineTime, -type), 
                                   sapWineTime$type)$white, 
                             use = 'na.or.complete'),
                         2)
wineCorMatRed <- round(cor(split(select(sapWineTime, -type), 
                                 sapWineTime$type)$red, 
                           use = 'na.or.complete'),
                       2)

# reducing information redundancy for the purposes of creating a clean plot later on

wineCorMat[lower.tri(wineCorMat)] <- NA
wineCorMatWhite[lower.tri(wineCorMatWhite)] <- NA
wineCorMatRed[lower.tri(wineCorMatRed)] <- NA


# creating tidy data frames to for easy plotting and creating a data frame that's the difference between red and white wine correlations

wineCorMatMelt <- melt(wineCorMat, na.rm = TRUE)
wineCorMatWhiteMelt <- melt(wineCorMatWhite, na.rm = TRUE)
wineCorMatRedMelt <- melt(wineCorMatRed, na.rm = TRUE)
wineCorMatDiff <- data.frame(wineCorMatMelt[, 1:2], 
                             value = abs(wineCorMatWhiteMelt[, 3] - wineCorMatRedMelt[, 3]))

ggplot(data = wineCorMatMelt, 
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'All Wines - Correlation Matrix') +
  scale_fill_gradient2_tableau(palette = 'Temperature')
```

<img src="SAPiO_wine_time_files/figure-markdown_github/plotting correlations-1.png" style="display: block; margin: auto;" />

``` r
ggplot(data = wineCorMatWhiteMelt, 
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'White Wines - Correlation Matrix') +
  scale_fill_gradient2_tableau(palette = 'Temperature')
```

<img src="SAPiO_wine_time_files/figure-markdown_github/plotting correlations-2.png" style="display: block; margin: auto;" />

``` r
ggplot(data = wineCorMatRedMelt, 
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Red Wines - Correlation Matrix') +
  scale_fill_gradient2_tableau(palette = 'Temperature')
```

<img src="SAPiO_wine_time_files/figure-markdown_github/plotting correlations-3.png" style="display: block; margin: auto;" />

``` r
ggplot(data = wineCorMatDiff, 
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme_solarized_2() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Difference Between White and Red Wine Correlation Coefficients') +
  scale_fill_gradient2_tableau(palette = 'Temperature')
```

<img src="SAPiO_wine_time_files/figure-markdown_github/plotting correlations-4.png" style="display: block; margin: auto;" />

#### Interpreting correlations

Three correlations jump out when looking at wine overall:

-   `fixed.acidity`-`astringency.rating` (r = 0.99)
-   `free.sulfur.dioxide`-`total.sulfur.dioxide` (r = 0.73)
-   `density`-`alcohol` (r = -0.67)

Looking at white wines only:

-   `fixed.acidity`-`astringency.rating` (r = 0.99)
-   `residual.sugar`-`density` (r = 0.85)
-   `density`-`alcohol` (r = -0.76)
-   `free.sulfur.dioxide`-`total.sulfur.dioxide` (r = 0.62)

And red wines:

-   `fixed.acidity`-`astringency.rating` (r = 0.99)
-   `fixed.acidity`-`citric.acid` (r = 0.68)
-   `free.sulfur.dioxide`-`total.sulfur.dioxide` (r = 0.68)
-   `astringency.rating`-`density` (r = 0.68)
-   `fixed.acidity`-`pH` (r = -0.68)
-   `astringency.rating`-`pH` (r = -0.67)
-   `fixed.acidity`-`density` (r = 0.66)
-   `citric.acidity`-`astringency.rating` (r = 0.63)

For the purposes of a prelimary model, I think there is evidence to run separate models for white and red wines.

### Decanting the data

You know what, I'm going to pour these data into a random forest model for a variety of reasons, one of which is not having to do much feature engineering. For other types of models, we'd have to decant the data a bit more.

### See, Sniff, and Sip - Modeling

I've decided to take a walk through a random forest. While a random forest model poses some limitations in trying to assess what makes a good wine, I think as a preliminary analysis, it poses a number of advantages I'll touch upon when looking at the results.

``` r
# setting a seed for reproducibility 
#

set.seed(1786)

# creating separate data frames for the red and white wine data
#

sapWineTimeWhite <- (split(sapWineTime, f = sapWineTime$type))$white
sapWineTimeRed <- (split(sapWineTime, f = sapWineTime$type))$red
```

#### Fitting models

``` r
### wines overall

# dropping highly correlated variables 
#

sapWineTimeModel <- sapWineTime %>%
  select(-residual.sugar, -free.sulfur.dioxide) %>%
  na.omit()

# creating training & testing data sets (70/30 split)
#

sapWineTimeModelIndex <- sample.split(row.names(sapWineTimeModel), SplitRatio = .7)
sapWineTimeTrain <- sapWineTimeModel[sapWineTimeModelIndex, ]
sapWineTimeTest <- sapWineTimeModel[!sapWineTimeModelIndex, ]

# fitting a random forest model
#

model <- 
  train(quality ~ .,
        tuneGrid = data.frame(mtry = c(2, 4, 6, 8)),
        data = sapWineTimeTrain, 
        method = 'rf',
        preProcess = c('scale', 'center'), 
        trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
        importance = TRUE
)

### white wines

# dropping highly correlated variables 
#

sapWineTimeWhiteModel <- sapWineTimeWhite %>%
  select(-residual.sugar, -type, -fixed.acidity, -density) %>%
  na.omit()

# creating training & testing data sets (70/30 split)
#

sapWineTimeWhiteModelIndex <- sample.split(row.names(sapWineTimeWhiteModel), SplitRatio = .7)
sapWineTimeWhiteTrain <- sapWineTimeWhiteModel[sapWineTimeWhiteModelIndex, ]
sapWineTimeWhiteTest <- sapWineTimeWhiteModel[!sapWineTimeWhiteModelIndex, ]

# fitting a random forest model 
#

modelWhite <- 
  train(quality ~ .,
        tuneGrid = data.frame(mtry = c(2, 4, 6, 8)),
        data = sapWineTimeWhiteTrain, 
        method = 'rf',
        preProcess = c('scale', 'center'), 
        trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
        importance = TRUE
)

#### Red Wines

# dropping highly correlated variables 
#

sapWineTimeRedModel <- sapWineTimeRed %>%
  select(-residual.sugar, -type, -fixed.acidity, -astringency.rating) %>%
  na.omit()

# creating training & testing data sets (70/30 split)
#

sapWineTimeRedModelIndex <- sample.split(row.names(sapWineTimeRedModel), SplitRatio = .7)
sapWineTimeRedTrain <- sapWineTimeRedModel[sapWineTimeRedModelIndex, ]
sapWineTimeRedTest <- sapWineTimeRedModel[!sapWineTimeRedModelIndex, ]

# fitting a random forest model 
#

modelRed <- 
  train(quality ~ .,
        tuneGrid = data.frame(mtry = c(2, 4, 6, 8, 12)),
        data = sapWineTimeRedTrain, 
        method = 'rf',
        preProcess = c('scale', 'center'), 
        trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
        importance = TRUE
)
```

#### Exploring results

Here we'll take a look at a plot of RMSE by the number of predictors selected to evaluate each tree's split (which informs tuning a random forest), variable importance (basically, how much does MSE change if we shuffled the values of each predictor), and model RMSE, R^2, and MAE.

First, all wines, then white wines, and finally red wines.

``` r
# all wines
#

plot(model)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-1.png)

``` r
varImp(model$finalModel)
```

    ##                        Overall
    ## typewhite            11.482076
    ## fixed.acidity        35.397110
    ## volatile.acidity     70.706551
    ## citric.acid          46.935944
    ## astringency.rating   36.400236
    ## chlorides            41.371509
    ## total.sulfur.dioxide 46.710809
    ## density              42.956197
    ## pH                   50.749855
    ## sulphates            50.558336
    ## alcohol              87.013074
    ## vintage               1.759617

``` r
modelAllPlot <- varImpPlot(model$finalModel)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-2.png)

``` r
predQualTest <- data.frame(obs = sapWineTimeTest$quality, 
                                pred = predict(model, sapWineTimeTest))
defaultSummary(predQualTest)
```

    ##      RMSE  Rsquared       MAE 
    ## 0.5966600 0.5126449 0.4514246

``` r
# white wines
#

plot(modelWhite)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-3.png)

``` r
varImp(modelWhite$finalModel)
```

    ##                         Overall
    ## volatile.acidity      76.959334
    ## citric.acid           41.886993
    ## astringency.rating    39.231212
    ## chlorides             39.811943
    ## free.sulfur.dioxide   62.438892
    ## total.sulfur.dioxide  43.000423
    ## pH                    48.371861
    ## sulphates             38.432464
    ## alcohol              109.773568
    ## vintage               -1.860498

``` r
modelWhitePlot <- varImpPlot(modelWhite$finalModel)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-4.png)

``` r
predWhiteQualTest <- data.frame(obs = sapWineTimeWhiteTest$quality, 
                                pred = predict(modelWhite, sapWineTimeWhiteTest))
defaultSummary(predWhiteQualTest)
```

    ##      RMSE  Rsquared       MAE 
    ## 0.6141031 0.4810829 0.4546544

``` r
# red wines
#

plot(modelRed)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-5.png)

``` r
varImp(modelRed$finalModel)
```

    ##                         Overall
    ## volatile.acidity     32.9586902
    ## citric.acid          20.2356202
    ## chlorides            23.3502440
    ## free.sulfur.dioxide  16.2976840
    ## total.sulfur.dioxide 31.6620406
    ## density              25.1654396
    ## pH                   21.0013527
    ## sulphates            47.6665851
    ## alcohol              61.2870524
    ## vintage               0.6595523

``` r
modelRedPlot <- varImpPlot(modelRed$finalModel)
```

![](SAPiO_wine_time_files/figure-markdown_github/see,%20sniff,%20&%20sip%20-%20results-6.png)

``` r
predRedQualTest <- data.frame(obs = sapWineTimeRedTest$quality, 
                                pred = predict(modelRed, sapWineTimeRedTest))
defaultSummary(predRedQualTest)
```

    ##      RMSE  Rsquared       MAE 
    ## 0.5599837 0.4866800 0.4240723

Random forests are nice for a first look in that I don't have to do much feature engineering, they perform relatively well, and they can point me in the direction of which of the physicochemical properties of wine are predictive of quality. The primary drawback is that a random forest model doesn't give me as granular of a look at the direction and magnitude of the phsyicochemical-quality relationship as say a logistic regression or a decision tree.

The results of the random forest suggests that for wines overall, `alcohol` and `volatile.acidity` are highly predictive of quality ratings relative to other physicochemical properties. This is similar for white wines. Red wines, `alcohol` and `sulphates` have the greatest predictive power. We don't have much of a sense of which direction of these relationships, which is something we can explore utilizing other models.

### Next pour - if time were as plentiful as 2-buck-chuck

-   Measures
    -   I'd take a little time to brush up on some of the physicochemical measures and how they relate to the winemaking process--context matters!
    -   consider transformations/binning
-   Missingness
    -   explore imputation
    -   consider the use of binned variables and include a 'missing' category
-   Correlations
    -   more meaningful investigation of correlation differences
    -   dimension reduction through principal components or factor analyses (at the risk of losing interpretability)
-   Feature engineering
    -   use of principal components or factors to mitigate , but this would come at the cost of interpretability
-   Modeling
    -   logistic regression and interaction investigation (it's likely wine type interacts with physicochemical characteristics in the relationship with quality ratings)
    -   ordered logistic regression
    -   linear discriminant analysis
    -   plan ole' decision tree
    -   ensembling
-   Programming
    -   use of functions for repeated tasks
    -   clean up output a bit
-   Misc
    -   drink more wine
