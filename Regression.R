########################################################################
#1-var Linear Regression
########################################################################
#lm(formula you wanna fit, dataFrame)
cmodel <- lm(temperature ~ chirps_per_sec, data = cricket)

#Formula
fmla <- temperature ~ chirps_per_sec
fmla <- blood_pressure ~ age + weight
#LHS: outcome, RHS: inputs. 
fmla <- as.formula("temperature ~ chirps_per_sec") #converts string to formula

#conveniently package diagnostics from summary(cmodel) in a dataframe
broom::glance(cmodel)

sigr:wrapFTest(cmodel) #R^2 test

#by default returns training data predictions
predict(model)

ggplot(cricket, aes(x = prediction, y = temperature)) + geom_point() + geom_abline(color = "darkblue") + ggtitle("temperature vs linear model prediction")

#apply model to new data
predict(model, newdata = newChirps)

########################################################################
#Predicting once you fit a model
########################################################################

# unemployment is in your workspace
summary(unemployment)

# newrates is in your workspace
newrates

# Predict female unemployment in the unemployment data set
unemployment$prediction <-  predict(unemployment_model)

# load the ggplot2 package
library(ggplot2)

# Make a plot to compare predictions to actual (prediction on x axis). 
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + 
  geom_point() +
  geom_abline(color = "blue")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newdata = newrates)
# Print it
pred

########################################################################
#Multivariate Linear Regression
########################################################################

# bloodpressure is in the workspace
summary(bloodpressure)

# Create the formula and print it
fmla <- blood_pressure ~ age + weight
fmla

# Fit the model: bloodpressure_model
bloodpressure_model <- lm(fmla, data = bloodpressure)

# Print bloodpressure_model and call summary() 
bloodpressure_model
summary(bloodpressure_model)

# predict blood pressure using bloodpressure_model :prediction
bloodpressure$prediction <- predict(bloodpressure_model)

# plot the prediction vs actual
ggplot(bloodpressure, aes(x = prediction, y = blood_pressure)) + 
  geom_point() +
  geom_abline(color = "blue")

#Pros of Linear Regression
#easy to fit and to apply, concise, less prone to overfitting, interpretable
#Cons of Linear Regression
#can only express linear and additive relationships
#collinearity -- when input variables are partially correlated.
#when variables are highly correlated, signs of coefficients might not be what you expect, but does not affect model accuracy
#model may be unstable (e.g. unusually high coefficients when variables are highly correlated)

########################################################################
#Evaluating Regression Models (graphically, RMSE, R^2, (and F-test from link))
########################################################################
#http://facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
#https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R

#Evaluating model graphically 
#(by looking at plots of ground truth vs predictions aka y vs y_hat)
#a well-fitted model would have:
#x=y line runs through center of points
#line of perfect prediction
#a poorly fitting model would have:
#regions where points are all on one side of x=y line
#systematic errors - errors that are correlated w/ value of outcome
#either you don't have all the variables in the model or
#you need an algorithm that can find more complex relationships in data

#Residual Plot
#residual plot: difference between outcome & prediction vs prediction
#good fit: no systematic errors (all the errors evenly distributed between positive and negative and same magnitude above and below)
#bad fit: systematic errors (clusters of all positives or all negatives of residuals)

#Gain Curve/Plot (http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html)
#useful when sorting instances is more important than predicting exact outcome values
#measures how well model sorts the outcome
#x-axis: fraction of items in model-sorted order (decreasing)
#y-axis: fraction of total accumulated home sales
#Wizard curve: perfect model
GainCurvePlot(frame, xvar, truthvar, title)
#frame is a data frame
#xvar and truthvar are strings naming the prediction and actual outcome columns of frame
#title is the title of the plot
#When the predictions sort in exactly the same order, the relative Gini coefficient is 1. 
#When the model sorts poorly, the relative Gini coefficient is close to zero, or even negative.


#Residuals
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions
ggplot(unemployment, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. linear model prediction")


#RMSE
error <- houseprices$predictedPrices - houseprices$actualPrices #residuals
err <- err^2
rmse <- sqrt(mean(err))
#is RMSE large or small? one way to tell is to compare it to the standard deviation of outcome

#R^2
#near 1: model fits well
#near 0: no better than guessing the average value
#R^2 = 1 - RSS (variance from model)/SST (variance of data)

error <- houseprices$predictedPrices - houseprices$actualPrices #residuals
rss <- sum(err^2)
totalerr <- houseprices$actualPrices - mean(houseprices$actualPrices)
sstot <- sum(toterr^2)
s_squared <- 1 - (rss/sstot)

#Correlation and R^2
p = cor(prediction, actual) = 0.8995709
p^2 = 0.8092278 = R^2
#true for models that minimize squared error: linear regression & GAM regression

#can get R^2 from summary() or glance()$r.squared or adj.r.squared

cor(pred_y, y) #= sqrt(R^2) ONLY for trainng data & models that minimize square error

#FAST RMSE/R_SQUARED FUNCTIONS
rmse(actual, predicted)
r_squared(actual, predicted)

########################################################################
#Training a Model
########################################################################
#if we don't have enough data to split training vs test sets, use cross validation

#N-fold Cross Validation
#partition data into n subsets
#e.g. 3-fold cross validation
#first, train a model using A and B and use that model to make pred. on C
#then B and C to predict A
#then A and C to predict B

#splitPlan = vtreat::kWayCrossValidation(nRows in training data, nSplits aka n-way, dframe, y)
#dframe and y aren't technically used so can set them both to be NULL
#The resulting splitPlan is list of nSplits elements; each element contains two vectors:
  #train: the indices of dframe that will form the training set
  #app: the indices of dframe that will form the test (or application) set

#split <- splitPlan[[1]], model <- lm(fmla, data = df[split$train,])
#df$pred.cv[split$app] <- predict(model, newdata = df[split$app,])

#if the n-fold cross validation tests look good enough, create the final model via all data
#unfortunately you can't evaluate final model for future data because training/test
#cross validation only tests the modelling process whereas training/test tests for future value prediction

########################################################################
#EX. Generating a random split of training/test set
#If you have a dataset dframe of size N & want random subset of X% of N, where 0 < X < 1,
gp = runif(N) #vector of uniform random numbers
dframe[gp < X,] #will be what you want
dframe[gp >= X,] #will be complement
#note: (close to x/1-x split, but not exactly due to uniform random distribution)

#train the model
fmla = cty ~ hwy
mpg_model = lm(fmla, data = mpg_train)

#test model using rmse(predCol, ycol) & r_squared(predcol, ycol)
mpg_train$pred <- predict(mpg_model, mpg_train)
mpg_test$pred <- predict(mpg_model, mpg_test)
rmse_train <- rmse(mpg_train$pred, mpg_train$cty)
rmse_test <- rmse(mpg_test$pred, mpg_test$cty)
rsq_train <- r_squared(mpg_train$pred, mpg_train$cty)
rsq_test <- r_squared(mpg_test$pred, mpg_test$cty)
ggplot(mpg_test, aes(x = pred, y = cty)) + 
  geom_point() + 
  geom_abline()
########################################################################
#EX. Create a Cross Validation Plan
k <- 3 # Number of folds
splitPlan <- kWayCrossValidation(nRows, k, NULL, NULL)

# Run the 3-fold cross validation plan from splitPlan
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty ~ hwy, data = mpg[split$train,])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app,])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))

# Get the rmse of the full model's predictions
rmse(mpg$cty, mpg$pred)

# Get the rmse of the cross-validation predictions
rmse(mpg$cty, mpg$pred.cv)

########################################################################
#Categorical Inputs (One-hot encoding)
########################################################################
model.matrix(WtLoss24 ~ Diet + age + BMI, data = diet)
#all numerical values, converts categorical var. with N levels into N-1 indicator var

#use unique() to see how many possible values Time takes (two: "Late" and "Early")
unique(flowers$Time)
fmla <- as.formula("Flowers ~ Intensity + Time")
mmat <- model.matrix(fmla, data = flowers) #Time column changes to boolean: TimeLate (0 or 1)

#Partial F-Test on a Factor Coded as a Set of Dummies
full <- lm(y~x+as.factor(color), data=dat)
red <- lm(y~x, data=dat)
anova(red,full)
#Look at the F value & Pr(>F) this anova prints out.

########################################################################
#Dummy Code Categorical Variables
#https://stats.stackexchange.com/questions/115049/why-do-we-need-to-dummy-code-categorical-variables

########################################################################
#Variable Interactions that are bad for Additive/Linear Regression
########################################################################
#Additive relationship: e.g. plant_height ~ bacteria + sun
#increase in sun causes increase in plant_height regardless of level of bacteria level

#plant_head ~ bacteria + sun + bacteria:sun (interaction between bacteria and sunlight)
#change in height is more (or less) the sum of the effets due to sun/bacteria
#at higher levels of sunlight, 1 unit change in bacteria causes more change in height

#Expressing Interactions in R
#Interaction - Colon (:)
y ~ a:b

#Main Effects and Interaction:
y ~ a*b === y ~ a + b + a:b

#Expressing product of two variables: I
y ~ I(a*b)

########################################################################
#Transforming the response before modelling
########################################################################
#Sometimes get better models by transforming output rather than predicting output directly

#e.g. log transform

#e.g. Log Transform for Monetary Data.
  #monetary values tend to be lognormally distributed
  #long tail wide dynamic range (60-700k)
  #in such a case, mean > median (50k vs 39k)
  #predicting the mean will overpredict typical values (as what regression typically does)
  #if you take the log of monetary data, it will be transformed into a normal distribution
    #AKA mean ~= median again, more reasonable dynamic range (1.8 - 5.8)

#The Procedure
model <- lm(log(y) ~ x, data = train)
logpred <- predict(model, data = test)
pred <- exp(logpred) #transform predictions to outcome space

#Consequences
#prediction errors are multiplicative in outcome space (size of error relative to size of outcome)
#multiplicative error: pred/y
#relative error: (pred - y)/y = pred/y - 1
#reducing multiplicative error reduces relative error
#log transformations are useful when you want to reduce relative error rather than additive error
#good example: monetary. Being off by $100 is less important for $100000 but more for $200 income

#Root Mean Squared Relative Error
#RMSRE = sqrt(mean((pred-y)/y)^2))
#predicting log-outcome reduces RMS-relative error
#but model will often have larger RMSE

########################################################################
#Ex. Log Transform

# Examine Income2005 in the training set
summary(income_train$Income2005)

# Write the formula for log income as a function of the tests and print it
(fmla.log <- log(Income2005) ~ Arith + Word + Parag + Math + AFQT)

# Fit the linear model
model.log <-  lm(fmla.log, data=income_train)

# Make predictions on income_test
income_test$logpred <- predict(model.log, newdata= income_test)
summary(income_test$logpred)

# Convert the predictions to monetary units
income_test$pred.income <- exp(income_test$logpred)
summary(income_test$pred.income)

#  Plot predicted income (x axis) vs income
ggplot(income_test, aes(x = pred.income, y = Income2005)) + 
  geom_point() + 
  geom_abline(color = "blue")

# Add predictions to the test set
income_test <- income_test %>%
  mutate(pred.absmodel = predict(model.abs, income_test),#model.abs
         pred.logmodel = exp(predict(model.log, income_test)))#model.log

# Gather the predictions and calculate residuals and relative error
income_long <- income_test %>% 
  gather(key = modeltype, value = pred, pred.absmodel, pred.logmodel) %>%
  mutate(residual = pred - Income2005,   # residuals
         relerr   = residual/Income2005 )   # relative error

# Calculate RMSE and relative RMSE and compare
income_long %>% 
  group_by(modeltype) %>%      # group by modeltype
  summarize(rmse     = sqrt(mean(residual^2)),    # RMSE
            rmse.rel = sqrt(mean((relerr)^2)))    # Root mean squared relative error

########################################################################
#Transforming the inputs before modelling
########################################################################

#Why to Transform Input Variables
#domain knowledge/synthetic variables
  #intelligenceAnimals ~ mass.brains/mass.body^2.3
#pragmatic reasons
  #log transform to reduce dynamic range
  #log transform because meaningful changes in variable are multiplicative
  #y approximately linear in f(x) rather than in x

#anx ~ I(hassles^3)
#I(): treat an expression literally (not as an interaction)

#Linear, Quadratic, and Cubic Models
mod_lin <- lm(anx ~ hassles, hassleframe)
summary(mod_lin)$r.squared #0.5334847
mod_quad <- lm(anx ~ I(hassles^2), hassleframe)
summary(mod_quad)$r.squared #0.6241029
mod_tritic <- lm(anx ~ I(hassles^3), hassleframe)
summary(mod_tritic)$r.squared #0.6474421 - clearly the best

########################################################################
#EX. Transforming inputs

# houseprice is in the workspace
summary(houseprice)

# Create the formula for price as a function of squared size
(fmla_sqr <- (price ~ I(size^2)))

# Fit a model of price as a function of squared size (use fmla_sqr)
model_sqr <- lm(fmla_sqr, data = houseprice)

# Fit a model of price as a linear function of size
model_lin <- lm(price ~ size, data = houseprice)

# Make predictions and compare
houseprice %>% 
  mutate(pred_lin = predict(model_lin),       # predictions from linear model
         pred_sqr = predict(model_sqr)) %>%   # predictions from quadratic model 
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>% # gather the predictions
  ggplot(aes(x = size)) + 
  geom_point(aes(y = price)) +                   # actual prices
  geom_line(aes(y = pred, color = modeltype)) + # the predictions
  scale_color_brewer(palette = "Dark2")

########################################################################
#Logistic Regression to Predict Probabilities https://www.stat.wisc.edu/courses/st572-larget/handouts08-4.pdf
########################################################################
#Generalized Linear Model
glm(formula, data = train, family = binomial(link = "logit"))
#assumes inputs additive, linear in log-odds: log(p/(1-p))
#family: describes error distribution of model
  #logistic regresssion: family = binomial
#outcome: two classes, e.g. a and b
#model returns Prob(b)
  #Recommend: 0/1 or FALSE/TRUE

predict(model, newdata = test, type = "response")
#newdat: by default, trainng data
#to get probabilities: use type = "response"
  #by default: returns log-odds of the event, not the probability

#Methods of Evaluating Logistic Regression Model: pseudo-R^2
#R^2 = 1 - RSS/TSS
#pseudR^2 = 1 - deviance/null.deviance
#Deviance: analogous to variance (RSS)
#Null Deviance: Similar to TSS
#Pseudo R^2: Deviance explained

#Pseudo-R^2 on Training Data
#Using broom::glance()
glance(model) %>% summarize(pR2 = 1 - deviance/null.deviance)
##  pseudoR2
## 1 0.5922402

#Using sigr::wrapChiSqTest()
wrapChiSqTest(model)
## "... pseudo-R2=0.59 ..."

#Pseudo R^2 on Test Data
test %>% mutate(pred = predict(model, newdata = test, type = "response")) %>%
  wrapChiSqTest("predColName", "trueValuesColName of 0 or 1", TRUE) #the "TRUE" is the target value/target event

#GainCurvePlot great for logistic regression
GainCurvePlot(test, "pred", "has_dmd", "DMd model on test")

########################################################################
#EX. Logistic Regresssion on Sparrow Survival Probability

#predict status (survived vs perished) based on toatl_length, weight, and humerus

# sparrow is in the workspace
summary(sparrow)

# Create the survived column based on values TRUE/FALSE rather than Survived/Perished
sparrow$survived <- sparrow$status == "Survived"

# Create the formula
(fmla <- as.formula("survived ~ total_length + weight + humerus"))

# Fit the logistic regression model
sparrow_model <- glm(fmla, data = sparrow, family = binomial(link = "logit"))

# Call summary
summary(sparrow_model)

# Call glance
(perf <- glance(sparrow_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - (sparrow_model$deviance)/(sparrow_model$null.deviance))

# Make predictions
sparrow$pred <- predict(sparrow_model, type = "response")

# Look at gain curve
GainCurvePlot(sparrow, "pred", "survived", "sparrow survival model")

########################################################################
#Poisson and Quasipoisson Regression to Predict Counts
########################################################################
#Predicting Counts
#linear regression: predict values in [-infinity, infinity]
#counts: integers in range [0, infinity]

glm(formula, data, family = poisson_or_quasipoisson)
#inputs are additive and linear in log(count)
#outcome: integers
  #counts: e.g. number of traffic tickets a driver gets
  #rates: e.g. number of website hits/day
#prediction: expected rate or intensity (not integral)
  #expected # traffic tickets; expected hits/day

#Poisson vs Quasipoisson
  #poisson assumes that mean(y) = var(y) ("the same" as in, should be of a similar order of magnitude***)
  #if vary(y) much different from mean(y) - use quasipoisson
  #generally required a large sample size
  #if rates/counts >> 0 - regular regression is fine

#use same pseudo R^2

#use same predict method as logistic regression, along with type = "response"

#usage of paste function:
paste(outcome, "~", paste(vars, collapse = " + "))
#"cnt ~ hr + holiday + workingday + weathersit + temp + atemp + hum + windspeed"

########################################################################
#EX. Poisson/Quasipoisson

# The outcome column
outcome #"cnt"

# The inputs to use
vars ["hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"]

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Calculate the mean and variance of the outcome
(mean_bikes <- mean(bikesJuly$cnt))
(var_bikes <- var(bikesJuly$cnt))

# Fit the model
bike_model <- glm(fmla, data = bikesJuly, family = quasipoisson)

# Call glance
(perf <- glance(bike_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance/perf$null.deviance)

# bikesAugust is in the workspace
str(bikesAugust)

# bike_model is in the workspace
summary(bike_model)

# Make predictions on August data
bikesAugust$pred  <- predict(bike_model, newdata = bikesAugust, type = "response")

# Calculate the RMSE (CAN USE RMSE AS LONG AS YOU DON'T TRANSFORM THE OUTPUT RESPONSES)
bikesAugust %>% 
  mutate(residual = pred - cnt) %>%
  summarize(rmse  = sqrt(mean(residual^2)))

########################################################################
#EX. Visualize the Bike Rental Predictions as a Function of Time
# Plot predictions and cnt by date/time
bikesAugust %>% 
  # set start to 0, convert unit to days
  mutate(instant = (instant - min(instant))/24) %>%  
  # gather cnt and pred into a value column
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # restric to first 14 days
  # plot value by instant
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Quasipoisson model")

########################################################################
#Generalized Additive Models (GAM) to learn Non-Linear Transformations
########################################################################
#GAM: output depends additively on unknown smooth functions of input variables
#e.g. y ~ b0 + s1(x1) + s2(x2)
#a GAM learns the intercept  b0 and functions si's of the input
#Use GAM to learn the relationship: quadratic? cubic? quartic?

mgcv::gam(formula, family, data)

#family:
  #gaussian (default): "regular" expression
  #binomial: probabilities
  #poisson/quassipoisson: counts
#Best for larger datasets, as they are complex and more likely to overfit

#s() function (fits a spline between input and outcome)
anxiety ~ s(hassles)
#to specify that an input has a non-linear relationship with the outcome
#use s() with continuous variables NOT CATEGORICAL******************
  #more than about 10 unique values

model <- gam(anxiety ~ s(hassles), data = hassleFrame, family = gaussian)
summary(model)
## 
## ...
## 
## R-sq.(adj) =  0.619   Deviance explained = 64.1% (pseudo R-squared)
## GCV = 49.132  Scale est. = 45.153    n = 40
#The "deviance explained" reports the model's unadjusted R^2

plot(model) #gives plots of variable transformations that the algorithm learned

#y-values of these plots
predict(model, type = "terms")

#make predictions with model. use as.numeric to convert the matrix from predict(gam) to a vector
as.numeric(predict(model, newdata = hassleFrame, type = "response"))

########################################################################
#EX. GAM Example

# soybean_train is in the workspace
summary(soybean_train)

# Plot weight vs Time (Time on x axis)
ggplot(soybean_train, aes(x = Time, y = weight)) + 
  geom_point()

# Load the package mgcv
library(mgcv)

# Create the formula 
(fmla.gam <- weight ~ s(Time))

# Fit the GAM Model
model.gam <- gam(fmla.gam, data = soybean_train, family = gaussian)

# Call summary() on model.lin and look for R-squared
summary(model.lin)

# Call summary() on model.gam and look for R-squared
summary(model.gam)

# Call plot() on model.gam
plot(model.gam)

# soybean_test is in the workspace
summary(soybean_test)

# Get predictions from linear model
soybean_test$pred.lin <- predict(model.lin, newdata = soybean_test)

# Get predictions from gam model
soybean_test$pred.gam <- as.numeric(predict(model.gam, newdata = soybean_test))

# Gather the predictions into a "long" dataset
soybean_long <- soybean_test %>%
  gather(key = modeltype, value = pred, pred.lin, pred.gam)

# Calculate the rmse
soybean_long %>%
  mutate(residual = weight - pred) %>%     # residuals
  group_by(modeltype) %>%                  # group by modeltype
  summarize(rmse = sqrt(mean(residual^2))) # calculate the RMSE

# Compare the predictions against actual weights on the test data
soybean_long %>%
  ggplot(aes(x = Time)) +                          # the column for the x axis
  geom_point(aes(y = weight)) +                    # the y-column for the scatterplot
  geom_point(aes(y = pred, color = modeltype)) +   # the y-column for the point-and-line plot
  geom_line(aes(y = pred, color = modeltype, linetype = modeltype)) + # the y-column for the point-and-line plot
  scale_color_brewer(palette = "Dark2")

########################################################################
#Decision Trees
########################################################################
#Rules of form:
  #if A and B and C then Y
#non-linear concepts
  #intervals
  #non-monotonic relationships
#non-additive interactions
  #AND: similar to multiplication

#Pros:
  #trees have an expressive concept space (lower RMSE)
#Cons:
  #coarse-grained predictions (can give six possible values, whereas linear can give continuous value predictions)

#Ensembles of Trees (Several Trees) (Random Forest & Gradient-Boosted Trees)
  #ensemble model fits animal intelligence data better than single tree
  #ensembles give finer-grained predictions than single trees

########################################################################
#Random Forests
########################################################################

#Multiple diverse decision trees averaged together
  #reduces overfit
  #increases model expressiveness
  #finer grain predictions

#Building a Random Forest Model
#1. Draw bootstrapped sample from training data
#2. For each sample grow a tree
  #at each node, pick best variable to split on (from a random subset of all variables)
  #continue until tree is grown
#3. To score a datum, evaluate it with all the trees and average the results

#CAN BE USED FOR BOTH CLASSIFICATION AND REGRESSION PROBLEMS
model <- ranger::ranger(formula, trainngData, num.trees = 500, respect.unordered.factors = "order")
#use at least 200 trees
#mtry - number of variables to try at each node
  #by default, square root of the total number of variables
#respect.unordered.factors
  #Specifies how to treat unordered factor variables. We recommend setting this to "order" for regression.

## Ranger result
## ...
## OOB (OUT OF BAG) estimate, AKA prediction error (MSE):       3103.623 
## R squared (OOB):                  0.7837386
#NOTE: Random forest algorithm returns estimates of out-of-sample performance

predict(model, test_data)$predictions
#predict() inputs: model and data
#predictions can be accessed in the element $predictions

########################################################################
#EX. Random Forests

# bikesJuly is in the workspace
str(bikesJuly)

# Random seed to reproduce results
seed

# The outcome column
(outcome <- "cnt")

# The input variables
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Load the package ranger
library(ranger)

# Fit and print the random forest model
(bike_model_rf <- ranger(fmla, # formula 
                         bikesJuly, # data
                         num.trees = 500, 
                         respect.unordered.factors = "order", 
                         seed = seed))

# bikesAugust is in the workspace
str(bikesAugust)

# bike_model_rf is in the workspace
bike_model_rf

# Make predictions on the August data
bikesAugust$pred <- predict(bike_model_rf, newdata = bikesAugust)$predictions

# Calculate the RMSE of the predictions
bikesAugust %>% 
  mutate(residual = pred - cnt)  %>% # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2)))      # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()

########################################################################
#Categorical Inputs (One-hot encoding) MANUALLY
########################################################################
#Most R functions manage the conversion for you i.e. model.matrix as we learned before
#however, some do not e.g xgboost()
  #must convert categorical variables to numeric representation
#conversion to indicators: one-hot encoding

#vtreat to one-hot encode categorical variables (by transforming it into indicator variables 
#coded "lev" & to clean bad values out of numerical variables coded "clean")
#Basic Idea:
  #designTreatmentsZ() to design a treatment plan from the training data
  #prepare() to create "clean" data
    #all numerical
    #no missing values
    #use prepare() with treatment plan for all future data

#DESIGN TREATMENT PLAN
vars <- c("x", "u")
treatplan <- designTreatmentsZ(dframe, varslist, verbose = FALSE)
  #varslist: list of input variable names as strings
  #Set verbose to FALSE to suppress progress messages
  #designTreatmentsZ has property scoreFrame that describe the var-mapping & the types of each new variable.

scoreFrame <- treatplan %>% 
  magrittr::use_series(scoreFrame) %>% 
  select(varName, origName, code)

newvars <- scoreFrame %>% 
  filter(code %in% c("clean", "lev")) %>%
           magrittr::use_series(varName)

#PREPARE
training.treat <- prepare(treatmentplan, dframe, varRestriction = newvars)
#inputs to prepare():
  #treatmentplan: treatment plan
  #dframe: data frame
  #varestriction: list of variables to prepare (optional)
    #default: prepare all variables

########################################################################
#EX. Categorical Inputs

# dframe is in the workspace
dframe

# Create and print a vector of variable names
(vars <- c("color", "size"))

# Load the package vtreat
library(vtreat)

# Create the treatment plan
treatplan <- designTreatmentsZ(dframe, vars)

# Examine the scoreFrame
(scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))

# We only want the rows with codes "clean" or "lev"
(newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

# Create the treated training data
(dframe.treat <- prepare(treatplan, dframe, varRestriction = newvars))

########################################################################
#EX. Novel Levels that e.g. model.matrix can't handle but vtreat can

# newvars is in the workspace
newvars

# Print dframe and testframe
dframe #has 'r', 'g', and 'b'
testframe #has 'y'

# Use prepare() to one-hot-encode testframe
(testframe.treat <- prepare(treatplan, testframe, varRestriction = newvars))

########################################################################
#Gradient Boosting
########################################################################
#ensemble method that builds up a model by incrementally improving the existing one

#1. Fit a shallow tree T1 to the data. M1 = T1
#2. Fit a tree T2 to the residuals. Find gamma such that M2 = M1 + gamma*T2 is the best fit to data.
#For regularized boosting, decrease learning by a factor eta in range (0,1).
  #M2 = M1 + eta*gamma*T2. A larger eta = faster learning, smaller eta = less risk of overfit
#3. Repeat (2) until stopping condition is met.

#Final Model: M = M1 + eta*sum(gamma_i * Ti)

#Because gradient boosting optimizes error on the training data, it's very easy to overfit the model.
#best practice is to estimate out of sample error via cross validation for each incremental model
#then retroactively decide how many trees to use

#xgboost()
#1. Run xgb.cv() with a large number of rounds (trees). Fits the model & calculates cross-validated error as well.
#2. xgb.cv()$evaluation_log: records estimated RMSE for each round.
  #find the number of trees that minimizes estimated RMSE: n_best
#3. Run xgboost(), setting nrounds = n_best

#EX.
treatplan <- designTreatmentsZ(bikesJan, vars)
newvars <- treatplan$scoreFrame %>%
  filter(code %in% c("clean", "lev")) %>%
  use_series(varName)

bikesJan.treat <- prepare(treatplan, bikesJan, varRestriction = newvars)

cv <- xgb.cv(data = as.matrix(bikesJan.treat), #a numeric matrix
             label = bikesJan$cnt, #vector of outcomes (numeric)
             objective = "reg:linear",
             nrounds = 100, nfold = 5, eta = 0.3, max_depth = 6)

#Key inputs to xgb.cv() and xgboost()
  #data: input data as matrix; label: outcome
  #objective: for regression - "reg:linear"
  #nrounds: maximum number of trees to fit
  #eta: learning rate
  #max_depth: maximum depth of individual trees
  #nfold: (xgb.cv() only): number of folds for cross validation
  #early_stopping_rounds: after this many rounds without improvement, stop.

elog <- as.data.frame(cv$evaluation_log)
nrounds <- which.min(elog$test_rmse_mean) #out of sample error
## 78 #the index of the minimum value corresponds to best number of trees

#run cgboost() with right number of trees to get final model
nrounds <- 78
model <- xgboost(data = as.matrix(bikesJan.treat), 
                 label = bikesJan$cnt,
                 nrounds = nrounds,
                 objective = "reg:linear",
                 eta = 0.3,
                 depth = 6)

#Predict with xgboost() model
bikesFeb.treat <- prepare(treatplan, bikesFeb, varRestriction = newvars)
bikesFeb$pred <- predict(model, as.matrix(bikesFeb.treat))

########################################################################
#EX. Gradient Boosting Machines

# bikesJuly & bikesJuly.treat are in the workspace

# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(bikesJuly.treat), 
             label = bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- as.data.frame(cv$evaluation_log)

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
#ntrees.train = 67, ntrees.test = 57

#In most cases, ntrees.test is less than ntrees.train. The training error keeps decreasing 
#even after the test error starts to increase. It's important to use cross-validation to find 
#the right number of trees (as determined by ntrees.test) and avoid an overfit model.

# Examine the workspace
ls()

# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
bike_model_xgb <- xgboost(data = as.matrix(bikesJuly.treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
bikesAugust$pred <- predict(bike_model_xgb, as.matrix(bikesAugust.treat))

# Plot predictions (on x axis) vs actual bike rental count
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()

#Sometimes it might make negative prediction. However if you look at RMSE it will be smaller than other models.
#perhaps rounding negative predictions up to zero is a reasonable tradeoff