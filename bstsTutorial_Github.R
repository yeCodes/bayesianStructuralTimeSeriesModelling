
# code snippets - https://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

# Inspecting initial cliams dataset
plot(initial.claims)
plot(initial.claims$michigan.unemployment)
plot(initial.claims$pennsylvania.unemployment)
plot(initial.claims$iclaimsNSA)

# add linear trend + include the dependent variable.  From initial.claims$iclaimsNSA,
# can see that non-zero trend. Try drawing a trend intuitively to prove this to yourself 
# non-zero trend line for parts of the time series
ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)

# adding seasonal component + specifying no seasons -- weekly seasonality
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)

model1 <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)

# plots of components + of posterior == what is generated from SAMPLING the 
# distribution that results from sampling the independent variables using whatever
# the defined sampling strategy is, whether gibbs sampling, metropolis hastings algo...
plot(model1)
plot(model1, "components")  # plot(model1, "comp") works too!
plot(model1, "help")

pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)  # 156 argument says to plot the prediction 
# along with the last 156 time points (3 years) of the original series. 
# The results are shown in Figure 3.


##************************ADD REGRESSION COMPONENT****************************#
# The bsts package only includes 10 search terms with the initial claims data set, 
# to keep the package size small, but Scott and Varian (2014) considered examples 
# with several hundred predictor variables. When faced with large numbers of 
# potential predictors it is important to have a prior distribution that induces 
# sparsity. A spike and slab prior is a natural way to express a prior belief 
# that most of the regression coefficients are exactly zero.

# Fit a bsts model with expected model size 1, the default. --> 1 regression component expected, it seems
model2 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims)


# Fit a bsts model with expected model size 5, to include more coefficients.
# note: the dataset intiial.claims passed in here
model3 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.


# NOTE now using INFERENCE here to look at COEFFICIENTS
# Less focus on PREDICTION here!!! --> different problems!!!!

####***************INFERENCE***********************#
# Inclusion probability of regression coefficient
plot(model2, "coef")
plot(model3, "coef")

bsts.prediction.errors(model1)

CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))

# The result of the call is the plot shown in Figure 6. The bottom panel shows 
# the original series. The top panel shows the cumulative total of the mean 
# absolute one step prediction errors for each model. The final time point in 
# the top plot is proportional to the mean absolute prediction error for each 
# model, but plotting the errors as a cumulative total lets you see particular 
# spots where each model encountered trouble, rather than just giving a single 
# number describing each model’s predictive accuracy. Figure 6 shows that the 
# Google data help explain the large spike near 2009, where model 1 accumulates
# errors at an accelerated rate, but models 2 and 3 continue accumulating errors
# at about the same rate they had been before. The fact that the lines for models
# 2 and 3 overlap in Figure 6 means that the additional predictors allowed by the
# relaxed prior used to fit model 3 do not yield additional predictive accuracy.
