#' ---
#' author: Brandon Espino Hernandez
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' geometry: margin=2cm
#' number_sections: true
#' ---
#' <style type="text/css">
#' .main-container {
#' max-width: 800px !important;
#' font-size: 18px;
#' }
#' code.r{
#' font-size: 18px;
#' }
#' pre {
#' font-size: 18px
#' }
#' h1.title {
#'   font-size: 30px;
#' }
#' h1 {
#'   font-size: 24px;
#' }
#' h2 {
#'   font-size: 18px;
#' }
#' h3 {
#'   font-size: 12px;
#' }
#' </style>

#+ setup, include = FALSE
knitr::opts_chunk$set(comment=NA, warning=FALSE, message=FALSE, size=12)

#' <BR><BR>
#'
#' # Problem 1
library(ggplot2)
library(gridExtra)
library(L1pack)
library(stringr)
library(MASS)

load("king_county_house_sales.rda")
#' ## Part A
housing = subset(king_county_house_sales, select=c("price","sqft_living"))
housing = setNames(housing, c('price', 'sqft'))
summary(housing$price)
summary(housing$sqft)
plt1 = ggplot(data=housing, aes(x=sqft, y=price)) + geom_point()
plt2 = ggplot(data=housing, aes(x=log(sqft), y=log(price))) + geom_point(col='green')
grid.arrange(plt1, plt2, nrow=2, ncol=1)
#' From the summary statistics it appears that the data is right skewed as the mean is greater than the median in both price and sqft_living. 
#' We observe in the plots how applying a log transformation to both variables allows allows us to see a more linear relationship between them.
#' 
#' ## Part B
housing[c('log_price', 'log_sqft')] = c(log(housing$price), log(housing$sqft))
fit.ls = lm(log_price ~ log_sqft, data=housing)
fit.lad = lad(log_price ~ log_sqft, data=housing)
colors = c('Least Squares'='red', 'Least Absolute'='blue')
ggplot(data=housing, aes(x=log_sqft, y=log_price)) + geom_point() +
  geom_abline(aes(slope=coef(fit.ls)[['log_sqft']], intercept=coef(fit.ls)[['(Intercept)']], color='Least Squares')) +
  geom_abline(aes(slope=coef(fit.lad)[['log_sqft']], intercept=coef(fit.lad)[['(Intercept)']], color='Least Absolute')) +
  labs(x = "log(Sqft)", y = "log(price)", color = "Legend") + scale_color_manual(values = colors) + theme(legend.position = c(.2,.8))
#'
#' ## Part C
K = 10
housing_shuff = housing[sample(nrow(housing)),][c('log_price', 'log_sqft')]  # shuffling
bins = cut(seq(1, nrow(housing_shuff)), breaks=K, labels=FALSE)
errors_ls = numeric(length = K)
errors_lad = numeric(length = K) 
for(bin_idx in 1:K){
  valIndexes = which(bins == bin_idx, arr.ind=TRUE)  # indices of 'bin_idx' bins
  val_price = housing_shuff$log_price[valIndexes]
  val_sqft = housing_shuff$log_sqft[valIndexes]
  train_price = housing_shuff$log_price[-valIndexes]
  train_sqft = housing_shuff$log_sqft[-valIndexes]
  
  ls = lm(train_price ~ train_sqft)
  lad = lad(train_price ~ train_sqft)
  
  ls_val_predictions = predict(ls, newdata=data.frame(train_sqft=val_sqft))
  lad_val_predictions = predict(lad, newdata=data.frame(train_sqft=val_sqft))

  interior_diffs_ls = (val_price - ls_val_predictions)^2
  interior_diffs_lad = (val_price - lad_val_predictions)^2

  errors_ls[bin_idx] = mean(interior_diffs_ls)
  errors_lad[bin_idx] = mean(interior_diffs_lad)
}
ls_cv_error = mean(errors_ls)
ls_cv_error
lad_cv_error = mean(errors_lad)
lad_cv_error
diff(c(ls_cv_error, lad_cv_error))
#'
#' ## Part D
housing['sqft_lot'] = king_county_house_sales["sqft_lot"]
summary(housing$price)
summary(housing$sqft_lot)
plt1 = ggplot(data=housing, aes(x=sqft_lot, y=price)) + geom_point()
plt2 = ggplot(data=housing, aes(x=log(sqft_lot), y=log(price))) + geom_point(col='green')
grid.arrange(plt1, plt2, nrow=2, ncol=1)
#' We again see from the summary statistics that the data is right skewed as the mean is greater than the median in both price and sqft_lot. From the
#' plots we see how applying a log transformation to both variables gives us a clearer regression line just as in Part A with sqft, however the relationship
#' between the size of the lot and the price does not appear as strong as that of the house size and price.
housing['log_sqft_lot'] = log(housing['sqft_lot'])
fit2.ls = lm(log_price ~ log_sqft_lot, data=housing)
fit2.lad = lad(log_price ~ log_sqft_lot, data=housing)
colors = c('Least Squares'='red', 'Least Absolute'='blue')
ggplot(data=housing, aes(x=log_sqft_lot, y=log_price), ) + geom_point() + labs(title="Least Squares vs Least Absolute Regression") + 
  geom_abline(aes(slope=coef(fit2.ls)[['log_sqft_lot']], intercept=coef(fit2.ls)[['(Intercept)']], color='Least Squares')) +
  geom_abline(aes(slope=coef(fit2.lad)[['log_sqft_lot']], intercept=coef(fit2.lad)[['(Intercept)']], color='Least Absolute')) +
  labs(x = "log(Sqft_lot)", y = "log(price)", color = "Legend") + scale_color_manual(values = colors) + theme(legend.position = c(.15,.85))
#'
#' ## Part E
par(mfrow=c(2,3))
fits = list(fit.ls, fit2.ls)
regress = c('log(sqft)', 'log(sqft_lot)')
for(i in 1:2){
  print(summary(fits[[i]]))
  residual = resid(fits[[i]])
  plot(fitted(fits[[i]]), residual, xlab='Fitted Value', ylab='Residual', main=paste('Residual plot for', regress[i], 'fit'))
  abline(0, 0, col='red')
  qqnorm(residual)
  qqline(residual, col='red')
  plot(density(residual))
}
#' From the plots we note the more favorable Residual value spread of the sqft model 'fit.ls' vs the sqft_lot model 'fit2.ls. Residuals seem to follow a normal distribution
#' more closely as well according to the QQ plot. These plots lead me to believe my regression assumptions are met. The sqft also has a substantially greater
#' R-squared value at 0.4555 vs 0.01897 for the sqft_lot model. This leads me to believe that this first model fits the data better and thus the sqft variable
#' is the better predictor. 
#' <BR><BR>
#' 
#' # Problem 2
#' ## Part A
piecewise.constant = function(x, y, M=10, x.points=seq(from=min(x), to=max(x), length.out=100)){
  bins = cut(x, breaks=M, labels=F) 
  Bs = numeric(length=M) #  b_hats
  for(k in 1:M){
    kth_bin_indices = which(bins == k, arr.ind=T)
    y_vals = y[kth_bin_indices]
    Bs[k] = mean(y_vals)
  }
  x_predictions = numeric(length=length(x.points))
  bin_levels = levels(cut(x, breaks=M))
  bin_cutoffs = c(0, sapply(str_extract_all(bin_levels, "-?[0-9.]+"), function(x) max(as.numeric(x))))
  bin_indices = cut(x.points, breaks=bin_cutoffs, include.lowest=T, right=F, labels=F)
  for(i in 1:length(x.points)){
    x_predictions[i] = Bs[bin_indices[i]]
  }
  return(x_predictions)
}
#' ## Part B
#' Simulated data
X = seq(0, 1, by=.01)
Y = dnorm(X, mean=.5, sd=.5) + runif(n=length(X), min=-.05, max=.05)
plot(X, Y, xlim=c(0, 1))
predictions = piecewise.constant(X, Y, M=10, x.points=seq(0, 1, .005))
lines(x=seq(0, 1, .005), y=predictions, type="l", lwd=2, col="blue")

#' real data from https://machinelearningmastery.com/time-series-datasets-for-machine-learning/ called "Daily Female Births Dataset"
birth_data = read.csv("daily-total-female-births.csv")
birth_data$Date = seq(1, length(birth_data$Date))
plot(birth_data$Date, birth_data$Births)
predictions = piecewise.constant(birth_data$Date, birth_data$Births, M = 12)
lines(x = seq(from=min(birth_data$Date), to=max(birth_data$Date), length.out=100),
      y = predictions, type = "l", lwd = 2, col = "red")
#' ## Part C
par(mfrow = c(2, 2))
X = birth_data$Date
Y = birth_data$Births
x.points=seq(from=min(X), to=max(X), length.out=100)
spans = c(.1,.2,.4,.7)
Ms = c(10, 20, 40, 50)

for(i in 1:length(spans)){
  plot(X, Y, xlim=c(min(X), max(X)), main=paste('Local ( span =', spans[i], ') vs \nPiecewise ( M =', Ms[i], ') fit comparison'))
  predictions_piece = piecewise.constant(X, Y, M=Ms[i])
  lines(x.points, predictions_piece, type='l', lwd=2, col='green')
  fit_loess = loess(Y ~ X, span=spans[i])
  predictions_loess = predict(fit_loess, data.frame(X=x.points))
  lines(x.points, predictions_loess, type='l', lwd=2, col='red')
  legend('topleft', c('Local', 'Piecewise'), col=c('red', 'green'), lwd=2, lty=c(1,1), cex=0.5)
}

#' From the various plots we see pretty clearly the significance of the span and M parameters in LOESS and in piecewise respectively. With the increases in M,
#' we increase the number of bins we use and so our fit more closely follows the data. The increase in span, and thus the increase in the amount of data
#' being used for averaging, smooths our curve to a greater extent as it tries to account for the wider data spread. A low span and a high M give us 
#' very similar looking graphs. 
#' <BR><BR>