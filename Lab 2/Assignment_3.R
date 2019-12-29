setwd("~/Programming/TDDE01/Lab 2")
library(ggplot2)
library(tree)
library(boot)
RNGversion('3.5.1')


#The data file State.csv contains per capita state and local public expenditures
#and associated state demographic and economic characteristics, 1960, and there are variables
#• MET: Percentage of population living in standard metropolitan areas
#• EX: Per capita state and local public expenditures ($)

#1
#Reorder your data with respect to the increase of MET and plot EX versus MET.
#Discuss what kind of model can be appropriate here. Use the reordered data in steps 2-5.
data <- read.csv2("State.csv")

#Orders data in ascending order
ordered_data <- data[order(data$EX), ]

plot <- ggplot() + geom_point(aes(x = EX, y = MET), data = data)
plot

#2
#Use package tree and fit a regression tree model with target EX and feature MET in which the number
#of the leaves is selected by cross-validation, use the entire data set and set minimum number of
#observations in a leaf equal to 8 (setting minsize in tree.control). Report the selected tree.
#Plot the original and the fitted data and histogram of residuals. Comment on the distribution of
#the residuals and the quality of the fit.

#Nobs is the number of rows to be used from the data. In this case it is everything
model.tree <-
  tree(EX ~ MET,
       data = ordered_data,
       control = tree.control(nobs = 48, minsize = 8))
model.tree.cv <- cv.tree(model.tree, FUN = prune.tree)
#Can plots dev vs size and can see that 3 is the best tree
plot_cv_tree <-
  ggplot() + geom_line(aes(x = model.tree.cv$size, y = model.tree.cv$dev)) + labs(x =
                                                                                    "Tree size")
plot_cv_tree

model.tree.pruned <- prune.tree(model.tree, best = 3)

#Plots the pruned tree with 3 leaves
plot(model.tree.pruned)
text(model.tree.pruned)

#Plots the residuals of the tree model
histo_tree_res <-
  ggplot(data.frame(residuals(model.tree)), aes(residuals(model.tree))) + geom_histogram(bins = 25, color =
                                                                                           "red", fill = "red") +
  labs(x = "Residuals") + geom_density(alpha = .2, fill =
                                         "#FF6666")
histo_tree_res

#Predicts the tree with same ordered data with the pruned tree with 3 leaves
model.tree.predicted <- predict(model.tree.pruned, ordered_data)

#Plots the tree data with the original data as blue dots and the
plot_tree_data <-
  ggplot() + geom_line(aes(x = ordered_data$MET, y = data.frame(model.tree.predicted)[, 1]), color =
                         "green") +
  geom_point(aes(x = ordered_data$MET, y = ordered_data$EX), color =
               "black") + labs(title = "Plotted tree data", x = "MET", y = "Predicted EX")
plot_tree_data

#3
#Compute and plot the 95% confidence bands for the regression tree model from step 2
#(fit a regression tree with the same settings and the same number of leaves as in step 2 to
#the resampled data) by using a non-parametric bootstrap. Comment whether the band is smooth or
#bumpy and try to explain why. Consider the width of the confidence band and comment whether
#results of the regression model in step 2 seem to be reliable.

boot_function <- function(data_boot, id) {
  data_boot <- data_boot[id, ]
  model.tree.boot <-
    tree(EX ~ MET,
         data = data_boot,
         control = tree.control(nobs = 48, minsize = 8))
  model.tree.boot.pruned <- prune.tree(model.tree.boot, best = 3)
  return(predict(model.tree.boot.pruned, newdata = ordered_data))
}

set.seed(12345)
bootres <- boot(ordered_data, boot_function, R = 1000)
bootres.envelope <- envelope(bootres)

#Plots everything including the confidence intervals
plot_boot <-
  ggplot() + geom_point(aes(
    x = ordered_data$MET,
    y = ordered_data$EX,
    color = "Original data-points"
  )) +
  geom_line(aes(
    x = ordered_data$MET,
    y = data.frame(model.tree.predicted)[, 1],
    color = "Predicted tree data"
  )) +
  scale_color_manual(
    name = '',
    values = c(
      'Confidence level 95%' = 'red',
      'Original data-points' = 'black',
      'Predicted tree data' = 'green',
      'Confidence level 95% parametric' = 'blue',
      'Confidence level 95% predicted' = 'black'
    )
  )

plot_boot_3 <- plot_boot +
  geom_line(aes(
    x = ordered_data$MET,
    y = bootres.envelope$point[1, ],
    color = "Confidence level 95%"
  )) +
  geom_line(aes(
    x = ordered_data$MET,
    y = bootres.envelope$point[2, ],
    color = "Confidence level 95%"
  )) + labs(title = "Non-parametric bootstrap", x = "MET", y = "EX")

plot_boot_3

#4
#Compute and plot the 95% confidence and prediction bands the regression tree model
#from step 2 (fit a regression tree with the same settings and the same number of
#leaves as in step 2 to the resampled data) by using a parametric bootstrap,
#assume 𝑌𝑌~𝑁𝑁(𝜇𝜇𝑖𝑖, 𝜎𝜎2) where 𝜇𝜇𝑖𝑖 are labels in the tree leaves
# and 𝜎𝜎2 is the residual variance. Consider the width of the confidence band and
#comment whether results of the regression model in step 2 seem to be reliable.

boot_function2 <- function(data_boot2) {
  model.tree.boot2 <-
    tree(EX ~ MET,
         data = data_boot2,
         control = tree.control(nobs = 48, minsize = 8))
  model.tree.pruned.boot2 <- prune.tree(model.tree.boot2, best = 3)
  return(predict(model.tree.pruned.boot2, data_boot2))
}

#Random generating function for generating new EX values
boot.random <- function(data, mle) {
  dat <- data.frame(EX = data$EX, MET = data$MET)
  n = length(data$EX)
  dat$EX <- rnorm(n, predict(mle, dat), sd(resid(mle)))
  return(dat)
}


set.seed(12345)
bootres2 <-
  boot(
    data = ordered_data,
    statistic = boot_function2,
    mle = model.tree.pruned,
    ran.gen = boot.random,
    R = 1000,
    sim = "parametric"
  )
bootres2.envelope <- envelope(bootres2)

plot_boot4 <- plot_boot +
  #Plots the confidence interval
  geom_line(
    aes(
      x = ordered_data$MET,
      y = bootres2.envelope$point[1, ],
      color = "Confidence level 95% parametric"
    )
  ) +
  geom_line(
    aes(
      x = ordered_data$MET,
      y = bootres2.envelope$point[2, ],
      color = "Confidence level 95% parametric"
    )
  ) + geom_line(
    #Plots the predicted bands
    aes(
      x = ordered_data$MET,
      y = bootres2.envelope$overall[2, ],
      color = "Confidence level 95% predicted"
    )
  ) + geom_line(
    aes(
      x = ordered_data$MET,
      y = bootres2.envelope$overall[1, ],
      color = "Confidence level 95% predicted"
    )
  ) + labs(title = "Parametric bootstrap", x = "MET", y = "EX")


plot_boot4
#This graph shows the confidence band (blue) and the prediction band (black)
#for the parametric bootstrap. The confidence band shows the confidence interval
#for the sample and the prediction band shows the prediction interval for the
#estimations. As one can see the prediction interval is quite good,
#but the confidence interval isnt that good as it misses more than 5 % of the
#data points.


#5
#Consider the histogram of residuals from step 2 and suggest what 
#kind of bootstrap is actually more appropriate here.

#What one can see in the residual plot from step 2 is that there isnt a
#predictable distribution and it is thereby better to use the non-parametric
#bootstrap solution as that one doesnt depend on a certain distribution