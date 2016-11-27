# Introductory example using the housing data used here: http://www.r2d3.us/visual-intro-to-machine-learning-part-1/
# rpart library
library(rpart)
library(rpart.plot)
setwd('~/Documents/info-201/m15-special-topics/demo-1')
homes <- read.csv('data/housing-data.csv')

# Function to compare values
AssessFit <- function(model, data = homes, outcome = 'in_sf') {
  predicted <- predict(model, data, type='class')
  accuracy <- format(length(which(data[,outcome] == predicted)) / length(predicted) * 100, digits=4)
  return(paste0(accuracy, '% accurate!'))
}

# Tree function
SimpleTree <- function(predictors) {
  # Write out forumula
  predictors <- paste( predictors, collapse = "+")
  print(predictors)
  formula <- as.formula(paste0('in_sf ~ ', predictors))
  
  # Set test / training data
  sample_size <- floor(.25 * nrow(homes))
  train_indicies <- sample(seq_len(nrow(homes)), size = sample_size)
  training_data <- homes[train_indicies,]
  test_data <- homes[-train_indicies,]
  
  # Use rpart to fit a model: predict `in_sf` using other variables
  fit <- rpart(formula, data = training_data, method="class")
  
  # List of info to return
  info <- list()
  info$accuracy <- AssessFit(fit, data=test_data)
  
  info$tree <- rpart.plot(fit)
  return(info)
}