# Introductory example using the housing data used here: 
# http://www.r2d3.us/visual-intro-to-machine-learning-part-1/
library(rpart)
library(rpart.plot)

# Read in data
setwd('~/Desktop/Informatics/m15-special-topics/exercise-2')
homes <- read.csv('data/housing-data.csv')

# Function to compare values
AssessFit <- function(model, data = homes, outcome = 'in_sf') {
  predicted <- predict(model, data, type='class')
  accuracy <- length(which(data[,outcome] == predicted)) / length(predicted) * 100
  return(accuracy)
}

# Assess fit for different models

# Use rpart to fit a model: predict `in_sf` using all other variables
basic.fit <- rpart(in_sf ~ ., data = homes, method="class")

# How well did we do?
AssessFit(basic.fit)

# Create empty vectors to store results
basic.fits <- vector()
perfect.fits <- vector()

# Sample size for training dataset
sample.size <- floor(.75 * nrow(homes))
for(i in 1:100) {
  # Create test and training data
  # Hint: http://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function-in-r-program
  # 1. Create training and testing datasets by sampling 75% of your data from your `homes` dataframe.
  train_ind <- sample(seq_len(nrow(homes)), size = sample.size)
  train <- homes[train_ind, ]
  test <- homes[-train_ind, ]
  
  # 2. Pass your **training data** to the `rpart` function to run a simple classification operation
  test.fit <- rpart(in_sf ~ ., data = train, method="class")
  
  # 3. Pass your results to the `AssessFit` function to assess the fit
  assess <- AssessFit(test.fit)
  
  # 4. Store your assessment in the `basic.fits` vector
  basic.fits <- c(basic.fits, assess)
  
}

# 5. Make a histogram of your `basic.fits` vector



# 6. Take the mean of your `basic.fits` vector



# 7. Pass your most recent model to the `rpart.plot` function to graph it

