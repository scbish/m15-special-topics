# Exercise-2
In this exercise, you'll practice running a simple classification algorithm, and splitting your data into test and train splits. As usual, you should fork/clone the repository, then follow the instructions below (also indicated in the `exercise.R` file).

See `complete` branch for answers

Inside your loop, do the following:

1. Create training and testing datasets by sampling 75% of your data from your `homes` dataframe.

2. Pass your **training data** to the `rpart` function to run a simple classification operation

3. Pass your results to the `AssessFit` function to assess the fit

4. Store your assessment in the `basic.fits` vector

Below your loop, do the following:

5. Make a histogram of your `basic.fits` vector

6. Take the mean of your `basic.fits` vector

7. Pass your most recent model to the `rpart.plot` function to graph it
