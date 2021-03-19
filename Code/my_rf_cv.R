#' the t-test calculator
#'
#' This function calculates values associated to the t-test such as the
#' test statistics, p-value, degree of freedom, and the conclusion on
#' whether we reject or fail to reject the null hypothesis tested from
#' lifeExp data in the gapminder package.
#'
#' @param x the numeric vector of lifeExp pulled from the gapminder data
#' @param alternative the one-sided or two-sided test
#' @param mu the mean value tested given from the null hypothesis (mu = 60)
#' @keywords hypothesis, inference
#'
#' @return A list indicating test statistics, p-value, degree of freedom,
#' and the conclusion on whether we reject or fail to reject the null hypothesis
#' tested from lifeExp data in the gapminder package.
#'
#' @examples
#' set.seed(302)
#' p <- 0.4
#' flip_coin <- rbinom(100, size = 1, prob = p)
#' my_t.test(as.numeric(flip_coin), alternative = "greater", mu = p)
#'
#' @export
my_rf_cv <- function(k) {
  # Variable fold based on input parameter
  fold <- k
  my_penguins <- my_penguins %>% drop_na()
  total <- nrow(my_penguins)
  # Randomly assign observation to folds with equal probability
  inds <- sample(rep(1:fold, length = total))
  n_data <- data.frame(my_penguins) %>% mutate("split" = inds)

  # Introduce variable mse_fold to store and track mse
  mse_fold <- 0

  # for loops to iterate every fold for cross validation
  for (i in 1:fold) {

    # In each iteration, the data splitted into folds where we take the fold
    # data as test data and others as training
    x_train <- n_data %>% filter(split != i) %>% select(-"split")
    x_test <- n_data %>% filter(split == i) %>% select(-"split")
    sum_train <- n_data %>% filter(split == i)
    # Set up the model for random forest
    model <- randomForest(body_mass_g ~ bill_length_mm +
                            bill_depth_mm + flipper_length_mm,
                          data = x_train, ntree = 100)
    # predict the mass on data based on model
    prediction <- predict(model, x_test)

    # calculate the mean squared error between true and predict mass
    squared_err <- sum((sum_train[6] - prediction)^2)
    mse_fold <- mse_fold + squared_err
  }
  # return average of cv mse
  return(mse_fold/total)
}
