# A simple functions for splitting into test and training
# sets.
splitTrainTest <- function(cites_df, train_pct, seed=NULL) {
    stopifnot(train_pct > 0 & train_pct < 1)

    if (!is.null(seed)) { set.seed(seed) }

    df_size <- nrow(cites_df)
    train_size <- floor(df_size * train_pct)

    train_idx <- sample(1:df_size)[1:train_size]
    list(train=cites_df[train_idx, ], test=cites_df[-train_idx, ])
}

trainTestByYear <- function(cites_df, test_years) {
    list(train=cites_df %>% filter(!(year %in% test_years)),
         test=cites_df %>% filter(year %in% test_years))
}

# Fit the model on training data and get the RMSE on the test data
getTestError <- function(train_df, test_df, ...) {
    linreg <- runLinearModel(train_df %>% addDivisions, ...)
    preds <- predict(linreg, test_df %>% addDivisions)
    actual <- log1p(test_df$citations)

    sqrt(mean((preds - actual)^2))
#    return(list(p=preds, a=actual))
}


# Get the fit errors on each fold.
randomSplitCrossValidate <- function(cites_df, train_pct=.8,
                                    n=floor(1/(1-train_pct)),
                                    seed=NULL, opts1=list(), opts2=list()) {

    splitFit <- function(i) {
        train_test <- splitTrainTest(cites_df, train_pct, seed)
        tt_args <- list(train_df = train_test$train, test_df = train_test$test)
        model_args1 <- c(tt_args, opts1)
        model_args2 <- c(tt_args, opts2)

        err1 <- do.call(getTestError, model_args1)
        err2 <- do.call(getTestError, model_args2)
        c(err1, err2)
    }
    parallel::mclapply(as.list(1:n), splitFit) %>% do.call(rbind, .)
}

# Fit errors from training on some years, predicting on others.
yearlyCrossValidate <- function(cites_df, test_years=c(2010:2012), opts1=list(), opts2=list()) {
    splitFit <- function(year) {
        train_test <- trainTestByYear(cites_df, year)
        tt_args <- list(train_df=train_test$train, test_df=train_test$test)
        model_args1 <- c(tt_args, opts1)
        model_args2 <- c(tt_args, opts2)
        err1 <- do.call(getTestError, model_args1)
        err2 <- do.call(getTestError, model_args2)
        c(year, train_test$train %>% nrow, err1, err2)
    }
    lapply(as.list(test_years), splitFit) %>% do.call(rbind, .)
}
