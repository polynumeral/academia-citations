#' Create a formula describing a regression of citations on covariates.
#'
#' Creates a formula of the form `y ~ ...` where `y` is the citation
#' count variable, possibly transformed, and `...` are covariates and their
#' interactions.
#'
#' The linear model log-transforms the citations variable, while the negative
#' binomial and ZINB models do not. The logistic model transforms citations
#' into a 0/1 variable where 1 indicates the article received at least one
#' citation.
#'
#' @param df A dataframe with citations data, and divisions as created by
#' `importData` followed by `addDivisions`
#' @param model One of 'linear', 'logistic', 'negbin', or 'zinb', the last for
#' zero-inflated negative binomial model.
#' @param division_interactions If TRUE (default), interact each division dummy
#' with the on-Academia dummy.
#'
#' @return An R formula.
#'
makeFormula <- function(df,
                       model=c('linear', 'logistic', 'negbin', 'zinb'),
                       division_interactions=TRUE) {

    division_terms <- df %>%
        selectDivisionCols %>%
        names %>%
        (function(x) if (length(x) == 0) { stop('No divisions found.') } else { x }) %>%
        (function(x) { paste0('`', x, '`')}) %>%
        paste(collapse=' + ')

    rhs <- if (division_interactions) {
        paste0('on_acad * (scale(log1p(impact_factor), scale=FALSE) +
                           scale(age, scale=FALSE) +
                           scale(I(age^2), scale=FALSE) +
                           online + ',
                           division_terms, ')')
    } else {
        # Leave out on_acad * division interatctions.
        paste0('on_acad * (scale(log1p(impact_factor), scale=FALSE) +
                           scale(age, scale=FALSE) +
                           scale(I(age^2), scale=FALSE) +
                           online) + ',
               division_terms)
    }

    model = match.arg(model)
    lhs = switch(model,
                 'linear' = 'log1p(citations)',
                 'logistic' = 'I(citations <= 0)',
                 'negbin' = 'citations',
                 'zinb' = 'citations',
                 # Default to raw citation counts.
                 'citations')
    formula(paste(lhs, '~', rhs))
}


#' Fit a linear model of citations using lm.
#'
#' @param df A dataframe with citations data, and
#' divisions as created by `importData` followed by
#' `addDivisions`
#'
#' @param ... Extra arguments to `lm`
#'
#' @return An `lm` object.
#'
runLinearModel <- function(df, ...) {
    frm <- makeFormula(df, model='linear', ...)
    lm(frm, data=df, ...)
}


#' Fit a Logistic model of citations using glm.
#'
#' @param df A dataframe with citations data, and
#' divisions as created by `importData` followed by
#' `addDivisions`
#' @param ... Extra arguments to `glm`.
#'
#' @return A fitted GLM object.
#'
runLogisticModel <- function(df, ...) {
    frm <- makeFormula(df, model='logistic', ...)
    glm(frm, df, family=binomial(link=logit))
}


#' Fit a Negative Binomial model of citations using glm.
#'
#' @param df A dataframe with citations data, and
#' keyword factors as created by `importData` followed by
#' `addDivisions`
#' @param ... Extra arguments to `MASS::glm.nb`
#'
#' @return A fitted GLM object.
#'
runNegBinModel <- function(df, ...) {
    frm <- makeFormula(df, model='negbin', ...)
    MASS::glm.nb(frm, df, link='log')
}


#' Fit a Zero Inflated Negative Binomial model of
#' citations.
#'
#' Uses `pscl::zeroinfl` to fit the model. For parameters initial guesses
#' we use the coefficients from a logistic regression
#' (for the 'zero' parameters) and from a negative binomial regression
#' (for the 'count' parameters).
#'
#' @param df A dataframe with citations data, and
#' divisions as created by `importData` followed by
#' `addDivisions`
#' @param ... Extra arguments to `pscl::zeroinfl`
#'
#' @return A fitted `zeroinfl` object.
#'
runZeroInflNegBinGLM <- function(df, ...) {
    logit_coef <- runLogisticModel(df, division_interactions=FALSE, ...)$coef
    negbin_coef <- runNegBinModel(df,  division_interactions=FALSE, ...)$coef

    frm <- makeFormula(df, model='zinb', division_interactions=FALSE)

    starts = pscl::zeroinfl.control(start=list(count=negbin_coef,
                                               zero=logit_coef))

    pscl::zeroinfl(frm, df, dist='negbin', control=starts, ...)
}
