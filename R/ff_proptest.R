#' @title Compare two proportions
#'
#' @description
#' Returns a list containing the confusion table, the two proportions,
#' and the difference in the proportions with a confidence interval
#'
#' @import data.table
#' @param var a binary variable
#' @param byvar a stratifying binary variable
#' @param data a data frame (or data.table)
#' @param dp number of decimal places in formatted response
#' @return a table and a series of strings formatted for a scientific paper
#' @keywords dsbc rmarkdown
#' @examples
#' # Create data.frame and summarise proportion of A's and B's
#' set.seed(1)
#' dd <- data.frame(
#'      X=sample(c(0,1), 100, replace=TRUE),
#'      strata=sample(c("A", "B"), 100, replace=TRUE))
#'
#' with(dd, table(strata, X))
#' ff_proptest(X, strata, data=dd)
#' # $table
#'
#' #      0  1
#' #   A 23 23
#' #   B 29 25
#'
#' # $est1
#' # [1] "50.0%"
#'
#' # $est2
#' # [1] "53.7%"
#'
#' # $d
#' # [1] "3.7%"
#'
#' # $ci
#' # [1] "17.9% to 25.4%"

#' @export
ff_proptest <- function(var, byvar, data=dt, dp=1) {
    # returns differences as percentages
    # First of all extract the args without requiring the user to use quotes
    pars <- as.list(match.call()[-1])

    var <- as.character(pars$var)
    byvar <- as.character(pars$byvar)

    # Relies on data.table
    if (!"data.table" %in% class(data)) {
      dd <- data.table(data)
    } else {
      dd <- data
    }

    dd <- dd[,c(var, byvar),with=FALSE]
    fmt <- paste("%.", dp, "f", sep="")
    t <- with(dd, table(dd[[2]], dd[[1]]))
    byvar.var <- prop.test(t)

    byvar.var.d     <- byvar.var$estimate[1] - byvar.var$estimate[2]

    est1            <- paste0(sprintf(fmt, 100*byvar.var$estimate[1]), "%")
    est2            <- paste0(sprintf(fmt, 100*byvar.var$estimate[2]), "%")
    l95             <- sprintf(fmt, 100*abs(byvar.var$conf.int[1]))
    u95             <- sprintf(fmt, 100*abs(byvar.var$conf.int[2]))

    # get order right
    if (abs(byvar.var$conf.int[1])>abs(byvar.var$conf.int[2])) {
        x   <- l95
        l95 <- u95
        u95 <- x
    }

    byvar.var.d     <- paste0(sprintf(fmt, 100*abs(byvar.var.d)), "%")
    byvar.var.ci    <- paste0(l95, "% to ", u95, "%")

    return(list(
        table = t,
        est1 = est1,
        est2 = est2,
        d = byvar.var.d,
        ci = byvar.var.ci))
}
