#' @title Parse a binary variable as 'median' and 'interquartile range'
#'
#' @description
#' Return a list that summarises the median and IQR, or mean and SD
#'
#' @import data.table
#' @param var a numerical variable
#' @param data a data frame (or data.table)
#' @param dp number of decimal places in formatted response
#' @keywords dsbc rmarkdown
#' @examples
#' # Create data.frame and summarise proportion of A's and B's
#' set.seed(1)
#' dt <- data.frame(choice=sample(c("A", "B"), 100, replace=TRUE), x=rnorm(100))
#' head(dt)
#' ff_mediqr(x, data=dt, dp=1)
#' dd <- data.table(x=rnorm(100,42,6))
#' ff_msd(x, dd)

#' @export
ff_mediqr <- function(var, data=dt, dp=0) {
    if (!"data.table" %in% class(data)) {
        dd <- data.table(data)
    } else {
        dd <- data
    }
    # Return median and IQR
    # Extract var with respect to data provided
    v <- eval(substitute(var), dd)
    fmt <- paste("%.", dp, "f", sep="")
    v.q <- sprintf(fmt, quantile(v, na.rm=TRUE))
    v.iqr <- paste(v.q[2], " to ", v.q[4], sep="")
    return(list(q50=v.q[3], iqr=v.iqr))

}

#' @export
ff_msd <- function(var, data=wdt, dp=0) {
    # Return mean and SD
    # Extract var with respect to data provided
    v <- eval(substitute(var), data)
    fmt <- paste("%.", dp, "f", sep="")
    v.mean <- sprintf(fmt, mean(v, na.rm=TRUE))
    v.sd <- sprintf(fmt, sd(v, na.rm=TRUE))
    return(list(mean=v.mean, sd=v.sd))

}

