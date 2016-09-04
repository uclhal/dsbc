#' @title Parse a binary variable as 'n' and 'percent'
#'
#' @description
#' Return a list that summarises the count (n) and percentage of a vector
#'
#' @import data.table
#' @param var a categorical variable 
#' @param data a data frame (or data.table)
#' @keywords dsbc rmarkdown
#' @examples
#' # Create data.frame and summarise proportion of A's and B's
#' set.seed(1)
#' dt <- data.frame(choice=sample(c("A", "B"), 100, replace=TRUE))
#' table(dt$choice)
#' npct <- ff_np(choice, data=dt)
#' # choice
#' #    v  n pct
#' # 1: A 52  52
#' # 2: B 48  48

#' @export
ff_np <- function(var, data=dt, dp=1) {
    # Relies on data.table
    if (!"data.table" %in% class(data)) {
        dt <- data.table(data)
    }
    # Return n and % for binary vars
    v <- eval(substitute(var), dt)
    var <- substitute(var)
    # Error checking - should be coercible to factor
    if(nlevels(v)==0) {
        v <- factor(v)
        print(paste("WARNING", var, "is not a factor - ", nlevels(v), "levels found at conversion"))
    }
    fmt <- paste("%.", dp, "f", sep="")
    v.yn  <- dt[,.(n=.N,pct=100*.N/nrow(dt)),by=v]
    setorder(v.yn, v)
    print(v.yn)
    v.n <- sprintf("%.0f", v.yn$n)
    v.p <- paste0(sprintf(fmt, v.yn$pct), "%")
    # Return as list so you can use $ for subsetting
    return(list(n=v.n,p=v.p))
}
