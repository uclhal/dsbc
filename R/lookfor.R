#' @title R version of the Stata lookfor function
#'
#' @description This will try to find a variable when you can't remember its exact name.
#' R version of the stata lookfor function
#'
#' @param string_to_find This is the string you wish to find passed. May be quoted or not.
#' @param data data frame or table to search
#' @keywords stata dsbc
#' @examples
#' # Create dt dataframe called dt then find vars with 'y' in the name
#' dt <- data.frame(id=c(1:10), x=letters[1:10], y1=rnorm(10), y2=round(runif(10)*100))
#' lookfor(y, data=dt)


#' @export
lookfor <- function(string_to_find, data=dt){
    # Extract the arguments and force conversion to string
    pars <- as.list(match.call()[-1])
    data.name <- as.character(pars$data)
    var <- as.character(pars$string_to_find)

    # Regular expression search through names
    result <- names(data)[grep(var, names(data))]

    if(length(result) == 0) {
        warning(paste(var, "not found in", data.name))
        return(NULL)
    }
    else {
        return(result)
    }
}

