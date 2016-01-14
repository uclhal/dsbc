#' R version of the stata lookfor function
#'
#' This will try to find a variable when you can't remember its exact name.
#' @param string_to_find This is the string you wish to find passed. May be quoted or not.
#' @keywords stata, dsbc
#' @export
#' @examples
#' lookfor_function()

lookfor <- function(string_to_find, data=wdt){
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

