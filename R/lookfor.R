#' R version of the stata lookfor function
#'
#' This will try to find a variable when you can't remember its exact name.
#' @param string_to_find This is the string you wish to find passed. May be quoted or not.
#' @keywords stata dsbc
#' @export
#' @examples
#' # Create a dataframe called ddata then find vars with 'y' in the name
#' > ddata <- data.frame(id=c(1:10), x=letters[1:10], y1=rnorm(10), y2=round(runif(10)*100))
#' > ddata
#'    id x         y1 y2
#' 1   1 a  0.3468032 28
#' 2   2 b  0.8948499  9
#' 3   3 c  0.3853698 82
#' 4   4 d  0.3134482 88
#' 5   5 e -1.7609531 52
#' 6   6 f  0.1448522 26
#' 7   7 g -1.0863107 11
#' 8   8 h  0.6146917 92
#' 9   9 i  0.5649187 37
#' 10 10 j  0.5078450 86
#' > lookfor(y, data=ddata)
#' [1] "y1" "y2"


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

