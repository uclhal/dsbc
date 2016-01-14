# Scratch file for testing code
load("~/aor/academic/paper-spotepi/data/paper-spotepi.RData")
library(data.table)
ls()

lookfor <- function(string_to_find, data=NULL){
    # Extract the arguments and force conversion to string
    pars <- as.list(match.call()[-1])
    data.name <- as.character(pars$data)
    var <- as.character(pars$string_to_find)

    things <- ls(parent.frame())
    if (is.null(data)) {
        for (thing in length(things)) {
            if (is.data.frame(thing)) {
                result <- names(data)[grep(var, names(data))]
                if(length(result) == 0) {
                    warning(paste(var, "not found in", name))
                    return(NULL)
                }
                else {
                    return(result)
                }

            }
        }
    }

    
    result <- names(data)[grep(var, names(data))]
    if(length(result) == 0) {
        warning(paste(var, "not found in", data.name))
        return(NULL)
    }
    else {
        return(result)
    }
}

lookfor(dead99, wdt)
lookfor("dead99", wdt)
lookfor(dead, wdt)
lookfor("dead", wdt)

getwd()
