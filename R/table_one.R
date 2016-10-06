#' @title Prepare Table 1 for biomedical research papers
#'
#' @description
#' Given a long form data.table, then summarise each item, group as necessary,
#' and prepare a publication quality Table 1
#'
#' @import data.table
#' @param dt a long data.table with columns for 'id', 'item', and 'val' (and optionally 'group')
#' @param table.dict (see notes below)
#' @param grp a grouping variable (will produce separate summaries per group)
#' @keywords dsbc rmarkdown cchic
#' @examples
#' # table_one <- function(dt, table.dict, grp=NULL) {

#  ==========
#  = Issues =
#  ==========
# - [ ] TODO(2016-10-06): generalise so not CCHIC specific

row_normal <-  function(d, row.dict) {

    fmt <- paste0("%.", row.dict$decimal_places, "f")

    setDT(d) # convert to data.table by reference

    d <- d[, .(
        label = row.dict$dataItem,
        level = "",
        summ  = "Mean (SD)",
        col1 = sprintf(fmt, mean(val)),
        col2 = sprintf(fmt, sd(val))),
        by=grp]

    d <- dcast(melt(d, id.vars=c(1:4)), label + level + summ ~ grp + variable)

    return(d)
}

# val <- dd[item=="NIHR_HIC_ICU_0108"]$val
# val <- as.numeric(as.character(val))
# grp <- dd[item=="NIHR_HIC_ICU_0108"]$site_id
# row.dict <- table.dict["NIHR_HIC_ICU_0108"][[1]]
# d <- data.frame(val=val, grp=grp, stringsAsFactors=FALSE)
# row_normal(d, row.dict)

row_cont <-  function(d, row.dict) {

    fmt <- paste0("%.", row.dict$decimal_places, "f")

    setDT(d) # convert to data.table by reference

    d <- d[, .(
        label = row.dict$dataItem,
        level = "",
        summ  = "Median (IQR)",
        col1 = sprintf(fmt, quantile(val, 0.5)),
        col2 = paste0(sprintf(fmt, quantile(val, 0.25)),
                " to ",
                sprintf(fmt, quantile(val, 0.75)))
            ), by=grp]

    d <- dcast(melt(d, id.vars=c(1:4)), label + level + summ ~ grp + variable)

    return(d)
}

# val <- dd[item=="NIHR_HIC_ICU_0122"]$val
# val <- as.numeric(as.character(val))
# grp <- dd[item=="NIHR_HIC_ICU_0122"]$site_id
# row.dict <- table.dict["NIHR_HIC_ICU_0122"][[1]]
# d <- data.frame(val=val, grp=grp, stringsAsFactors=FALSE)
# row_cont(d, row.dict)

row_cat <- function(d, row.dict) {

    # - [ ] TODO(2016-05-07): allow for ordering of categories

    setDT(d)
    # Label categories as per ANALYSIS_REF.yaml
    if (is.null(row.dict$levels)) {
        d[,val := factor(d$val)]
    } else {
        llabels <- sapply(row.dict$levels, function(x) x)
        llevels <- names(llabels)
        d[,val := factor(d$val, levels=llevels, labels=llabels) ]
    }

    # Prepare table by grp
    tt.n <- with(d, table(grp, val))

    tt.p <- data.table(prop.table(tt.n, 1)) # row margins
    tt.p[, N := paste0(sprintf("%.1f", (100L * N)), "%")]
    tt.p[, col := "col2"]

    tt.n <- data.table(tt.n)
    tt.n[, col := "col1"]

    d <- rbind(tt.n, tt.p)

    setnames(d, "val", "level")
    setnames(d, "N", "variable")

    d[, `:=` (
        label = row.dict$dataItem,
        summ = "n (%)"
        ) ]

    d <- dcast(d, label + level + summ ~ grp + col, value.var = "variable" )

    return(d)
}

# val <- dd[item=="NIHR_HIC_ICU_0058"]$val
# val <- dd[item=="adm.cat"]$val
# val <- as.character(val)
# val
# # grp <- dd[item=="NIHR_HIC_ICU_0058"]$site_id
# row.dict <- table.dict["NIHR_HIC_ICU_0058"][[1]]
# row.dict <- table.dict["adm.cat"][[1]]
# row.dict
# is.null(row.dict$levels)
# head(factor(val))
# llabels <- sapply(row.dict$levels, function(x) x)
# llabels
# d <- data.frame(val=val, grp=grp, stringsAsFactors=FALSE)
# row_cat(d, row.dict)


row_maker <- function(val, row.dict, grp=NULL, big_N=NULL) {

    row.fmt <- NULL

    if (row.dict$Datatype == "numeric") {
        val <- as.numeric(as.character(val))
    } else {
        val <- as.character(val)
    }

    # Create single group if necessary or bind existing group
    if (is.null(grp)) {
        d <- data.frame(val=val, grp="All", stringsAsFactors=FALSE)
    } else {
        assert_that(length(val) == length(grp))
        d <- data.frame(val=val, grp=grp, stringsAsFactors=FALSE)
    }


    tryCatch({

        # Mean / SD
        if (row.dict$distribution == "normal") {
            rr <- row_normal(d, row.dict)

        # Median / IQR
        } else if (row.dict$distribution != "normal" & row.dict$Datatype == "numeric") {
            rr <- row_cont(d, row.dict)

        # List and binary
        } else if (row.dict$Datatype != "numeric") {

            # Assume categorical but warn if not pre-specified
            distribution.cat <- c("binary", "nominal", "ordinal")
            if (!(row.dict$distribution %in% distribution.cat)) {
                warning(paste(row.dict$dataItem,
                    "handled as categorical, but distribution is",
                    row.dict$distribution ))
            }

            rr <- row_cat(d, row.dict)

        # Error management
        } else {
            stop(paste(row.dict$dataItem,
                "not handled. Please check row dictionary, see",
                row.dict))
        }
    },

         error = function(err) {
            print(err)
            diagnostics <- paste0(c$message, " (with ",  row.dict, ")", "See data from:", str(d))
            print(diagnostics)
            stop(err)
        }
    )

    return(rr)

}

# Check row maker
# rows.test <- c("NIHR_HIC_ICU_0097", "NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0122")
# for (i in 1:length(rows.test)) {
#     val <- dd[item==rows.test[i]]$val
#     grp <- dd[item==rows.test[i]]$site_id
#     row.dict <- table.dict[rows.test[i]][[1]]
#     print(row_maker(val, row.dict, grp=grp))
# }

# row.dict <- table.dict[i][[1]]
# val <- dd[item==row.dict$NHICcode]$val
# grp <- dd[item==row.dict$NHICcode]$site_id
# (rr <- row_maker(val, row.dict, grp=grp))
# class(rr)


#' @export
table_one <- function(dt, table.dict, grp=NULL) {

    # dt <- dd
    # Check that the data contains all the fields needed for the table
    (fields.d <- unique(dt$item))
    (fields.t <- map_chr(table.dict, "NHICcode"))
    test.fields_in_data <- sapply(fields.t, function(x) x %in% fields.d)
    tryCatch(
        assert_that(sum(test.fields_in_data) == length(test.fields_in_data)),

            error = function(err) {
                print("!!! ERRROR: table.dict fields not found in data provided (see list below)")
                print(test.fields_in_data)
                stop(err)

            }
     )

    # Needs to receive data including the variables, values and (optionally) the grouping

    message.progress.table <- paste0("Fields to be parsed: ", names(table.dict), " for group: ", as.character(grp) )
    map_chr(table.dict, "dataItem")
    print(message.progress.table)

    for (i in 1:length(table.dict)) {

        # Define row and generate data
        row.dict <- table.dict[i][[1]]
        val <- dd[item==row.dict$NHICcode]$val

        message.progress.row <- paste0("Parsing: ", row.dict$dataItem, " (", row.dict$NHICcode, ")", str(val, 1))
        print(message.progress.row)

        if (!is.null(grp)) {
            val.grp <- dd[item==row.dict$NHICcode][grp]
        } else {
            val.grp <- NULL
        }

        # Generate new row
        tryCatch(
            rr <- row_maker(val, row.dict, grp=val.grp),

             error = function(err) {
                diagnostics <- paste0(c$message, " (with ",  row.dict, ") ", "See data from: ", str(d))
                print(diagnostics)
                stop(err)
            }
         )

        # Append to data.frame
        if (i==1) {
            table.one <- rr
        }
        else {
            table.one <- rbind(table.one, rr)
        }
    }

    return(table.one)
}

