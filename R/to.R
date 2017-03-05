#' Turnover
#'
#' Using start date and end date, this function generates the turnover rate between two dates.
#' @param df data frame containing start date, end date, and any group variables preferred.
#' @param start.date character string containing the queried beginning date.
#' @param end.date character string containing the queried ending date.
#' @param start.var character string containing name of start date variable.
#' @param term.var character string containing name of end date variable.
#' @param group.var character string containing the name of a grouping variable. Defaults to NULL.
#' @keywords turnover
#' @export
#' @examples
#' to(df, "2016-01-01-", "2016-12-31", Start.date", "End.Date", "Department")

to <- function(df, start.date, end.date, start.var, term.var, group.var = NULL){
        if(!is.null(group.var)){
                df <- split(df, df[,group.var])
        }
        if(class(df) == "list"){
                by.group <- lapply(df, function(dfl){
                        hc1 <- hc(dfl, "Start.date", "End.Date", start.date) # headcount at start
                        hc2 <- hc(dfl, "Start.date", "End.Date", end.date) # headcount at end
                        tc <- length(which((dfl[,term.var] >= as.Date(start.date)) & (dfl[,term.var] <= as.Date(end.date)))) # headcount that left
                        tr <- tc/((hc1+hc2)/2) # turnover rate
                        c(hc1, hc2, tc, round(tr*100,1))
                })
                t(as.data.frame(by.group))
        }
        else{
                hc1 <- hc(df, "Start.date", "End.Date", start.date) # headcount at start
                hc2 <- hc(df, "Start.date", "End.Date", end.date) # headcount at end
                tc <- length(which((df[,term.var] >= as.Date(start.date)) & (df[,term.var] <= as.Date(end.date)))) # headcount that left
                tr <- tc/((hc1+hc2)/2) # turnover rate
                c(hc1, hc2, tc, round(tr*100,1))
        }
}