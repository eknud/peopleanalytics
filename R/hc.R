#' Headcount
#'
#' Using start date and end date, this function generates the headcount on a specific date.
#' @param df data frame containing start date, end date, and any group variables preferred.
#' @param start.var character string containing name of start date variable.
#' @param term.var character string containing name of end date variable.
#' @param date character string of the date to be queried
#' @param group.var character string containing the name of a grouping variable. Defaults to NULL.
#' @keywords headcount
#' @export
#' @examples
#' hc(df, "Start.date", "End.Date", "2016-12-31", "Department")

hc <- function(df, start.var, term.var, date, group.var = NULL){
        if(!is.null(group.var)){
                df <- split(df, df[,group.var])
        }
        if(class(df) == "list"){
                by.group <- lapply(df, function(dfl){
                        sum(as.Date(dfl[,start.var]) < as.Date(date))-sum(as.Date(dfl[,term.var]) < as.Date(date), na.rm=T)
                })
                t(as.data.frame(by.group))
        }
        else{
                sum(as.Date(df[,start.var]) < as.Date(date))-sum(as.Date(df[,term.var]) < as.Date(date), na.rm=T)
        }
}