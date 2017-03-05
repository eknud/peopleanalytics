#' Headcount Growth
#'
#' Using start date and end date, this function generates the headcount growth percent between two dates.
#' @param df data frame containing start date, end date, and any group variables preferred.
#' @param start.date character string containing the queried beginning date.
#' @param end.date character string containing the queried ending date.
#' @param start.var character string containing name of start date variable.
#' @param term.var character string containing name of end date variable.
#' @param group.var character string containing the name of a grouping variable. Defaults to NULL.
#' @keywords headcount growth
#' @export
#' @examples
#' hc.growth(df, "2016-01-01-", "2016-12-31", Start.date", "End.Date", "Department")

hc.growth <- function(df, start.date, end.date, start.var, term.var, group.var = NULL){
        if(!is.null(group.var)){
                df <- split(df, df[,group.var])
        }
        if(class(df) == "list"){
                by.group <- lapply(df, function(dfl){
                        sta <- hc(dfl, "Start.date", "End.Date", start.date)
                        end <- hc(dfl, "Start.date", "End.Date", end.date)
                        change <- round(100*(end-sta)/sta,1)
                        c(sta, end, change)
                })
                t(as.data.frame(by.group))
        }
        else{
                sta <- hc(df, "Start.date", "End.Date", start.date)
                end <- hc(df, "Start.date", "End.Date", end.date)
                change <- round(100*(end-sta)/sta,1)
                c(sta, end, change)
        }
}