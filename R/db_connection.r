#' Reads a TADM mdb file and extracts the TADM info into a table
#'
#' This function Reads a TADM mdb file and extracts the TADM info into a table
#'
#' @param database_path pathway to the mdb TADM file
#' @return a table containing all the information of the TADM mdb file
#' @export
db_connection <- function(database_path){
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- database_path
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- RODBC::odbcDriverConnect(PATH, rows_at_time = 1)
main_table <- RODBC::sqlFetch(channel,"TadmCurve")
return (main_table)
}