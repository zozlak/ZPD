#' @title Zwraca połączenie dplyr-a z bazą danych ZPD
#' @description 
#' Zwraca połączenie dplyr-a z bazą danych ZPD
#' @param dbname nazwa bazy danych
#' @param host adres serwera bazy danych
#' @param port port, na którym nasłuchuje serwer baz danych
#' @param user użytkownik
#' @param password hasło
#' @export
#' @import RPostgreSQL
polacz = function(
  dbname = 'ewd',
  host = 'ewd.ibe.edu.pl',
  port = 5432,
  user = 'ewd_baza',
  password = 'CalEBo9'
){
  conn = src_postgres(dbname, host, port, user, password)
  enc = tolower(Sys.getlocale('LC_CTYPE'))
  if(!grepl('utf-8', enc)){
    # get the encoding name and set client encoding for the connection
    enc = paste0('WIN', sub('^[^0-9]+([0-9]+)$', '\\1', enc))
    RPostgreSQL::dbGetQuery(conn$con, sprintf("SET NAMES '%s'", enc))
  }
  return(conn)
}
