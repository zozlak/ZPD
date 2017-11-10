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
  host = '89.231.23.130',
  port = 5432,
  user = 'ewd_baza',
  password = 'CalEBo9'
){
  conn = src_postgres(dbname, host, port, user, password)
  #conn = DBI::dbConnect(RPostgres::Postgres(), dbname, host, port, password, user)
  #conn = dbplyr::src_dbi(conn, auto_disconnect = TRUE)
  enc = tolower(Sys.getlocale('LC_CTYPE'))
  if (!grepl('utf-8', enc)) {
    # get the encoding name and set client encoding for the connection
    enc = paste0('WIN', sub('^[^0-9]+([0-9]+)$', '\\1', enc))
    DBI::dbGetQuery(conn$con, sprintf("SET NAMES '%s'", enc))
  }
  DBI::dbGetQuery(conn$con, 'SET client_min_messages to WARNING')
  return(conn)
}
