#' @title Zwraca połączenie dplyr-a z bazą danych ZPD
#' @description 
#' Zwraca połączenie dplyr-a z bazą danych ZPD
#' @param user użytkownik
#' @param password hasło
#' @param dbname nazwa bazy danych
#' @param host adres serwera bazy danych
#' @param port port, na którym nasłuchuje serwer baz danych
#' @param ... dodatkowe parametry do przekazania do DBI::dbConnect()
#' @export
polacz = function(
  user = NULL,
  password = NULL,
  dbname = 'ewd',
  host = '194.54.27.158',
  port = 5432,
  ...
){
  conn = DBI::dbConnect(RPostgres::Postgres(), dbname, host, port, password, user, ...);
  conn = dbplyr::src_dbi(conn, auto_disconnect = TRUE)
  DBI::dbExecute(conn$con, 'SET client_min_messages to WARNING')
  return(conn)
}
