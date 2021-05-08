#' @title Zwraca połączenie dplyr-a z bazą danych ZPD
#' @description 
#' Zwraca połączenie dplyr-a z bazą danych ZPD
#' @param dbname nazwa bazy danych
#' @param host adres serwera bazy danych
#' @param port port, na którym nasłuchuje serwer baz danych
#' @param user użytkownik
#' @param password hasło
#' @export
polacz = function(
  dbname = 'ewd',
  host = '127.0.0.1',
  port = 5432,
  user = 'mzoltak',
  password = 'oT_us5f29YRQ'
){
  conn = DBI::dbConnect(RPostgres::Postgres(), dbname, host, port, password, user, check_interrupts = TRUE);
  conn = dbplyr::src_dbi(conn, auto_disconnect = TRUE)
  DBI::dbExecute(conn$con, 'SET client_min_messages to WARNING')
  return(conn)
}
