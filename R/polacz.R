#' @title Zwraca połączenie dplyr-a z bazą danych ZPD
#' @description 
#' Zwraca połączenie dplyr-a z bazą danych ZPD
#' @param dbname nazwa bazy danych
#' @param host adres serwera bazy danych
#' @param port port, na którym nasłuchuje serwer baz danych
#' @param user użytkownik
#' @param password hasło
#' @param backend nazwa pakietu do komunikacji z bazą danych
#'   RPostgreSQL lub RPostgres
#' @export
#' @import RPostgreSQL
polacz = function(
  dbname = 'ewd',
  host = '89.231.23.130',
  port = 5432,
  user = 'ewd_baza',
  password = 'CalEBo9',
  backend = 'RPostgreSQL'
){
  stopifnot(
    is.vector(backend), is.character(backend), length(backend) == 1, all(!is.na(backend)), all(backend %in% c('RPostgreSQL', 'RPostgres'))
  )
  if (backend == 'RPostgres') {
    conn = DBI::dbConnect(RPostgres::Postgres(), dbname, host, port, password, user);
  } else {
    conn = DBI::dbConnect(RPostgreSQL::PostgreSQL(), user, password, host, dbname, port);
  }
  conn = dbplyr::src_dbi(conn, auto_disconnect = TRUE)
  enc = tolower(Sys.getlocale('LC_CTYPE'))
  if (!grepl('utf-8', enc)) {
    # get the encoding name and set client encoding for the connection
    enc = paste0('WIN', sub('^[^0-9]+([0-9]+)$', '\\1', enc))
    DBI::dbGetQuery(conn$con, sprintf("SET NAMES '%s'", enc))
  }
  DBI::dbGetQuery(conn$con, 'SET client_min_messages to WARNING')
  return(conn)
}
