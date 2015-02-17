#' @title Zwraca połączenie dplyr-a z bazą danych ZPD
#' @param dbname nazwa bazy danych
#' @param host adres serwera bazy danych
#' @param port port, na którym nasłuchuje serwer baz danych
#' @param user użytkownik
#' @param password hasło
#' @import dplyr
#' @export
#' @import RPostgreSQL
polacz = function(
  dbname = 'ewd',
  host = 'ewd.ibe.edu.pl',
  port = 5432,
  user = 'ewd_baza',
  password = 'CalEBo9'
){
  return(src_postgres(dbname, host, port, user, password))
}
