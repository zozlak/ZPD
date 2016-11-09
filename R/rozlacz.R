#' @title Rozłącza połączenie z bazą
#' @description 
#' @param src uchwyt połączenia zwrócony przez funkcję polacz()
#' @import dplyr
#' @export
#' @import DBI
rozlacz = function(
  src
){
  stopifnot(is.src(src))
  DBI::dbDisconnect(src$con)
  return(invisible(TRUE))
}
