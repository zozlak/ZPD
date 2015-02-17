#' @title Pobiera wyniki testu
#' @param src uchwyt źródła danych dplyr-a
#' @param idTestu identyfikator testu do pobrania
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_wyniki_testu = function(
  src,
  idTestu, 
  punktuj        = TRUE,
  idSkali        = NULL,
  skroc          = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(idTestu), is.numeric(idTestu), length(idTestu) == 1, 
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, punktuj %in% c(T, F),
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, skroc %in% c(T, F)
  )
  
  tests = pobierz_testy(src) %>% 
    collect() %>%
    filter_(~id_testu == idTestu)
  if(nrow(tests) == 0){
    stop('nie ma takiego testu')
  }
  
  tmpName = sub('[.]', '_', paste0('t', as.numeric(Sys.time(), runif(1))))
  DBI::dbGetQuery(src$con, paste0("CREATE TEMPORARY VIEW ", tmpName, " AS SELECT 1"))
  query = sprintf(
    "SELECT zbuduj_widok_testu('%s', %d, %s, %s, %s, true);",
    tmpName,
    as.numeric(idTestu),
    ifelse(punktuj, 'true', 'false'),
    ifelse(is.null(idSkali), 'null', as.numeric(idSkali)),
    ifelse(skroc, 'true', 'false')
  )
  DBI::dbGetQuery(src$con, query)
  data = tbl(src, sql(paste0("SELECT * FROM ", tmpName)))
  
  attr(data, 'idSkali') = idSkali
  
  return(data)	
}	
attr(pobierz_wyniki_testu, 'grupa') = 'wyniki'
attr(pobierz_wyniki_testu, 'testArgs') = list('idTestu' = 636, 'idSkali' = 41)
