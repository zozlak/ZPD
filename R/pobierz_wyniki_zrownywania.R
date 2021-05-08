#' @title Pobiera wyniki testow zrownujacych
#' @description 
#' Pobiera ramke danych z wynikami testow zrownujacych w postaci szerokiej
#' @param src uchwyt źródła danych dplyr-a
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param rok rok, z ktorego dane maja zostac pobrane
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_wyniki_zrownywania = function(
  src,
  rodzajEgzaminu, 
  rok, 
  punktuj = TRUE, 
  idSkali = NULL,
  skroc   = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.vector(rok), is.numeric(rok), length(rok) == 1, 
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, punktuj %in% c(T, F),
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, skroc %in% c(T, F)
  )
  
  regExp = e(paste0('^zrównywanie;', rodzajEgzaminu, ';', rok, ';.*$'))
  tests = pobierz_testy(src) %>% 
    collect() %>%
    filter(grepl(regExp, .data$opis_testu))
  if (nrow(tests) == 0) {
    stop(e('w bazie nie ma takiego zrownywania'))
  }

  tmpName = sub('[.]', '_', paste0('t', as.numeric(Sys.time(), stats::runif(1))))
  query = sprintf(
    "SELECT zbuduj_widok_zrownywania(%s, %s, %d, %s, %s, %s, true)",
    dbplyr::escape(tmpName, con = src$con),
    dbplyr::escape(rodzajEgzaminu, con = src$con),
    rok,
    ifelse(punktuj, 'true', 'false'),
    ifelse(is.null(idSkali), 'null', as.numeric(idSkali)),
    ifelse(skroc, 'true', 'false')
  )
  DBI::dbExecute(src$con, e(query))
  data = tbl(src, sql(e(paste0("SELECT * FROM ", tmpName))))

  attr(data, 'idSkali') = idSkali
  
  return(data)
}
attr(pobierz_wyniki_zrownywania, 'grupa') = 'wyniki'
attr(pobierz_wyniki_zrownywania, 'testArgs') = list(
  'rodzajEgzaminu' = 'sprawdzian', 'rok' = 2013, 'idSkali' = 41
)
