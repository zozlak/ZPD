#' @title Pobiera wyniki części egzaminu
#' @description 
#' Pobiera wyniki części egzaminu w postaci szerokiej
#' @param src uchwyt źródła danych dplyr-a
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param czescEgzaminu czesc egzaminu, ktorego wyniki maja zostac pobrane
#' @param rokEgzaminu rok egzaminu, ktorego wyniki maja zostac pobrane
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy PAOU
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_wyniki_egzaminu = function(
  src,
  rodzajEgzaminu, 
  czescEgzaminu, 
  rokEgzaminu, 
  czyEwd,
  punktuj = TRUE,
  idSkali = NULL,
  skroc   = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.vector(czescEgzaminu), is.character(czescEgzaminu), length(czescEgzaminu) == 1,
    is.vector(rokEgzaminu), is.numeric(rokEgzaminu), length(rokEgzaminu) == 1, 
    is.vector(czyEwd), is.logical(czyEwd), length(czyEwd) == 1, czyEwd %in% c(T, F),
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, punktuj %in% c(T, F),
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, skroc %in% c(T, F)
  )

  tests = pobierz_testy(src) %>% 
    collect() %>%
    filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~czesc_egzaminu == czescEgzaminu, ~rok == rokEgzaminu) %>%
    group_by_('dane_ewd') %>%
    summarize(n = n())
  if (nrow(tests) == 0) {
    stop(e('w bazie nie ma wyników takiego egzaminu'))
  }
  if (nrow(tests %>% filter_(~dane_ewd == czyEwd)) == 0) {
    stop(e('w bazie nie ma wyników takiego egzaminu, ale istnieją dla innego źródła danych (patrz parametr czyEwd)'))
  }
  
  tmpName = sub('[.]', '_', paste0('t', as.numeric(Sys.time(), stats::runif(1))))
  DBI::dbGetQuery(src$con, e(paste0("CREATE TEMPORARY VIEW ", tmpName, " AS SELECT 1")))
  query = sprintf(
    "SELECT zbuduj_widok_czesci_egzaminu('%s', %s, %s, %d, %s, %s, %s, %s, true);",
    tmpName,
    dbplyr::escape(rodzajEgzaminu),
    dbplyr::escape(czescEgzaminu),
    rokEgzaminu,
    ifelse(czyEwd, 'true', 'false'),
    ifelse(punktuj, 'true', 'false'),
    ifelse(is.null(idSkali), 'null', idSkali),
    ifelse(skroc, 'true', 'false')
  )
  DBI::dbGetQuery(src$con, e(query))
  data = tbl(src, sql(e(paste0("SELECT * FROM ", tmpName))))

  attr(data, 'idSkali') = idSkali
  
  return(data)	
}
attr(pobierz_wyniki_egzaminu, 'grupa') = 'wyniki'
attr(pobierz_wyniki_egzaminu, 'testArgs') = list(
  'rodzajEgzaminu' = 'sprawdzian', 'czescEgzaminu' = '', 'rok' = 2010, 'czyEwd' = TRUE, 'idSkali' = 41
)
