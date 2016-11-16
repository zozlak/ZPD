#' @title Zamienia liczbowe kody dystraktorów na kody literowe
#' @description
#' Z powodów wydajnościowych dystraktory w bazie zapisywane są w postaci kodów
#' liczbowych i tak też pobierają je funkcje \code{pobierz_wyniki_...()}. 
#' Funkcja \code{odkoduj_dystraktory()} zamienia kody liczbowe na oznaczenia
#' literowe użyte pierwotnie w arkuszach testowych (złączając przekazane wyniki
#' z danymi zwracanymi przez funkcję \code{pobierz_schemary_odp()}).
#' 
#' Z uwagi na brak przenośności niektórych operacji pomiędzy kodem R i SQL, jak
#' również z uwagi na to, że operacja zamiany po stronie bazy danych na danych
#' w postaci szerokiej byłaby bardzo kosztowna obliczeniowo, funkcja pobiera
#' najpierw przekazane dane za pomocą \code{collect()}
#' 
#' Kody liczbowe -1 i -2 oznaczają, odpowiednio, opuszczenie i wielokrotne
#' zaznaczenie.
#' @param dane ramka danych z wynikami uczniów
#' @param src uchwyt źródła danych dplyr-a
#' @param kolDystr wyrażenie regularne dopasowujące nazwy kolumn z kodami
#'   liczbowymi dystraktorów do zamiany na kody literowe
#' @param kolKryt wyrażenie regularne dopasowujęce nazwę kolumny z
#'   identyfikatorem kryterium oceny (tylko dane w postaci długiej)
#' @import dplyr
#' @export
odkoduj_dystraktory = function(
  dane,
  src,
  kolDystr = '^(odpowiedz|k_[0-9]+)$',
  kolKryt  = '^kryterium$'
){
  stopifnot(
    is.src(src),
    is.data.frame(dane) | is.tbl(dane),
    is.vector(kolDystr), is.character(kolDystr), length(kolDystr) == 1, all(!is.na(kolDystr))
  )
  
  schematy = pobierz_schematy_odp(src) %>%
    collect()
  if(any(class(dane) %in% 'tbl_sql')){
    message('Pobieram dane z bazy...')
    dane = dane %>%
      collect()
  }
  
  kolKryt  = grep(kolKryt, colnames(dane), value = TRUE)
  kolDystr = grep(kolDystr, colnames(dane), value = TRUE)
  if(length(kolKryt) > 1 | length(kolKryt) > 0 & length(kolDystr) > 1){
    stop(e('Zbyt wiele kolumn kandydatów na identyfikator kryterium oceny i/lub kod odpowiedzi'))
  }
  if(length(kolKryt) > 0 & length(kolDystr) < 1){
    stop(e('W zbiorze danych brak kolumny z kodami odpowiedzi'))
  }
  if(1 == length(kolKryt)){
    # dane w postaci dlugiej
    dane = suppressMessages(
      dane %>%
      left_join(schematy %>% rename_(.dots = stats::setNames(list('kolejnosc_dystr'), kolDystr))) %>%
      mutate_(.dots = stats::setNames(list(paste0('ifelse(is.na(dystraktor), ', kolDystr, ', dystraktor)')), kolDystr)) %>%
      select_('-dystraktor')
    )
  }else{
    # dane w postaci szerokiej
    for(kol in kolDystr[kolDystr %in% schematy$kryterium]){
      schemat = schematy %>%
        filter_(~kryterium == kol) %>%
        rename_(.dots = stats::setNames(list('kolejnosc_dystr'), kol)) %>%
        select_('-kryterium')
      dane = suppressMessages(
        dane %>%
        left_join(schemat) %>%
        mutate_(.dots = stats::setNames(list(paste0('ifelse(is.na(dystraktor), ', kol, ', dystraktor)')), kol)) %>%
        select_('-dystraktor')
      )
    }
    filtr = !kolDystr %in% schematy$kryterium
    if(sum(filtr) > 0){
      message(paste0('Pominięto pytania otwarte: ', paste0(kolDystr[filtr], collapse = ', ')))
    }
  }
  
  return(dane)
}
