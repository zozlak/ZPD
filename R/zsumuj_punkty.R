#' @title Sumuje punkty w ramach testu
#' @description
#' Parametr "usunKryteria" dziala tylko dla danych w postaci szerokiej. Dla 
#' danych w postaci dlugiej nastepuje agregacja, ktora sila rzeczy usuwa
#' informacje o poszczegolnych kryteriach.
#' @param dane wynik dzialania dowolnej z funkcji pobierz_wyniki_...()
#' @param usunKryteria czy usuwac ze zbioru kolumny z wynikami za poszczegolne kryteria
#' @import dplyr
#' @export
zsumuj_punkty = function(
  dane,
  usunKryteria = TRUE
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane),
    is.vector(usunKryteria), is.logical(usunKryteria), length(usunKryteria) == 1, usunKryteria %in% c(T, F)
  )
  
  colNames = colnames(dane)
  if(czy_postac_dluga(dane)){
    # postać długa
    groupCols = colNames[! colNames %in% c('kryterium', 'odpowiedz', 'ocena')]
    dane = dane %>% 
      group_by(across({{groupCols}})) %>%
      summarize(wynik = sum(.data$ocena, na.rm = TRUE)) %>%
      ungroup()
  } else if(czy_postac_szeroka(dane)){
    # postać szeroka
    sumCols = grep('^[pk]_[0-9]+$', colNames, value = T)
    sumForm = str2lang(paste0(sumCols, collapse = '+'))
    dane = dane %>% 
      mutate(wynik = !!sumForm)
    if(usunKryteria == TRUE){
      dane = dane %>% select(-{{ sumCols }})
    }
  }
  
  return(dane)
}
