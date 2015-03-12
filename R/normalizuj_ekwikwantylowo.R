#' @title Normalizuje wyniki ekwikwantylowo
#' @description
#' Znormalizowane wyniki doklejane są do danych jako nowa zmienna z sufiksem
#' "_norm".
#' 
#' Wyniki normalizowane są do rozkładu normalnego o średniej 100 i odchyleniu
#' standardowym 15.
#' 
#' IdSkali, której norma ma zostać zastosowana jest typowo przekazywane przez
#' atrybut parametru "dane" (funkcje pobierz_wyniki_...() ustawiają go 
#' automatycznie, jeśli stosowaly skale przy pobieraniu danych). Jest to 
#' bezpieczny, a przez to najbardziej polecany sposob uzycia.
#' 
#' Jesli jednak jestesmy swiadomi ryzyka zastosowania normy niewlasciwej do 
#' danych, mozliwe jest wymuszenie skali, ktorej norma ma zostac uzyta za 
#' pomoca parametru "idSkali".
#' 
#' Normalizacji mozna tez dokonac na podstawie danych - uznane one zostana
#' wtedy za populacyjne, na ich podstawie obliczone zostana normy, ktore
#' nastepnie zostana zastosowane do danych.
#' @param dane ramka danych zawierająca wyniki
#' @param kolWynik nazwa kolumny zawierającej wyniki
#' @param src uchwyt źródła danych dplyr-a (gdy normalizacja na podstawie norm w bazie)
#' @param idSkali skala, której norma z bazy ma zostać zastosowana
#' @param ... ew. parametry funkcji normy_ekwikwantylowe() (gdy normalizacja na podstawie danych)
#' @import dplyr
#' @export
normalizuj_ekwikwantylowo = function(
  dane,
  src      = NULL,
  kolWynik = 'wynik',
  idSkali  = NULL,
  ...
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane),
    is.null(src) | is.src(src),
    is.vector(kolWynik), is.character(kolWynik), length(kolWynik) == 1,
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1
  )
  
  resultCol = paste0(kolWynik, '_norm')
  if(!is.null(src)){
    if(is.null(idSkali)){
      idSkali = attr(dane, 'idSkali')
    }
    if(is.null(idSkali)){
      stop(e('Nie okreslono skali, której normy mają zostać zastosowane'))
    }
    norms = tbl(src, sql(e("SELECT * FROM normy_ekwikwantylowe"))) %>%
      filter_(~id_skali == idSkali) %>%
      select_('wartosc', 'wartosc_zr') %>%
      rename_(.dots = setNames(list('wartosc', 'wartosc_zr'), c(kolWynik, resultCol)))
    if(nrow(norms %>% collect()) == 0){
      stop(e('W bazie nie ma określonych norm dla tej skali (lub nie ma takiej skali)'))
    }
  }else{
    dane = as.data.frame(dane)
    norms = suppressMessages(normy_ekwikwantylowe(dane[, kolWynik], ...))
    norms = data_frame(as.numeric(names(norms)), norms)
    names(norms) = c(kolWynik, resultCol)
  }
  
  dane = dane %>%
    left_join(norms, copy = TRUE)
  
  return(dane)
}
