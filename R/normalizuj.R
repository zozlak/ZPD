#' @title Normalizuje wyniki
#' @description
#' Znormalizowane wyniki doklejane są do danych jako nowa zmienna z sufiksem
#' "_norm".
#' 
#' Wyniki normalizowane są dna jeden z dwóch sposobów - albo ekwikwantylowo na 
#' podstawie przekazanych danych (gdy nie podano parametru \code{src}) albo za
#' pomoca wskazanej normy w bazie danych (podajac parametry \code{src},
#' \code{idSkali}, \code{skalowanie} i ew. \code{grupa}).
#' @seealso normy_ekwikwantylowe  
#' @param dane ramka danych zawierająca wyniki
#' @param kolWynik nazwa kolumny zawierającej wyniki
#' @param src uchwyt źródła danych dplyr-a (gdy normalizacja na podstawie norm w
#'   bazie)
#' @param idSkali skala, której norma z bazy ma zostać zastosowana
#' @param skalowanie skalowanie w ramach wybranej skali, którego norma z bazy ma
#'   zostać zastosowana
#' @param grupa grupa w ramach wybranego skalowania, którego norma z bazy ma
#'   zostać zastosowana
#' @param ... ew. parametry funkcji normy_ekwikwantylowe() (gdy normalizacja na
#'   podstawie danych)
#' @import dplyr
#' @export
normalizuj = function(
  dane,
  src        = NULL,
  kolWynik   = 'wynik',
  idSkali    = NULL,
  skalowanie = NULL,
  grupa      = NULL,
  ...
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane), sum(colnames(dane) %in% kolWynik) == 1, 
    is.null(src) | is.src(src),
    is.vector(kolWynik), is.character(kolWynik), length(kolWynik) == 1,
    suppressWarnings(
      is.null(idSkali) & is.null(skalowanie) & is.null(grupa) | 
        is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1 & all(!is.na(idSkali)) &
        is.vector(skalowanie) & is.numeric(skalowanie) & length(skalowanie) == 1 & all(!is.na(skalowanie)) &
        is.vector(grupa) & is.character(grupa) & length(grupa) == 1 & all(!is.na(grupa))
    )
  )
  
  kolWynikNazwa = paste0(kolWynik, '_norm')
  if(!is.null(src)){
    if(is.null(idSkali)){
      idSkali = attr(dane, 'idSkali')
    }
    if(is.null(idSkali)){
      stop(e('Nie określono skali, której normy mają zostać zastosowane'))
    }
    
    skl = skalowanie
    grp = grupa
    normy = pobierz_normy(src) %>%
      filter_(~id_skali == idSkali, ~skalowanie == skl, ~grupa == grp) %>%
      select_('grupa', 'wartosc', 'wartosc_zr') %>%
      rename_(.dots = stats::setNames(list('wartosc', 'wartosc_zr'), c(kolWynik, kolWynikNazwa)))
    normyPobrane = normy %>% collect()
    if(nrow(normyPobrane) == 0){
      stop(e(paste0(
        'W bazie nie ma określonych norm dla podanych skali, skalowania oraz grupy (lub w ogóle nie ma danej skali/skalowania/grupy)\n',
        'Aby wyszukać dostępne normy, użyj polecenia:\n  ',
        'pobierz_skale(src, PvEap = FALSE, doPrezentacji = NA) %>% ',
        'filter(posiada_normy == TRUE) %>% ',
        'select(id_skali, opis_skali, skalowanie, opis_skalowania, grupa) %>% ',
        'distinct()'
      )))
    }
    
    if(length(unique(normyPobrane$grupa)) > 1 & !any(colnames(dane) %in% 'grupa')){
      stop(e('Norma rozróżnia wiele grup, tymczasem dane nie zawierają zmiennej "grupa"'))
    }
  }else{
    dane = as.data.frame(dane)
    normy = suppressMessages(normy_ekwikwantylowe(dane[, kolWynik], ...))
    normy = data_frame(as.numeric(names(normy)), normy)
    names(normy) = c(kolWynik, kolWynikNazwa)
  }
  
  dane = suppressMessages(
    dane %>%
    left_join(normy, copy = TRUE)
  )
  
  return(dane)
}
normalizuj_ekwikwantylowo = normalizuj