#' @title Pobiera wyniki testow (odpowiedzi) w postaci dlugiej
#' @description
#' Poniewaz skale definiuja przeksztalcenia wynikow w punktach (i nie da sie 
#' tych przeksztalcen zastosowac dla dystraktorow), zastosowanie skali powoduje
#' ze zwrocony zbior danych pozbawiony bedzie kolumny "odpowiedz"
#' 
#' Zastosowanie skali spowoduje rowniez automatyczne przyciecie kryteriow 
#' jedynie do tych, ktore naleza do wskazanej skali.
#' @param dane wynik dzialania dowolnej z funkcji pobierz_wyniki_...()
#' @param src uchwyt źródła danych dplyr-a
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy stosować skróty skal
#' @import dplyr
#' @export
zastosuj_skale = function(
  dane,
  src,
  idSkali,
  skroc = TRUE
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane),
    is.vector(idSkali), is.numeric(idSkali), length(idSkali) == 1, all(!is.na(idSkali)),
    is.src(src),
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, all(!is.na(skroc))
  )
  
  query = sprintf(
    "SELECT id_skrotu, 'k_' || COALESCE(s.id_kryterium, p.id_kryterium) AS kryterium, COALESCE('k_' || s.id_kryterium, 'p_' || id_pseudokryterium) AS kryterium_s
       FROM skale_elementy s LEFT JOIN pseudokryteria_oceny_kryteria p USING (id_pseudokryterium) 
       WHERE id_skali = %d",
    idSkali
  )
  skala = tbl(src, sql(e(query))) %>% collect()
  if(nrow(skala) == 0){
    stop(e('Nie ma takiej skali lub skala nie ma określonych elementów'))
  }
  
  skroty = NULL
  if(skroc == TRUE){
    query = "SELECT id_skrotu, wartosc, nowa_wartosc FROM skroty_skal_mapowania"
    skroty = tbl(src, sql(e(query)))
  }
  
  if(czy_postac_dluga(dane)){
    return(zastosuj_skale_dluga(dane, skala, skroty))
  }else if(czy_postac_szeroka(dane)){
    return(zastosuj_skale_szeroka(dane, skala, skroty))
  }
}

zastosuj_skale_dluga = function(
  dane,
  skala,
  skroty
){
  dane = dane %>% 
    inner_join(skala, copy = TRUE) %>%
    select_('-kryterium') %>%
    rename_('kryterium' = 'kryterium_s')
  kol = setdiff(colnames(dane), c('odpowiedz', 'ocena'))
  
  dane = dane %>% 
    group_by_(.dots = kol) %>% 
    summarize_(.dots = list('ocena' = 'sum(ocena)')) %>% 
    ungroup()
  
  if(is.tbl(skroty)){
    przeksztalcenie = ifelse(
      any(class(dane) %in% 'tbl_sql'),
      'coalesce(nowa_wartosc, wartosc)',
      'ifelse(!is.na(nowa_wartosc), nowa_wartosc, wartosc)'
    )
    dane = dane %>% 
      rename_('wartosc' = 'ocena') %>% 
      left_join(skroty, copy = TRUE) %>% 
      mutate_('ocena' = przeksztalcenie) %>%
      select_('-wartosc', '-nowa_wartosc')
  }
  
  return(dane %>% select_('-id_skrotu'))
}

zastosuj_skale_szeroka = function(
  dane,
  skala,
  skroty
){
  zbuduj_przeksztalcenie = function(k, d){
    if(nrow(k) == 1){
      return(data_frame(mutate = NA_character_))
    }
    return(data_frame(
      mutate = paste(k$kryterium, collapse = ' + ')
    ))
  }
  
  kolUsun = grep('^[pk]_[0-9]+$', setdiff(colnames(dane), skala$kryterium), value = TRUE)
  if(length(kolUsun) > 0){
    dane = dane %>%
      select_(.dots = paste0('-', kolUsun))
  }
  skala = skala %>%
    filter_(~kryterium %in% colnames(dane))
  
  przekszt = skala %>% 
    group_by_('kryterium_s') %>% 
    do_(~zbuduj_przeksztalcenie(., dane)) %>%
    filter_('!is.na(mutate)') %>%
    collect()
  if(nrow(przekszt) > 0){
    przekszt = setNames(as.list(przekszt$mutate), przekszt$kryterium_s)
    usun = paste0('-', (skala %>% filter_(~kryterium_s %in% names(przekszt)))$kryterium)
    dane = dane %>%
      mutate_(.dots = przekszt) %>%
      select_(.dots = usun)
  }
  
  doSkrocenia = skala %>% 
    select_('id_skrotu', 'kryterium_s') %>% 
    distinct() %>% 
    filter_(~!is.na(id_skrotu)) %>%
    collect()
  if(is.tbl(skroty) & nrow(doSkrocenia) > 0){
    sufiks = '__'
    przezwij = c()
    for(i in seq_along(doSkrocenia$id_skrotu)){
      i = as.list(doSkrocenia[i, ])
      i$kryterium_ss = paste0(i$kryterium_s, sufiks)
      tmp = skroty %>% 
        filter_(~id_skrotu == i$id_skrotu) %>%
        select_('-id_skrotu') %>%
        rename_(.dots = setNames(list('wartosc', 'nowa_wartosc'), c(i$kryterium_s, i$kryterium_ss))) %>%
        collect()
      dane = left_join(dane, tmp, copy = TRUE)
      przezwij = append(przezwij, setNames(list(i$kryterium_ss), i$kryterium_s))
    }
    dane = dane %>%
      select_(.dots = paste0('-', names(przezwij))) %>%
      rename_(.dots = przezwij)
  }
  
  return(dane)
}
