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
#' @importFrom rlang :=
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
  if (nrow(skala) == 0) {
    stop(e('Nie ma takiej skali lub skala nie ma określonych elementów'))
  }
  
  skroty = NULL
  if (skroc == TRUE) {
    query = "SELECT id_skrotu, wartosc, nowa_wartosc FROM skroty_skal_mapowania"
    skroty = tbl(src, sql(e(query)))
  }
  
  if (czy_postac_dluga(dane)) {
    dane = zastosuj_skale_dluga(dane, skala, skroty)
  } else if(czy_postac_szeroka(dane)) {
    dane = zastosuj_skale_szeroka(dane, skala, skroty)
  }
  attr(dane, 'idSkali') = idSkali
  return(dane)
}

zastosuj_skale_dluga = function(
  dane,
  skala,
  skroty
){
  dane = suppressMessages(
    dane %>% 
    inner_join(skala, copy = TRUE) %>%
    select(-.data$kryterium) %>%
    rename(kryterium = .data$kryterium_s)
  )
  kol = setdiff(colnames(dane), c('odpowiedz', 'ocena'))
  
  dane = dane %>% 
    group_by(across({{kol}})) %>% 
    summarize(ocena = sum(.data$ocena)) %>% 
    ungroup()
  
  if (is.tbl(skroty)) {
    przeksztalcenie = str2lang(ifelse(
      any(class(dane) %in% 'tbl_sql'),
      'coalesce(nowa_wartosc, wartosc)',
      'ifelse(!is.na(nowa_wartosc), nowa_wartosc, wartosc)'
    ))
    dane = suppressMessages(
      dane %>% 
      rename(wartosc = .data$ocena) %>% 
      left_join(skroty, copy = TRUE) %>% 
      mutate(ocena = !!przeksztalcenie) %>%
      select(-.data$wartosc, -.data$nowa_wartosc)
    )
  }
  
  return(dane %>% select(-.data$id_skrotu))
}

zastosuj_skale_szeroka = function(
  dane,
  skala,
  skroty
){
  zbuduj_przeksztalcenie = function(k, d){
    if (nrow(k) == 1) {
      return(tibble(mutate = NA_character_))
    }
    return(tibble(
      mutate = paste(k$kryterium, collapse = ' + ')
    ))
  }
  
  kolUsun = grep('^[pk]_[0-9]+$', setdiff(colnames(dane), skala$kryterium), value = TRUE)
  if (length(kolUsun) > 0) {
    dane = dane %>%
      select(-{{ kolUsun }})
  }
  skala = skala %>%
    filter(.data$kryterium %in% colnames(dane))
  
  przekszt = skala %>% 
    group_by(.data$kryterium_s) %>% 
    do(zbuduj_przeksztalcenie(.data, dane)) %>%
    filter(!is.na(.data$mutate)) %>%
    collect()
  if (nrow(przekszt) > 0) {
    for (i in seq_along(przekszt$mutate)) {
      kolP = przekszt$kryterium_s[i]
      rownanie = str2lang(przekszt$mutate[i])
      dane = dane %>%
        mutate({{kolP}} := !!rownanie)
    }
    usun = (skala %>% filter(.data$kryterium_s %in% przekszt$kryterium_s))$kryterium
    dane = dane %>%
      select(-{{usun}})
  }
  
  doSkrocenia = skala %>% 
    select(.data$id_skrotu, .data$kryterium_s) %>% 
    distinct() %>% 
    filter(!is.na(.data$id_skrotu)) %>%
    collect()
  if (is.tbl(skroty) & nrow(doSkrocenia) > 0) {
    sufiks = '__'
    for (i in seq_along(doSkrocenia$id_skrotu)) {
      i = as.list(doSkrocenia[i, ])
      kryterium_s = i$kryterium_s
      kryterium_ss = paste0(kryterium_s, sufiks)
      tmp = skroty %>% 
        filter(.data$id_skrotu == local(i$id_skrotu)) %>%
        select(-.data$id_skrotu) %>%
        rename({{kryterium_s}} := .data$wartosc, {{kryterium_ss}} := .data$nowa_wartosc) %>%
        collect()
      dane = suppressMessages(left_join(dane, tmp, copy = TRUE))
      dane = dane %>%
        select(-{{kryterium_s}}) %>%
        rename({{kryterium_s}} := {{kryterium_ss}})
    }
  }
  
  return(dane)
}
