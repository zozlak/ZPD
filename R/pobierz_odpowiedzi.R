#' @title Pobiera wyniki testow (odpowiedzi) w postaci dlugiej
#' @description
#' Poniewaz skale definiuja przeksztalcenia wynikow w punktach (i nie da sie 
#' tych przeksztalcen zastosowac dla dystraktorow), zastosowanie skali powoduje
#' ze zwrocony zbior danych pozbawiony bedzie kolumny "odpowiedz"
#' 
#' Zastosowanie skali spowoduje rowniez automatyczne przyciecie kryteriow 
#' jedynie do tych, ktore naleza do wskazanej skali.
#' @param src uchwyt źródła danych dplyr-a
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_odpowiedzi = function(
  src,
  idSkali = NULL,
  skroc   = TRUE
){
  stopifnot(
    is.src(src),
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, skroc %in% c(T, F)
  )

  if(!is.null(idSkali)){
    query = sprintf("SELECT * FROM skale_elementy WHERE id_skali = %d", idSkali)
    scale = tbl(src, sql(query)) %>% collect()
    if(nrow(scale) == 0){
      stop(e('Nie ma takiej skali lub skala nie ma określonych elementów'))
    }
  }
    
  query = "
    SELECT 
      id_obserwacji, id_testu, id_szkoly, extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok, 
      id_kryterium,
      'k_' || id_kryterium AS kryterium,
      odpowiedz,
      CASE odpowiedz IS NULL
        WHEN false THEN (
          CASE odpowiedz IS NOT NULL
            WHEN true THEN (
              CASE odpowiedz = popr_dystraktor
                WHEN true THEN l_punktow
                ELSE 0
              END
            )
            ELSE null
          END
        )
        ELSE (
          CASE ocena >= 0
            WHEN true THEN ocena
            ELSE 0
          END
        )
      END AS ocena
    FROM 
      odpowiedzi 
      JOIN testy_obserwacje USING (id_obserwacji, id_testu)
      JOIN testy_kryteria USING (id_testu, id_kryterium)
      JOIN kryteria_oceny USING (id_kryterium)
      JOIN testy t USING (id_testu)
      LEFT JOIN arkusze a USING (arkusz)
  "
  data = tbl(src, sql(e(query)))
  
  if(!is.null(idSkali)){
    query = sprintf(
      "SELECT id_skrotu, COALESCE(s.id_kryterium, p.id_kryterium) AS id_kryterium, COALESCE('k_' || s.id_kryterium, 'p_' || id_pseudokryterium) AS kryterium
       FROM skale_elementy s LEFT JOIN pseudokryteria_oceny_kryteria p USING (id_pseudokryterium) 
       WHERE id_skali = %d",
      idSkali
    )
    scale = tbl(src, sql(e(query)))
    data = data %>% 
      inner_join(scale) %>% 
      group_by_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'id_skrotu') %>% 
      summarize_(.dots = list('ocena' = 'sum(ocena)')) %>% 
      ungroup()
    
    query = "SELECT id_skrotu, wartosc, nowa_wartosc FROM skroty_skal_mapowania"
    shorten = tbl(src, sql(e(query)))
    data = data %>% 
      rename_(.dots = list('wartosc' = 'ocena')) %>% 
      left_join(shorten) %>% 
      mutate_(.dots = list('ocena' = 'coalesce(nowa_wartosc, wartosc)'))
    
    data = data %>% select_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'ocena')
  }else{
    data = data %>% select_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'odpowiedz', 'ocena')
  }

  attr(data, 'idSkali') = idSkali
  
  return(data)  
}
attr(pobierz_odpowiedzi, 'grupa') = 'wyniki'
