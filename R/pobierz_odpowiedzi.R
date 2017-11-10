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

  query = "
    SELECT 
      id_obserwacji, id_testu, id_szkoly, extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok, 
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
      odpowiedzi o 
      JOIN testy t USING (id_testu)
      LEFT JOIN arkusze a USING (arkusz)
      JOIN testy_obserwacje USING (id_obserwacji, id_testu)
      JOIN testy_kryteria USING (id_testu, id_kryterium)
      JOIN kryteria_oceny USING (id_kryterium)
  "
  data = tbl(src, sql(e(query)))
  
  if(!is.null(idSkali)){
    data = zastosuj_skale(data, src, idSkali, skroc)
  }else{
    data = data %>% select_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'odpowiedz', 'ocena')
  }

  attr(data, 'idSkali') = idSkali
  
  return(data)  
}
attr(pobierz_odpowiedzi, 'grupa') = 'wyniki'
