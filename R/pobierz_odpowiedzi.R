#  Copyright 2013-2015 Mateusz Zoltak
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published by
#    the Free Software Foundation; either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
#    Niniejszy program jest wolnym oprogramowaniem; mozesz go
#    rozprowadzac dalej i/lub modyfikowac na warunkach Mniej Powszechnej
#    Licencji Publicznej GNU, wydanej przez Fundacje Wolnego
#    Oprogramowania - wedlug wersji 3 tej Licencji lub (wedlug twojego
#    wyboru) ktorejs z pozniejszych wersji.
#
#    Niniejszy program rozpowszechniany jest z nadzieja, iz bedzie on
#    uzyteczny - jednak BEZ JAKIEJKOLWIEK GWARANCJI, nawet domyslnej
#    gwarancji PRZYDATNOSCI HANDLOWEJ albo PRZYDATNOSCI DO OKRESLONYCH
#    ZASTOSOWAN. W celu uzyskania blizszych informacji siegnij do
#    Powszechnej Licencji Publicznej GNU.
#
#    Z pewnoscia wraz z niniejszym programem otrzymales tez egzemplarz
#    Powszechnej Licencji Publicznej GNU (GNU General Public License);
#    jesli nie - napisz do Free Software Foundation, Inc., 59 Temple
#    Place, Fifth Floor, Boston, MA  02110-1301  USA
#

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
  skroc   = FALSE
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
      stop('Nie ma takiej skali lub skala nie ma określonych elementów')
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
  data = tbl(src, sql(query))
  
  if(!is.null(idSkali)){
    query = sprintf(
      "SELECT id_skrotu, COALESCE(s.id_kryterium, p.id_kryterium) AS id_kryterium, COALESCE('k_' || s.id_kryterium, 'p_' || id_pseudokryterium) AS kryterium
       FROM skale_elementy s LEFT JOIN pseudokryteria_oceny_kryteria p USING (id_pseudokryterium) 
       WHERE id_skali = %d",
      idSkali
    )
    scale = tbl(src, sql(query))
    data = data %>% 
      inner_join(scale) %>% 
      group_by_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'id_skrotu') %>% 
      summarize_('ocena = sum(ocena)') %>% 
      ungroup()
    
    shorten = tbl(src, sql("SELECT id_skrotu, wartosc, nowa_wartosc FROM skroty_skal_mapowania"))
    data = data %>% 
      rename_('wartosc = ocena') %>% 
      left_join(shorten) %>% 
      mutate_('ocena = coalesce(nowa_wartosc, wartosc)')
    
    data = data %>% select_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'ocena')
  }else{
    data = data %>% select_('id_obserwacji', 'id_testu', 'id_szkoly', 'rok', 'kryterium', 'odpowiedz', 'ocena')
  }

  attr(data, 'idSkali') = idSkali
  
  return(data)  
}
attr(pobierz_odpowiedzi, 'grupa') = 'wyniki'
