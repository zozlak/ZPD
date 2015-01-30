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

#' @title Pobiera informacje o kryteriach oceny i pytaniach oraz w których
#'   testach występują
#' @description Każde kryterium oceny występuje w danych tyle razy, w ilu
#' różnych testach występuje.
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_kryteria_oceny = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT
      'k_' || id_kryterium AS kryterium,
      id_wiazki, id_pytania, p.typ AS typ_pytania, schemat_odp,
      substring(p.opis from ';0?([^;]+)$') AS numer_pytania,
      k.opis AS numer_kryterium,
      l_punktow, sposob_oceny, schemat_pkt,
      s.tag AS standard,
      sz.tag AS standard_szcz,
      o.tag AS opis_standardu,
      tk.id_testu, tk.kolejnosc AS kolejnosc_kryt, tk.popr_dystraktor,
      string_agg(DISTINCT pt.typ, ', ' ORDER BY pt.typ) AS tresc_pytania,
      string_agg(DISTINCT wt.typ, ', ' ORDER BY wt.typ) AS tresc_wiazki
    FROM 
      pytania p 
      JOIN kryteria_oceny k USING (id_pytania)
      LEFT JOIN pytania_tresci pt USING (id_pytania)
      LEFT JOIN pytania_wiazki_tresci wt USING (id_wiazki)
      LEFT JOIN testy_kryteria tk USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny'
      ) AS s USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny - szczegółowy'
      ) AS sz USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny - opis'
      ) AS o USING (id_kryterium)
    GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
  "
  data = tbl(src, sql(query))
  return(data)
}
attr(pobierz_kryteria_oceny, 'grupa') = 'kryteriaOceny'
