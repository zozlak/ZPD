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

#' @title Ppobiera wartości wskaźników (EWD i PWE)
#' @param src uchwyt źródła danych dplyr-a
#' @param czyPomin czy uwzględniać wartości wskaźników oznaczone jako do pominięcia
#' @import dplyr
#' @export
pobierz_wartosci_wskaznikow = function(
  src,
  czyPomin = FALSE
){
  stopifnot(
    is.src(src),
    is.vector(czyPomin), is.logical(czyPomin), length(czyPomin) == 1, czyPomin %in% c(TRUE, FALSE)
  )
  
  query = "
    SELECT 
      id_ww, ww.rodzaj_wsk, ww.wskaznik,
      CASE okres > 1 WHEN true THEN (ww.rok_do - okres + 1) || '-' || ww.rok_do ELSE ww.rok_do::text END AS okres_wsk,
      rok_do, id_szkoly, rok, polska,
      id_wojewodztwa * 10000 + COALESCE(id_powiatu * 100, 0) + COALESCE(id_gminy, 0) AS teryt_jst,
      g.nazwa AS gmina, p.nazwa AS powiat, w.nazwa AS wojewodztwo,
      pomin, kategoria, k.wyswietlaj, k.komunikat,
      srednia, bs, q1, mediana, q3, min, max,
      ww.ewd, bs_ewd, trend_ewd, bs_trend_ewd, korelacja,
      l.lu, l.lu_ewd, l.lu_wszyscy, 
      lr.lu AS lu_r, lr.lu_ewd AS lu_ewd_r
    FROM 
      wartosci_wskaznikow_ ww
      JOIN sl_wskazniki ws USING (rodzaj_wsk, wskaznik)
      LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
      LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
      LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
      JOIN sl_kategorie k USING (kategoria)
      LEFT JOIN (SELECT * FROM liczba_zdajacych_ WHERE kategoria_lu = 'ogółem') l USING (id_ww)
      LEFT JOIN (SELECT * FROM liczba_zdajacych_ WHERE kategoria_lu = 'ogółem - poziom rozszerzony') lr USING (id_ww)
  "
  if(czyPomin == FALSE){
    query = paste(query, 'WHERE pomin = false')
  }
  data = tbl(src, sql(query))
  return(data)
}
attr(pobierz_wartosci_wskaznikow, 'grupa') = 'wartosciWskaznikow'
