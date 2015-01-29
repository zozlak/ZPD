#  Copyright 2013 Mateusz Zoltak
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

#' @title Pobiera informacje o szkołach
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_szkoly = function(
  src
){
  stopifnot(is.src(src))

  query = "
	  SELECT
      s.id_szkoly, s.typ_szkoly, s.publiczna, s.dla_doroslych, s.specjalna, s.przyszpitalna,
      sd.rok, sd.id_szkoly_oke, sd.nazwa AS nazwa_szkoly, sd.adres, sd.miejscowosc, sd.pna, sd.poczta, sd.wielkosc_miejscowosci, sd.matura_miedzynarodowa, 
      sd.id_wojewodztwa * 10000 + sd.id_powiatu * 100 + sd.id_gminy AS teryt_szkoly,
      w.nazwa AS wojewodztwo, p.nazwa AS powiat, g.nazwa AS gmina, g.rodzaj_gminy
	  FROM
	    szkoly s
	    JOIN szkoly_dane sd USING (id_szkoly)
	    LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
	    LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
	    LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
	"
  data = tbl(src, sql(query))
  return(data)
}
attr(pobierz_szkoly, 'grupa') = 'szkoly'
