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

#' @title Zbiera nazwy wszystkich zmiennych zwracanych przez funkcje pobierz_...()
#' @description 
#' Funkcja używana do utrzymywania aktualności dokumentacji grup danych oraz
#' weryfikacji, czy wszystkie funkcje pobierające (wyłączywszy te pobierające
#' wyniki w postaci szerokiej) odpalają się w domyślnej parametryzacji.
#' @param src uchwyt źródła danych dplyr-a
.sprawdz_dokumentacje_zmiennych = function(
  src
){
  stopifnot(is.src(src))
  
  obj = objects('package:ZPD')

  vars = c()
  grps = c()
  funcs = c()
  for(f in obj){
    fname = f
    f = eval(parse(text = f))
    g = attr(f, 'grupa')
    aa = attr(f, 'testArgs')
    if(is.function(f) & !is.null(g)){
      a = list('src' = src)
      if(!is.null(aa)){
        a = append(a, aa)
      }
      tmp = colnames(do.call(f, a))
      
      tmp = sub('^([pk])_[0-9]+$', '\\1_LICZBA', tmp)
      tmp = unique(tmp)

      vars = append(vars, tmp)
      grps = append(grps, rep(g, length(tmp)))
      funcs = append(funcs, rep(fname, length(tmp)))
    }
  }
  vars = data_frame('zmienna' = vars, 'grupa_danych' = grps, 'funkcja' = funcs)
  vars$pakiet = TRUE
  
  missing = tbl(src, sql("SELECT zmienna, grupa_danych, funkcja, true AS baza FROM sl_wiki_zmienne")) %>% 
    collect() %>%
    full_join(vars) %>%
    filter_(~is.na(baza) | is.na(pakiet))
  
  return(missing)
}
