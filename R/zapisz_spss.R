#  Copyright 2014 Mateusz Zoltak
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

#' @title Zapisuje dane jako plik SAV korzystając z PSPP
#' @description 
#' Zapis realizowany jest następująco:
#' \itemize{
#'  \item dane eksportowane są do pliku CSV;
#' 	\item wtorzony jest skrypt SPSS-a, który wczyta dane z pliku CSV i zapisze
#'        je do pliku SAV;
#' 	\item PSPP wywoływany jest wsadowo z przygotowanym skryptem;
#' 	\item plik CSV jest usuwany.
#' }
#' W pliku SAV stosowane są tylko trzy typy zmiennych:
#' \itemize{
#'  \item string N, gdzie N jest odczytywane za pomocą stringr::stringi::stri_length()
#'  \item numeric N,M, gdzie N i M są odczytywane w przemyślny sposób
#' }
#' @param dane ramka danych do zapisania
#' @param plik nazwa docelowego pliku SAV
#' @param katalogTmp katalog, w którym zapisane zostaną tymczasowy plik CSV i skrypt SPS
#' @param sprzatnij czy usuwać pliki tymczasowe; jeśli nie zostaną zwrócone ścieżki do nich
#' @return [vector] NULL lub ścieżki do plików tymczasowych (patrz parametr "sprzatnij")
#' @examples
#' \dontrun{
#' 	zapisz_spss(mtcars, 'mtcars.sav')
#' }
#' @export
zapisz_spss = function(dane, plik, katalogTmp = NULL, sprzatnij = TRUE){
  dane = as.data.frame(dane)
  if(is.null(katalogTmp)){
  	katalogTmp = tempdir()
  }
  
  skrypt = paste0(
    "GET DATA /TYPE=TXT /FILE='%s' /ARRANGEMENT=DELIMITED",
    "/DELCASE=LINE /DELIMITERS=',' /QUALIFIER='\"'",
    "/FIRSTCASE=2 /IMPORTCASE=ALL",
    "/VARIABLES=%s\n",
    ".\n",
    "EXECUTE.\n",
    "SAVE OUTFILE='%s'.\n"
  )

  kolumny = paste(
    names(dane),
    sapply(dane, typDanych),
    collapse = ' '
  )
  
  plikiTmp = tempfile(fileext = c('.csv', '.sps'), tmpdir = katalogTmp)
  write.csv(dane[, ], plikiTmp[1], row.names = F, na = '')
  writeLines(sprintf(skrypt, plikiTmp[1], kolumny, plik), plikiTmp[2])
  system(sprintf("pspp -b '%s'", plikiTmp[2]))
  
  if(sprzatnij){
    unlink(plikiTmp)
    return(invisible(NULL))
  }else{
    return(plikiTmp)
  }
}

typDanych = function(kolumna){
  kolumna = unique(na.exclude(kolumna))
  if(length(kolumna) == 0){
    return('A1')
  }
  
  if(is.factor(kolumna)){
    kolumna = levels(kolumna)[kolumna]
  }
  
  if(is.character(kolumna)){
    N = max(stringi::stri_length(kolumna))
    if(is.infinite(N)){
      N = 1
    }
    return(paste0('A', max(N, 1)))
  }
  calk = floor(kolumna)
  if(all(calk == kolumna)){
    M = 0
  }else{
    tmp = sprintf("%.15f", kolumna)
    tmp = sub('0+$', '', tmp)
    tmp = sub('.*[.]', '', tmp)
    M = max(stringi::stri_length(tmp))
  }
  wMax = max(abs(calk))
  if(is.infinite(wMax)){
    N = 1
  }else{
    tmp = c(0, unlist(lapply(10, '^', 1:15)))
    N = max((1:21)[wMax >= tmp])
    N = M + N
  }
  if(N > 13){
    return(paste0('A', N))
  }
  return(paste0('F', N, '.', M))
}