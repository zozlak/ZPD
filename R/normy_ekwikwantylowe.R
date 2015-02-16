#	Copyright 2014 Tomasz Zoltak
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

#' @title Normalizacja ekwikwantylowa.
#' @description
#' Funkcja wylicza normalizację ekwikwantylową zadanej zmiennej.
#' @param x wektor nieujemnych liczb całkowitych, na podstawie którego ma zostać wyliczona normalizacja
#' @param max liczba, opcjonalnie maksymalna wartość skali, na jakiej wyrażony jest x
#' @param prog liczba, opcjonalnie, wszystkim wartościom, których skumulowana częstość występowania jest mniejsza niż \code{prog} lub większa niż \code{1-prog} zostaną przypisane wartości znormalizowane odpowiadające skumulowanej częstości odpowiednio \code{prog} i \code{1-prog}
#' @param sr liczba - średnia znormalizowanej skali
#' @param os liczba - odchylenie standardowe znormalizowanej skali
#' @param uzupLuki wartość logiczna - czy jeśli pomiędzy wartościami występującymi w wektorze \code{x} (oraz 0 i ew. wartością parametru \code{max}, jeśli został podany) istnieją liczby całkowite, które nie wystąpiły w \code{x}, to wartości znormalizowane mają być wyliczone również dla nich?
#' @param ekstrapolacja wartość logiczna - czy w przypadku, gdy wartości znormalizowane mają być przypisane wartościom niższym, niż najmniejsza występująca w wektorze \code{x} oraz większym, niż największa występujacaw wektorze \code{x}, to mają być one wyliczone poprzez ekstrapolację liniową?
#' @details
#' Normalizacja wyliczana jest poprzez przekształcenie postaci: \code{u(X=i) = sr + os * F( [N(X<i) + N(X=i)/2 ] / n )}, gdzie \code{n} to liczba obserwacji (liczba elementów wektora \code{x} nie będących brakami danych), a \code{F} to funkcja odrotna do dystrybuanty rozkładu normalnego stanaryzowanego.
#' Jeśli parametr \code{uzupLuki} ma wartość \code{FALSE}, to wartości, które nie wystąpiły w wektorze \code{x} nie zostaną uwzględnione w wynikach. Jeśli ma wartość \code{TRUE}, to wartości znormalizowane zostaną im przypisane na podstawie interpolacji/ekstrapolacji liniowej w oparciu o dwie najbliższe wartości (większą i mniejszą), które wystąpiły w wektorze \code{x}. Wartościom mniejszym niż najmniejsze i większym niż największe wartości, które wystąpiły w wektorze \code{x} przypisane zostaną wartości znormalizowane odpowiadające najmniejszej/największej wartości, która wystąpiła w danych (jeśli parametr \code{ekstrapolacja} przyjmuje wartość \code{FALSE}). Jeśli parametr \code{ekstrapolacja} przyjmuje wartość \code{TRUE}, wartościom tym zostaną przypisane wartości znormalizowane na podstawie ekstrapolacji liniowej w oparciu o dwie najmniejsz/największe wartości, które wystąpiły w wektorze \code{x}.
#' @return Wektor liczbowy wartości znormalizowanych, z przypisanymi nazwami opisującymi wartości wejściowej zmiennej.
#' @export
normy_ekwikwantylowe = function(
  x, 
  max           = NULL, 
  prog          = 0.000001, 
  sr            = 100, 
  os            = 15, 
  uzupLuki      = TRUE, 
  ekstrapolacja = FALSE
){
  stopifnot(
    is.numeric(x), length(x) > 1,
    is.numeric(x) | is.null(x),
    is.numeric(prog), length(prog) == 1,
    is.numeric(sr), length(sr) == 1,
    is.numeric(os), length(os) == 1,
    is.logical(uzupLuki), length(uzupLuki) == 1, uzupLuki %in% c(TRUE, FALSE),
    is.logical(ekstrapolacja), length(ekstrapolacja) == 1, ekstrapolacja %in% c(TRUE, FALSE)
  )
  if(!is.null(max)){
    stopifnot(as.integer(max) == max)
  }
  stopifnot(
    all(as.integer(x) == x | is.na(x)), all(x >=0 | is.na(x)),
    prog > 0, prog < 0.5,
    is.finite(sr), is.finite(os)
  )
  if(any(!is.finite(x) & !is.na(x))) {
    x[!is.finite(x)] = NA
    warning("W danych wystąpiły wartości Inf, -Inf lub NaN, które zostały potraktowane jako braki danych.")
  }
  if(all(is.na(x))) {
    warning("Nie podano żadnych wartości nie będących brakami danych. Normalizacja nie może zostać przeprowadzona.")
    return(NA)
  }
  # zapewniam sobie uwzględnienie w rozkładzie również tych wartości punktowych, które nie występują w danych
  if(uzupLuki) {
    if(is.null(max)) {
      x = factor(x, levels=c(0:max(x, na.rm=TRUE)))
    }else {
      x = factor(x, levels=c(0:max))
    }
  }
  rozklad = table(x)
  skum = (cumsum(rozklad) - rozklad / 2) / sum(rozklad)
  skum[rozklad == 0] = NA
  skum[skum > (1 - prog)] = 1 - prog
  skum[skum < prog] = prog
  norma = qnorm(skum, 0, 1)
  # generowanie wartości znormalizowanych dla wartości surowych, które nie występują w danych
  zera = c(1:length(norma))[is.na(norma)]
  lz = length(zera)
  if(uzupLuki & lz > 0) {
    message("W danych nie wystąpiły wartości: ", paste0(names(norma)[zera], collapse=", "), ". Wartości znormalizowane dla nich zostały sztucznie wygenerowane.")
    zeraPocz = sum(zera == (1:lz)) # stwierdzamy ile dziur jest na początku skali
    zeraKon = sum(zera == ((length(norma) - lz + 1):length(norma))) # i na końcu
    if (zeraPocz > 0) zera = zera[-(1:zeraPocz)] # i chwilowo bierzemy je w nawias
    if (zeraKon > 0) zera = zera[-((lz-zeraKon+1):lz)+zeraPocz] # i chwilowo bierzemy je w nawias
    i = 1
    lz = length(zera)
    while(i <= lz) { # żeby najpierw zabrać się za ew. dziury w środku skali
      dlLuki = sum( zera[i:lz] == (zera[i]:( zera[i] + lz - i )) )
      norma[(zera[i] - 1):(zera[i] + dlLuki)] = seq(norma[zera[i] - 1],
                                                    norma[zera[i] + dlLuki],
                                                    (norma[zera[i] + dlLuki] - norma[zera[i] - 1]) / (dlLuki + 1))
      i = i + dlLuki
    }
    if(zeraPocz > 0) {
      if(ekstrapolacja) {
        # luki na początku wypełniamy w oparciu o różnicę między dwoma pierwszymi elementami, dla których mamy już wartości wyskalowane (tak to działało do 2013 r. włącznie)
        norma[1:zeraPocz] =
          norma[zeraPocz + 1] - c(zeraPocz:1) * (norma[zeraPocz +2] - norma[zeraPocz + 1])
      }else {
        # albo przypisujemy najniższą zanotowaną wartość (domyślne rozwiązanie od 2014 r.)
        norma[1:zeraPocz] = norma[zeraPocz + 1]
      }
    }
    ln = length(norma)
    if(zeraKon > 0) {
      if(ekstrapolacja) {
        # luki na końcu analogicznie, z tym że w oparciu o różnicę pomiędzy dwoma ostatnimi elementami, dla których mamy już wartości wyskalowane (tak to działało do 2013 r. włącznie)
        norma[-c(1:(ln - zeraKon))] = norma[ln - zeraKon] + c(1:zeraKon) * (norma[ln - zeraKon] - norma[ln - zeraKon - 1])
      } else {
        # albo przypisujemy najniższą zanotowaną wartość (domyślne rozwiązanie od 2014 r.)
        norma[-c(1:(ln - zeraKon))] = norma[ln - zeraKon]
      }
    }
  }
  # raczej się nie zdarza, żeby w wyniku działań jw. otrzymać przeliczenie, które da nam zmienną o średniej dokładnie 0 i odchyleniu standardowym dokładnie 1, więc trochę to jeszcze ręcznie dostroimy:
  rozklad = rozklad / sum(rozklad)
  przesuniecie = sum(norma * rozklad)
  skala = sum(rozklad * (norma - przesuniecie)^2 )^0.5
  norma = os * (norma - przesuniecie) / skala + sr
  return(setNames(as.numeric(norma), names(norma)))
}