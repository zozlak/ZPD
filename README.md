# ZPD

[![Travis-CI Build Status](https://travis-ci.org/zozlak/ZPD.png?branch=master)](https://travis-ci.org/zozlak/ZPD)
[![Coverage Status](https://coveralls.io/repos/zozlak/ZPD/badge.svg)](https://coveralls.io/r/zozlak/ZPD)

Pakiet zawierający zestaw funkcji ułatwiających korzystanie z połączonych baz EWD i ZPD.

## Uwagi

1. Baza danych, do której dostęp daje pakiet podlega stałemu rozwojowi, co może powodować niekompatybilność ze starszymi wersjami pakietu. Stąd **jeśli zobaczysz nieznany błąd, na początek upewnij się, że korzystasz z najnowszej wersji pakietu**.

  Od czasu do czasu warto również zapoznać się z zawartością pliku NEWS.

2. Aby dowiedzieć się więcej o strukturze bazy danych, zapoznaj się z:
  * http://zpd.ibe.edu.pl/doku.php?id=obazie
  * http://zpd.ibe.edu.pl/doku.php?id=r_gr
  * http://zpd.ibe.edu.pl/doku.php?id=r_zpd

## Instalacja

Pakiet nie jest (i zapewne nigdy nie będzie) dostępny poprzez CRAN, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach:

### Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko gdy nie jest jeszcze zainstalowany
devtools::install_github('zozlak/ZPD')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

### "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/zozlak/ZPD.git
R CMD INSTALL ZPD
```

## Cytowanie

Jeśli korzystasz z danych udostępnianych przez pakiet ZPD, jako źródło danych zacytuj proszę:

Szaleniec, H., Kondratek, B., Kulon, F., Pokropek, A., Skórska, P., Świst, K., Wołodźko, T. i Żółtak, M. (2015). _Porównywalne wyniki egzaminacyjne._ Warszawa: Instytut Badań Edukacyjnych. 

## Sponsorzy

Pakiet został opracowany w ramach projektów systemowych "Badanie efektywności edukacji oraz instytucjonalizacja zaplecza badawczego" oraz "Rozwój metody edukacyjnej wartości dodanej na potrzeby wzmocnienia ewaluacyjnej funkcji egzaminów zewnętrznych", współfinansowanych przez Unię Europejską ze środków Europejskiego Funduszu społecznego, realizowanych przez Instytut Badań Edukacyjnych.
![KL+IBE+EFS](http://zpd.ibe.edu.pl/logo-IBE-EE.png)

WIC 2016!
