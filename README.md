# ZPD

[![Travis-CI Build Status](https://travis-ci.org/zozlak/ZPD.png?branch=master)](https://travis-ci.org/zozlak/ZPD)

Pakiet zawierający zestaw funkcji ułatwiających korzystanie z połączonych baz EWD i ZPD.

## Uwagi

1. Baza danych, do której dostęp daje pakiet podlega stałemu rozwojowi, co może powodować niekompatybilność ze starszymi wersjami pakietu. Stąd **jeśli zobaczysz nieznany błąd, na początek upewnij się, że korzystasz z najnowszej wersji pakietu**. Od czasu do czasu warto również zapoznać się z zawartością pliku NEWS.

2. Aby dowiedzieć się więcej o strukturze bazy danych, zapoznaj się z:
  * http://zpd.ibe.edu.pl/doku.php?id=obazie
  * http://zpd.ibe.edu.pl/doku.php?id=r_gr
  * http://zpd.ibe.edu.pl/doku.php?id=r_zpd

## Instalacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach:

1. Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko gdy nie jest jeszcze zainstalowany
devtools::install_github('zozlak/ZPD')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

2. "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/zozlak/ZPD.git
R CMD INSTALL ZPD
```
