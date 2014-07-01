# ZPD

Pakiet zawierający zestaw funkcji ułatwiających korzystanie z połączonych baz EWD i ZPD.

## Instalacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach:

1) Z użyciem pakietu devtools (uwaga - na skutek błędu w pakiecie devtools nie działa na 64-bitowych linuksach!):
```r
install.packages('devtools') # potrzbne tylko gdy nie jest jeszcze zainstalowany
devtools::install_github('zozlak/ZPD')
```

2) "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/zozlak/ZPD.git
R CMD INSTALL ZPD
```
