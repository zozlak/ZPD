% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_kryteria_oceny.R
\name{pobierz_kryteria_oceny}
\alias{pobierz_kryteria_oceny}
\title{Pobiera informacje o pytaniach, (pseudo)kryteriach ocenypytaniach oraz
  tym, w których testach i skalach występują}
\usage{
pobierz_kryteria_oceny(src, testy = TRUE, skale = TRUE, krytSkladowe = TRUE)
}
\arguments{
\item{src}{uchwyt źródła danych dplyr-a}

\item{testy}{czy pobierać informacje o występowaniu kryteriów w testach
(zmienne id_testu, kolejnosc_w_tescie, popr_dystraktor)}

\item{skale}{czy pobierać informacje o występowaniu kryteriów w skalach
(zmienne id_skali, kolejnosc_w_skali)}

\item{krytSkladowe}{czy dla pseudokryteriów oceny pobierać informacje o
kryteriach, z których się składają}
}
\description{
Pobierane są informacje o wszystkich pytaniach, (pseudo)kryteriach oceny, ich
wystąpieniach w testach oraz skalach. Ponieważ często tak szczegółowe dane
nie są potrzebne (a ich szczegółowość wydłuża pobieranie danych oraz naraża
na błędy związane z wielokrotnym występowaniem tego samego
pytania/(pseudo)kryterium oceny w danych), za pomocą parametrów \code{testy}
oraz \code{skale} można pominąć pobieranie informacji o występowaniu
(pseudo)kryteriów oceny w testach i/lub skalach.

Warto zwrócić uwagę, że pobierane dane zawierają także "nietypowe" rekordy,
jak np. pseudokryteria oceny czy pseudopytania (te ostatnie odpowiadają
całemu zestawowi pytań, dla którego w zbiorach danych z wynikami egzaminu
gromadzona była jedynie suma punktów, bez podziału na poszczególne kryteria
oceny), które na ogół trzeba będzie usunąć, np. filtrując po zmiennej
\code{typ_pytania}.

Parametry \code{testy} i \code{skale} wpływają z jednej strony (w mniejszym
stopniu) na szybkość pobierania danych (pobieranie każdej z tych informacji
wydłuża czas ok. dwukrotnie), z drugiej zaś strony na liczbę zwracanych
wierszy (rzędu 10^4 bez informacji o testach i skalach, ok. dwukrotnie więcej
z informacją o testach lub skalach, rzędu 10^5 z informacją o testach i o
skalach).
}
