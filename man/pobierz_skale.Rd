% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_skale.R
\name{pobierz_skale}
\alias{pobierz_skale}
\title{Pobiera informacje o skalach i skalowaniach}
\usage{
pobierz_skale(
  src,
  doPrezentacji = TRUE,
  czyKtt = FALSE,
  skalowania = TRUE,
  grupy = TRUE
)
}
\arguments{
\item{src}{uchwyt źródła danych dplyr-a}

\item{doPrezentacji}{czy wyświetlać tylko skale i skalowania oznaczone do
publicznej prezentacji}

\item{czyKtt}{czy czy wyświetlać skale i skalowania KTT}

\item{skalowania}{czy pobierać informacje o skalowaniach w ramach skal}

\item{grupy}{czy pobierać informacje o grupach w ramach skalowań (tylko gdy parametr skalowania = TRUE)}
}
\description{
Pobiera listę skal, skalowań i grup wyróżnionych w ramach skalowań połączoną
z listą testów przypisanych danej skali.

Ponieważ domyślna forma pobierania danych może być zbyt szczegółowa, za 
pomocą parametrów \code{skalowania} oraz \code{grupy} można pominąć 
pobieranie informacji skalowaniach i/lub grupach wyróżnionych w ramach
skalowań.

Domyślnie pomijane są skale i skalowania, które nie zostały oznaczone jako
do_prezentacji. W większości wypadków powoduje to, że danej skali przypisane
jest co najwyżej jedno skalowanie, co powinno ułatwiać wybór poszukiwanego
skalowania. Jeśli chcemy uzyskać listę wszystkich skal, należy parametr
\code{doPrezentacji} ustawić na NA.

Domyślnie pomijane są wszystie skale i skalowania KTT. Aby wyszukać skale i
skalowania KTT, ustaw parametr "ktt" na TRUE.
}
