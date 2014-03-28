\name{wczytaj_spss}
\alias{wczytaj_spss}
\title{Wczytuje wskazany plik SPSS stosujac standardowe przeksztalcenia}
\usage{
  wczytaj_spss(plik)
}
\arguments{
  \item{plik}{sciezka do pliku SPSS}
}
\value{
  data.frame
}
\description{
  Funkcja: - wczytuje plik SPSS z to.data.frame=T,
  use.missings=F, trim.factor.names=T - zamienia nazwy
  wszystkich zmiennych na pisane malymi literami - zamienia
  wszystkie factor-y na zwykle zmienne typu character -
  wykonuje str_trim() na wszystkich zmiennych
}
\details{
  _
}
\examples{
a=1
}

