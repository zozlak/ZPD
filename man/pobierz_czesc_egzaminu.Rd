\name{pobierz_czesc_egzaminu}
\alias{pobierz_czesc_egzaminu}
\title{Pobiera ramke danych z wynikami egzaminacyjnymi wskazanej czesci egzaminu}
\usage{
  pobierz_czesc_egzaminu(rodzajEgzaminu, czescEgzaminu,
    rokEgzaminu, czyEwd, punktuj = TRUE, idSkali = NULL,
    skroc = TRUE, zrodloDanychODBC = "EWD")
}
\arguments{
  \item{rodzajEgzaminu}{rodzaj egzaminu, ktorego wyniki
  maja zostac pobrane}

  \item{czescEgzaminu}{czesc egzaminu, ktorego wyniki maja
  zostac pobrane}

  \item{rokEgzaminu}{rok egzaminu, ktorego wyniki maja
  zostac pobrane}

  \item{czyEwd}{wybor, czy maja byc pobrane wyniki
  gromadzone przez EWD, czy PAOU}

  \item{punktuj}{wybor, czy dane maja byc pobrane w postaci
  dystraktorow, czy punktow}

  \item{idSkali}{identyfikator skali, ktora ma zostac
  zastosowana do danych}

  \item{skroc}{czy do danych zastosowac skrocenia skal
  opisane w skali}

  \item{zrodloDanychODBC}{nazwa zrodla danych ODBC, ktorego
  nalezy uzyc}
}
\value{
  data frame
}
\description{
  aaa
}
\details{
  bbb
}
\examples{
a=1
}

