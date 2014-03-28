\name{pobierz_zrownywanie}
\alias{pobierz_zrownywanie}
\title{Pobiera ramke danych z wynikami egzaminacyjnymi testow zrownujacych}
\usage{
  pobierz_zrownywanie(rodzajEgzaminu, rok, punktuj = TRUE,
    idSkali = NULL, skroc = TRUE, zrodloDanychODBC = "EWD")
}
\arguments{
  \item{rodzajEgzaminu}{rodzaj egzaminu, ktorego wyniki
  maja zostac pobrane}

  \item{punktuj}{wybor, czy dane maja byc pobrane w postaci
  dystraktorow, czy punktow}

  \item{rok}{rok, z ktorego dane maja zostac pobrane}

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

