\name{pobierz_arkusz}
\alias{pobierz_arkusz}
\title{Pobiera ramke danych z wynikami egzaminacyjnymi wskazanego arkusza egzaminacyjnego}
\usage{
  pobierz_arkusz(idArkusza, czyEwd, punktuj = TRUE,
    idSkali = NULL, skroc = TRUE, zrodloDanychODBC = "EWD")
}
\arguments{
  \item{idArkusza}{kod arkusza, ktorego wyniki maja zostac
  pobrane}

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

