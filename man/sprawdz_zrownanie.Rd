\name{sprawdz_zrownanie}
\alias{sprawdz_zrownanie}
\title{Weryfikuje, czy dane zrownania zostaly poprawnie zaimportowane do bazy}
\usage{
  sprawdz_zrownanie(rodzajEgzaminu, rok, kolumnaId,
    zrodloDanychODBC = "EWD")
}
\arguments{
  \item{rodzajEgzaminu}{rodzaj egzaminu}

  \item{rok}{rok badania zrownujacego}

  \item{zrodloDanychODBC}{nazwa zrodla danych ODBC}

  \item{kolumnaId}{nazwa kolumny przechowujacej "zewnetrzne
  id"}
}
\value{
  logical
}
\description{
  Funkcja: - pobiera dane wskazanego zrownania (rodzaj
  egzaminu i rok) z bazy - dla kazdego testu zrownujacego
  sprawdza jego dane w bazie z plikiem "id_testu.csv" pliki
  te powinny sie znajdowac w katalogu roboczym
}
\details{
  _
}
\examples{
a=1
}

