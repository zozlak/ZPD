\name{wydziel_testy}
\alias{wydziel_testy}
\title{Dzieli plik danych z badan zrownujacych na testy}
\usage{
  wydziel_testy(dane, zeszyt, zeszytBaza, kolIdZeszytu,
    kolUdzial, kolIdSzkOKE, prefiksZadan, rok,
    zrodloDanychODBC = "EWD")
}
\arguments{
  \item{dane}{ramka danych}

  \item{zeszyt}{string z nazwa zeszytu (taka, jak w
  kolumnie wskazywanej przez "kolIdZeszytu")}

  \item{zeszytBaza}{string z nazwa zeszytu w bazie danych
  ("zrownywanie;RODZAJ_EGZ;ROK;KOD")}

  \item{kolIdZeszytu}{numer kolumny w "dane" przechowujacej
  nazwy zeszytow}

  \item{kolUdzial}{numer kolumny w "dane" przechowujacej
  informacje o udziale ucznia w badaniu}

  \item{kolIdSzkOKE}{numer kolumny w "dane" przechowujace
  id OKE szkoly}

  \item{prefiksZadan}{prefiks kolumn zawierajacych wyniki
  zadan dla danego zeszytu}

  \item{rok}{rok testu zrownujacego (niezbedny do
  skonstruowania identyfikatora 'zrwn')}

  \item{zrodloDanychODBC}{nazwa zrodla danych ODBC}
}
\value{
  void
}
\description{
  Funkcja: - filtruje rekordy pasujace do zadanego zeszytu
  - zmienia nazwy kolumn z wynikami zadan na id_kryteriow -
  dodaje kolumne "zrwn" zawierajace kolumne "id_ucz"
  przeksztalcona do postaci uzywanego w bazie
  identyfikatora "zrwn" - dubluje kolumne "id_szk" pod
  nazwa "id_szk_oke" - zapisuje tak odfiltrowany i
  skonwertowany zbior pod nazwa "id_testu.csv"
}
\details{
  _
}
\examples{
a=1
}

