\name{znajdz_skale}
\alias{znajdz_skale}
\title{Wyszukuje skale w bazie danych}
\usage{
  znajdz_skale(nazwa = NULL, rodzajEgzaminu = NULL,
    czescEgzaminu = NULL, rok = NULL,
    jednoznacznie = FALSE, dokladnie = FALSE,
    zrodloDanychODBC = "EWD")
}
\arguments{
  \item{nazwa}{nazwa skali (zwyczajowo w formacie
  "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub
  "ewd;gh_h;2012")}

  \item{rodzajEgzaminu}{jesli skala dotyczy jednej
  konkretnej czesci egzaminu, rodzaj tego egzaminu (patrz
  http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)}

  \item{czescEgzaminu}{jesli skala dotyczy jednej
  konkretnej czesci egzaminu, czesc tego egzaminu (patrz
  http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)}

  \item{rok}{jesli skala dotyczy jednej konkretnej czesci
  egzaminu, rok tego egzaminu (patrz
  http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)}

  \item{jednoznacznie}{czy w wypadku znalezienia wiecej niz
  jednej pasujace skali powinien byc rzucany wyjatek?}

  \item{dokladnie}{sposob porownywania nazwy skali, rodzaju
  i czesci egzaminu (TRUE - literalna zgodnosc wartosci,
  FALSE - zawieranie ciagu znakow)}

  \item{zrodloDanychODBC}{nazwa zrodla danych ODBC, ktorego
  nalezy uzyc}
}
\value{
  data.frame
}
\description{
  _
}
\details{
  _
}
\examples{
a=1
}

