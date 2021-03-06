% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_schematy_odp.R
\name{pobierz_schematy_odp}
\alias{pobierz_schematy_odp}
\title{Pobiera dystraktory kryteriów oceny}
\usage{
pobierz_schematy_odp(src)
}
\arguments{
\item{src}{uchwyt źródła danych dplyr-a}
}
\description{
W wypadku pobierania z bazy wyników w postaci niewypunktowanej wybrana przez 
ucznia odpowiedź zakodowana jest liczbowo. Dane pobierane funkcją 
pobierz_schematy_odp() pozwalają przekodować je na faktyczne oznaczenia użyte
w teście.

Innym zastosowaniem może być sprawdzanie, czy zbiór danych z wynikami testu 
nie zawiera wartości spoza możliwych do wyboru dla danego zadania odpowiedzi.
}
