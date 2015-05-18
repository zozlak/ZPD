#' @title Sprawdza, czy wyniki testow/egzaminow sa w postaci dlugiej
#' @description 
#' Postac dluga musi zawierac kolumny \code{kryterium} oraz \code{ocena}
#' @param dane wynik dzialania dowolnej z funkcji pobierz_wyniki_...()
czy_postac_dluga = function(
  dane
){
  stopifnot(
    is.tbl(dane) | is.data.frame(dane)
  )
  return(sum(colnames(dane) %in% 'kryterium') == 1 & sum(colnames(dane) %in% 'ocena') == 1)
}