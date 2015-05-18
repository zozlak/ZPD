#' @title Sprawdza, czy wyniki testow/egzaminow sa w postaci szerokiej
#' @description 
#' Postac szeroka musi zawierac kolumny z prefiksem \code{k_} lub \code{p_}
#' @param dane wynik dzialania dowolnej z funkcji pobierz_wyniki_...()
czy_postac_szeroka = function(
  dane
){
  stopifnot(
    is.tbl(dane) | is.data.frame(dane)
  )
  return(sum(grepl('^[pk]_[0-9]+$', colnames(dane))) > 0)
}