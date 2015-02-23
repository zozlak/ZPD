#' @title Konwertuje łańcuch znaków na lokalne kodowanie komputera
#' @param s wektor lub macierz do konwersji
#' @export
e = function(s){
  stopifnot(
    is.character(s)
  )
  Encoding(s) = 'UTF-8'
  return(enc2native(s))
}