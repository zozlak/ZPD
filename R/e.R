#' @title Konwertuje łańcuch znaków na lokalne kodowanie komputera
#' @param s wektor lub macierz do konwersji
#' @export
e = function(s){
  stopifnot(
    is.character(s)
  )
  if(nchar('ą') == 2){
    Encoding(s) = 'UTF-8'
    s = enc2native(s)
  }
  return(s)
}
