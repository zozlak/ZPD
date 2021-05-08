#' @title Oblicza wartosci funkcji informacyjnej (pseudo)kryterium oceny
#' @description 
#' Ramka danych \code{parametry} musi zawierać kolumny \code{kryterium,
#' parametr, model, wartosc}.
#' 
#' Zwraca ramkę danych z kolumnami \code{theta} oraz \code{informacja}.
#' @param parametry ramka danych z parametrami (pseudo)kryterium oceny (np.
#'   pobrana za pomocą pobierz_parametry())
#' @param theta wektor wartości poziomu umiejętności uczniów, dla którego
#'   obliczona zostanie funkcja informacji
#' @import dplyr
#' @export
oblicz_informacje_kryterium = function(
  parametry, 
  theta
){
  stopifnot(
    is.data.frame(parametry) | is.tbl(parametry), nrow(parametry) > 0,
    length(unique(parametry$kryterium)) == 1,
    is.vector(theta), is.numeric(theta), length(theta) > 0,
    length(unique(parametry$model)) == 1, all(parametry$model %in% c('Rasch', '1PL', '2PL', '3PL', 'GRM'))
  )
  
  a = (parametry %>% filter(.data$parametr == 'a'))$wartosc
  if(length(a) == 0){
    a = 1
  }
  b = (parametry %>% filter(grepl('^b[0-9]+$', .data$parametr)) %>% arrange(.data$parametr))$wartosc
  c = (parametry %>% filter(.data$parametr == 'c'))$wartosc
  if(length(c) == 0){
    c = 0
  }
  
  stopifnot(
    is.vector(a), is.numeric(a), length(a) == 1, all(!is.na(a)),
    is.vector(b), is.numeric(b), length(b) > 0, all(!is.na(b)),
    is.vector(c), is.numeric(c), length(c) == 1, all(!is.na(c))
  )
  
  thetaRel = matrix(theta, length(theta), length(b), byrow = FALSE) - matrix(b, length(theta), length(b), byrow = TRUE)
  colnames(thetaRel) = b
  rownames(thetaRel) = theta
  expVal = exp(a * thetaRel)
  p = c + (1 - c) * expVal / (1 + expVal)
  
  if(length(b) == 1){
    q = 1 - p
    I = as.vector(a^2 * (p - c)^2 * q / ((1 - c)^2 * p))
  }else{
    p = matrix(c(rep(1, nrow(p)), p, rep(0, nrow(p))), nrow(p), ncol(p) + 2)
    q = 1 - p
    pq = p * q
    I = (pq[, -ncol(pq)] - pq[, -1])^2 / (p[, -ncol(p)] - p[, -1])
    colnames(I) = 0:length(b)
    rownames(I) = theta
    I = rowSums(I)
  }
  
  I = data.frame(theta = theta, informacja = I, stringsAsFactors = FALSE)
  return(I)
}
