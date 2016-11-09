#' @title Pobiera informacje o uczniach
#' @description 
#' @param src uchwyt źródła danych dplyr-a
#' @param daneOsobowe czy pobierac z bazy takze dane osobowe (wymaga specjalnych uprawnien)
#' @import dplyr
#' @export
pobierz_uczniow = function(
  src,
  daneOsobowe = FALSE
){
  stopifnot(
    is.src(src),
    is.vector(daneOsobowe), is.logical(daneOsobowe), length(daneOsobowe) == 1, daneOsobowe %in% c(TRUE, FALSE)
  )

  query = "
	  SELECT o.*, oi.id AS id_cke
	  FROM 
			obserwacje o 
			LEFT JOIN (
        SELECT * FROM obserwacje_id WHERE typ_id = 'cke'
    ) oi USING (id_obserwacji)
	"
  if(daneOsobowe == TRUE){
    query = sub('obserwacje o', 'dane_osobowe.obserwacje o', query)
  }
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_uczniow, 'grupa') = 'uczniowie'
