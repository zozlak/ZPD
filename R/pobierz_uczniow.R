#' @title Pobiera informacje o uczniach
#' @description 
#' Pobiera informacje o uczniach
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
	  SELECT o.id_obserwacji, o.plec::text, o.rocznik, oi.id AS id_cke
	  FROM 
			obserwacje o 
			LEFT JOIN (
        SELECT * FROM obserwacje_id WHERE typ_id = 'cke'
      ) oi USING (id_obserwacji)
	"
  if (daneOsobowe == TRUE) {
    query = paste0(query, 'JOIN dane_osobowe.obserwacje doo USING (id_obserwacji)')
    query = sub('o.rocznik', 'o.rocznik, doo.data_ur', query)
  }
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_uczniow, 'grupa') = 'uczniowie'
