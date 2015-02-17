#' @title Pobiera informacje o uczniach specyficzne dla testu / egzaminu
#' @param src uchwyt źródła danych dplyr-a
#' @param daneOsobowe czy pobierac z bazy takze dane osobowe (wymaga specjalnych uprawnien)
#' @import dplyr
#' @export
pobierz_dane_uczniowie_testy = function(
	src,
  daneOsobowe = FALSE
){
  stopifnot(
    is.src(src),
    is.vector(daneOsobowe), is.logical(daneOsobowe), length(daneOsobowe) == 1, daneOsobowe %in% c(TRUE, FALSE)
  )
  
	query = "
		SELECT 
			tob.*, 
			extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok
    FROM
			testy_obserwacje tob
			JOIN testy t USING (id_testu)
			LEFT JOIN arkusze a USING (arkusz)
	"
  if(daneOsobowe == TRUE){
    query = sub('testy_obserwacje tob', 'dane_osobowe.testy_obserwacje tob', query)
  }

	data = tbl(src, sql(query))
	return(data)
}
attr(pobierz_dane_uczniowie_testy, 'grupa') = 'uczniowieTesty'
