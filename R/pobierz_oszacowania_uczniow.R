#' @title Pobiera oszacowania umiejętności uczniów
#' @description 
#' Pobiera oszacowania umiejętności uczniów
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_oszacowania_uczniow = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT 
			so.*,
      tob.id_szkoly,
			extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok
    FROM 
      skalowania_obserwacje so
      JOIN testy_obserwacje tob USING (id_testu, id_obserwacji)
      JOIN testy t USING (id_testu)
      LEFT JOIN arkusze a USING (arkusz)
	"
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_oszacowania_uczniow, 'grupa') = 'oszacowania'
