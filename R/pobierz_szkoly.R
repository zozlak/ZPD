#' @title Pobiera informacje o szkołach
#' @description 
#' Pobiera informacje o szkołach
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_szkoly = function(
  src
){
  stopifnot(is.src(src))

  query = "
	  SELECT
      s.id_szkoly, s.typ_szkoly, s.publiczna, s.dla_doroslych, s.specjalna, s.przyszpitalna, s.artystyczna,
      sd.rok, sd.id_szkoly_oke, sd.nazwa AS nazwa_szkoly, sd.adres, sd.miejscowosc, sd.pna, sd.poczta, sd.wielkosc_miejscowosci, sd.matura_miedzynarodowa, 
      sd.id_wojewodztwa * 10000 + sd.id_powiatu * 100 + sd.id_gminy AS teryt_szkoly,
      w.nazwa AS wojewodztwo_szkoly, p.nazwa AS powiat_szkoly, g.nazwa AS gmina_szkoly, g.rodzaj_gminy,
      sd.id_rspo
	  FROM
	    szkoly s
	    JOIN szkoly_dane sd USING (id_szkoly)
	    LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
	    LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
	    LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
	"
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_szkoly, 'grupa') = 'szkoly'
