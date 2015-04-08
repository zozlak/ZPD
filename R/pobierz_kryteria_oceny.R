#' @title Pobiera informacje o kryteriach oceny i pytaniach oraz w których
#'   testach występują
#' @description Każde kryterium oceny występuje w danych tyle razy, w ilu
#' różnych testach występuje.
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_kryteria_oceny = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT
      'k_' || id_kryterium AS kryterium,
      id_wiazki, id_pytania, p.typ AS typ_pytania, schemat_odp,
      substring(p.opis from ';0?([^;]+)$') AS numer_pytania,
      k.opis AS numer_kryterium,
      l_punktow, sposob_oceny, schemat_pkt,
      s.tag AS standard,
      sz.tag AS standard_szcz,
      o.tag AS opis_standardu,
      tk.id_testu, tk.kolejnosc AS kolejnosc_kryt, tk.popr_dystraktor,
      string_agg(DISTINCT pt.typ, ', ' ORDER BY pt.typ) AS tresc_pytania,
      string_agg(DISTINCT wt.typ, ', ' ORDER BY wt.typ) AS tresc_wiazki
    FROM 
      pytania p 
      JOIN kryteria_oceny k USING (id_pytania)
      LEFT JOIN pytania_tresci pt USING (id_pytania)
      LEFT JOIN pytania_wiazki_tresci wt USING (id_wiazki)
      LEFT JOIN testy_kryteria tk USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny'
      ) AS s USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny - szczegółowy'
      ) AS sz USING (id_kryterium)
      LEFT JOIN (
        SELECT * 
        FROM kryteria_oceny_tagi JOIN sl_tagi USING (tag)
        WHERE grupa = 'standard egzaminacyjny - opis'
      ) AS o USING (id_kryterium)
    GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
    UNION
    SELECT 
      'p_' || id_pseudokryterium AS kryterium, 
      null::integer, null::integer, 'pseudokryterium', null,
      substring(p.opis from ';([^;]+);[^;]+$') AS numer_pytania,
      substring(p.opis from '[^;]+$') AS numer_kryterium,
      sum(l_punktow) AS l_punktow, null, null,
      null, null, null,
      null::integer, null::integer, null::integer,
      null, null
    FROM 
      pseudokryteria_oceny p
      JOIN pseudokryteria_oceny_kryteria USING (id_pseudokryterium)
      JOIN kryteria_oceny USING (id_kryterium)
    GROUP BY 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
  "
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_kryteria_oceny, 'grupa') = 'kryteriaOceny'
