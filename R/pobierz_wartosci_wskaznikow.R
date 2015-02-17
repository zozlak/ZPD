#' @title Ppobiera wartości wskaźników (EWD i PWE)
#' @param src uchwyt źródła danych dplyr-a
#' @param czyPomin czy uwzględniać wartości wskaźników oznaczone jako do pominięcia
#' @import dplyr
#' @export
pobierz_wartosci_wskaznikow = function(
  src,
  czyPomin = FALSE
){
  stopifnot(
    is.src(src),
    is.vector(czyPomin), is.logical(czyPomin), length(czyPomin) == 1, czyPomin %in% c(TRUE, FALSE)
  )
  
  query = "
    SELECT 
      id_ww, ww.rodzaj_wsk, ww.wskaznik,
      CASE okres > 1 WHEN true THEN (ww.rok_do - okres + 1) || '-' || ww.rok_do ELSE ww.rok_do::text END AS okres_wsk,
      rok_do, id_szkoly, rok, polska,
      id_wojewodztwa * 10000 + COALESCE(id_powiatu * 100, 0) + COALESCE(id_gminy, 0) AS teryt_jst,
      g.nazwa AS gmina, p.nazwa AS powiat, w.nazwa AS wojewodztwo,
      pomin, kategoria, k.wyswietlaj, k.komunikat,
      srednia, bs, q1, mediana, q3, min, max,
      ww.ewd, bs_ewd, trend_ewd, bs_trend_ewd, korelacja,
      l.lu, l.lu_ewd, l.lu_wszyscy, 
      lr.lu AS lu_r, lr.lu_ewd AS lu_ewd_r
    FROM 
      wartosci_wskaznikow ww
      JOIN sl_wskazniki ws USING (rodzaj_wsk, wskaznik)
      LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
      LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
      LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
      JOIN sl_kategorie k USING (kategoria)
      LEFT JOIN (SELECT * FROM liczba_zdajacych WHERE kategoria_lu = 'ogółem') l USING (id_ww)
      LEFT JOIN (SELECT * FROM liczba_zdajacych WHERE kategoria_lu = 'ogółem - poziom rozszerzony') lr USING (id_ww)
  "
  if(czyPomin == FALSE){
    query = paste(query, 'WHERE pomin = false')
  }
  data = tbl(src, enc2utf8(sql(query)))
  return(data)
}
attr(pobierz_wartosci_wskaznikow, 'grupa') = 'wartosciWskaznikow'
