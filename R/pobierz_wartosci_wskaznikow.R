#' @title Ppobiera wartości wskaźników (EWD i PWE)
#' @description 
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
      rok_do, id_szkoly, rok,
      CASE polska
        WHEN true THEN 'Polska'
        ELSE
          CASE id_gminy IS NOT NULL
            WHEN true THEN 'gmina'
            ELSE
              CASE id_powiatu IS NOT NULL
                WHEN true THEN 'powiat'
                ELSE
                  CASE id_wojewodztwa IS NOT NULL
                    WHEN true THEN 'województwo'
                    ELSE 'szkoła'
                  END
              END
          END
      END AS poziom_agregacji,
      id_wojewodztwa * 10000 + COALESCE(id_powiatu * 100, 0) + COALESCE(id_gminy, 0) AS teryt_jst,
      g.nazwa AS gmina_jst, p.nazwa AS powiat_jst, w.nazwa AS wojewodztwo_jst,
      pomin, kategoria, k.wyswietlaj, k.komunikat,
      srednia, bs, q1, mediana, q3, min, max,
      ww.ewd, bs_ewd, trend_ewd, bs_trend_ewd, korelacja,
      l.lu, l.lu_ewd, l.lu_wszyscy
    FROM 
      wartosci_wskaznikow ww
      JOIN sl_wskazniki ws USING (rodzaj_wsk, wskaznik)
      LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
      LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
      LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
      JOIN sl_kategorie k USING (kategoria)
      LEFT JOIN (SELECT * FROM liczba_zdajacych WHERE kategoria_lu = 'ogółem') l USING (id_ww)
  "
  if(czyPomin == FALSE){
    query = paste(query, 'WHERE pomin = false')
  }
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_wartosci_wskaznikow, 'grupa') = 'wartosciWskaznikow'
