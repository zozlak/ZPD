#' @title Oblicza wariancję wyników w populacji na podstawie PV
#' @description
#' _
#' @param dane macierz z wartosciami PV
#' @param kolPV wyrażenie regularne dopasowujące kolumny z wartościami PV
#' @param kolNrPV nazwa kolumny z numerem PV (gdy dane w postaci długiej)
#' @return [numeric] obliczona wariancja
#' @export
oblicz_wariancje_populacji = function(
  dane, 
  kolPV   = '^[pP][vV]_[0-9]+|wynik$', 
  kolNrPV = 'nr_pv'
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane),
    is.vector(kolPV), is.character(kolPV), length(kolPV) == 1, !is.na(kolPV),
    is.vector(kolNrPV), is.character(kolNrPV), length(kolNrPV) == 1, !is.na(kolNrPV)
  )
  
	dane = ramka_danych_pv(dane, kolPV, kolNrPV)
	
	warPop = dane %>%
	  group_by(.data$nr_pv) %>%
	  summarize(war = stats::var(.data$wynik)) %>%
	  ungroup() %>%
	  summarize(warPop = mean(.data$war))

	return(as.numeric(warPop[1, 1]))
}
