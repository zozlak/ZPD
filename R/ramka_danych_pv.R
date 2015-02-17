#' @title Upewnia, że ramka danych z wartościami PV jest w postaci długiej.
#' @description
#' _
#' @param dane ramka danych z wartosciami PV
#' @param kolPV wyrażenie regularne dopasowujące kolumny z wartościami PV
#' @param kolNrPV nazwa kolumny z numerem PV (gdy dane w postaci długiej)
#' @param kolPozostale nazwy wszystkich innych kolumn, które mają zostać zachowane
#' @return [data.frame] dane w postaci długiej (z kolumnami "nr_pv" i "wynik")
ramka_danych_pv = function(
  dane, 
  kolPV        = '^[pP][vV]_[0-9]+|wynik$', 
  kolNrPV      = 'nr_pv', 
  kolPozostale = NULL
){
  stopifnot(
    is.data.frame(dane) | is.tbl(dane),
    is.vector(kolPV), is.character(kolPV), length(kolPV) == 1, !is.na(kolPV),
    is.vector(kolNrPV), is.character(kolNrPV), length(kolNrPV) == 1, !is.na(kolNrPV),
    is.null(kolPozostale) | is.vector(kolPozostale) & is.character(kolPozostale)
  )
  dane = as.data.frame(dane)
	
	kolPV = grep(kolPV, names(dane), value = T)
	if(length(kolPV) < 1){
		stop('w ramce danych nie znaleziono zmiennych z wartościami PV')
	}
	
	kolNrPV = grep(kolNrPV, names(dane), value = T)
	if(length(kolNrPV) > 1){
		stop('niejednoznaczna nazwa kolumny z numerem PV')
	}	else if(length(kolNrPV) == 0){
		# ramka danych w postaci szerokiej - skonwertuj do długiej
		dane = reshape2::melt(dane[, na.omit(c(kolPozostale, kolPV))], id.vars = kolPozostale)	
		names(dane) = sub('^variable$', 'nr_pv', names(dane))
		names(dane) = sub('^value$', 'wynik', names(dane))
	} else if(length(kolPV) != 1){
		stop('dane w postaci długiej, ale dopasowano wiele zmiennych z wartościami PV')
	} else{
		# ramka danych w postaci długiej
		dane = dane[, c(kolPozostale, kolNrPV, kolPV)]
		names(dane) = sub(paste0('^', kolNrPV, '$'), 'nr_pv', names(dane))
		names(dane) = sub(paste0('^', kolPV, '$'), 'wynik', names(dane))
	}
	
	return(dane)
}
