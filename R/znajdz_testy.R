#	Copyright 2013 Mateusz Zoltak
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published by
#    the Free Software Foundation; either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
#    Niniejszy program jest wolnym oprogramowaniem; mozesz go
#    rozprowadzac dalej i/lub modyfikowac na warunkach Mniej Powszechnej
#    Licencji Publicznej GNU, wydanej przez Fundacje Wolnego
#    Oprogramowania - wedlug wersji 3 tej Licencji lub (wedlug twojego
#    wyboru) ktorejs z pozniejszych wersji.
#
#    Niniejszy program rozpowszechniany jest z nadzieja, iz bedzie on
#    uzyteczny - jednak BEZ JAKIEJKOLWIEK GWARANCJI, nawet domyslnej
#    gwarancji PRZYDATNOSCI HANDLOWEJ albo PRZYDATNOSCI DO OKRESLONYCH
#    ZASTOSOWAN. W celu uzyskania blizszych informacji siegnij do
#    Powszechnej Licencji Publicznej GNU.
#
#    Z pewnoscia wraz z niniejszym programem otrzymales tez egzemplarz
#    Powszechnej Licencji Publicznej GNU (GNU General Public License);
#    jesli nie - napisz do Free Software Foundation, Inc., 59 Temple
#    Place, Fifth Floor, Boston, MA  02110-1301  USA

#' @title Wyszukuje w bazie testy spełniające podane kryteria
#' @description
#' Wyszukuje testy na podstawie dowolnej kombinacji cech przekazanych jako argumenty.
#' 
#' Każdy argument może być wyrażeniem regularnym.
#' @param opis opis testu (w wypadku testów egzaminacyjnych złączany z rodzaju, części egzaminu i roku)
#' @param rodzajEgzaminu rodzaj egzaminu
#' @param czescEgzaminu część egzaminu
#' @param rok rok testu lub egzaminu
#' @param data data testu lub egzaminu
#' @param arkusz arkusz egzaminacyjny
#' @param prefiks prefiks części egzaminu
#' @param ewd czy test EWD
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [data.frame] ramka danych z wyszukanymi testami
#' @export
znajdz_testy = function(
	opis=NULL, 
	rodzajEgzaminu=NULL, 
	czescEgzaminu=NULL, 
	rok=NULL,
	arkusz=NULL,
	prefiks=NULL,
	data=NULL, 
	ewd=NULL,
	zrodloDanychODBC='EWD'
){
	zapytanie = "
		SELECT *
		FROM
			(
				SELECT 
					id_testu, ewd,
					COALESCE(data, data_egzaminu) AS data, 
					COALESCE(extract(year FROM data), extract(year FROM data_egzaminu)) AS rok, 
					COALESCE(opis, rodzaj_egzaminu||';'||czesc_egzaminu||';'||extract(year FROM data_egzaminu)) AS opis,
					rodzaj_egzaminu, czesc_egzaminu, arkusz, prefiks
				 FROM 
					testy 
					LEFT OUTER JOIN arkusze USING (arkusz)
						LEFT OUTER JOIN sl_czesci_egzaminow USING (rodzaj_egzaminu, czesc_egzaminu)
			) AS t
	"
		
	param = list(
		'opis'=opis, 'rodzaj_egzaminu'=rodzajEgzaminu, 'czesc_egzaminu'=czescEgzaminu,
		'rok'=rok, 'arkusz'=arkusz, 'prefiks'=prefiks, 'data'=data, 'ewd'=ewd
	)
	warunek = ""
	parametry = list()
	for(kol in names(param)){
		wartosc = param[[kol]]
		if(is.null(wartosc) | !is.vector(wartosc)){
			next
		}
		if(is.logical(wartosc)){
			wartosc = tolower(as.character(wartosc)) # na wypadek np. podania wartości kolumny "ewd" jako typu logicznego
		}
		warunek = paste0(warunek, " AND ", kol, "::text ~ ?")
		parametry = append(parametry, wartosc[1])
	}
	
	if(warunek != ""){
		zapytanie = paste0(
			zapytanie,
			" WHERE ", substring(warunek, 5)
		)
	}
		
	P = odbcConnect(zrodloDanychODBC)
	tryCatch({
		tmp = .sqlQuery(P, zapytanie, parametry)
		odbcClose(P)
		return(tmp)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}