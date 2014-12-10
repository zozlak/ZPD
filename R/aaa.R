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
#

#' @title Pobiera oszacowania umiejętności uczniów
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @import DBI
#' @export
get_pupils_estimates = function(
	src
){
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
	data = tbl(src, sql(query))
	return(data)
}
	
#' @title Pobiera wyniki części egzaminu
#' @param src uchwyt źródła danych dplyr-a
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param czescEgzaminu czesc egzaminu, ktorego wyniki maja zostac pobrane
#' @param rokEgzaminu rok egzaminu, ktorego wyniki maja zostac pobrane
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy PAOU
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @import DBI
#' @export
get_exam_results = function(
	src,
	rodzajEgzaminu, 
	czescEgzaminu, 
	rokEgzaminu, 
	czyEwd,
	punktuj        = TRUE,
	idSkali        = NULL,
	skroc          = FALSE
){
	query = sprintf(
		"SELECT zbuduj_widok_czesci_egzaminu('tmp_view', '%s', '%s', %d, %s, %s, %s, %s, true);",
		sub("'", "''", rodzajEgzaminu),
		sub("'", "''", czescEgzaminu),
		rokEgzaminu,
		ifelse(czyEwd, 'true', 'false'),
		ifelse(punktuj, 'true', 'false'),
		ifelse(is.null(idSkali), 'null', idSkali),
		ifelse(skroc, 'true', 'false')
	)
	# R Postgresql DBI driver is extremely stupid and switches every message from
	# a database into R error.
	# This means "DROP VIEW IF EXISTS view_name;" executed on the backstage of the
	# zbuduj_widok_czesci_egzaminu() call will generate an R error if a view named
	# "view_name" doesn't exist.
	# So we need to make sure it exists before calling zbuduj_widok_czesci_egzaminu()
	dbGetQuery(src$con, "CREATE TEMPORARY VIEW tmp_view AS SELECT 1")
  dbGetQuery(src$con, query)
	data = tbl(src, sql("SELECT * FROM tmp_view"))
	return(data)	
}

#' @title Pobiera wyniki testu
#' @param src uchwyt źródła danych dplyr-a
#' @param idTestu identyfikator testu do pobrania
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
get_test_results = function(
	src,
	idTestu, 
	punktuj        = TRUE,
	idSkali        = NULL,
	skroc          = FALSE
){
	query = sprintf(
		"SELECT zbuduj_widok_testu('tmp_view', %d, %s, %s, %s, true);",
		idTestu,
		ifelse(punktuj, 'true', 'false'),
		ifelse(is.null(idSkali), 'null', idSkali),
		ifelse(skroc, 'true', 'false')
	)
	# R Postgresql DBI driver is extremely stupid and switches every message from
	# a database into R error.
	# This means "DROP VIEW IF EXISTS view_name;" executed on the backstage of the
	# zbuduj_widok_czesci_egzaminu() call will generate an R error if a view named
	# "view_name" doesn't exist.
	# So we need to make sure it exists before calling zbuduj_widok_czesci_egzaminu()
	dbGetQuery(src$con, "CREATE TEMPORARY VIEW tmp_view AS SELECT 1")
	dbGetQuery(src$con, query)
	data = tbl(src, sql("SELECT * FROM tmp_view"))
	return(data)	
}	

#' @title Pobiera informacje o uczniach
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
get_pupils = function(
	src
){
	query = "
	  SELECT o.*, oi.id AS id_cke
	  FROM 
			obserwacje o 
			LEFT JOIN obserwacje_id oi USING (id_obserwacji)
	  WHERE oi.typ_id = 'cke' OR oi.typ_id IS NULL
	"
	data = tbl(src, sql(query))
	return(data)
}

#' @title Pobiera informacje o szkołach
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
get_schools = function(
  src
){
	query = "
	  SELECT
      s.id_szkoly, s.typ_szkoly, s.publiczna, s.dla_doroslych, s.specjalna, s.przyszpitalna,
      sd.rok, sd.id_szkoly_oke, sd.nazwa, sd.adres, sd.miejscowosc, sd.pna, sd.poczta, sd.wielkosc_miejscowosci, sd.matura_miedzynarodowa, 
      sd.id_wojewodztwa * 10000 + sd.id_powiatu * 100 + sd.id_gminy AS teryt,
      sd.id_wojewodztwa, sd.id_powiatu, sd.id_gminy,
      w.nazwa AS wojewodztwo, p.nazwa AS powiat, g.nazwa AS gmina, g.rodzaj_gminy
	  FROM
	    szkoly s
	    JOIN szkoly_dane sd USING (id_szkoly)
	    LEFT JOIN teryt_gminy g USING (rok, id_wojewodztwa, id_powiatu, id_gminy)
	    LEFT JOIN teryt_powiaty p USING (rok, id_wojewodztwa, id_powiatu)
	    LEFT JOIN teryt_wojewodztwa w USING (rok, id_wojewodztwa)
	"
	data = tbl(src, sql(query))
	return(data)
}

#' @title Pobiera informacje o testach / egzaminach
#' #' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
get_tests = function(
	src
){
	query = "
    SELECT 
    	id_testu, ewd, arkusz, rodzaj_egzaminu, czesc_egzaminu, 
      extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok,
      COALESCE(a.data_egzaminu, t.data) AS data, 
      COALESCE(t.opis, trim(rodzaj_egzaminu || ' ' || czesc_egzaminu) || ' ' || extract(year FROM data_egzaminu)) AS opis
    FROM 
    	testy t
			LEFT JOIN arkusze a USING (arkusz)
	"
	data = tbl(src, sql(query))
	return(data)
}

#' @title Pobiera informacje o uczniach specyficzne dla testu / egzaminu
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
get_pupils_tests_data = function(
	src
){
	query = "
		SELECT 
			tob.*, 
			extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok
    FROM
			testy_obserwacje tob
			JOIN testy t USING (id_testu)
			LEFT JOIN arkusze a USING (arkusz)
	"
	data = tbl(src, sql(query))
	return(data)
}
