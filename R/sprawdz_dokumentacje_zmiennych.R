#' @title Zbiera nazwy wszystkich zmiennych zwracanych przez funkcje pobierz_...()
#' @description 
#' Funkcja używana do utrzymywania aktualności dokumentacji grup danych oraz
#' weryfikacji, czy wszystkie funkcje pobierające (wyłączywszy te pobierające
#' wyniki w postaci szerokiej) odpalają się w domyślnej parametryzacji.
#' @param src uchwyt źródła danych dplyr-a
.sprawdz_dokumentacje_zmiennych = function(
  src
){
  stopifnot(is.src(src))
  
  obj = objects('package:ZPD')

  vars = c()
  grps = c()
  funcs = c()
  for(f in obj){
    fname = f
    f = eval(parse(text = f))
    g = attr(f, 'grupa')
    aa = attr(f, 'testArgs')
    if(is.function(f) & !is.null(g)){
      a = list('src' = src)
      if(!is.null(aa)){
        a = append(a, aa)
      }
      tmp = colnames(do.call(f, a))
      
      tmp = sub('^([pk])_[0-9]+$', '\\1_LICZBA', tmp)
      tmp = unique(tmp)

      vars = append(vars, tmp)
      grps = append(grps, rep(g, length(tmp)))
      funcs = append(funcs, rep(fname, length(tmp)))
    }
  }
  vars = data_frame('zmienna' = vars, 'grupa_danych' = grps, 'funkcja' = funcs)
  vars$pakiet = TRUE
  
  missing = tbl(src, sql(e("SELECT zmienna, grupa_danych, funkcja, true AS baza FROM sl_wiki_zmienne"))) %>% 
    collect() %>%
    full_join(vars) %>%
    filter_(~is.na(baza) | is.na(pakiet))
  
  return(missing)
}
