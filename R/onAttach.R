.onAttach = function(libname, pkgname){
  # pobieranie opisu pakietu
  tmp = tempfile()
  url = 'https://raw.githubusercontent.com/zozlak/ZPD/master/DESCRIPTION'
  flaga = FALSE
  for(metoda in c('internal', 'curl', 'wget', 'lynx')){
    wynik = try(
      {
        suppressMessages(suppressWarnings(
          download.file(url, destfile = tmp, method = metoda, quiet = TRUE)
        ))
        TRUE
      },
      silent = TRUE
    )
    if(is.logical(wynik)){
cat(tmp, ': ', file.exists(tmp), ', ', wynik, '\n')
      flaga = TRUE
      break
    }
  }

  # sprawdzanie wersji
  if(flaga){
    opis = suppressWarnings(readLines(tmp))
    unlink(tmp)
  
    wersja = opis[grepl('Version', opis)]
    wersja = gsub('^[^0-9]+', '', wersja)

    if(wersja != packageVersion('ZPD')){
      message('\n\nPosiadasz nieaktualna wersje pakietu ZPD\nRozwaz aktualizacje:\n  devtools::install_github("zozlak/ZPD")')
    }
  }
}