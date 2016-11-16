.onAttach = function(libname, pkgname){
  packageStartupMessage(paste0(
    '\nJeśli korzystasz z danych udostępnianych przez pakiet ZPD, zacytuj proszę:\n\n',
    'Szaleniec, H., Kondratek, B., Kulon, F., Pokropek, A., Skórska, P., Świst, K., Wołodźko, T. i Żółtak, M. (2015).\n',
    '"Porównywalne wyniki egzaminacyjne." Warszawa: Instytut Badań Edukacyjnych.'
  ))
  return(invisible(NULL))
}