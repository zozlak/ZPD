context('normalizuj_ekwikwantylowo')

src = polacz()

test_that('normalizuj_ekwikwantylowo works', {
  dane = data.frame(wynik = rep(1:40, 50))
  
  norm = normalizuj_ekwikwantylowo(dane, zBazy = FALSE)
  
  expect_equal(norm$wynik, dane$wynik)
  expect_less_than(abs(mean(norm$wynik_norm) - 100), 1)
  expect_less_than(abs(median(norm$wynik_norm) - 100), 1)
  expect_less_than(abs(sd(norm$wynik_norm) - 15), 1)
  
  skale = pobierz_skale(src) %>% 
    filter(normy_ekwikwantylowe == T, rodzaj_egzaminu == 'sprawdzian') %>% 
    select(id_skali) %>%
    distinct() %>%
    collect()
  norm = normalizuj_ekwikwantylowo(dane, src, idSkali = skale$id_skali[1])
  
  expect_equal(norm$wynik, dane$wynik)
  expect_less_than(abs(mean(norm$wynik_norm)- 100), 15)
})