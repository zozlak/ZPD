context('normalizuj_ekwikwantylowo')

src = polacz()

test_that('normalizuj_ekwikwantylowo działa', {
  dane = data.frame(wynik = rep(1:40, 50))
  
  norm = normalizuj_ekwikwantylowo(dane)
  
  expect_equal(norm$wynik, dane$wynik)
  expect_less_than(abs(mean(norm$wynik_norm) - 100), 1)
  expect_less_than(abs(median(norm$wynik_norm) - 100), 1)
  expect_less_than(abs(sd(norm$wynik_norm) - 15), 1)
  
  skale = pobierz_skale(src) %>% 
    filter(posiada_normy == T, rodzaj_egzaminu == 'sprawdzian', rok == 2010, rodzaj_skali == 'zrównywanie') %>% 
    select(id_skali, skalowanie, grupa) %>%
    distinct() %>%
    collect()
  norm = normalizuj_ekwikwantylowo(dane, src, idSkali = skale$id_skali[1], skalowanie = skale$skalowanie[1], grupa = skale$grupa[1])
  
  expect_equal(norm$wynik, dane$wynik)
  expect_more_than(min(norm$wynik_norm), 0)
  expect_less_than(max(norm$wynik_norm), 40.001)
  expect_less_than(sd(norm$wynik_norm), 15)
})