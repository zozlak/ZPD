context('normalizuj')

src = polacz()

test_that('normalizuj działa', {
  dane = data.frame(wynik = rep(1:40, 50))
  
  norm = normalizuj(dane)
  
  expect_equal(norm$wynik, dane$wynik)
  expect_lt(abs(mean(norm$wynik_norm) - 100), 1)
  expect_lt(abs(median(norm$wynik_norm) - 100), 1)
  expect_lt(abs(sd(norm$wynik_norm) - 15), 1)
  
  skale = pobierz_skale(src) %>% 
    filter(posiada_normy == T, rodzaj_egzaminu == 'sprawdzian', rok == 2010, rodzaj_skali == 'zrównywanie') %>% 
    select(id_skali, skalowanie, grupa) %>%
    distinct() %>%
    collect()
  norm = normalizuj(dane, src, idSkali = skale$id_skali[1], skalowanie = skale$skalowanie[1], grupa = skale$grupa[1])
  
  expect_equal(norm$wynik, dane$wynik)
  expect_gt(min(norm$wynik_norm), 0)
  expect_lt(max(norm$wynik_norm), 40.001)
  expect_lt(sd(norm$wynik_norm), 15)
})

DBI::dbDisconnect(src$con)
