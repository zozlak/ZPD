context('sprawdz_dokumentacje')

src = polacz()

test_that('sprawdz_dokumentacje nie zwraca danych', {
  expect_equal(nrow(.sprawdz_dokumentacje_zmiennych(src)), 0)
})

test_that('oszacuj_czas_wykonania działa', {
  wynik = oszacuj_czas_wykonania(pobierz_odpowiedzi(src))
  expect_is(wynik, 'numeric')
  expect_equal(length(wynik), 2)
  
  wynik = oszacuj_czas_wykonania(pobierz_odpowiedzi(src), TRUE)
  expect_is(wynik, 'data.frame')
})