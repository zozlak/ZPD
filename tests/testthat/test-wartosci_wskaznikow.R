test_that('pobierz_wartosci_wskaznikow działa', {
  src = polacz()
  wynik = pobierz_wartosci_wskaznikow(src) %>% 
    filter(.data$rok == 2012 & .data$wskaznik == 'paou_sp_war') %>%
    select(.data$lu) %>%
    collect() %>%
    summarize(lu_na = sum(.data$lu >= 0) - n())
  expect_equal(wynik$lu_na, 0)
})
