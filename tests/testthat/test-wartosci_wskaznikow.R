context('wartości wskaźników')

test_that('pobierz_wartosci_wskaznikow działa', {
  src = polacz()
  wynik = pobierz_wartosci_wskaznikow(src) %>% 
    filter_(~rok == 2012, ~wskaznik == 'paou_sp_war') %>%
    select_('lu') %>%
    collect() %>%
    summarize_('lu_na' = 'sum(lu >= 0) - n()')
  expect_equal(wynik$lu_na, 0)
})
  