src = polacz()
dane = tibble(
  id_obserwacji = 1:10,
  k_6543 = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 1), # skala 760 - skroty
  k_6540 = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 1), # skala 760 - skroty
  k_6546 = c(0, 1, 3, 5, 0, 1, 3, 5, 0, 5), # skala 760 - skroty
  k_5515 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1), # skala 765 - pseudokryteria
  k_4325 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1), # skala 765 - pseudokryteria
  k_4326 = c(0, 1, 1, 0, 2, 2, 0, 0, 1, 1), # skala 765 - pseudokryteria
  k_4329 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0), # skala 765 - pseudokryteria
  k_4330 = c(0, 1, 1, 0, 2, 2, 0, 0, 1, 1)  # skala 765 - pseudokryteria
)

test_that('zastosuj_skale dane szerokie', {
  wzor = dane %>% select(-.data$k_5515, -.data$k_4325, -.data$k_4326, -.data$k_4329, -.data$k_4330)
  expect_equal(zastosuj_skale(dane, src, 760, FALSE), wzor, ignore_attr = TRUE)
  
  wzor$k_6540 = c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
  wzor$k_6546 = c(0, 0, 1, 2, 0, 0, 1, 2, 0, 2)
  expect_equal(zastosuj_skale(dane, src, 760), wzor, ignore_attr = TRUE)
  expect_equal(zastosuj_skale(dane, src, 760, TRUE), wzor, ignore_attr = TRUE)

  wzor = tibble(
    id_obserwacji = 1:10,
    k_5515 = dane$k_5515,
    p_520  = c(0, 2, 1, 1, 2, 3, 0, 1, 1, 2),
    p_521  = c(1, 1, 2, 0, 3, 2, 1, 0, 2, 1)
  )
  expect_equal(zastosuj_skale(dane, src, 765, FALSE), wzor, ignore_attr = TRUE)
  expect_equal(zastosuj_skale(dane, src, 765, TRUE), wzor, ignore_attr = TRUE)
  expect_equal(zastosuj_skale(dane, src, 765), wzor, ignore_attr = TRUE)
})

test_that('zastosuj_skale dane dlugie', {
  daneDl = dane %>% 
    tidyr::pivot_longer(-.data$id_obserwacji, names_to = 'kryterium', values_to = 'ocena') %>%
    arrange(.data$kryterium, .data$id_obserwacji)
  
  wzor = daneDl %>% 
    filter(.data$kryterium %in% c('id_obserwacji', 'k_6543', 'k_6540', 'k_6546'))
  wynik = zastosuj_skale(daneDl, src, 760, FALSE) %>% 
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
  
  tab = c(k_6540_0 = 0, k_6540_1 = 0, k_6540_2 = 1, k_6546_0 = 0, k_6546_1 = 0, k_6546_3 = 1, k_6546_5 = 2, k_6543_0 = 0, k_6543_1 = 1, k_6543_2 = 2)
  wzor = daneDl %>% 
    filter(.data$kryterium %in% c('id_obserwacji', 'k_6543', 'k_6540', 'k_6546')) %>%
    mutate(ocena = tab[paste0(.data$kryterium, "_", .data$ocena)])
  names(wzor$ocena) = NULL
  wynik = zastosuj_skale(daneDl, src, 760, TRUE) %>% 
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
  wynik = zastosuj_skale(daneDl, src, 760) %>% 
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
  
  wzor = tibble(
    id_obserwacji = 1:10,
    k_5515 = dane$k_5515,
    p_520  = c(0, 2, 1, 1, 2, 3, 0, 1, 1, 2),
    p_521  = c(1, 1, 2, 0, 3, 2, 1, 0, 2, 1)
  ) %>%
    tidyr::pivot_longer(-.data$id_obserwacji, names_to = 'kryterium', values_to = 'ocena') %>%
    arrange(.data$kryterium, .data$id_obserwacji)
  wynik = zastosuj_skale(daneDl, src, 765, FALSE) %>%
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
  wynik = zastosuj_skale(daneDl, src, 765, TRUE) %>%
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
  wynik = zastosuj_skale(daneDl, src, 765) %>%
    arrange(.data$kryterium, .data$id_obserwacji)
  expect_equal(wynik, wzor, ignore_attr = TRUE)
})

DBI::dbDisconnect(src$con)
