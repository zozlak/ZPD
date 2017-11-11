context('zastosuj_skale')

src = polacz()
dane = data_frame(
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
  wzor = dane %>% select_('-k_5515', '-k_4325', '-k_4326', '-k_4329', '-k_4330')
  expect_equal(zastosuj_skale(dane, src, 760, FALSE), wzor)
  
  wzor$k_6540 = c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
  wzor$k_6546 = c(0, 0, 1, 2, 0, 0, 1, 2, 0, 2)
  expect_equal(zastosuj_skale(dane, src, 760), wzor)
  expect_equal(zastosuj_skale(dane, src, 760, TRUE), wzor)

  wzor = data_frame(
    id_obserwacji = 1:10,
    k_5515 = dane$k_5515,
    p_520  = c(0, 2, 1, 1, 2, 3, 0, 1, 1, 2),
    p_521  = c(1, 1, 2, 0, 3, 2, 1, 0, 2, 1)
  )
  expect_equal(zastosuj_skale(dane, src, 765, FALSE), wzor)
  expect_equal(zastosuj_skale(dane, src, 765, TRUE), wzor)
  expect_equal(zastosuj_skale(dane, src, 765), wzor)
})

test_that('zastosuj_skale dane dlugie', {
  daneDl = dane %>% 
    reshape2::melt(id.vars = 'id_obserwacji', variable.name = 'kryterium', value.name = 'ocena') %>%
    arrange_('kryterium', 'id_obserwacji') %>%
    tbl_df()
  daneDl$kryterium = as.character(daneDl$kryterium)
  
  wzor = daneDl %>% 
    filter_(~kryterium %in% c('id_obserwacji', 'k_6543', 'k_6540', 'k_6546'))
  expect_equal(zastosuj_skale(daneDl, src, 760, FALSE), wzor)
  
  tab = c(k_6540_0 = 0, k_6540_1 = 0, k_6540_2 = 1, k_6546_0 = 0, k_6546_1 = 0, k_6546_3 = 1, k_6546_5 = 2, k_6543_0 = 0, k_6543_1 = 1, k_6543_2 = 2)
  wzor = daneDl %>% 
    filter_(~kryterium %in% c('id_obserwacji', 'k_6543', 'k_6540', 'k_6546')) %>%
    mutate_(ocena = ~tab[paste0(kryterium, "_", ocena)])
  names(wzor$ocena) = NULL
  expect_equal(zastosuj_skale(daneDl, src, 760, TRUE), wzor)
  expect_equal(zastosuj_skale(daneDl, src, 760), wzor)

  wzor = data_frame(
    id_obserwacji = 1:10,
    k_5515 = dane$k_5515,
    p_520  = c(0, 2, 1, 1, 2, 3, 0, 1, 1, 2),
    p_521  = c(1, 1, 2, 0, 3, 2, 1, 0, 2, 1)
  ) %>%
    reshape2::melt(id.vars = 'id_obserwacji', variable.name = 'kryterium', value.name = 'ocena') %>%
    tbl_df()
  wzor$kryterium = as.character(wzor$kryterium)
  expect_equal(zastosuj_skale(daneDl, src, 765, FALSE), wzor)
  expect_equal(zastosuj_skale(daneDl, src, 765, TRUE), wzor)
  expect_equal(zastosuj_skale(daneDl, src, 765), wzor)
})

DBI::dbDisconnect(src$con)
