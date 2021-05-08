src = polacz()

test_that('odkoduj_dystraktory działa', {
  dane = pobierz_odpowiedzi(src) %>%
    filter(kryterium %in% c('k_1000', 'k_1001'), id_testu == 871L, id_szkoly == 17705L) %>%
    collect()

  dystr = odkoduj_dystraktory(dane, src)
  expect_s3_class(dystr, 'data.frame')
  expect_type(dystr$odpowiedz, 'character')
  expect_equal(all(dystr$odpowiedz %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$odpowiedz)), TRUE)

  dane = dane %>% 
    tidyr::pivot_wider(c('id_obserwacji', 'id_testu', 'id_szkoly', 'rok'), names_from = 'kryterium', values_from = 'odpowiedz')
  dystr = odkoduj_dystraktory(dane, src)
  expect_s3_class(dystr, 'data.frame')
  expect_type(dystr$k_1000, 'character')
  expect_type(dystr$k_1001, 'character')
  expect_equal(all(dystr$k_1000 %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$k_1000)), TRUE)
  expect_equal(all(dystr$k_1001 %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$k_1001)), TRUE)
})

DBI::dbDisconnect(src$con)
