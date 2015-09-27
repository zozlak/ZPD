context('odkoduj_dystraktory')

src = polacz()

test_that('odkoduj_dystraktory działa', {
  dane = pobierz_odpowiedzi(src) %>% 
    filter(kryterium %in% c('k_1000', 'k_1001'), id_testu == 871, id_szkoly == 17705) %>%
    collect()

  dystr = odkoduj_dystraktory(dane, src)
  expect_is(dystr, 'data.frame')
  expect_is(dystr$odpowiedz, 'character')
  expect_equal(all(dystr$odpowiedz %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$odpowiedz)), TRUE)
  
  dane = dane %>% reshape2::dcast(id_obserwacji + id_testu + id_szkoly + rok ~ kryterium, value.var = 'odpowiedz')
  dystr = odkoduj_dystraktory(dane, src)
  expect_is(dystr, 'data.frame')
  expect_is(dystr$k_1000, 'character')
  expect_is(dystr$k_1001, 'character')
  expect_equal(all(dystr$k_1000 %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$k_1000)), TRUE)
  expect_equal(all(dystr$k_1001 %in% c('A', 'B', 'C', 'D', '-1', '-2') | is.na(dystr$k_1001)), TRUE)
})

DBI::dbDisconnect(src$con)
