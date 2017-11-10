context('zsumuj_punkty')

src = polacz()

test_that('zsumuj_punkty sumuje poprawnie', {
  daneSql = pobierz_wyniki_zrownywania(src, 'sprawdzian', 2014, TRUE)
  daneLok = daneSql %>% collect()
  testy   = unique(daneLok$id_testu)
  daneDl  = pobierz_odpowiedzi(src) %>% filter_(~id_testu %in% testy)
  daneDlL = daneDl %>% collect()
  
  sumy = rowSums(daneLok[, grep('^k_[0-9]+$', names(daneLok))])
  
  lokUsun = zsumuj_punkty(daneLok)
  lokDop  = zsumuj_punkty(daneLok, FALSE)
  sqlUsun = zsumuj_punkty(daneSql)
  sqlDop  = zsumuj_punkty(daneSql, FALSE)
  dlUsun  = zsumuj_punkty(daneDl)
  dlDop   = zsumuj_punkty(daneDl, FALSE)
  dlLUsun = zsumuj_punkty(daneDlL)
  dlLDop  = zsumuj_punkty(daneDlL, FALSE)
  
  expect_is(sqlUsun, 'tbl_sql')
  expect_is(sqlDop, 'tbl_sql')
  expect_is(dlUsun, 'tbl_sql')
  expect_is(dlDop, 'tbl_sql')
  
  expect_equal(ncol(lokUsun), 5)
  expect_equal(ncol(lokDop), ncol(daneSql) + 1)
  expect_equal(ncol(sqlUsun), 5)
  expect_equal(ncol(sqlDop), ncol(daneSql) + 1)
  expect_equal(ncol(dlUsun), 5)
  expect_equal(ncol(dlDop), 5)
  expect_equal(ncol(dlLUsun), 5)
  expect_equal(ncol(dlLDop), 5)
  
  sqlUsun = sqlUsun %>% collect()
  sqlDop  = sqlDop %>% collect()
  dlUsun  = dlUsun %>% collect()
  dlDop   = dlDop %>% collect()
  
  expect_equal(all(lokUsun$wynik == sumy), TRUE)
  expect_equal(all(lokDop$wynik == sumy), TRUE)
  expect_equal(all(sqlUsun$wynik == sumy), TRUE)
  expect_equal(all(sqlDop$wynik == sumy), TRUE)
  
  kol = match(lokUsun$id_obserwacji, dlUsun$id_obserwacji)
  expect_equal(all(dlUsun$wynik[kol] == sumy), TRUE)
  kol = match(lokUsun$id_obserwacji, dlDop$id_obserwacji)
  expect_equal(all(dlDop$wynik[kol] == sumy), TRUE)
  
  kol = match(lokUsun$id_obserwacji, dlLUsun$id_obserwacji)
  expect_equal(all(dlLUsun$wynik[kol] == sumy), TRUE)
  kol = match(lokUsun$id_obserwacji, dlLDop$id_obserwacji)
  expect_equal(all(dlLDop$wynik[kol] == sumy), TRUE)
})

DBI::dbDisconnect(src$con)
