context('sprawdz_dokumentacje')

test_that('sprawdz_dokumentacje returns no rows', {
  expect_equal(nrow(.sprawdz_dokumentacje_zmiennych(polacz())), 0)
})