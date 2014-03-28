test_that('pobierz_czesc_egzaminu throws error', {
	expect_error(pobierz_czesc_egzaminu('?', 'matematyka', 2012, FALSE, TRUE))
	expect_error(pobierz_czesc_egzaminu('egzamin gimnazjalny', '?', 2012, FALSE, TRUE))
	expect_error(pobierz_czesc_egzaminu('egzamin gimnazjalny', 'matematyka', 2000, FALSE, TRUE))
})
	
test_that('pobierz_czesc_egzaminu loads data', {
	tmp=pobierz_czesc_egzaminu('egzamin gimnazjalny', 'matematyka', 2012, FALSE, TRUE)
	expect_is(tmp, 'data.frame')
	expect_true(nrow(tmp)>0)
	expect_true(ncol(tmp)>0)
	expect_false(is.na(tmp$teryt[1]))
})
