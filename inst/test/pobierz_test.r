test_that('pobierz_test throws error', {
	expect_error(pobierz_test(-100, TRUE))
})
					
test_that('pobierz_test loads data', {
	tmp=pobierz_test(626, TRUE)
	expect_is(tmp, 'data.frame')
	expect_true(nrow(tmp)>0)
	expect_true(ncol(tmp)>0)
})
					