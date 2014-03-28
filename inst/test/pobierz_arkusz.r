test_that('pobierz_arkusz throws error', {
	expect_error(pobierz_arkusz('?', TRUE, FALSE))
})
	
test_that('pobierz_arkusz loads data', {
	tmp=pobierz_arkusz('S-B1-092', TRUE, FALSE)
	expect_is(tmp, 'data.frame')
	expect_true(nrow(tmp)>0)
	expect_true(ncol(tmp)>0)
	expect_false(is.na(tmp$teryt[1]))
})