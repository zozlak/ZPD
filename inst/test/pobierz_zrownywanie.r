test_that('pobierz_zrownywanie throws error', {
	expect_error(pobierz_zrownywanie('sprawdzian', 2010, TRUE))
	expect_error(pobierz_zrownywanie('?', 2012, TRUE))
})

test_that('pobierz_zrownywanie loads data', {
	tmp=pobierz_zrownywanie('sprawdzian', 2013, TRUE)
	expect_is(tmp, 'data.frame')
	expect_true(nrow(tmp)>0)
	expect_true(ncol(tmp)>0)
	
	tmp=pobierz_zrownywanie('egzamin gimnazjalny', 2013, TRUE)
	expect_is(tmp, 'data.frame')
	expect_true(nrow(tmp)>0)
	expect_true(ncol(tmp)>0)
})