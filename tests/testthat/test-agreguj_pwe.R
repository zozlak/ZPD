context('agreguj_pwe')

pv = data.frame(
  rok = rep(2010, 30), 
  nr_pv = rep(1:15, 2), 
  wynik = rnorm(30, 100, 15)
)

test_that('agreguj_pwe działa', {
  expect_is(agreguj_pwe(pv, 'rok', wariancjaPop = oblicz_wariancje_populacji(pv)), 'data.frame')
  expect_is(agreguj_pwe(pv, 'rok'), 'data.frame')
})