elementy=data.frame( #    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
	'id_kryterium'=			 c(-1, NA, NA, 35, 35, NA, NA, 35, NA, NA, NA, NA, NA, NA, 35, NA, NA, NA, NA),
	'id_pseudokryterium'=c(NA, -1, NA, NA, NA,  1, -1, NA,126, NA, NA, NA, NA, NA, NA,  1, NA, NA, NA),
	'id_skrotu'=				 c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,'0;|0;1','0;1|0','0;a|0:1','0;1;2|0;0;1', NA),
	'id_kryterium_1'=		 c(NA, NA, -1, 36, NA, 36, NA, NA, NA,158,158,158,158,158, NA, NA, 95, 95, 95),
	'id_kryterium_2'=    c(NA, NA, -2, NA, 36, NA, 36, NA, NA, NA,159,180,180,180, NA, NA, 94, 99, 99),
	'opis'=							 c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, '','x', NA, NA, NA,'y','x')
)
zdODBC='EWD'
P=odbcConnect(zdODBC)
przywrocIdSkali=sqlQuery(P, "SELECT max(id_skali) FROM skale")[1, 1]
przywrocIdPkryt=sqlQuery(P, "SELECT max(id_pseudokryterium) FROM pseudokryteria_oceny")[1, 1]

test_that('edytuj_skale throws error', {
	idSkali=stworz_skale('test1', '', NULL, NULL, NULL)
	expect_error(edytuj_skale(idSkali, elementy[1, ])) # nieistniejace id_kryterium
	expect_error(edytuj_skale(idSkali, elementy[2, ])) # nieistniejace id_pseudokryterium
	expect_error(edytuj_skale(idSkali, elementy[3, ])) # niestniejace id_krytreium w kolumnach id_kryterium_N
	expect_error(edytuj_skale(idSkali, elementy[4, ])) # jednoczesne id_kryterium i id_kryterium_1
	expect_error(edytuj_skale(idSkali, elementy[5, ])) # jednoczesne id_kryterium i id_kryterium_2
	expect_error(edytuj_skale(idSkali, elementy[6, ])) # jednoczesne id_pseudokryterium i id_kryterium_1
	expect_error(edytuj_skale(idSkali, elementy[7, ])) # nieistniejace id_pseudokryterium oraz jednoczesne id_pseudokryterium i id_kryterium_1
	expect_error(edytuj_skale(idSkali, elementy[10, ])) # tylko jedno sposrod id_kryterium_N nie jest NA
	expect_error(edytuj_skale(idSkali, elementy[12, ])) # brak opisu dla pseudokryterium, ktore trzeba utworzyc
	expect_error(edytuj_skale(idSkali, elementy[13, ])) # brak opisu dla pseudokryterium, ktore trzeba utworzyc
	expect_error(edytuj_skale(idSkali, elementy[15, ])) # bledny skrot skali
	expect_error(edytuj_skale(idSkali, elementy[16, ])) # bledny skrot skali
	expect_error(edytuj_skale(idSkali, elementy[17, ])) # bledny skrot skali
	expect_error(edytuj_skale(idSkali, rbind(elementy[8, ], elementy[8, ]))) # zdublowane elementy skali
	expect_error(edytuj_skale(idSkali, rbind(elementy[9, ], elementy[9, ]))) # zdublowane elementy skali
	expect_error(edytuj_skale(idSkali, rbind(elementy[9, ], elementy[11, ]))) # zdublowane elementy skali
	expect_error(edytuj_skale(idSkali, rbind(elementy[14, ], elementy[19, ]))) # zdublowane elementy skali
	expect_true(0==sqlQuery(P, sprintf("SELECT count(*) FROM skale_elementy WHERE id_skali=%d", idSkali)))
})

test_that('edytuj skale edits scale', {
	idSkali=stworz_skale('test2', '', NULL, NULL, NULL)
	tmp=edytuj_skale(idSkali, elementy[c(8, 11, 14, 18), ])
	expect_true(4==sqlQuery(P, sprintf("SELECT count(*) FROM skale_elementy WHERE id_skali=%d", idSkali)))
	expect_error(edytuj_skale(idSkali, elementy[c(8, 11, 14, 18), ])) # bo "nadpisz"=F
	expect_is(edytuj_skale(idSkali, elementy[c(8, 11, 14, 18), ], TRUE), 'data.frame')
})

sqlQuery(P, sprintf("DELETE FROM skale_elementy WHERE id_skali>%d", przywrocIdSkali))
sqlQuery(P, sprintf("DELETE FROM skale WHERE id_skali>%d", przywrocIdSkali))
sqlQuery(P, sprintf("DELETE FROM pseudokryteria_oceny_kryteria WHERE id_pseudokryterium>%d", przywrocIdPkryt))
sqlQuery(P, sprintf("DELETE FROM pseudokryteria_oceny WHERE id_pseudokryterium>%d", przywrocIdPkryt))
sqlQuery(P, sprintf("SELECT setval('skale_id_skali_seq', %d)", przywrocIdSkali))
sqlQuery(P, sprintf("SELECT setval('pseudokryteria_id_pseudokryterium_seq', %d)", przywrocIdPkryt))
odbcClose(P)