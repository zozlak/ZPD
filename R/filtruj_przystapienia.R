#' @title Zwraca pierwsze lub ostatnie przystąpienia do danego egzaminu w
#'   zadanych latach
#' @description
#' Najczęściej podczas analiz interesuje nas jedynie pierwsze podejście ucznia 
#' do danego egzaminu. Czasem ważne jest także podejście ostatnie (np. do 
#' egzaminu na końcu poprzedniego etapu edukacji).
#' 
#' Funkcja filtruj_przystapienia() zwraca dla zadanego egzaminu i zakresu lat
#' tabelę z kolumnami "id_obserwacji" i "id_testu" umożliwiającą odfiltrowanie
#' jedynie pierwszych/ostatnich przystąpień uczniów do tego egzaminu.
#' 
#' Następuje to poprzez przeanalizowanie wszystkich danych z grupy
#' "uczniowieTesty" dostępnych w bazie dla danego egzaminu, jednak ostateczny
#' wynik ograniczany jest do lat wskazanych w parametrach "rokOd" i "rokDo".
#' Uwzględnienie wszystkich danych znajdujących się w bazie jest konieczne, aby
#' poprawnie dokonać filtrowania (uniknąć "efektów brzegowych" w ostatnich
#' latach, dla których chcemy pobrać informacje).
#' 
#' Jeśli podano tylko rodzaj egzaminu, odfiltrowanie nastąpi na poziomie
#' rodzajów egzaminów (a więc aby uczeń został w danym roku zaliczony jako
#' podchodzący do egzaminu wystarczy, że w bazie będą wyniki dla choćby jednej,
#' dowolnej części tego egzaminu). Jeśli podano także część(części) egzaminu, 
#' wtedy odfiltrowanie nastąpi tylko w ramach danej części egzaminu (jeśli uczeń
#' w danym roku uczeń brał udział jedynie w innych częściach egzaminu, ale nie
#' we wskazanej, nie będzie mu to policzone jako podejście do egzaminu).
#' 
#' Bez względu na to, czy określono część egzaminu podczas odfiltrowywania
#' pierwszych przystąpień brane są pod uwagę wartości kolumny "pop_podejście" -
#' uczniowie z niepustymi jej wartościami są pomijani.
#' 
#' Istotne znaczenie ma argument "czyEwd". Służy on do wybrania, na podstawie
#' których danych (EWD/CKE) ma zostać dokonana filtracja.
#' \itemize{
#'   \item Jego wartość powinna być zgodna z wynikami egzaminacyjnymi i/lub 
#'     oszacowaniami umiejętności uczniów, które chcemy odfiltrować za pomocą 
#'     wyników działania funkcji filtruj_przystapienia().
#'   \item Należy sobie zdawać sprawę, że dane CKE przed 2010 rokiem są
#'     niełączliwe między latami, a więc nie zostaną dla nich odfiltrowani
#'     żadni uczniowie (funkcja ostrzega w takich wypadkach).
#'   \item Jego podanie jest obowiązkowe, gdyż odfiltrowywanie przystąpień
#'     do egzaminu z pominięciem wyboru źródła danych nie ma sensownej
#'     interpretacji praktycznej.
#' }
#' Patrz też - http://zpd.ibe.edu.pl/doku.php?id=czyewd
#' 
#' Zakres filtrowanych danych można ograniczyć przekazując w parametrze 
#' "obserwacje" id_obserwacji, do których ma zostać zawężona analiza. Parametr
#' ten może być albo wektorem id_obserwacji albo ramką danych z kolumną
#' "id_obserwacji".
#' @param src uchwyt źródła danych dplyr-a
#' @param pierwsze czy odfiltrować pierwsze podejście (jeśli FALSE, odfiltrowane
#'   zostanie ostatnie)
#' @param rodzajEgzaminu rodzaj egzaminu - patrz
#'   http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu
#' @param czescEgzaminu wektor części egzaminu - patrz
#'   http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu
#' @param czyEwd czy korzystać z danych EWD czy CKE (patrz opis)
#' @param obserwacje wektor id_obserwacji lub ramka danych z kolumną
#'   id_obserwacji
#' @import dplyr
#' @export
filtruj_przystapienia = function(
  src,
  pierwsze,
  rodzajEgzaminu,
  czescEgzaminu = NULL,
  czyEwd,
  obserwacje = NULL
){
  stopifnot(
    is.src(src),
    is.logical(pierwsze),
    length(pierwsze) == 1,
    is.character(rodzajEgzaminu),
    length(rodzajEgzaminu) == 1,
    is.null(czescEgzaminu) | is.vector(czescEgzaminu) & is.character(czescEgzaminu),
    is.logical(czyEwd),
    length(czyEwd) == 1,
    is.null(obserwacje) |
      is.vector(obserwacje) & is.numeric(obserwacje) | 
      is.tbl(obserwacje) & sum(colnames(obserwacje) %in% 'id_obserwacji') == 1
  )

  tests = pobierz_testy(src) %>%
    select_('id_testu', 'rodzaj_egzaminu', 'czesc_egzaminu', 'dane_ewd') %>%
    filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~dane_ewd == czyEwd)
  if (length(czescEgzaminu) > 0) {
    czescEgzaminu = append(czescEgzaminu, '-1') # paskudne obejście błędnego mapowania dplyr-a na SQL-owe IN
    tests = tests %>% 
      filter_(~czesc_egzaminu %in% czescEgzaminu) 
  }
  if (nrow(tests %>% collect()) == 0) {
    stop(e('Nie ma takiej części i/lub rodzaju egzaminu'))
  }

  if (!is.null(obserwacje)) {
    if (is.vector(obserwacje)) {
      obserwacje = data_frame('id_obserwacji' = obserwacje)
    }
    obserwacje = obserwacje %>%
      select_('id_obserwacji')
  }
    
  tob = suppressMessages(
    pobierz_dane_uczniowie_testy(src) %>%
    select_('id_obserwacji', 'id_testu', 'rok', 'pop_podejscie') %>%
    inner_join(tests)
  )
  if (!is.null(obserwacje)) {
    tob = suppressMessages(
      tob %>%
      inner_join(obserwacje, copy = T)
    )
  }
  tob = tob %>%
    group_by_('id_obserwacji', 'dane_ewd', 'rodzaj_egzaminu')
  
  if (length(czescEgzaminu) > 0) {
    tob = tob %>%
      group_by_('czesc_egzaminu', add = TRUE)
  }

  if (pierwsze) {
    tob = tob %>%
      summarize_(.dots = list(rok = 'min(rok, na.rm = TRUE)'))
  } else {
    tob = tob %>%
      summarize_(.dots = list(rok = 'max(rok, na.rm = TRUE)'))
  }
  
  tob = tob %>% 
    ungroup()

  return(tob)
}
attr(filtruj_przystapienia, 'grupa') = 'uczniowieTesty'
attr(filtruj_przystapienia, 'testArgs') = list('pierwsze' = TRUE, 'rodzajEgzaminu' = 'sprawdzian', 'czyEwd' = TRUE)
