
# Analizės funkcijos =========================================================

# Šias funkcijas reikia užsikrauti prieš atliekant analizę,
# pvz., naudojant funkciją `source()`.
#
# Virš kiekvienos funkcijos yra jos aprašymas ir keletas naudojimo pavyzdžių.


# ~~~ Vidurkio PI pagal vid., SD, n ------------------------------------------

#' Vidurkio PI skaičiavimas pagal aprašomąsias statistikas
#'
#' Funkcija `ci_mean_t_stat()` skaičiuoja vidurkio PI pagal klasikinę formulę
#' su t koeficientu, kai duotos aprašomosios statistikos (vidurkis, standartinis
#' nuokrypis, imties dydis). Naudinga, kai tokie dydžiai būna pateikti
#' mokslinėje literatūroje.
#'
#' @param vidurkis     Vektorius su kiekvienos grupės vidurkiais.
#' @param st_nuokrypis Vektorius su kiekvienos grupės standartiniu nuokrypiu.
#' @param n            Vektorius su kiekvienos grupės dydžiu.
#' @param grupes_pavadinimas Grupės pavadinimas.
#'                     Pagal nutylėjimą naudojama tuščia eilutė "".
#' @param conf.level   Pasikliovimo lygmuo.
#'
#' @return
#' Rezultatas – duomenų lentelė, su grupių pavadinimais, vidurkio įverčiu,
#' pasikliautinojo intervalo ribomis, standartiniu nuokrypiu ir imties dydžiu
#' kiekvienai grupei.
#'
#' @examples
#' # Pavyzdžiai:
#'
#' ci_mean_t_stat(vidurkis = 362, st_nuokrypis = 35, n = 100)
#'
#' ci_mean_t_stat(362, 35, 100)
#'
#' vidurkis     <- c(1, 2, 3)
#' st_nuokrypis <- c(3, 2, 3)
#' n            <- c(50, 20, 40)
#' grupe        <- c("A", "B", "C")
#'
#' ci_mean_t_stat(vidurkis, st_nuokrypis, n, grupe)
#'
#'
#' # Jei norite, kad rodytų daugiau skaitmenų po kablelio:
#'
#' rez_pi <- ci_mean_t_stat(362, 35, 100)
#' as.data.frame(rez_pi)
#'
#' # Arba
#' View(rez_pi)
#'
ci_mean_t_stat <- function(vidurkis, st_nuokrypis, n, grupes_pavadinimas = "",
                           conf.level = 0.95) {

  Q <- conf.level

  # Taikome formulę su t koeficientu:
  t <- qt(p = (1 - Q) / 2, df = n - 1, lower.tail = FALSE)
  paklaida <- t * st_nuokrypis / sqrt(n)

  apatine_riba   <- vidurkis - paklaida
  virsutine_riba <- vidurkis + paklaida

  # Sudėliojame rezultatus
  vidurkio_pi_t <-
    tibble::tibble(
      grupe             = forcats::as_factor(grupes_pavadinimas),
      vidurkis          = vidurkis,
      pi_apatine_riba   = apatine_riba,
      pi_virsutine_riba = virsutine_riba,
      sd                = st_nuokrypis,
      n                 = as.integer(n)
    )

  vidurkio_pi_t
}

# ~~~ Vidurkio PI grupėms ----------------------------------------------------

#' Vidurkio PI skaičiavimas grupėms
#'
#' `ci_mean_t()` yra vidurkio PI skaičiavimo funkcija, kuriai duomenys turi būti
#' pateikti duomenų lentelės pavidalu ir kuri reaguoja į [dplyr::group_by()],
#' tad skaičiavimus gali atlikti ir pogrupiams.
#'
#' @param .data Duomenų lentelė.
#' @param x     Stulpelio pavadinimas (be kabučių).
#' @param conf.level Pasikliovimo lygmuo.
#' @param ... Kiti parametrai, kuriuos priima [DescTools::MeanCI()].
#'
#' @return
#' Rezultatas – duomenų lentelė, kurios stulpeliai:
#'
#' - `mean` – vidurkio įvertis,
#' - `lwr.ci`, `upr.ci` – (lower CI, upper CI) apatinė ir viršutinė
#'                        pasikliautinojo intervalo ribos.
#' – kiti stulpeliai (jei yra) – grupavimo kintamųjų pavadinimai.
#'
#' @examples
#' # Pavyzdžiai:
#'
#' ci_mean_t(npk, yield)
#'
#' library(tidyverse)
#' npk %>% ci_mean_t(yield)
#'
#' npk %>% dplyr::group_by(N) %>% ci_mean_t(yield)
#'
#' npk %>% dplyr::group_by(N, P, K) %>% ci_mean_t(yield)
#'
#' @note
#' Kaip veikia funkcija, suprasti nebūtina, bet reikia mokėti ją naudoti ir
#' suprasti rezultatus.
#'
ci_mean_t <- function(.data, x, conf.level = 0.95, ...) {

  `%>%` <- dplyr::`%>%`

  vidurkio_pi <- function(x) {
    # Rezultatas turi būti duomenų lentelė
    DescTools::MeanCI(x, conf.level = conf.level, ...) %>%
      t() %>%
      as.data.frame()
  }

  rezultatas <-
    .data %>%
    tidyr::nest(data = c(dplyr::everything(), -dplyr::group_vars(.))) %>%
    dplyr::mutate(
      ci = purrr::map(data, ~ dplyr::pull(., {{ x }}) %>% vidurkio_pi())
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(ci) %>%
    dplyr::ungroup()

  rezultatas
}

# ~~~ Proporcijos PI ---------------------------------------------------------

#' Proporcijos PI: 2 grupės
#'
#' Funkcija naudojama taip pat kaip [DescTools::BinomCI()], tik numatytasis
#' metodas yra modifikuotasis Wilson metodas, o rezultatas – duomenų lentelė,
#' o ne vektorius. Todėl, pvz., rezultatą galima braižyti naudojant **ggplot2**.
#'
#' @param x Mus dominančių/ Mums palankių įvykių skaičius. /
#'          Mus dominančios grupės dydis.
#'
#' @param n Įvykių skaičius iš viso. / Imties dydis.
#' @param method,conf.level,... Kiti parametrai, kuriuos priima
#'        [DescTools::BinomCI()]. Žiūrėti šios funkcijos dokumentaciją.
#'
#' @return
#' Rezultatas – duomenų lentelė, kurios stulpeliai:
#'
#' - `est` – proporcijos įvertis,
#' - `lwr.ci`, `upr.ci` – (lower CI, upper CI) apatinė ir viršutinė
#'                        pasikliautinojo intervalo ribos.
#' @examples
#' x <- 54 # mus dominančių įvykių skaičius
#' n <- 80 # įvykių skaičius iš viso
#'
#' ci_binom(x = 54, n = 80)
#'
ci_binom <- function(x, n, method = "modified wilson", conf.level = 0.95, ...) {
  rez <- DescTools::BinomCI(
    x = x, n = n, conf.level = conf.level, method = method, ...
  )
  tibble::as_tibble(rez)
}


#' Proporcijos PI: 3 ar daugiau grupių
#'
#' Funkcija naudojama taip pat kaip [DescTools::MultinomCI()], tik numatytasis
#' metodas yra Goodman metodas, o rezultatas – duomenų lentelė, o ne vektorius.
#' Todėl, pvz., rezultatą galima braižyti naudojant **ggplot2**.
#'
#' @param x Vektorius su grupių dydžiais. Geriausia, jei vektoriaus elementai
#'          turėtų prasmingu pavadinimus.
#'
#' @param method,conf.level,... Kiti parametrai, kuriuos priima
#'        [DescTools::MultinomCI()]. Žiūrėti šios funkcijos dokumentaciją.
#'
#' @param gr_name Pavadinimas (kabutėse) rezultatų stulpeliui, kuriame bus
#'        parašyti grupių pavadinimai. Numatytoji reikšmė yra `"group"`.
#'
#' @return
#' Rezultatas – duomenų lentelė, kurios stulpeliai:
#'
#' - `group` (arba kitas vartotojo pasirinktas pavadinimas) – grupės pavadinimas.
#' - `est` – proporcijos įvertis.
#' - `lwr.ci`, `upr.ci` – (lower CI, upper CI) apatinė ir viršutinė
#'                        pasikliautinojo intervalo ribos.
#'
#' @examples
#' ci_multinom(c(20, 35, 54))
#'
#' ci_multinom(c(20, 35, 54), method = "goodman")
#'
#' x <- c("dideli" = 20, "vidutiniai" = 35, "maži" = 54)
#' ci_multinom(x, method = "goodman")
#'
#' library(tidyverse)
#' c("dideli" = 20, "vidutiniai" = 35, "maži" = 54) %>%
#'   ci_multinom(method = "goodman")
#'
#' c("dideli" = 33, "vidutiniai" = 35, "maži" = 30) %>%
#'   ci_multinom(method = "sisonglaz")
ci_multinom <- function(x, method = "goodman", conf.level = 0.95,
                        gr_name = "group", ...) {
  `%>%` <- dplyr::`%>%`

  x %>%
    DescTools::MultinomCI(method = method, conf.level = conf.level, ...) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(gr_name) %>%
    tibble::as_tibble()
}


# ~~~ PI savirankos metodu ----------------------------------------------------

#' PI skaičiavimas savirankos metodu
#'
#' Funkcija `ci_boot()` savirankos metodu skaičiuoja pasikliautinuosius
#' intervalus. Naudojama analogiškai taip, kaip [DescTools::BootCI()],
#' bet:
#'
#'  - jos pirmas argumentas yra duomenų lentelė,
#'  - argumentai `x` (ir, jei reikia, `y`) stulpelių pavadinimai be kabučių.
#'  - funkcija reaguoja į [dplyr::group_by()], tad skaičiavimus gali atlikti
#'    pogrupiams.
#'
#' @param .data Duomenų lentelė.
#' @param x,y   Stulpelio pavadinimas (be kabučių).
#' @param conf.level Pasikliovimo lygmuo.
#' @param ... Kiti parametrai, kuriuos priima [DescTools::BootCI()], tarp kurių:
#' 1) `FUN` -- funkcija, kurios rezultatui skaičiuojamai PI.
#' 2) `bci.method` -- intervalų sudarymo metodai:
#'    + `perc` -- procentilių metodas.
#'    + `bca`  -- koreguotasis procentilių metodas BCa
#'               (bias corrected and accelerated)
#'    + kiti
#' 3) `R` -- replikacijų (pakartojimų skaičius).
#'           Įprastai turi būti tarp 1'000 ir 10'000.
#'
#' @return
#' Rezultatas – duomenų lentelė, su pasikliautinaisiais intervalais.
#' Stulpelių skaičius ir pavadinimai priklauso nuo funkcijos argumentų reikšmių:
#'
#' - Jei duomenų lentelė grupuotoji, pirmųjų stulpelių pavadinimai sutampa su
#'   grupavimo kintamųjų pavadinimais.
#' - Stulpelio pavadinimas, sutampantis su skaičiuojamos statistikos pavadinimu
#'   (argumento `FUN` reikšme).
#'   Jame yra skaičiuojamos statistikos įvertis.
#' - `lwr.ci`, `upr.ci` – (lower CI, upper CI) apatinė ir viršutinė
#'                        pasikliautinojo intervalo ribos.
#' @examples
#' set.seed(1)
#'
#' ci_boot(iris, Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' library(tidyverse)
#' iris %>%
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' iris %>%
#'   group_by(Species) %>%
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "bca")
#'
#' iris %>%
#'   group_by(Species) %>%
#'   ci_boot(Petal.Length, FUN = median, R = 1000, bci.method = "perc")
#'
#'
#' med_pi_gr <-
#'   iris %>%
#'   group_by(Species) %>%
#'   ci_boot(
#'     Petal.Length,
#'     FUN = median, na.rm = TRUE,
#'     R = 1000, bci.method = "bca"
#'   )
#'
#' med_pi_gr
#'
#'
#' # Dviejų kintamųjų funkcijoms
#' spearman_pi_gr <-
#'   iris %>%
#'   group_by(Species) %>%
#'   ci_boot(
#'     Petal.Length, Petal.Width,
#'     FUN = cor, method = "spearman",
#'     R = 1000, bci.method = "bca"
#'   )
#'
#' spearman_pi_gr
#'
#'
#' pearson_pi_gr <-
#'   iris %>%
#'   group_by(Species) %>%
#'   ci_boot(
#'     Petal.Length, Petal.Width,
#'     FUN = cor, method = "pearson",
#'     R = 1000, bci.method = "bca"
#'   )
#'
#' pearson_pi_gr
#'
ci_boot <- function(.data, x, y = NULL, conf.level = 0.95, ...) {

  `%>%` <- dplyr::`%>%`

  ci_function <- function(x, y) {
    # Rezultatas turi būti duomenų lentelė
    DescTools::BootCI(x = x, y = y, conf.level = conf.level, ...) %>%
      t() %>%
      as.data.frame()
  }

  missing_y <- missing(y)

  .data %>%
    tidyr::nest(data = c(dplyr::everything(), -dplyr::group_vars(.))) %>%
    dplyr::mutate(ci =
      purrr::map(data, ~ ci_function(
        x = dplyr::pull(., {{ x }}),
        y = if (missing_y) NULL else dplyr::pull(., {{ y }})
      ))
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(ci) %>%
    dplyr::ungroup()
}
