# CLD žymėjimai ==============================================================

# Jei šios funkcijos reikės, nusikopijuokite ją į savo analizės bylą
# (pvz., į „setup“ bloką) arba užkraukite naudodami source().
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' CLD žymėjimų sudarymas
#'
#' @description
#' Pagalbinė funkcija, sudaranti *post-hoc* porinių lyginimų apibendrinimą
#' CLD žymėjimais ir pateikianti juos duomenų lentele.
#' CLD (compact letter display) -- kompaktiškas raidinis žymėjimas.
#'
#' Veikia su kai kuriais paketų „rstatix“, „DescTools“ ir „PMCMR“ funkcijomis
#' generuotais porinių lyginimų objektais, kurie pateikiami kaip argumentas
#' `test_rez`.
#'
#' @param test_rez Objektas su poriniais palyginimais. Įprastai generuotas
#'         paketų „rstatix“, „DescTools“ ir „PMCMR“ funkcijomis. Palaikomos
#'         klasės:
#'         - `rstatix_test` (paketas „rstatix“);
#'         - `PostHocTest`, `DunnTest` (paketas „DescTools“);
#'         - `PMCMR` (paketai „PMCMR“ ir „PMCMRplus“).
#'
#' @param alpha Reikšmingumo lygmuo. Skaičius tarp 0 ir 1.
#'        Pagal nutylėjimą – 0.05.
#'
#' @note
#' PASTABA: dėl to, kaip veikia funkcija [multcompView::multcompLetters()], 
#' rezultatas bus galimai NETEISINGAS (sugadinti grupių pavadinimai), jei 
#' grupių pavadinimuose yra „-“ ženklas.
#'
#' @return
#' Rezultatas -- duomenų lentelė, kurioje yra grupių pavadinimai 
#' (stulpelis `group`) bei stulpeliai `cld` ir `cld_sulygiuota`.
#' 
#' @export
#'
#' @examples
#' data(ToothGrowth, package = "datasets")
#' 
#' obj_1 <- rstatix::games_howell_test(len ~ dose, data = ToothGrowth)
#' sudaryk_cld(obj_1)
#'
#' obj_2 <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)
#' sudaryk_cld(obj_2)


sudaryk_cld <- function(test_rez, alpha = 0.05) {

  if (inherits(test_rez, "rstatix_test")) {
    # rstatix paketo funkcijoms
    p_su_pavadinimais <-
      with(test_rez, setNames(p.adj, paste0(group1, "-", group2)))

  } else if (inherits(test_rez, c("DunnTest", "PostHocTest"))) {
    # DescTools paketo funkcijoms
    p_su_pavadinimais <- test_rez[[1]][, 'pval']

  } else if (inherits(test_rez, c("PMCMR"))) {
    # PMCMR paketo funkcijoms
    p_su_pavadinimais <- PMCMRplus:::get.pvalues(test_rez)

  } else {
    stop("The method for the following class is not implemented. ",
      "The class(es) of your object: ", paste(class(test_rez), collapse = ", ")
    )
  }

  # Tai pagrindinė CLD sudaranti funkcija
  cld_obj <- multcompView::multcompLetters(p_su_pavadinimais, threshold = alpha)

  # Jei skirtumų nerasta, tada laukelis "$monospacedLetters" nesukuriamas,
  # tad naudojamas  "$Letters"
    if (is.null(cld_obj$monospacedLetters)) {
      cld_sulygiuota <- cld_obj$Letters

    } else {
      cld_sulygiuota <- gsub(" ", "_", cld_obj$monospacedLetters)
    }

  cld <-
    tibble::tibble(
      group          = names(cld_obj$Letters),
      cld            = cld_obj$Letters,
      cld_sulygiuota = cld_sulygiuota
    )

  cld
}
# ========================================================================== ~
