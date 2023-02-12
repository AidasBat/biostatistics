```{r setup, warning=FALSE, message=FALSE, results='hide'}
library(conflicted)
library(tidyverse)
library(ggpubr)
library(DescTools)
library(rstatix)
library(PMCMRplus)
library(effectsize)
```
if (getRversion() < "4.2.0" && tolower(Sys.info()[['sysname']]) == "windows") {
  Sys.setlocale(locale = "Lithuanian")    # Windows ir R < 4.2.0
} else {
  Sys.setlocale(locale = "lt_LT.UTF-8")   # Linux, Mac arba R ≥ 4.2.0
}

ggplot2::theme_set(ggplot2::theme_bw())

conflict_prefer("cohens_d", "rstatix")

source("funkcijos/sudaryk_cld.R", encoding = "UTF-8")

duomenys <-
  readxl::read_excel("duomenys/ldaep.xlsx", sheet = "sutvarkyti")
glimpse(duomenys)
head(duomenys)

duomenys_xl <-
  duomenys %>%
  mutate(elektrodas = as_factor(elektrodas))

duomenys_xl %>%
  group_by(elektrodas) %>%
  mutate(elektrodas = as_factor(elektrodas)) %>%
  ggplot(aes(y = amp, x = skaicius, color = elektrodas)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, color = "black", alpha = 0.5)

duomenys_xl %>%
  ggplot(aes(x = amp, color = elektrodas, fill = elektrodas)) +
  geom_density(alpha = 0.2, lwd = 1, adjust = 1) +
  geom_rug(alpha = 0.7)

duomenys_xl %>% group_by(elektrodas) %>% identify_outliers(amp)

duomenys_xl %>% ggqqplot("amp", facet.by = "elektrodas", scales = "free")

duomenys_xl %>%
  group_by(elektrodas) %>%
  summarise(dispersija = var(amp), SD = sd(amp)) %>%
  ungroup() %>%
  mutate(
    max_min_dispersiju_santykis = round(max(dispersija) / min(dispersija), 1),
    max_min_SD_santykis         = round(max(SD)         / min(SD),         1)
  )

kw_rez <- duomenys_xl %>% kruskal_test(amp ~ elektrodas)
kw_rez

get_test_label(kw_rez, type = "text", detailed = TRUE)

duomenys_xl %>% kruskal_effsize(amp ~ elektrodas)


conover_rez <- ConoverTest(amp ~ elektrodas, data = duomenys_xl, method = "holm")
conover_rez

conover_rez_df <-
  as.data.frame(conover_rez[[1]]) %>%
  rownames_to_column("groups") %>%
  separate("groups", into = c("group1", "group2"), sep = "-")

conover_rez_df


cld_rez_c <- sudaryk_cld(conover_rez, alpha = 0.05)
cld_rez_c

efekto_dydis_poroms_rw <-
  duomenys %>%
  wilcox_effsize(amp ~ elektrodas) %>%
  arrange(desc(abs(effsize)))

efekto_dydis_poroms_rw


conover_rez_df %>%
  mutate(p_adj = scales::pvalue(pval, decimal.mark = ",")) %>%
  select(-`mean rank diff`, -pval) %>%
  rename("Elktrodas 1" = group1, "Elektrodas 2" = group2, "p (koreguotoji)" = p_adj) %>%
  knitr::kable(
    caption = "Lentelė 1.
    Conover-Iman analizės (porinių palyginimų) rezultatai.
    P reikšmės koreguotos Holm metodu.
    Reikšmingumo lygmuo – 0,05."
  )

# We investigated the amplitudes of 3 electrodes (cz, pz and fz, each with 185
# fixed amplitude values) at different sound intensities. We analysed the data
# by non-parametric Kruskal-Wallis analysis (significance level α = 0.05)
# because the assumption of normality was violated (visually assessed by the QQ
# chart) and by a corrective Conover-Iman post-hoc analysis, which showed, that
# the changes in amplitudes recorded by the cz electrode were the most
# different from the other two electrodes (in terms of p-values), and that the
# readings of all the electrodes differed from each other in a statistically
# significant way, with the p-value of the p-pair comparisons in all the cases
# being lower than the level of significance (table 1).

