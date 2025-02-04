# ----------------------------------------------------------------------------

# Results of the study to examine whether intra-muscular magnesium is
# better than placebo for the treatment of chronic fatigue syndrome.

# Does intake of magnesium has influence on symptoms of chronic fatigue
# syndrome?

# Data are available as Table 1 in
# https://www.sheffield.ac.uk/polopoly_fs/1.43998!/file/tutorial-9-fishers.pdf

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(DescTools)

Sys.setlocale(locale = "Lithuanian")

chronic_fatigue_syndrome_1 <- DescTools::TextToTable('
                        Magnis Placebas
"Savijauta pagerėjo"        12        3
"Savijauta nepakito"         3       14
',

  dimnames = c("Savijauta", "Preparatas")
)

DescTools::Desc(chronic_fatigue_syndrome_1, verbose = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

