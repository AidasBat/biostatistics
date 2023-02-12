# Version 2021-12-11

# Required packages ----------------------------------------------------------
# Install
if (!require(remotes)) install.packages("remotes")
remotes::install_cran(
  c("tidyverse", "factoextra", "ggstatsplot", "ggpubr", "plotly")
)

# Load
library(tidyverse)
library(factoextra)
library(ggstatsplot)


# PCA ========================================================================

## Recommended steps for proper PCA ------------------------------------------

# Recommended steps to perform PCA when the purpose is to plot multivariate
# data:
#
# 1. QUESTION.
#    Define clear (scientific) question or problem.
#    As each investigation and each analysis must have a clear purpose.
#
# 2. KNOW YOUR DATA.
#    Get acquainted with your data: read the documentation and do some
#    exploration on your machine.
#    Know the dimensions of dataset.
#    Know data types of variables. PCA can be performed on numeric variables
#    only. ID variables should not be included in PCA analysis too.
#    Do descriptive statistics, when possible/reasonable.
#    Inspect if missing (NA) values are present and how many of them.
#
# 3. PRE-PROCESS DATA.
#    Do proper data pre-processing.
#    PCA requires mean-centering (almost always) and
#    scaling (in most situations).
#    Before these, sometimes other types of pre-processing might be needed,
#    e.g.,
#      removal of outliers,
#      logarithmic transformation,
#      removal of zero-variance variables,
#      dummy-coding,
#      etc.
#
# 4. CREATE PCA MODEL.
#    Chose PCA algorithm which is the most suitable in your situation and
#    create a PCA model.
#
# 5. MODEL DIAGNOSTICS / FEASIBILITY.
#    Investigate if PCA does its job well enough. Usually investigation of
#    eigenvalues, percentage of explained variance and scree plots is used
#    for this purpose. In some situations when there are just a few (e.g., <20)
#    variables are present, correlation coefficient matrix can be assessed
#    before PCA as an additional step.
#
# 6. INTERPRET THE RESULTS.
#    You should plot PCA plots of interest and properly interpret the results.
#    More details on how to analyze PCA plots are present in the text below.
#
# 7. Perform additional analysis, if needed.
#    E.g., add new data to PCA plots and interpret the new results again.
#    Use a subset of the most relevant variables and repeat the analysis.
#    Etc.


# NOTE!
# PCA is a type of an EXPLORATORY and not a confirmatory analysis.
# So its results MUST be treated appropriately.


## Recommended resources on PCA ----------------------------------------------

# For more information on PCA, you should explore these sources of information:

# [EN]:
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html
# https://rpkgs.datanovia.com/factoextra/reference/index.html



# PCA Example 2: Banknote =====================================================

## Data ----------------------------------------------------------------------

data(banknote, package = "mclust")

# The data set contains six measurements made on 100 genuine and 100 counterfeit
# old-Swiss 1000-franc banknotes. More information in the documentation:

help(banknote, package = "mclust")


## QUESTION / PROBLEM --------------------------------------------------------

# Exploratory PCA analysis may help answering to the following questions:
#
# 1. Do numeric properties of banknotes reveal some inner structure (clusters)
#    of data?
# 2. If yes, do these clusters match the status of banknotes?
# 3. If yes, which variables have most influence on the structure?
#    Which variables have most relevant and which most redundant information?


## KNOW YOUR DATA ------------------------------------------------------------

# Previously be read the description of the dataset.
# Now we'll "look" at data in R.
dim(banknote)
glimpse(banknote)

View(banknote)

map_lgl(banknote, is.numeric)
count(banknote, Status)


## PRE-PROCESSING ------------------------------------------------------------

### Remove (or dummy-recode) factor variables --------------------------------

# Several ways to do the same thing.
banknote_num <- banknote %>% select(-Status)

banknote_num <- banknote %>% select(where(is.numeric))

glimpse(banknote_num)


## FEASIBILITY ---------------------------------------------------------------

# We'll now explore the feasibility of PCA for the dataset.


### Evaluate correlation in the dataset --------------------------------------

# If there is no correlation in data, you should not use PCA
banknote_num %>% cor() %>% round(digits = 2)

ggstatsplot::ggcorrmat(banknote_num) +
  scale_y_discrete(limits = rev)


## CREATE PCA MODEL -----------------------------------------------------------

# Principal Component Analysis
banknote_pca <- prcomp(banknote_num, center = TRUE, scale = TRUE)


### Exploration of 'prcomp' object structure ----------------------------------

# We can explore the PCA object, if we need.
# This section on PCA object structure is for educational purposes only and
# in read-world analysis may be skipped.

# Here is a printed version of the object:
banknote_pca

# The printed information does not reflect the structure of the object.
# The object is a list of class `prcomp`:
is.list(banknote_pca)
class(banknote_pca)

# The list has the following fields
names(banknote_pca)


# We can look at some contents of each list element.
# `x` contains PCA coordinates of the objects used to create the PCA model.
map(banknote_pca, head)


## MODEL DIAGNOSTICS ---------------------------------------------------------

# Eigenvalue analysis of PCA model.
#
# Eigenvalue is a squared standard deviation (i.e., variance) of a principal
# component (PC). The more variance is concentrated on the first PCs, the more
# effectively those PCs represent data.
#
# By investigating eigenvalues of PCs, you should answer to questions like
# these:
#  1. How many PCs do I need to represent at least x% (e.g., 80%) of variance?
#  2. How much variance is represented by the first 2 PCs (3 PCs, 5 PCs, etc.)?
#     Is it enough?
#  3. How many PCs should I use in the further stages of my analysis?
#
#
# Helper rules to choose the number of components:
#
#  1. At least 80% explained variance (other percentage could be used too);
#  2. „Elbow“ method;
#  3. Select eigenvalues above 1 (for scaled data only).
#
#  You may use the "broken stick" rule, fractal dimensions, etc. too.
#  (They are not covered here.)
#
#  NOTE!
#  Choice of the number of principal components to keep is a SUBJECTIVE
#  decision.


# Numeric diagnostics
get_eigenvalue(banknote_pca) %>% round(1)

# Graphical diagnostics
fviz_screeplot(banknote_pca, addlabels = TRUE, choice = "eigenvalue")
fviz_screeplot(banknote_pca, addlabels = TRUE, choice = "variance")


# "Eigenvalue > 1" and "more that 80% explained variance" rules suggest that
# 2-3 components could be enough in this case.
#
# NOTE!
# We can update our decision after exploring the results in the following
# section too.


## RESULTS --------------------------------------------------------------------

# We should plot and analyze the results.


### A. Plot objects: PCA objects map ------------------------------------------

# The PURPOSE of this plot is to explore if there are any patterns between data
# points and if there are, which PC's have most influence on this.


# While analyzing PCA objects map, we should answer to the questions:
#
# 1. Which PC's are there in the plot?
#    Which PC is represented by which axis?
#
# 2. How much variance is explained by the visualized PCs?
#    Is it enough for a good enough representation of data?
#
# 3. Is there a visible structure or visible patterns in data?
#    Are there any visible groups of points (clusters)?
#
# 4. Do point distribution ("spatial") patterns match any known grouping or
#    other known tendency?
#    (I.e., if we add colors of known groups or for values of a continuous
#     variable, do they match the patterns in the distribution of points?
#    )
#

# By default, the space of the first 2 PCs is drawn.

fviz_pca_ind(banknote_pca, geom = "point")

fviz_pca_ind(banknote_pca, geom = "point", alpha.ind = 0.3)

fviz_pca_ind(banknote_pca, geom = "point", habillage = banknote$Status)

fviz_pca_ind(
  banknote_pca,
  axes = c(1, 2),
  geom = "point",
  habillage = banknote$Status,
  addEllipses = TRUE,
  alpha.ind = 0.3
)

# NOTE: There is just an example below to compare with the previous plots.
# It shows an example, when "spatial" structure of the points (clusters in this
# case) does not match colors.
fviz_pca_ind(
  banknote_pca,
  geom = "point",
  habillage = sample(rep(c("A", "B"), times = 100)),
  palette = c("black", "tan4"),
  addEllipses = TRUE
) +
  labs(
    shape = "Random groups",
    color = "Random groups",
    title = "Example: random groups"
  )


# Show 1 and 3 PCs
fviz_pca_ind(banknote_pca, axes = c(1, 3), geom = "point")

fviz_pca_ind(
  banknote_pca,
  axes = c(1, 3),
  geom = "point",
  habillage = banknote$Status
)

# Show 2 and 3 PCs
fviz_pca_ind(banknote_pca, axes = c(2, 3), geom = "point")

fviz_pca_ind(
  banknote_pca,
  axes = c(2, 3),
  geom = "point",
  habillage = banknote$Status
)


# The first two PCs explain around 70% percent of the original variance.
# In the plot, two clusters of more densely situated points separated by a gap
# with just a few points are visible. Addition of colors to the points
# indicated, that these clusters almost perfectly just with a small overlap
# match genuine and counterfeit banknote groups.
#
# Using space of other than the first two PCs do not improve this
# interpretation.



### B. Plot variables: correlation circle -------------------------------------

# The PURPOSE of this plot is to describe relationships between variables and
# between variables and PCs.

# This type of plot is used usually when the number of variables is small
# enough. E.g., < 20. Otherwise only a subset of variables could be plotted,
# only the variables of interest may be highlighted. Is some situations th
# plot may not be used at all.

# While analyzing a correlation circle plot, we should answer to the questions:
#
# 1. Which PC's are there in the plot?
#    Which PC is represented by which axis?
#
# 2. How much variance is explained by the visualized PCs?
#    Is it enough for a good enough representation of data?
#
# 3. Are variables represented well enough in the space of these PCs?
#    (I.e., what is the length of the arrows? Are the arrows long enough?)
#
# 4. How are the variables related to each other?
#    Which have the most similar, which the most different information?
#    (I.e., what is the angle between the the arrows?
#     Which are most perpendicular (orthogonal)?
#     Which are most parallel (colinear)?
#    )
#
# 5. How are the variables related to principal components?
#    Which PCs are determined by which variables?
#    What is the strength of correlation between a variable and a PC?
#    (I.e. Analyze variable (arrow) projection lengths and projection directions
#    on PC axes).
#
#
# By default, the space of the first 2 PCs is drawn.
#
# NOTE!
#  You must see a CIRCLE and not an oval in the plot. Otherwise the
#  interpretation of arrow lengths and angles will re inaccurate and
#  misleading.
#
# Long arrows (their tips touch the circle) indicate that the variables are
# well represented in the space of visualized PCs.
#
# The shorter the arrow is, the worse representation of that variable in the
# space of visualized PCs is.
# Short arrows may mean, that:
#   - EITHER the variables are not important
#   - OR that you should explore the space of other PCs too.
#
#
# The same (or opposite) direction of arrows show that the projections
# of variables are positively (or negatively) correlated in the space of
# visualized PCs.
#
# Perpendicular direction shows that the projections are uncorrelated in the
# space of the visualized PCs.
#
# NOTE!
#  The results in a correlation matrix and in a correlation circle plot MAY
#  NOT MATCH perfectly: one method can show stronger correlation than the other.
#  The reason is that the matrix contains information about the whole dataset.
#  And some variance is excluded from the correlation circle plot (the point of
#  PCA to remove some "unimportant" variance).
#  The only thing that must match is the sign (+ or -) of a correlation
#  coefficient.
#  The more variance is explained by PCs in the plot, the more the results in
#  the plot should match the results in the matrix.
#

fviz_pca_var(banknote_pca)

fviz_pca_var(banknote_pca, axes = c(1, 3))

fviz_pca_var(banknote_pca, axes = c(2, 3))



# In the space of PC1 and PC2, it seems that `Bottom` and `Diagonal` have the
# strongest correlation. The correlation is negative as the arrows point to
# opposite directions. `Left` and `Right` seem to have the strongest positive
# correlation (arrows point to almost the same direction) while `Length` and
# `Top` seems to be almost uncorrelated in the space of the first two PCs as
# their arrows form angle which is close to 90°.
#
#
# In the space of PC1 and PC2, the length of arrow `Top` is shortest.
# It's projection on PC1 is longer and points to the left which means negative
# correlation of `Top` and PC1. The projection on PC2 axis is shorter.
# This means that PC1 explains variance of `Top` better than PC 2, but as the
# arrow's length is just around a half distance to from point (0, 0) to the
# boundary of circle, this means that some of `Top`s variance is explained by
# other PCs. In the space of PC1 and PC3 we see that `Top`s arrow is almost
# touching the boundary of the circle, this means that `Top`s variance is mostly
# explained by PC1 and PC3.
#
# In the space of PC1 and PC2, the length of `Length`'s arrow is one of the
# longest and the arrow's direction is almost parallel to the direction of PC2
# axis. In the space of PC1 and PC3, the length of arrow is almost zero.
# These facts mean that `Length`'s variance is mostly explained by PC2, and
# not by PC1 and PC3.



### C. Biplot: Plot both objects and variables ----------------------------------

# If there are some patterns visible in PCA objects map, the PURPOSE of biplot
# is to explore which variables have the most relevant information.
#
# In other words, the purpose is to explore, which variables determine the
# patterns and which are the most important/unimportant for this purpose.


# While analyzing a biplot, we should answer to the questions:
#
# 1. Which PC's are there in the plot?
#    Which PC is represented by which axis?
#
# 2. How much variance is explained by the visualized PCs?
#    Is it enough for a good enough representation of data?
#
# 3. Which variables determine the structure visible in the plot?
#    Which variables are the most influential to form the patterns of data
#    points?
#    Which variables are not related to the structure?

# By default, the space of the first 2 PCs is drawn.


fviz_pca_biplot(banknote_pca, label = "var")

fviz_pca_biplot(
  banknote_pca,
  label     = "var",
  # geom.ind  = "point",
  habillage = banknote$Status,
  legend    = "top",
  alpha.ind = 0.3
)

# It seems, that variables `Diagonal`, `Top` and `Bottom` have the most relevant
# information to form clusters. In the space of PC1 and PC2, these variables
# seem to be highly correlated.


## Add new data ---------------------------------------------------------------

# We can compute principal component scores (coordinate in the space of PCs)
# for new data.

new_banknotes <- tibble(
  Length   = c(214, 216, 215),
  Left     = c(130, 128, 129),
  Right    = c(132, 129, 130),
  Bottom   = c( 12,   7,   8),
  Top      = c( 12,   8,  10),
  Diagonal = c(138, 142, 140)
)
new_banknotes

# Represent variables as PCs (principal components)
pca_coord_new_points <- predict(banknote_pca, new_banknotes)
pca_coord_new_points

# Add new data points to the space of PCs
fviz_pca_ind(banknote_pca, geom = "point", habillage = banknote$Status) %>%
  fviz_add(pca_coord_new_points, color = "black", repel = TRUE)



## Further exploration: `Diagonal`, `Top` and `Bottom` -----------------------

#> "It seems, that variables `Diagonal`, `Top` and `Bottom` have the most
#> relevant information to form clusters."

# We will explore this idea further by taking a subset of variables `Diagonal`,
# `Top` and `Bottom`, which seem to have the most relevant information.
# Other variables (except the `Status`) will not be included in the analysis at
# all.


### Perform PCA again --------------------------------------------------------

# Let's perform PCA again. It is not necessary to include all the variables.
# Especially those which do not have relevant information.

pca_2 <-
  banknote %>%
  select(Top, Bottom, Diagonal) %>%
  prcomp(scale = TRUE)

# The same as fviz_screeplot()
fviz_eig(pca_2, addlabels = TRUE)


fviz_pca(pca_2, label = "var", habillage = banknote$Status, alpha.ind = 0.3)

# Note, that by using fewer variables, but those which contain more relevant
# information, better results might be achieved. In our case, with 3 variables
# in PCA, the gap between the clusters is more evident.

# Next, it seems that `Diagonal` is the most important variable to identify the
# clusters. And `Top` as well as `Bottom` have some additional information.


fviz_pca_var(pca_2)
# `Top` and `Bottom` seem to be almost uncorrelated in this plot, and
# `Diagonal` seem to have negative almost equally strong correlation with
# the before mentioned variables in the space of PC1 and PC2.

# The contribution of variables in percentages (the lengths of arrows and arrow
# projections) can be explored with function fviz_contrib(). We did not cover these plots before, so you may skip them if you want.
ggpubr::ggarrange(
  align = "hv",
  fviz_contrib(pca_2, "var", axes = 1) + ggtitle("PC1"),
  fviz_contrib(pca_2, "var", axes = 2) + ggtitle("PC2"),
  fviz_contrib(pca_2, "var", axes = c(1, 2)) + ggtitle("PC1 & PC2")
)


# Let's plot all the PCs in 3D

pca_2$x %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~PC1, y = ~PC2, z = ~PC3, color = banknote$Status,
    marker = list(opacity = 0.8, size = 3)
  ) %>%
  plotly::add_markers() %>%
  plotly::layout(title = "PCA objects map (3D)")


### Original dataset ---------------------------------------------------------

# Let's start by exploring the original variables.

ggpubr::ggarrange(common.legend = TRUE, nrow = 1,
  # `Diagonal`
  ggplot(banknote, aes(x = Status, y = Diagonal, fill = Status)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.4, width = 0.2),

  # `Bottom` is inversely correlated to `Diagonal`
  ggplot(banknote, aes(x = Status, y = Bottom, fill = Status)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.4, width = 0.2),

  # `TOP` is inversely correlated to `Diagonal`
  ggplot(banknote, aes(x = Status, y = Bottom, fill = Status)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.4, width = 0.2)
)


# Let's plot all 3 variables
banknote %>%
  as.data.frame() %>%
  plotly::plot_ly(
    x = ~Diagonal, y = ~Top, z = ~Bottom, color = ~Status,
    marker = list(opacity = 0.8, size = 3)
  ) %>%
  plotly::add_markers() %>%
  plotly::layout(title = "3D plot")


# Now let's look at pairs of scatterplots

ggpubr::ggarrange(align = "hv", common.legend = TRUE,

  ggplot(banknote, aes(x = Diagonal, y = Top, fill = Status)) +
    geom_point(alpha = 0.3, pch = 21),

  ggplot(banknote, aes(x = Bottom, y = Top, fill = Status)) +
    geom_point(alpha = 0.3, pch = 21),

  ggplot(banknote, aes(x = Diagonal, y = Bottom, fill = Status)) +
    geom_point(alpha = 0.3, pch = 21)
)

# It seems that a combination of variables `Diagonal` and `Top` contain the most
# relevant information to identify genuine and counterfeit old 1000 Swiss frank
# banknotes: only a single point of genuine banknote belongs to a wrong cluster.

# However, this dataset could be better explored and this question could be
# better answered by performing binary classification analysis (which is not
# covered here). Our insights from PCA could be used as supplementary
# information to achieve better results.
