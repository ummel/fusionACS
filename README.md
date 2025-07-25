::::::::::: {.container-fluid .main-container}
::: {#header}
# fusionACS {#fusionacs .title .toc-ignore}
:::

::: {#project-overview .section .level3}
### Project overview

A large amount of data concerning American households is collected by
surveys. Individual surveys typically focus on a single topic --
e.g. finances, housing, or health -- and field independent and
relatively small samples. As a result, data users are constrained by the
particular variables and spatial resolution available in a single
survey. The fusionACS data science platform [([Ummel et al.
2024](#ref-Ummel2024))]{.citation} addresses this problem by
statistically "fusing" variables from disparate surveys to simulate a
single, integrated, high-resolution survey.

Variables unique to "donor" surveys are simulated for American Community
Survey (ACS) households and individuals, thereby generating
probabilistic estimates of how ACS respondents might have answered a
donor survey's questionnaire. Respondent characteristics that are common
to both the donor and the ACS (e.g. income, age, household size) -- as
well as spatial information that can be merged to both
(e.g. characteristics of the local built environment) -- are used as
predictors variables in LightGBM machine learning models [([Ke et al.
2017](#ref-Ke2017))]{.citation}.

In 2025, an enhanced version of fusionACS was introduced that integrates
UrbanPop, a synthetic population data product produced by Oak Ridge
National Laboratory [([Tuccillo et al.
2023](#ref-Tuccillo2023))]{.citation}. UrbanPop provides probabilistic
estimates of the location of each ACS respondent household. The
fusionACS + UrbanPop platform makes it possible to derive estimates at
high spatial resolution -- even at the level of individual census block
groups -- for any ACS or donor survey variable.

The fusionACS project seeks to maximize the amount of useful information
that can be extracted from existing U.S. household survey data in order
to help academics and policymakers answer research questions that would
otherwise be impossible.
:::

::: {#how-to-access .section .level3}
### How to access

The fusionACS *R* package allows users to access and analyze a
"pseudo-sample" of the complete fusionACS database stored within the
Yale High Performance Computing (HPC) facility. The complete database --
consisting of multiple fusion implicates and integration of the ORNL
UrbanPop synthetic population -- is prohibitively large for public
dissemination and contains some data that cannot be released. The goal
of the fusionACS package is to enable users to perform exploratory
analysis, and design, refine, and test analyses using the full suite of
variables available in the private database.

The pseudo-sample contains a single record (observation) for each ACS
respondent (households and persons) for the period 2015-2019. Each
record includes actual/observed values for all variables in the ACS, as
well as a single simulation of each variable fused from donor surveys.
As a result, the pseudo-sample plausibly mimics how ACS respondents
might have answered questions unique to the donor survey questionnaires.

The sample also includes a range of geographic variables -- from census
region down to individual block groups -- for each ACS household.
Geographic variables smaller than Public Use Microdata Areas (PUMA's)
are obtained by random sampling of the underlying UrbanPop data in such
a way that all block groups nationwide are represented.

While the sample data cannot be used to derive "final"
production-quality estimates, it *can* be used to do just about
everything else a user might normally do during the analysis development
and prototyping stages. And for analyses that are not hyper-specific
(i.e. involve a comparatively large number of observations) the
estimates derived from the pseudo-sample will often be quite close to
the true values computed on the full database.

Our hope is that it will eventually be possible to remotely execute a
valid analysis -- designed and tested locally -- using the complete
fusionACS database and return the full "production" results to the user.
:::

::::::: {#references .section .level3 .unnumbered}
### References {.unnumbered}

:::::: {#refs .references .csl-bib-body .hanging-indent entry-spacing="0"}
::: {#ref-Ke2017 .csl-entry}
Ke, Guolin, Qi Meng, Thomas Finley, Taifeng Wang, Wei Chen, Weidong Ma,
Qiwei Ye, and Tie-Yan Liu. 2017. "LightGBM: A Highly Efficient Gradient
Boosting Decision Tree." In *Advances in Neural Information Processing
Systems 30*, 3149--57.
<https://papers.nips.cc/paper/6907-lightgbm-a-highly-efficient-gradient-boosting-decision-tree>.
:::

::: {#ref-Tuccillo2023 .csl-entry}
Tuccillo, Joseph V., Robert Stewart, Amy Rose, Nathan Trombley, Jessica
Moehl, Nicholas Nagle, and Budhendra Bhaduri. 2023. "UrbanPop: A Spatial
Microsimulation Framework for Exploring Demographic Influences on Human
Dynamics." *Applied Geography* 151: 102844.
<https://doi.org/10.1016/j.apgeog.2022.102844>.
:::

::: {#ref-Ummel2024 .csl-entry}
Ummel, Kevin, Miguel Poblete-Cazenave, Karthik Akkiraju, Nick Graetz,
Hero Ashman, Cora Kingdon, Steven Herrera Tenorio, Aaryaman Sunny
Singhal, Daniel Aldana Cohen, and Narasimha D. Rao. 2024.
"Multidimensional Well-Being of US Households at a Fine Spatial Scale
Using Fused Household Surveys." *Scientific Data* 11 (142).
<https://doi.org/10.1038/s41597-023-02788-7>.
:::
::::::
:::::::
:::::::::::
