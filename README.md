<div class="container-fluid main-container">

<div id="header">

# fusionACS

</div>

<div id="overview" class="section level3">

### Overview

A large amount of data concerning the experiences and wellbeing of
American households is collected by surveys. Household surveys typically
focus on a single topic – e.g. finances, housing, health – and field
independent and relatively small samples. As a result, data users are
often constrained by the particular variables and spatial resolution
available in a single survey.

The fusionACS data science platform <span class="citation">([Ummel et
al. 2024](#ref-Ummel2024))</span> helps address this problem by
statistically “fusing” microdata from disparate surveys to simulate a
single, integrated, high-resolution survey. The resulting fused
microdata can be used to perform analyses that would otherwise be
impossible. At its core, fusionACS seeks to maximize the amount of
useful information that can be extracted from the existing array of U.S.
survey data.

In 2025, an enhanced version of fusionACS was introduced that integrates
UrbanPop, a synthetic population data product produced by Oak Ridge
National Laboratory <span class="citation">([Tuccillo et al.
2023](#ref-Tuccillo2023))</span>. UrbanPop provides probabilistic
estimates of the location (block group) of each ACS respondent
household. The fusionACS + UrbanPop platform is able to generate
estimates for any donor survey variable for locales as small as
individual census block groups.

</div>

<div id="methodology" class="section level3">

### Methodology

fusionACS uses the American Community Survey (ACS) – the largest U.S.
household survey – as the “data backbone” of the fusion process.
Variables in “donor” surveys are fused onto ACS Public Use Microdata
Sample (PUMS) microdata to produce simulated values for variables unique
to the donor. This generates probabilistic estimates of how ACS
respondents might have answered a donor survey’s questionnaire.
Respondent characteristics that are common to both the donor and the ACS
(e.g. income, age, household size) – as well as spatial information that
can be merged to both (e.g. characteristics of the local built
environment) – are used as predictors variables in LightGBM machine
learning models <span class="citation">([Ke et al.
2017](#ref-Ke2017))</span>.

</div>

<div id="references" class="section level3 unnumbered">

### References

<div id="refs" class="references csl-bib-body hanging-indent" data-entry-spacing="0">

<div id="ref-Ke2017" class="csl-entry">

Ke, Guolin, Qi Meng, Thomas Finley, Taifeng Wang, Wei Chen, Weidong Ma,
Qiwei Ye, and Tie-Yan Liu. 2017. <span>“LightGBM: A Highly Efficient
Gradient Boosting Decision Tree.”</span> In *Advances in Neural
Information Processing Systems 30*, 3149–57.
<https://papers.nips.cc/paper/6907-lightgbm-a-highly-efficient-gradient-boosting-decision-tree>.

</div>

<div id="ref-Tuccillo2023" class="csl-entry">

Tuccillo, Joseph V., Robert Stewart, Amy Rose, Nathan Trombley, Jessica
Moehl, Nicholas Nagle, and Budhendra Bhaduri. 2023. <span>“UrbanPop: A
Spatial Microsimulation Framework for Exploring Demographic Influences
on Human Dynamics.”</span> *Applied Geography* 151: 102844.
<https://doi.org/10.1016/j.apgeog.2022.102844>.

</div>

<div id="ref-Ummel2024" class="csl-entry">

Ummel, Kevin, Miguel Poblete-Cazenave, Karthik Akkiraju, Nick Graetz,
Hero Ashman, Cora Kingdon, Steven Herrera Tenorio, Aaryaman Sunny
Singhal, Daniel Aldana Cohen, and Narasimha D. Rao. 2024.
<span>“Multidimensional Well-Being of US Households at a Fine Spatial
Scale Using Fused Household Surveys.”</span> *Scientific Data* 11 (142).
<https://doi.org/10.1038/s41597-023-02788-7>.

</div>

</div>

</div>

</div>
