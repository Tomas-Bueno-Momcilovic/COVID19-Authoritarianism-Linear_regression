Author: Tomas Bueno dos Santos Momcilovic
Date: 16.3.2021

This is a replication package for the final project in the (POL63100) Quantitative Methods lab at the Technical University of Munich.

The project represents an implementation of a simple multiple regression model that estimates the effectiveness of COVID-19 pandemic response based on crude death rate, calculated from the reported COVID-19 deaths and the measure of excess deaths during the March 2020 - February 2021 period. Multiple explanatory and outcome variables were used to estimate the effect found by using different estimates of crude death (i.e. outcome variables) and different indices of political authoritarianism (i.e. explanatory variables).

## VARIABLES

#### Explanatory variables:
used one at a time

-CSP Revised Polity Score (Center for Systemic Peace, 2020)

-EIU Democracy Index 2019 (Economist Intelligence Unit, 2020)

-FH Democracy Index 2020 (Freedom House, 2021)

-FI Economic Freedom Index 2018 (Fraser Institute, 2018)

-VDEM Regime Classification (Varieties of Democracy Institute, 2021)

#### Outcome variables:
used one at a time

-COVID-19 Crude Death Estimate (John Hopkins University, 2021)

-Excess deaths for 11 months (Roser et al., 2021)

-Excess deaths for 12 months (Roser et al., 2021)

Control variables (see report):

-Population-related

-Geographic and economic

-Health

-Political

#### Data used: 
see report for all references

-Center for Systemic Peace. (2018). Polity5: Regime Authority Characteristics and Transitions Datasets. Retrieved from https://www.systemicpeace.org/inscrdata.html

-Coppedge, M., Gerring, J., Knutsen, C. H. et al. (2021). V-Dem Dataset v11

Varieties of Democracy Project. DOI: https://doi.org/10.23696/vdems21

-Economist Intelligence Unit. (2019). Democracy Index 2019. EIU. Retrieved from https://www.eiu.com/topic/democracy-index

-Fraser Institute. (2020). Economic Freedom of the World: 2020 Annual Report. Fraser Institute. Retrieved from https://www.fraserinstitute.org/studies/economicfreedomof-the-world-2020-annual-report

-Freedom House. (2020). Countries and Territories - Global Freedom Scores,
Democracy Scores. Freedom House. Retrieved on 13th December 2020 from
https://freedomhouse.org/countries/freedom-world/scores

-John Hopkins University. (2020). COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University. Github. Retrieved on 12th December 2020 from https://github.com/CSSEGISandData/COVID19

-Roser, M., Ritchie, H., Ortiz-Ospina, E. & Hasell, J. (2021). Excess mortality during the Coronavirus pandemic (COVID-19). Our World in Data. Retrieved on 9th March 2021 from https://ourworldindata.org/excess-mortality-covid


Limitations:
-Computation intensive for loops
-Only an exercise in linear modeling, valid conclusions limited

