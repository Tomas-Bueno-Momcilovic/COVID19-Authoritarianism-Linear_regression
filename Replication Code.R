##### PACKAGES #####

library(devtools)
#devtools::install_github("RamiKrispin/coronavirus")
devtools::install_github("vdeminstitute/vdemdata")
devtools::install_github("ChandlerLutz/starpolishr")

library(tidyverse)
library(ggplot2)
library(readxl)
library(countrycode)
library(estimatr)
library(stargazer)
library(crudeDataTools)
library(MASS)
library(sandwich)
library(caret)
library(sm)
library(xtable)
library(starpolishr)
library(GGally)

#Creating the NOT IN function
`%notin%` <- Negate(`%in%`)

##### LOADING DATA #####

# COVID-19 DATA
jhu_covid <- read.csv("./Data/JHU Coronavirus/coronavirus.csv")
owid_excess <- read.csv("./Data/JHU Coronavirus/owid_excess_mortality.csv")

# DEMOCRACY INDICES
csp_polity <- read_xlsx("./Data/Polity Score/CSP Polity5 Score 2018.xlsx")
fh_demos <- read_xlsx("./Data/Demos & Freedom Indices/FH Democracy Score 2020.xlsx")
eiu_demos <- read_xlsx("./Data/Demos & Freedom Indices/EIU Democracy Index 2019.xlsx")

# FREEDOM INDICES
fh_press <- read_xlsx("./Data/Demos & Freedom Indices/FH Freedom of Press 2017.xlsx")
fh_global <- read_xlsx("./Data/Demos & Freedom Indices/FH Global Freedom Scores 2020.xlsx")
ci_human <- read.csv("./Data/Demos & Freedom Indices/CI Human Freedom Index 2020.csv")
fi_econ <- read_xlsx("./Data/Demos & Freedom Indices/FI Economic Freedom Index 2020.xlsx")
t_popul <- read_xlsx("./Data/Demos & Freedom Indices/T Populism Index 2018.xlsx")

# STATE FRAGILITY INDICES
ffp_sf <- read_xlsx("./Data/Fragile States Indices/FFP Fragile States Index 2020.xlsx")
csp_sf <- read_xlsx("./Data/Fragile States Indices/CSP State Fragility Index 2018.xlsx")

# POPULATION AND ECONOMIC DATA
un_pop <- read_xlsx("./Data/UN-DESA World Population.xlsx")
oecd_trust <- read.csv("./Data/Controls/oecd_trust.csv")
wb_above65 <- read.csv("./Data/Controls/wb_above65.csv")
wb_urban <- read.csv("./Data/Controls/wb_urban.csv")
wb_lpi <- read_xlsx("./Data/Controls/wb_lpi.xlsx")
wb_gdppc <- read.csv("./Data/Controls/wb_gdppc.csv")

# BMI AND HEALTH DATA
ncd_bmi <- read.csv("./Data/BMI/NCD_BMI_MEAN.csv")
who_tobacco <- read.csv("./Data/Controls/who_tobacco2018.csv")
cepii_dist_china <- read_xlsx("./Data/Controls/cepii_dist_china.xlsx")
oecd_health_gdp <- read.csv("./Data/Controls/oecd_health_gdp.csv")
who_health_gdp <- read_xlsx("./Data/Controls/who_health_gdp.xlsx")
who_health_infra <- read.csv("./Data/Controls/who_health_infra.csv")

# VDEM DATA
vdem <- vdemdata::vdem %>%
  #VDEM codes two different values for Palestine, so we have to exclude
  filter(year == "2019" & !grepl("Palestine",country_name)) %>%
  dplyr::select(c("country_name", "v2x_regime_amb",
           "v2x_polyarchy", "v2xeg_eqdr", "v2x_neopat",
           "v2pehealth", "v2smgovdom")) %>%
  rename(c("vdem.regime"="v2x_regime_amb",
           "vdem.polyarchy"="v2x_polyarchy",
           "vdem.eq_resource"="v2xeg_eqdr",
           "vdem.neopatri"="v2x_neopat",
           "vdem.health_acc"="v2pehealth",
           "vdem.govfakenews"="v2smgovdom"))


##### CLEANING DATA #####

#Add all dataframes to a list
data.list = mget(ls(pattern = "_"))
data.names = ls(pattern = "_")

#'* Country Codes *

un_pop$cc <- countrycode(sourcevar = un_pop$Country, origin = "country.name", destination = "iso3c")
un_pop$cc[un_pop$Country == c("Micronesia")] <- "FSM"
un_pop$cc[un_pop$Country == c("Channel Islands")] <- "CHI"
un_pop$cc[un_pop$Country == c("Saint Martin")] <- "MAF"

fh_demos$cc <- countrycode(sourcevar = fh_demos$Country, origin = "country.name", destination = "iso3c")
fh_demos$cc[fh_demos$Country == "Kosovo"] <- "XKO"

csp_polity$cc <- countrycode(sourcevar = csp_polity$country, origin = "country.name", destination = "iso3c")
csp_polity$cc[csp_polity$country == "Kosovo"] <- "XKO"

eiu_demos$cc <- countrycode(sourcevar = eiu_demos$Country, origin = "country.name", destination = "iso3c")
owid_excess$cc <- countrycode(sourcevar = owid_excess$location, origin = "country.name", destination = "iso3c")
fh_press$cc <- countrycode(sourcevar = fh_press$country, origin = "country.name", destination = "iso3c")
fh_global$cc <- countrycode(sourcevar = fh_global$Country, origin = "country.name", destination = "iso3c")
fi_econ$cc <- countrycode(sourcevar = fi_econ$Countries, origin = "country.name", destination = "iso3c")
ci_human$cc <- countrycode(sourcevar = ci_human$countries, origin = "country.name", destination = "iso3c")
t_popul$cc <- countrycode(sourcevar = t_popul$country, origin = "country.name", destination = "iso3c")
ffp_sf$cc <- countrycode(sourcevar = ffp_sf$Country, origin = "country.name", destination = "iso3c")
csp_sf$cc <- countrycode(sourcevar = csp_sf$country, origin = "country.name", destination = "iso3c")
ncd_bmi$cc <- countrycode(sourcevar = ncd_bmi$country, origin = "country.name", destination = "iso3c")
vdem$cc <- countrycode(sourcevar = vdem$country_name, origin = "country.name", destination = "iso3c")
oecd_trust$cc <- countrycode(sourcevar = oecd_trust$country, origin = "country.name", destination = "iso3c")
who_tobacco$cc <- countrycode(sourcevar = who_tobacco$ï..Country.Name, origin = "country.name", destination = "iso3c")
oecd_health_gdp$cc <- countrycode(sourcevar = oecd_health_gdp$ï..Country, origin = "country.name", destination = "iso3c")
wb_above65$cc <- countrycode(sourcevar = wb_above65$ï..Country.Name, origin = "country.name", destination = "iso3c")
wb_gdppc$cc <- countrycode(sourcevar = wb_gdppc$ï..Country.Name, origin = "country.name", destination = "iso3c")
wb_lpi$cc <- countrycode(sourcevar = wb_lpi$Country, origin = "country.name", destination = "iso3c")
wb_urban$cc <- countrycode(sourcevar = wb_urban$ï..Country.Name, origin = "country.name", destination = "iso3c")
who_health_gdp$cc <- countrycode(sourcevar = who_health_gdp$countries, origin = "country.name", destination = "iso3c")
who_health_infra$cc <- countrycode(sourcevar = who_health_infra$country, origin = "country.name", destination = "iso3c")

cepii_dist_china$cc <- countrycode(sourcevar = cepii_dist_china$iso_o, origin = "iso3c", destination = "iso3c")
cepii_dist_china$cc[cepii_dist_china$iso_o == "PAL"] <- "PSE"
cepii_dist_china$cc[cepii_dist_china$iso_o == "ROM"] <- "ROU"
cepii_dist_china$cc[cepii_dist_china$iso_o == "ZAR"] <- "COD"
cepii_dist_china$cc[cepii_dist_china$iso_o == "TMP"] <- "TLS"
cepii_dist_china$cc[cepii_dist_china$iso_o == "YUG"] <- "SRB"

#'* JHU_COVID, UN_POP and OWID_EXCESS Transformations *

#Neutral fix
un_pop$`est2020-1k`[un_pop$Country == "Serbia"] <- 6926.705

#Joining
jhu_pop <- jhu_covid %>%
  #Filter by 2020 only
  mutate(date = as.Date.character(jhu_covid$date, format = "%d-%m-%y", tryFormats = c("%d-%m-%y"))) %>%
  filter(date <= '2020-12-31') %>%
  #Sum up by type and retain relevant columns
  group_by(country, type) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  #Join with UN-DESA Population data using country codes
  mutate(cc = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c")) %>%
  spread(key = type, value = cases) %>%
  inner_join(., un_pop, by = "cc") %>%
  #Add real pop estimates, crude rates, and concluded cases
  mutate(est2020 = `est2020-1k` * 1000) %>%
  mutate(concluded = recovered + death,
         crude_death = (death/est2020)*(1e+05), 
         crude_confirmed = (confirmed/est2020)*(1e+05),
         crude_recovered = (recovered/est2020)*(1e+05)) %>%
  mutate(crude_concluded = crude_death + crude_recovered) %>%
  mutate(cfr = (crude_death/crude_concluded)*100)

#Make data for 11 and 12 months from OWID_EXCESS
owid_excess <- owid_excess %>% 
  mutate(date = as.Date.character(date, format = "%d-%m-%y", tryFormats = c("%d-%m-%y"))) %>%
  left_join(., un_pop, by = "cc") %>%
  mutate(est2020 = `est2020-1k` * 1000)


excess11 <- owid_excess %>%
  filter(date <= "2020-11-30") %>%
  group_by(cc) %>%
  summarize(owid.excess11 = ((sum(deaths_2020_all_ages, na.rm = TRUE)-
                                sum(average_deaths_2015_2019_all_ages, na.rm = TRUE))/
                               sum(average_deaths_2015_2019_all_ages, na.rm = TRUE))*100,
            owid.excess11_crude = ((sum(deaths_2020_all_ages, na.rm = TRUE)-
                                      sum(average_deaths_2015_2019_all_ages))/
                                     mean(est2020))*(1e+05))

excess12 <- owid_excess %>%
  filter(location %notin% location[is.na(deaths_2020_all_ages)]) %>%
  group_by(cc) %>%
  summarize(owid.excess12 = ((sum(deaths_2020_all_ages, na.rm = TRUE)-
              sum(average_deaths_2015_2019_all_ages, na.rm = TRUE))/
              sum(average_deaths_2015_2019_all_ages, na.rm = TRUE))*100,
            owid.excess12_crude = ((sum(deaths_2020_all_ages, na.rm = TRUE)-
                                      sum(average_deaths_2015_2019_all_ages))/
                                     mean(est2020))*(1e+05))


##### JOINING DATA #####

master <- jhu_pop %>%
  #Join into a master table
  left_join(., csp_polity, by="cc", suffix = c("",".csp_sf")) %>%
  left_join(., csp_sf, by="cc", suffix = c("",".csp_sf")) %>%
  left_join(., eiu_demos, by="cc", suffix = c("",".eiu_demos")) %>%
  left_join(., ffp_sf, by="cc", suffix = c("",".ffp_sf")) %>%
  left_join(., fh_demos, by="cc", suffix = c("",".fh_demos")) %>%
  left_join(., fh_global, by="cc", suffix = c("",".fh_global")) %>%
  left_join(., fh_press, by="cc", suffix = c("",".fh_press")) %>%
  left_join(., fi_econ, by="cc", suffix = c("",".fi_econ")) %>%
  left_join(., ncd_bmi, by="cc", suffix = c("",".ncd_bmi")) %>%
  left_join(., t_popul, by="cc", suffix = c("",".t_popul")) %>%
  left_join(., vdem, by="cc", suffix = c("",".vdem")) %>%
  left_join(., oecd_trust, by="cc", suffix = c("",".oecd")) %>%
  left_join(., excess11, by="cc", suffix = c("",".owid")) %>%
  left_join(., excess12, by="cc", suffix = c("",".owid")) %>%
  left_join(., cepii_dist_china, by="cc", suffix = c("",".cepii")) %>%
  left_join(., who_tobacco, by="cc", suffix = c("",".who")) %>%
  left_join(., oecd_health_gdp, by="cc", suffix = c("",".oecd")) %>%
  left_join(., wb_above65, by="cc", suffix = c("",".wb")) %>%
  left_join(., wb_gdppc, by="cc", suffix = c("",".wb")) %>%
  left_join(., wb_lpi, by="cc", suffix = c("",".wb")) %>%
  left_join(., wb_urban, by="cc", suffix = c("",".wb")) %>%
  left_join(., who_health_gdp, by="cc", suffix = c("",".who")) %>%
  left_join(., who_health_infra, by="cc", suffix = c("",".who")) %>%
  #Clean and rename
  dplyr::select(-starts_with(c('country.','Country','ci_','Countries'))) %>%
  dplyr::select(-ends_with(c('leg','eff'))) %>%
  dplyr::select(-contains(c('Country','ISO', 'year', 'Year', 
                     'region', 'code', 'rank', 'Rank', 'location'))) %>%
  dplyr::select(-c("A-Legal","B-Political","C-Economic","flag",polity_score,democ,autoc,"est2020-1k")) %>%
  rename(c("un.pop"="est2020","jhu.confirmed"="confirmed",
           "jhu.concluded"="concluded","jhu.death"="death","jhu.recovered"="recovered",
           "jhu.crude_confirmed"="crude_confirmed","jhu.crude_concluded"="crude_concluded",
           "jhu.crude_death"="crude_death","jhu.crude_recovered"="crude_recovered","jhu.cfr"="cfr",
           "csp.p5"="p5","csp.fragment"="fragment","csp.revised_polity"="revised_polity_score",
           "csp.durable_polity"="durable","csp.fragile2018"="state_fragility2018",
           "csp.fragile_effect"="effect","csp.fragile_legit"="legit","eiu.demos2019"="Score2019",
           "eiu.demos2018"="Score2018","eiu.demosChange"="ChangeScore","ffp.fragile2020"="Total2020",
           "fh.demosPerc"="Democracy Percentage","fh.demosScore2020"="Democracy Score",
           "fh.demos2020"="Total Score","fh.demosStatus"="Status",
           "fh.polRights"="Political Rights","fh.civLiber"="Civil Liberties",
           "fh.globalScore"="Total Score.fh_global","fh.globalStatus"="Status.fh_global",
           "fh.press2016"="score_2016","fh.pressStatus"="Status.fh_press",
           "fi.econ2018"="score2018","ncd.mean_bmi"="mean_bmi","t.popul2020"="populism_vote_share2020",
           "oecd.trust2017"="trustgov2017","cepii.dist_china"="dist_china",
           "cepii.landlocked"="landlocked","who.tobacco"="tobacco_use_perc",
           "wb.lpi_avg1218"="lpi_2012_2018","wb.gdppc2019"="gdppc2019")) %>% 
  mutate(csp.durable_polity = as.numeric(csp.durable_polity)) %>%
  relocate(owid.excess11_crude, .after = last_col()) %>%
  relocate(owid.excess12_crude, .after = last_col()) %>%
  relocate(who.tobacco, .after = last_col()) %>%
  relocate(vdem.health_acc, .after = last_col())

master <- jhu_covid %>%
  filter(cases != 0) %>%
  mutate(date = as.Date.character(date, format = "%d-%m-%y", tryFormats = c("%d-%m-%y"))) %>%
  filter(date <= '2020-12-31') %>%
  group_by(country) %>%
  summarize(jhu.first_case = min(date)) %>%
  right_join(.,master, by = "country") %>%
  relocate(jhu.first_case, .after = last_col())


##### CONTROL PREP #####

#'* CONTROL VARIABLE CORRELATIONS *

#Make a list of control variable names
myvars <- master %>% 
  ungroup() %>%
  dplyr::select(-starts_with('cc')) %>%
  dplyr::select(-contains(c(
    'jhu','fh','eiu','regime',
    'pol','country', 'status'))) %>%
  colnames()

#Create a list of linear models for all controls
fit <- lapply(myvars, function(dvar) 
  lm(as.formula(paste0('jhu.crude_death ~ ',dvar)), data = master))

#Create a list of robust linear models for all controls
fit_robust <- lapply(myvars, function(dvar) 
  lm_robust(as.formula(paste0('jhu.crude_death ~ ',dvar)), data = master))

#Create a list of robust standard errors
se_robust <- lapply(myvars, function(dvar)
  sqrt(diag(vcovHC(
    lm(as.formula(paste0('jhu.crude_death ~ ',dvar)), data = master), 
    type = "HC1")))
)

#'* CONTROL VARIABLE TABLES *

#Create a table with FRAGILITY controls
stargazer(fit[c(3:7)], se = se_robust[c(3:7)], type = "text", 
          out = "./Stargazer/table1.tex",
          df=FALSE,
          dep.var.labels = "jhu.crude\\_death",
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Fragility",
          single.row = TRUE, 
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#Create a table with POPULATION controls
stargazer(fit[c(1,14,20,29,10)], se = se_robust[c(1,14,20,29,10)], type = "text", 
          out = "./Stargazer/table2.tex",
          df=FALSE,
          dep.var.labels = "jhu.crude\\_death",
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Population",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#Create a table with GEO-ECONOMIC controls
stargazer(fit[c(17,18,22,21,23)], se = se_robust[c(17,18,22,21,23)], type = "text", 
          out = "./Stargazer/table3.tex",
          df=FALSE,
          dep.var.labels = "jhu.crude\\_death",
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Geography and Economics",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#Create a table with HEALTH controls
stargazer(fit[c(9,19,24,30,26)], se = se_robust[c(9,19,24,30,26)], type = "text", 
          out = "./Stargazer/table4.tex",
          df=FALSE,
          dep.var.labels = "jhu.crude\\_death",
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Health",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#Create a table with POLITICAL controls
stargazer(fit[c(11,12,13,27,28)], se = se_robust[c(11,12,13,27,28)], type = "text", 
          out = "./Stargazer/table5.tex",
          df=FALSE,
          dep.var.labels = "jhu.crude\\_death",
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Political",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#'* ONE BY ONE STARGAZER *

stargazer(model_list[1:15], se = model_se[1:15], type = "text", 
          out = "./Stargazer/table_obo1.tex",
          df=FALSE,
          dep.var.labels = c("jhu.crude\\_death"),
          column.sep.width = "-15pt",
          font.size = "tiny",
          align = TRUE,title="One-by-One Control Models: JHU COVID-19 Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2, float.env = "sidewaystable")

stargazer(model_list[46:60], se = model_se[46:60], type = "text", 
          out = "./Stargazer/table_obo2.tex",
          df=FALSE,
          dep.var.labels = c("owid.excess12\\_crude"),
          column.sep.width = "-15pt",
          font.size = "tiny",
          align = TRUE,title="One-by-One Control Models: OWID 12-Month Excess Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2, float.env = "sidewaystable")

##### FORMULAS #####

#'* MULTIVARIATE FORMULA *

#     TESTING SUCCESSFUL
#Make a model for each outcome y separately
#Regressed on each x
#With added controls z, one by one
#From data "data"

multivariate <- function(x, y, z, data){
  p=1
  model_list <- list()
  for(i in 1:length(y)) {
    for(j in 1:length(x)) {
      for(k in 0:length(z)){
        model_list[[p]] <- lm(as.formula(
          paste(y[i], sep = " ~ ", 
                paste0(x[j], ifelse(k==0,""," +"),
                       paste0(z[0:k], collapse = " +")
                )
          )),
          data = master)
        p=p+1
      }
    }
  }
  return(model_list)
}

#'* MULTIVARIATE ROBUST FORMULA *

multivariate_robust <- function(x, y, z, data){
  p=1
  model_list <- list()
  for(i in 1:length(y)) {
    for(j in 1:length(x)) {
      for(k in 0:length(z)){
        model_list[[p]] <- lm_robust(as.formula(
          paste(y[i], sep = " ~ ", 
                paste0(x[j], ifelse(k==0,""," +"),
                       paste0(z[0:k], collapse = " +")
                  )
                )),
          data = master)
        p=p+1
      }
    }
  }
  return(model_list)
}

#'* MULTIVARIATE SE FORMULA *

multivariate_se <- function(mod){
  se = list()
  for(m in 1:length(mod)){
    se[[m]] <- sqrt(diag(vcovHC(mod[[m]], type = "HC1")))
  }
  return(se)
}


#     END FORMULA CHUNK

##### LINEAR MODELS #####

#'* MODELS *

#Model 1: cdr ~ polity, no controls
m1.nc <- lm_robust(jhu.crude_death ~ csp.revised_polity, master)
#Model 2: cdr ~ polity + bmi
m2.c <- lm_robust(jhu.crude_death ~ csp.revised_polity +ncd.mean_bmi, master)

#'* CREATING X, Y AND Z MODEL PARAMETERS *

#Create a list of all outcomes
y.outcomes <- master %>%
  ungroup() %>%
  dplyr::select(c(jhu.crude_death,
                  owid.excess11_crude,
                  owid.excess12_crude
                  )) %>%
  colnames()

#Create a list of all variables
x.variables <- master %>%
  ungroup() %>%
  dplyr::select(c(
    'csp.revised_polity',
    'eiu.demos2019',
    'fh.demos2020',
    'fi.econ2018',
    'vdem.regime'
  )) %>%
  colnames()

#Create a list of all controls
z.controls <- master %>% 
  ungroup() %>%
  dplyr::select(-starts_with('cc')) %>%
  dplyr::select(-contains(c(
    'jhu','fh','eiu','regime',
    'pol','country', 'status','un.pop','popul',
    'csp.p5', 'csp.fragile', 'fi.','healthposts',
    'excess','fragment','oecd.gdp_h','cepii'))) %>%
  colnames()


#'*MAKING X*Y*Z NUMBER OF MODELS*
#Make a model list
model_list <- multivariate(x= x.variables, y= y.outcomes, z= z.controls, data= master)

#Make a list of standard errors
model_se <- multivariate_se(model_list)

#Make a list of base models
base_models <- model_list[seq(1,length(x.variables)*length(y.outcomes)*length(z.controls)+
                                length(x.variables)*length(y.outcomes),length(z.controls)+1)]



#Make a list of base model se
base_models_se <- model_se[seq(1,length(x.variables)*length(y.outcomes)*length(z.controls)+
               length(x.variables)*length(y.outcomes),length(z.controls)+1)]

#Make a list of full models
full_models <- model_list[seq(15,length(x.variables)*length(y.outcomes)*length(z.controls)+
                                length(x.variables)*length(y.outcomes),length(z.controls)+1)]

#Make a list of base model se
full_models_se <- model_se[seq(15,length(x.variables)*length(y.outcomes)*length(z.controls)+
                                 length(x.variables)*length(y.outcomes),length(z.controls)+1)]


#'* ADJUSTED MODEL *

#Multicollinearity
multicollinear_models <- multivariate(x = c("vdem.neopatri","vdem.eq_resource",
                                            "vdem.govfakenews","vdem.health_acc"),
                                      y = x.variables,
                                      z = c(),
                                      data = master)

multicollinear_se <- multivariate_se(multicollinear_models)

#
w.adj_controls <- list("ffp.fragile2020","ncd.mean_bmi",
                               "wb.above65_2019","log_gdppc",
                               "log_lpi", "wb.urban_perc2019",
                               "who.gdp_health2018","who.tobacco", "vdem.health_acc")


adj_models <- multivariate(x = x.variables[c(1,2,5)], 
                           y = y.outcomes, 
                           z = w.adj_controls,
                           data = master)

adj_models_se <- multivariate_se(adj_models)


stargazer(adj_models_x[seq(10,90,10)], se= adj_models_se_x[seq(10,90,10)], type = "text")

##### DESCRIPTIVE TABLES #####

#'* VARIABLE DESCRIPTIVE INFO *
descr <- data.frame(name = c("jhu","owid11","owid12",
                             "csp","eiu","fh","fi","vdem"),
                    mean = c(mean(master$jhu.crude_death, na.rm = TRUE),
                             mean(master$owid.excess11_crude, na.rm = TRUE),
                             mean(master$owid.excess12_crude, na.rm = TRUE),
                             mean(master$csp.revised_polity, na.rm = TRUE),
                             mean(master$eiu.demos2018, na.rm = TRUE),
                             mean(master$fh.demos2020, na.rm = TRUE),
                             mean(master$fi.econ2018, na.rm = TRUE),
                             mean(master$vdem.regime, na.rm = TRUE)),
                    sd = c(sd(master$jhu.crude_death, na.rm = TRUE),
                           sd(master$owid.excess11_crude, na.rm = TRUE),
                           sd(master$owid.excess12_crude, na.rm = TRUE),
                           sd(master$csp.revised_polity, na.rm = TRUE),
                           sd(master$eiu.demos2018, na.rm = TRUE),
                           sd(master$fh.demos2020, na.rm = TRUE),
                           sd(master$fi.econ2018, na.rm = TRUE),
                           sd(master$vdem.regime, na.rm = TRUE)))

descr$mean <- round(descr$mean, 1)
descr$sd <- round(descr$sd, 1)

descr_all <- data.frame("Abbreviations" = names(master),
                        "n" = as.numeric(unlist(lapply(master, function(x) length(x[!is.na(x)])))),
                        "Note" = c("Country name",
                                   "ISO3 Country code",
                                   "JHU: Confirmed COVID-19 Cases (Unused)",
                                   "JHU: Confirmed COVID-19 Deaths",
                                   "JHU: Confirmed COVID-19 Recoveries (Unused)",
                                   "UN: Population, 2020",
                                   "JHU: Concluded COVID-19 Cases",
                                   "JHU: Crude Death Rate",
                                   "JHU: Crude Case Rate (Unused)",
                                   "JHU: Crude Recovery Rate (Unused)",
                                   "JHU: Crude Concluded Rate (Unused)",
                                   "JHU: Case Fatality Ratio (Unused)",
                                   "CSP: Polity5 Case Indicator (Unused)",
                                   "CSP: Polity Fragmentation (Unused)",
                                   "CSP: Revised Combined Polity Score (p5)",
                                   "CSP: Regime Durability (p5)",
                                   "CSP: Regime Fragility, 2018",
                                   "CSP: Regime Fragility, Effectiveness",
                                   "CSP: Regime Fragility, Legitimacy",
                                   "EIU: Democracy Index, 2019",
                                   "EIU: Democracy Index, 2018",
                                   "EIU: Democracy Index Change, 2018-2019",
                                   "FFP: Fragility, 2020",
                                   "FH: Democracy Percentage (Unused)",
                                   "FH: Democracy Score (a), 2020 (Unused)",
                                   "FH: Democracy Score (b), 2020",
                                   "FH: Democracy Score Status",
                                   "FH: Political Rights",
                                   "FH: Civil Liberties",
                                   "FH: Global Score",
                                   "FH: Global Score Status",
                                   "FH: Freedom of Press, 2016",
                                   "FH: Freedom of Press Status",
                                   "FI: Economic Freedom, 2018",
                                   "NCD: Mean BMI, 2019",
                                   "T: Populism Index, 2020",
                                   "VDEM: Regimes of the World, 2020",
                                   "VDEM: Multiplicative Polyarchy Index",
                                   "VDEM: Equal Distribution of Resources Index",
                                   "VDEM: Neopatrimonial Rule Index",
                                   "VDEM: Government Dissemin. of False Info Domestic",
                                   "OECD: Trust in Government, 2017",
                                   "OWID: Excess Deaths, 11 Months of 2020",
                                   "OWID: Excess Deaths, 12 Months of 2020",
                                   "CEPII: Distance from China in km",
                                   "CEPII: Landlocked Countries",
                                   "OECD: % GDP Spent on Healthcare, 2018",
                                   "WB: % Population Older than 65, 2019",
                                   "WB: GDP per Capita, 2019",
                                   "WB: Logistics Performance Index, 2012-2018 (average)",
                                   "WB: % Urban Population, 2019",
                                   "WHO: % GDP Spent on Healthcare, 2018",
                                   "WHO: Health Posts per 100k, 2013",
                                   "WHO: Hospitals per 100k, 2013",
                                   "OWID: Crude Excess Death Rate, 11 Months 2020",
                                   "OWID: Crude Excess Death Rate, 12 Months 2020",
                                   "WHO: % Tobacco Use, Adults, 2019",
                                   "VDEM: Health Accessibility Index",
                                   "JHU: First Recorded Case in Country"))


print(xtable(descr_all,
             type = "latex", digits = 0, caption = "All variables"), 
      file = "./Stargazer/descr1.tex",tabular.environment="longtable", 
      size="\\scriptsize", floating = FALSE, caption.placement = "top")

#'* TABLE WITH MASTER DATA *

master$jhu.first_case <- as.character(master$jhu.first_case)
print(xtable(master[,c("country","cc","jhu.first_case","un.pop","csp.revised_polity",
                       "eiu.demos2019","fh.demos2020","fi.econ2018","vdem.regime",
                       "jhu.crude_death","owid.excess11_crude","owid.excess12_crude")],
       type = "latex", digits = 0, caption = "Explanatory and outcome variables per country"), 
      file = "./Stargazer/master_table.tex",tabular.environment="longtable", 
      size="\\scriptsize", floating = FALSE, caption.placement = "top")

#General polity tallies (run the whole chunk)
tally <- master %>%
  group_by("Polity"=round(csp.revised_polity,0)) %>%
  summarize("Count"=n())

tally2 <- master %>%
  filter(!is.na(jhu.crude_death)) %>%
  group_by("Polity"=round(csp.revised_polity,0)) %>%
  summarize(JHU = n(), "Mean JHU" = mean(jhu.crude_death))

tally3 <- master %>%
  filter(!is.na(owid.excess12_crude)) %>%
  group_by("Polity"=round(csp.revised_polity,0)) %>%
  summarize(OWID = n(), "Mean OWID" = mean(owid.excess12_crude))

tally <- tally %>%
  left_join(., tally2, by = "Polity") %>%
  left_join(., tally3, by = "Polity")

rm(tally2, tally3)

print(xtable(tally, type = "latex", 
             caption = "Outcome variables per each csp.revised\\_polity category"), 
      file = "./Stargazer/master_table2.tex", 
      size="\\footnotesize", caption.placement = "top")

#Percentage polity
tally4 <- tally %>%
  group_by(Polity = ifelse(Polity > 0, "Democratic", ifelse(is.na(Polity),"Uncategorized","Not democratic"))) %>%
  summarize(Count = sum(Count), 
            JHU = sum(JHU, na.rm = TRUE), "JHU%" = round((sum(JHU, na.rm = TRUE)/188)*100, 1),
            OWID = sum(OWID, na.rm = TRUE), "OWID%" = round((sum(OWID, na.rm = TRUE)/188)*100, 1))

print(xtable(tally4, type = "latex", 
             caption = "Outcome variables per binary csp.revised\\_polity"), 
      file = "./Stargazer/master_table3.tex", 
      size="\\footnotesize", caption.placement = "top")

#General EIU demos

tally_eiu2 <- master %>%
  filter(!is.na(jhu.crude_death)) %>%
  group_by(Demos = round(eiu.demos2019,0)) %>%
  summarize(JHU = n(), "Mean JHU" = mean(jhu.crude_death))

tally_eiu3 <- master %>%
  filter(!is.na(owid.excess12_crude)) %>%
  group_by(Demos = round(eiu.demos2019,0)) %>%
  summarize(OWID = n(), "Mean OWID" = mean(owid.excess12_crude))

tally_eiu <- master %>% ungroup() %>%
  group_by(Demos = round(eiu.demos2019,0)) %>%
  summarize(Count = n()) %>%
  left_join(., tally_eiu2, by = "Demos") %>%
  left_join(., tally_eiu3, by = "Demos")

rm(tally_eiu2,tally_eiu3)

print(xtable(tally_eiu, type = "latex",
             caption = "Outcome variables per each eiu.demos2019 score"), 
      file = "./Stargazer/master_table4.tex", 
      size="\\footnotesize", caption.placement = "top")


#General FH tallies

tally_fh2 <- master %>%
  filter(!is.na(jhu.crude_death)) %>%
  group_by("FH Status" = ifelse(is.na(fh.demosStatus),"Uncategorized",fh.demosStatus)) %>%
  summarize(JHU = n(), "Mean JHU" = mean(jhu.crude_death))

tally_fh3 <- master %>%
  filter(!is.na(owid.excess12_crude)) %>%
  group_by("FH Status" = ifelse(is.na(fh.demosStatus),"Uncategorized",fh.demosStatus)) %>%
  summarize(OWID = n(), "Mean OWID" = mean(owid.excess12_crude))

tally_fh <- master %>% ungroup() %>%
  group_by("FH Status" = ifelse(is.na(fh.demosStatus),"Uncategorized",fh.demosStatus)) %>%
  summarize(Count = n()) %>%
  left_join(., tally_fh2, by = "FH Status") %>%
  left_join(., tally_fh3, by = "FH Status") %>%
  arrange(match(`FH Status`,c("Consolidated Authoritarian Regime",
                         "Semi-Consolidated Authoritarian Regime",
                         "Transitional or Hybrid Regime",
                         "Semi-Consolidated Democracy",
                         "Consolidated Democracy",
                         NA)))

rm(tally_fh2, tally_fh3)

print(xtable(tally_fh, type = "latex",
             caption = "Outcome variables per fh.demos2020 status"), 
      file = "./Stargazer/master_table5.tex", 
      size="\\footnotesize", caption.placement = "top")


#General FI econ

tally_fi2 <- master %>%
  filter(!is.na(jhu.crude_death)) %>%
  group_by("Econ. Freedom" = round(fi.econ2018,0)) %>%
  summarize(JHU = n(), "Mean JHU" = mean(jhu.crude_death))

tally_fi3 <- master %>%
  filter(!is.na(owid.excess12_crude)) %>%
  group_by("Econ. Freedom" = round(fi.econ2018,0)) %>%
  summarize(OWID = n(), "Mean OWID" = mean(owid.excess12_crude))

tally_fi <- master %>% ungroup() %>%
  group_by("Econ. Freedom" = round(fi.econ2018,0)) %>%
  summarize(Count = n()) %>%
  left_join(., tally_fi2, by = "Econ. Freedom") %>%
  left_join(., tally_fi3, by = "Econ. Freedom")

rm(tally_fi2,tally_fi3)

print(xtable(tally_fi, type = "latex", 
             caption = "Outcome variables per each fi.econ2018 category"), 
      file = "./Stargazer/master_table6.tex", 
      size="\\footnotesize", caption.placement = "top")

#General VDEM regime

tally_vdem2 <- master %>%
  filter(!is.na(jhu.crude_death)) %>%
  group_by("Regime" = vdem.regime) %>%
  summarize(JHU = n(), "Mean JHU" = mean(jhu.crude_death))

tally_vdem3 <- master %>%
  filter(!is.na(owid.excess12_crude)) %>%
  group_by("Regime" = vdem.regime) %>%
  summarize(OWID = n(), "Mean OWID" = mean(owid.excess12_crude))

tally_vdem <- master %>% ungroup() %>%
  group_by("Regime" = vdem.regime) %>%
  summarize(Count = n()) %>%
  left_join(., tally_vdem2, by = "Regime") %>%
  left_join(., tally_vdem3, by = "Regime")

rm(tally_vdem2,tally_vdem3)

print(xtable(tally_vdem, type = "latex", 
             caption = "Outcome variables per each vdem.regime category"), 
      file = "./Stargazer/master_table7.tex", 
      size="\\footnotesize", caption.placement = "top")


##### STARGAZER TABLES #####

#'* BASE MODEL STARGAZER *

stargazer(base_models[1:5], se = base_models_se[1:5], type = "text", 
          out = "./Stargazer/table_base1.tex",
          df=FALSE,
          dep.var.labels = c("jhu.crude\\_death"),
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Base Models: JHU COVID-19 Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(base_models[6:10], se = base_models_se[6:10], type = "text", 
          out = "./Stargazer/table_base2.tex",
          df=FALSE,
          dep.var.labels = c("owid.excess11\\_crude"),
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Base Models: OWID 11-Month Excess Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(base_models[11:15], se = base_models_se[11:15], type = "text", 
          out = "./Stargazer/table_base3.tex",
          df=FALSE,
          dep.var.labels = c("owid.excess12\\_crude"),
          column.sep.width = "-10pt",
          font.size = "footnotesize",
          align = TRUE,title="Base Models: OWID 12-Month Excess Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

#'* FULL MODEL STARGAZER *

stargazer(full_models[1:5], se = full_models_se[1:5], type = "text",
          out = "./Stargazer/table_full1.tex",
          df=FALSE,column.sep.width = "-10pt",
          dep.var.labels = c("jhu.crude\\_death"),
          font.size = "footnotesize",
          align = TRUE,title="Full Models: JHU COVID-19 Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(full_models[6:10], se = full_models_se[6:10], type = "text",
          out = "./Stargazer/table_full2.tex",
          df=FALSE,column.sep.width = "-10pt",
          dep.var.labels = c("owid.excess11\\_crude"),
          font.size = "footnotesize",
          align = TRUE,title="Full Models: OWID 11-Month Excess Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(full_models[11:15], se = full_models_se[11:15], type = "text",
          out = "./Stargazer/table_full3.tex",
          df=FALSE,column.sep.width = "-10pt",
          dep.var.labels = c("owid.excess12\\_crude"),
          font.size = "footnotesize",
          align = TRUE,title="Full Models: OWID 12-Month Excess Crude Death Rate",
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*",
          digits = 2)



#'* MULTICOLLINEARITY *

stargazer(multicollinear_models[1:8], se = multicollinear_se[1:8], type = "text",
          out = "./Stargazer/table_multicol1.tex",
          df=FALSE,column.sep.width = "-20pt",
          dep.var.labels = c("Polity/Democracy Scores"), 
          font.size = "scriptsize",
          align = TRUE,title="Multicollinearity: VDEM and Polity Scores (1)",
          single.row = TRUE,#float.env = "sidewaystable",
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(multicollinear_models[c(9:12,17:20)], se = multicollinear_se[c(9:12,17:20)], type = "text",
          out = "./Stargazer/table_multicol2.tex",
          df=FALSE,column.sep.width = "-20pt",
          dep.var.labels = c("Polity/Democracy Scores"), 
          font.size = "scriptsize",
          align = TRUE,title="Multicollinearity: VDEM and Polity Scores (2)",
          single.row = TRUE,#float.env = "sidewaystable",
          no.space = TRUE,
          report = "vc*",
          digits = 2)



#'* ADJUSTED MODEL STARGAZER *
#Including only control variables with:
##smaller difference in sample size
##correlation at 99% confidence level


stargazer(adj_models[seq(10,90,10)], se = adj_models_se[seq(10,90,10)], type = "text",
          out = "./Stargazer/table_adj1.tex",
          df=FALSE,column.sep.width = "-20pt",
          dep.var.labels = c("JHU, OWID-11, OWID-12"), 
          font.size = "scriptsize",
          align = TRUE,title="Adjusted Model",
          single.row = TRUE,#float.env = "sidewaystable",
          no.space = TRUE,
          report = "vc*",
          digits = 2)

stargazer(adj_models[seq(9,89,10)], se = adj_models_se[seq(9,89,10)], type = "text",
          #out = "./Stargazer/table_adj2.tex",
          df=FALSE,column.sep.width = "-20pt",
          dep.var.labels = c("JHU, OWID-11, OWID-12"), 
          font.size = "scriptsize",
          align = TRUE,title="Adjusted Model",
          single.row = TRUE,#float.env = "sidewaystable",
          no.space = TRUE,
          report = "vc*",
          digits = 2)



##### GRAPHS #####

#'* COMPARISON B/W JHU AND OWID DATA *

par(mfrow = c(1,1))
group.index <- rep(1:3, c(length(master$jhu.crude_death[!is.na(master$jhu.crude_death)]), 
                          length(master$owid.excess11_crude[!is.na(master$owid.excess11_crude)]), 
                          length(master$owid.excess12_crude[!is.na(master$owid.excess12_crude)])))

sm.density.compare(c(master$jhu.crude_death[!is.na(master$jhu.crude_death)],
                     master$owid.excess11_crude[!is.na(master$owid.excess11_crude)],
                     master$owid.excess12_crude[!is.na(master$owid.excess12_crude)]),
                   group = group.index, xlab = "Crude Death Rate", lwd = 2, col=c(1:3))

legend("topleft", legend=c("JHU CDR","OWID CDR 11 months", "OWID CDR 12 months"),  
       fill=c(1:3), box.lty=0, cex = 0.8, text.font = 10, bg = "transparent")
legend("left", legend=c("n=188","n=58","n=55"),
       text.col = c(1:3), box.lty=0, cex = 0.8, text.font = 10, bg = "transparent")


#'* COMPARISON B/W DIFF POLITICAL SCORES *

par(mfrow = c(1,1))
group.index2 <- rep(1:5, c(length(master$csp.revised_polity[!is.na(master$csp.revised_polity)]), 
                          length(master$eiu.demos2019[!is.na(master$eiu.demos2019)]), 
                          length(master$fh.demos2020[!is.na(master$fh.demos2020)]),
                          length(master$fi.econ2018[!is.na(master$fi.econ2018)]),
                          length(master$vdem.regime[!is.na(master$vdem.regime)])
                          ))

toNormalize <- list(master$csp.revised_polity[!is.na(master$csp.revised_polity)],
                 master$eiu.demos2019[!is.na(master$eiu.demos2019)],
                 master$fh.demos2020[!is.na(master$fh.demos2020)],
                 master$fi.econ2018[!is.na(master$fi.econ2018)],
                 master$vdem.regime[!is.na(master$vdem.regime)])

normalized = lapply(toNormalize, function(x) (x-min(x))/(max(x)-min(x)))

sm.density.compare(unlist(normalized[c(1:5)]),col=c(1:5),
                   group = group.index2, xlab = "Political Regime Scores", lwd = 2)

legend("topleft", legend=c("CSP Polity Score","EIU Democracy Index", "FH Democracy Score",
                           "FI Economic Freedom","VDEM Regime Type"),  
       col = c(1:5), fill=c(1:5), box.lty=0, cex = 0.8, text.font = 10, bg = "transparent")
legend("left", legend=c("n=163","n=164","n=27", "n=161", "n=171"),
       text.col = c(1:5), box.lty=0, cex = 0.8, text.font = 10, bg = "transparent")


#'* BASE MODEL GRAPHS *
      
par(mfrow = c(5,3))

plot(x = master$csp.revised_polity, y = master$jhu.crude_death,
       xlab = "",ylab = "csp.revised_polity")
abline(base_models[[1]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(base_models[[6]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(base_models[[11]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(base_models[[2]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(base_models[[7]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(base_models[[12]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(base_models[[3]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(base_models[[8]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(base_models[[13]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(base_models[[4]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(base_models[[9]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(base_models[[14]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(base_models[[5]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(base_models[[10]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(base_models[[15]], col = "red", lwd = 2)


#'* FULL MODEL GRAPHS *

par(mfrow = c(5,3))

plot(x = master$csp.revised_polity, y = master$jhu.crude_death,
     xlab = "",ylab = "csp.revised_polity")
abline(full_models[[1]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(full_models[[6]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(full_models[[11]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(full_models[[2]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(full_models[[7]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(full_models[[12]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(full_models[[3]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(full_models[[8]], col = "red", lwd = 2)

plot(x = master$fh.demos2020, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(full_models[[13]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(full_models[[4]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(full_models[[9]], col = "red", lwd = 2)

plot(x = master$fi.econ2018, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(full_models[[14]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(full_models[[5]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(full_models[[10]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(full_models[[15]], col = "red", lwd = 2)

#'* MULTICOLLINEARITY *
ggpairs(master[,c("csp.revised_polity","fi.econ2018","vdem.eq_resource",
                  "vdem.govfakenews","vdem.neopatri",
                  "vdem.health_acc")], axis = "internal")

max(master$wb.lpi_avg1218, na.rm = TRUE)
#'* ADJUSTED MODEL *
par(mfrow=c(3,3))

plot(x = master$csp.revised_polity, y = master$jhu.crude_death,
     xlab = "",ylab = "csp.revised_polity")
abline(adj_models[[10]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(adj_models[[20]], col = "red", lwd = 2)

plot(x = master$csp.revised_polity, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(adj_models[[30]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(adj_models[[40]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(adj_models[[50]], col = "red", lwd = 2)

plot(x = master$eiu.demos2019, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(adj_models[[60]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$jhu.crude_death,
     xlab = "",ylab = "")
abline(adj_models[[70]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess11_crude,
     xlab = "",ylab = "")
abline(adj_models[[80]], col = "red", lwd = 2)

plot(x = master$vdem.regime, y = master$owid.excess12_crude,
     xlab = "",ylab = "")
abline(adj_models[[90]], col = "red", lwd = 2)




#'* MODEL 1 *
plot(x = master$csp.revised_polity, 
     y = master$jhu.crude_death,
     xlab = "CSP Revised Polity Score",
     ylab = "Crude Death Rate (Cases per 100,000)")
abline(m1.nc, col = "red", lwd = 2)
mtext(paste("alpha =", round(m1.nc$coefficients[1], 3), ";", 
            "beta_hat =", round(m1.nc$coefficients[2], 3), ";", 
            "p <", ifelse(summary(m1.nc)$coefficients[2,4] < 0.01, "0.01", 
                          round(summary(m1.nc)$coefficients[2,4], 3)), ";",  
            "df =", round(summary(m1.nc)$df, 3), sep=" "),
      side=3)


#'* MODEL 2 *

#Model 2 Test 1: polity ~ bmi
m2.test1 <- lm_robust(csp.revised_polity ~ ncd.mean_bmi, master)

par(mfrow=c(3,1))
plot(x = master$ncd.mean_bmi, 
     y = master$csp.revised_polity,
     xlab = "Mean BMI (2017)",
     ylab = "Polity (2018)")
abline(m2.test1, col = "red", lwd = 2)
mtext(paste("alpha =", round(m2.test1$coefficients[1], 3), ";",
            "beta_hat =", round(m2.test1$coefficients[2], 3), ";", 
            "p <", ifelse(summary(m2.test1)$coefficients[2,4] < 0.01, "0.01", 
                          round(summary(m2.test1)$coefficients[2,4], 3)), ";",  
            "df =", round(summary(m2.test1)$df, 3), sep=" "),
      side=3)

#Model 2 Test 2: cdr ~ bmi
m2.test2 <- lm_robust(jhu.crude_death ~ ncd.mean_bmi, master)
plot(x = master$ncd.mean_bmi, 
     y = master$jhu.crude_death,
     xlab = "Mean BMI (2017)",
     ylab = "Crude Death Rate")
abline(m2.test2, col = "red", lwd = 2)
mtext(paste("alpha =", round(m2.test2$coefficients[1], 3), ";",
            "beta_hat =", round(m2.test2$coefficients[2], 3), ";", 
            "p <", ifelse(summary(m2.test2)$coefficients[2,4] < 0.01, "0.01", 
                          round(summary(m2.test2)$coefficients[2,4], 3)), ";",  
            "df =", round(summary(m2.test2)$df, 3), sep=" "),
      side=3)

#Model 2: cdr ~ polity + bmi
plot(x = master$csp.revised_polity, 
     y = master$jhu.crude_death,
     xlab = "Polity (2018)",
     ylab = "Crude Death Rate")
abline(m2.c$coefficients[1,2], col = "red", lwd = 2)
mtext(paste("alpha =", round(m2.c$coefficients[1], 3), ";",
            "beta_hat =", round(m2.c$coefficients[2], 3), ";", 
            "p <", ifelse(summary(m2.c)$coefficients[2,4] < 0.01, "0.01", 
                          round(summary(m2.c)$coefficients[2,4], 3)), ";",  
            "df =", round(summary(m2.c)$df, 3), sep=" "),
      side=3)



