library(eurostat)
library(ecb)
library(lubridate)
library(vars)
library(tidyverse)
library(readxl)
rm(list=ls())

# Kopiranje dataframea u excel

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


#### 1. Dohvat podataka ####

# 1.1. Realni BDP HR
bdp <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item=="B1GQ" & unit=="CLV10_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,bdp=values) %>% arrange(datum) %>% mutate(bdp=(bdp/lag(bdp,4)-1)*100) %>% na.omit()

# 1.2. Realni BDP EU 
bdp_eu <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item=="B1GQ" & unit=="CLV10_MNAC" & geo=="EU28") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,bdp_eu=values) %>% arrange(datum) %>% mutate(bdp_eu=(bdp_eu/lag(bdp_eu,4)-1)*100) %>% na.omit()

# 1.3. Stopa nezaposlenosti
unemp <- get_eurostat(id="une_rt_q") %>% filter(s_adj=="SA" & age=="TOTAL" & sex=="T" & unit=="PC_ACT" & geo=="HR") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,unemp=values)

# 1.4. Inflacija
infl <- get_eurostat(id="prc_hicp_manr") %>% filter(coicop=="CP00" & unit=="RCH_A" & geo=="HR") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"month")-1) %>% filter(month(datum) %in% c(3,6,9,12)) %>% select(datum,infl=values)

# 1.5. Realne bruto plaće
wage <- read_excel("podaci.xlsx") %>% mutate(datum=as.Date(datum)) %>% filter(month(datum) %in% c(3,6,9,12)) %>% arrange(datum) %>% mutate(wage=(wage/lag(wage,4)-1)*100) %>% na.omit()

# 1.6. Sklapanje podataka
podaci <- inner_join(bdp,bdp_eu,by="datum") %>% inner_join(infl,by="datum") %>% inner_join(unemp,by="datum") %>% inner_join(wage,by="datum")
egzogene <- ts(podaci[,3],end = c(2019,4),frequency=4)
podaci <- ts(podaci[,-c(1,3)],end = c(2019,4),frequency=4)
rm(bdp,bdp_eu,infl,unemp,wage)

# Procjena unrestricted VAR modela, a tu je prvi korak određivanje broja lagova (oni su procjenjivali i za 1 i za 2 i za 3 laga modele, no mi ćemo gledati primjer sa samo 1 lagom). 
VARselect(podaci, lag.max = 4, type = "none",exogen = egzogene)
# procjena var modela
var_mdl <- VAR(podaci, p = 2, type = "none", exogen = egzogene)
var_mdl # ovo nam daje koeficijente svih jednadžbi
summary(var_mdl, equation = "bdp") # daje ključne podatke o jdbi za dbdp
summary(var_mdl, equation = "infl")
summary(var_mdl, equation = "unemp")
summary(var_mdl, equation = "wage")

# na kraju možemo plotati fit modela
plot(var_mdl) # svi zajedno

# impulse response plotovi
ir_plot <- irf(var_mdl, n.ahead = 10, boot = TRUE)
plot(ir_plot,nc=2, nr=2)
