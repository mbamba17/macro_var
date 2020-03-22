library(tidyverse)
library(readxl)
library(eurostat)
library(ecb)
library(lubridate)
rm(list=ls())

# Kopiranje dataframea u excel

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


#### 1. Dohvat podataka ####

# 1.1. Realni BDP HR ####
bdp <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item=="B1GQ" & unit=="CLV10_MNAC" & geo=="HR") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,bdp=values)

# %>% arrange(geo,time) %>% group_by(geo) %>% mutate(bdp_y=(values+lag(values,1)+lag(values,2)+lag(values,3))) %>% group_by(geo) %>% mutate(dbdp = (bdp_y/lag(bdp_y,4)-1)*100)  %>% select(datum,geo,dbdp) %>% na.omit()
# ggplot(dbdp,aes(x=datum,y=dbdp))+geom_line()+facet_wrap(~geo,scales="free")+geom_hline(aes(yintercept=0),linetype="dashed",color="red")

# 1.2. Realni BDP EU #### 
bdp_eu <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item=="B1GQ" & unit=="CLV10_MNAC" & geo=="EU28") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,bdp_eu=values)

# 1.3. Stopa nezaposlenosti ####
unemp <- get_eurostat(id="une_rt_q") %>% filter(s_adj=="SA" & age=="TOTAL" & sex=="T" & unit=="PC_ACT" & geo=="HR") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,unemp=values)


# 1.4. Inflacija ####
infl <- get_eurostat(id="prc_hicp_manr") %>% filter(coicop=="CP00" & unit=="RCH_A" & geo=="HR") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"month")-1) %>% filter(month(datum) %in% c(3,6,9,12)) %>% select(datum,infl=values)


# 1.5. Realne bruto plaće ####
wage <- read_excel("podaci.xlsx") %>% mutate(datum=as.Date(datum)) %>% filter(month(datum) %in% c(3,6,9,12))


# Sklapanje panelice za model yielda
panel = inner_join(yld,us_yld,by="datum") %>% inner_join(pdebt,by=c("datum","geo")) %>% inner_join(pdeficit,by=c("datum","geo")) %>% inner_join(ciss_bond,by="datum") %>% inner_join(euribor,by="datum") %>% inner_join(dbdp,by=c("datum","geo")) %>% inner_join(cabalance,by=c("datum","geo")) %>% inner_join(investicijski,by=c("datum","geo")) %>% na.omit() %>% filter(geo!="EU28")

