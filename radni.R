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
bdp_eu <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item=="B1GQ" & unit=="CLV10_MNAC" & geo=="EU28") %>% select(time,values) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,bdp=values)

# 1.3. Stopa nezaposlenosti ####



# 1.4. Inflacija ####



# 1.5. Plaće ####

# 1.3. Javni dug u BDP-u ####
pdebt <- get_eurostat(id="gov_10q_ggdebt") %>% filter(na_item=="GD" & sector=="S13" & unit=="PC_GDP") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,pdebt_gdp=values)
ggplot(pdebt,aes(x=datum,y=pdebt_gdp))+geom_line()+facet_wrap(~geo,scales="free")

# 1.5. Javni deficit/surplus ####
pdeficit <- get_eurostat(id="gov_10q_ggnfa") %>% filter(s_adj=="NSA" & na_item=="B9" & sector=="S13" & unit=="PC_GDP" & (substr(geo,1,2)!="EA")) %>% select(time,geo,values) %>% arrange(geo,time) %>% group_by(geo) %>% mutate(pdeficit_gdp=(values+lag(values,1)+lag(values,2)+lag(values,3))/4) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,pdeficit_gdp) %>% na.omit()
ggplot(pdeficit,aes(x=datum,y=pdeficit_gdp))+geom_line()+facet_wrap(~geo,scales="free")+geom_hline(aes(yintercept=0),linetype="dashed",color="red")



# 1.8. Current account deficit (ovo sidamo s ECB-a) ####
pom1 <- get_data("BP6.Q.N..W1.S1.S1.T.B.CA._Z._Z._Z.EUR._T._X.N") %>% select(obstime,ref_area,ca_balance=obsvalue)
pom2 <- get_data("MNA.Q.N..W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N") %>% select(obstime,ref_area,bdp=obsvalue)
cabalance = left_join(pom1,pom2,by=c("ref_area","obstime")) %>% mutate(datum = make_date(ifelse(substr(obstime,7,7)=="4",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,7,7)=="4",1,as.numeric(substr(obstime,7,7))*3+1), 1)-1) %>% select(datum,geo=ref_area,ca_balance,bdp)%>% group_by(geo) %>% mutate(bdp_y=(bdp+lag(bdp,1)+lag(bdp,2)+lag(bdp,3)),ca_balance_y=(ca_balance+lag(ca_balance,1)+lag(ca_balance,2)+lag(ca_balance,3))) %>% mutate(ca_deficit=ca_balance_y/bdp_y*100) %>% select(datum,geo,ca_deficit) %>% na.omit()
ggplot(cabalance,aes(x=datum,y=ca_deficit))+geom_line()+facet_wrap(~geo,scales="free")+geom_hline(aes(yintercept=0),linetype="dashed",color="red")
rm(pom1,pom2)

# 1.9. Kreditni rejtinzi - kreirati ćemo numeričku ocjenu rejtinga i dummy varijablu koja gleda da li je država u investicijskom rangu ili ne ####
rejting <- read_excel("D:/mbamba/R&D/hr_yld_mdl/kreditni_rejtinzi.xlsx",sheet = "rejtinzi") %>% mutate(datum=mdy(Date)) %>% select(datum,geo=ctry,Agency,Rating,Outlook) %>% filter(Agency %in% c("Fitch","Moody's","S&P"))
skala <- read_excel("D:/mbamba/R&D/hr_yld_mdl/kreditni_rejtinzi.xlsx",sheet = "numericki_rejtinzi") %>% gather(key = "Agency",value = "Rating",-rejting)
rejting <- left_join(rejting,skala,by=c("Agency","Rating"))
# ovi gore rejtinzi su u formatu tako da su prikazani samo datumi promjene, a nama treba izračun po kvartalima -> for petlja
investicijski <- yld %>% select(datum,geo,yld) %>% mutate(fitch=NA,moodys=NA,snp=NA) %>% arrange(geo,datum)

for (i in 1:nrow(investicijski)) {
  ctry <- investicijski$geo[i]
  dtm <- investicijski$datum[i]
  f <- rejting %>% filter(geo==ctry & Agency=="Fitch" & datum<=dtm) %>% arrange(datum)
  m <- rejting %>% filter(geo==ctry & Agency=="Moody's" & datum<=dtm) %>% arrange(datum)
  s <- rejting %>% filter(geo==ctry & Agency=="S&P" & datum<=dtm) %>% arrange(datum)
  if(nrow(f)>0){
    investicijski$fitch[i] <- f$rejting[nrow(f)]
  } else{
    investicijski$fitch[i] <- NA
  }
  if(nrow(m)>0){
    investicijski$moodys[i] <- m$rejting[nrow(m)]
  } else{
    investicijski$moodys[i] <- NA
  }
  if(nrow(s)>0){
    investicijski$snp[i] <- s$rejting[nrow(s)]
  } else{
    investicijski$snp[i] <- NA
  }
}
# izračun prosječnog numeričkog rejtinga i investicijske dummy varijable (=1, ako su 2 agencija u investicijskom)
investicijski <- investicijski %>% gather(key = "agencija",value = "iznos",-datum,-geo,-yld) %>% na.omit() %>% mutate(investicijski=ifelse(iznos<10.5,1,0)) %>% group_by(datum,geo) %>% summarise(yld=mean(yld,na.rm=T),invest_br=sum(investicijski,na.rm=T),rejting=mean(iznos,na.rm=T),investicijski=ifelse(invest_br>=2,1,0)) %>% select(-invest_br,-yld)
# facet grafikon
ggplot(investicijski %>% filter(datum>="2000-12-31"),aes(x=datum,y=rejting)) + geom_line() + facet_wrap(~geo) + geom_hline(yintercept = 10.5,color="red")
# brisanje privremenih varijabli
rm(rejting,skala,i,f,m,s,ctry,dtm)

# Sklapanje panelice za model yielda
panel = inner_join(yld,us_yld,by="datum") %>% inner_join(pdebt,by=c("datum","geo")) %>% inner_join(pdeficit,by=c("datum","geo")) %>% inner_join(ciss_bond,by="datum") %>% inner_join(euribor,by="datum") %>% inner_join(dbdp,by=c("datum","geo")) %>% inner_join(cabalance,by=c("datum","geo")) %>% inner_join(investicijski,by=c("datum","geo")) %>% na.omit() %>% filter(geo!="EU28")

#### 2. Sklapanje i spremanje panela ####

library(plm)
panel <- pdata.frame(panel,index = c("geo","datum"))
panel$datum=as.Date(panel$datum)
save(panel,file="panel_yld.Rda")
rm(cabalance,pdebt,pdeficit,us_yld,euribor,ciss_bond,dbdp,yld,investicijski)

#jdba <- yld ~ us_yld + pdebt_gdp + pdeficit_gdp + vstoxx + dbdp + ca_deficit
#jdba1 <- yld ~ us_yld + pdebt_gdp + vstoxx
#mdl <- plm(formula = jdba1,data = panel,model = "within",effect = "individual")
#summary(mdl)


#### 3. Procjena error correction modela - na panelu ####

# Long-run jdba
jdba_lr <- yld ~ us_yld + pdebt_gdp + pdeficit_gdp + investicijski
mdl_lr <- plm(formula = jdba_lr,data = panel,model = "within",effect = "individual")
summary(mdl_lr)
# grafikon long run fita
pom <- data.frame(index(mdl_lr),mdl_lr$model) %>% mutate(reziduali=mdl_lr$residuals,yld_proc=yld-reziduali) %>% select(datum,geo,yld,yld_proc,reziduali) %>% gather(key = varijabla,value = iznos,-datum,-geo)
ggplot(pom,aes(x=as.Date(datum),y=iznos,col=varijabla)) + geom_line() + facet_wrap(~geo,scales="free") + geom_hline(yintercept = 0) + theme(legend.position = "top",legend.title = element_blank())
rm(pom)

# Short-run jdba
indeksi = index(mdl_lr$model) # indeksi (geo & datum)
reziduali <- mdl_lr$residuals # reziduali iz long-run jednadžbe
varijable <- mdl_lr$model # ostale varijable iz long-run relacije
fiksni_efekti <- data.frame(geo =names(fixef(mdl_lr,effect = "individual")),fix_ef=fixef(mdl_lr,effect = "individual"))
podaci = data.frame(indeksi,varijable,reziduali)
podaci = left_join(podaci,fiksni_efekti,by="geo")
podaci$datum = as.Date(podaci$datum)
podaci = left_join(podaci,panel %>% select(geo,datum,ciss_bond,dbdp),by=c("geo","datum"))
rm(indeksi,fiksni_efekti,varijable,reziduali)
# Kreiranje lagiranog reziduala i diferencija iz long-run jednadžbe
podaci <- pdata.frame(podaci,index = c("geo","datum"))
podaci$dyld = diff(podaci$yld,1)
podaci$dus_yld = diff(podaci$us_yld,1)
podaci$dpdebt_gdp = diff(podaci$pdebt_gdp,1)
podaci$dpdeficit_gdp = diff(podaci$pdeficit_gdp,1)
podaci$dinvesticijski = diff(podaci$investicijski,1)
#podaci$deuribor = diff(podaci$euribor,1)
podaci$lreziduali = lag(podaci$reziduali,1)
# procjena modela
jdba_sr <- dyld ~ lreziduali + dus_yld + dpdebt_gdp + dpdeficit_gdp + dinvesticijski + ciss_bond + dbdp
mdl_sr <- lm(formula = jdba_sr,data = podaci)
summary(mdl_sr)
# grafikon short run fita
pom <- data.frame(mdl_sr$model)
reci <- rownames(pom)
pom <- pom %>% mutate(geo=substr(reci,1,2),datum=as.Date(substr(reci,4,13))) %>% mutate(reziduali=mdl_sr$residuals,dyld_proc=dyld-reziduali) %>% select(datum,geo,dyld,dyld_proc) %>% gather(key = varijabla,value = iznos,-datum,-geo)
ggplot(pom,aes(x=as.Date(datum),y=iznos,col=varijabla)) + geom_line() + facet_wrap(~geo,scales="free") + geom_hline(yintercept = 0) + theme(legend.position = "top",legend.title = element_blank())
rm(reci)




# Procjena error correction modela - samo za HR (NEMA DOBRA SVOJSTVA) ####
cro_data = podaci %>% filter(geo=="HR")
# procjena LR jednadžbe
jdba_lr_hr <- yld ~ us_yld + pdebt_gdp + pdeficit_gdp
mdl_lr_hr <- lm(formula = jdba_lr_hr,data = cro_data)
summary(mdl_lr_hr)
# procjena LR jednadžbe
jdba_sr_hr <- dyld ~ lreziduali + dus_yld + dpdebt_gdp + dpdeficit_gdp + dbdp
mdl_sr_hr <- lm(formula = jdba_sr_hr,data = cro_data)
summary(mdl_sr_hr)

# SOLIST - kalibracija scenarija ####
cro_data = podaci %>% filter(geo=="HR") %>% select(-geo,-vstoxx)
cro_data$time=as.Date(cro_data$time)
library(readxl)
baseline <- read_excel("~/Radni/SOLIST/STest/yield model/scenarij.xlsx", sheet = "baseline", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
adverse <- read_excel("~/Radni/SOLIST/STest/yield model/scenarij.xlsx", sheet = "adverse", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
baseline$time=as.Date(baseline$time)
adverse$time=as.Date(adverse$time)
baseline = rbind(cro_data,baseline)
adverse = rbind(cro_data,adverse)
rm(cro_data)

for (i in 65:72) {
  # baseline
  baseline$dus_yld[i] <- baseline$us_yld[i]-baseline$us_yld[i-1]
  baseline$dpdebt_gdp[i] <- baseline$pdebt_gdp[i]-baseline$pdebt_gdp[i-1]
  baseline$dpdeficit_gdp[i] <- baseline$pdeficit_gdp[i]-baseline$pdeficit_gdp[i-1]
  baseline$lreziduali[i] <- baseline$reziduali[i-1]
  baseline$dyld[i] <- coefficients(mdl_sr)[names(coefficients(mdl_sr))=="(Intercept)"] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="lreziduali"]*baseline$lreziduali[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dus_yld"]*baseline$dus_yld[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdebt_gdp"]*baseline$dpdebt_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdeficit_gdp"]*baseline$dpdeficit_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dbdp"]*baseline$dbdp[i]
  baseline$yld[i] <- baseline$yld[i-1] + baseline$dyld[i]
  baseline$reziduali[i] <- baseline$yld[i] - (baseline$fix_ef[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="us_yld"]*baseline$us_yld[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdebt_gdp"]*baseline$pdebt_gdp[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdeficit_gdp"]*baseline$pdeficit_gdp[i])
  # adverse
  adverse$dus_yld[i] <- adverse$us_yld[i]-adverse$us_yld[i-1]
  adverse$dpdebt_gdp[i] <- adverse$pdebt_gdp[i]-adverse$pdebt_gdp[i-1]
  adverse$dpdeficit_gdp[i] <- adverse$pdeficit_gdp[i]-adverse$pdeficit_gdp[i-1]
  adverse$lreziduali[i] <- adverse$reziduali[i-1]
  adverse$dyld[i] <- coefficients(mdl_sr)[names(coefficients(mdl_sr))=="(Intercept)"] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="lreziduali"]*adverse$lreziduali[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dus_yld"]*adverse$dus_yld[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdebt_gdp"]*adverse$dpdebt_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdeficit_gdp"]*adverse$dpdeficit_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dbdp"]*adverse$dbdp[i]
  adverse$yld[i] <- adverse$yld[i-1] + adverse$dyld[i]
  adverse$reziduali[i] <- adverse$yld[i] - (adverse$fix_ef[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="us_yld"]*adverse$us_yld[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdebt_gdp"]*adverse$pdebt_gdp[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdeficit_gdp"]*adverse$pdeficit_gdp[i])
}

# grafikon baseline vs. adverse
pom = rbind(baseline %>% select(time,yld) %>% mutate(scenarij="baseline"), adverse %>% select(time,yld) %>% mutate(scenarij="adverse"))
ggplot(pom,aes(x=time,y=yld)) + geom_line(aes(color=scenarij)) + theme(legend.position = "top") + geom_vline(xintercept = as.numeric(as.Date("2018-07-01")), linetype=2)

# grafikon fita
pom = baseline %>% mutate(yld_kapica = NA,dyld_kapica = NA,reziduali_kapica = NA,lreziduali_kapica = NA)
pom$reziduali_kapica[2] <- pom$reziduali[2]
pom$yld_kapica[2] <- pom$yld[2]
for (i in 3:72) {
  pom$lreziduali_kapica[i] <- pom$reziduali_kapica[i-1]
  pom$dyld_kapica[i] <- coefficients(mdl_sr)[names(coefficients(mdl_sr))=="(Intercept)"] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="lreziduali"]*pom$lreziduali_kapica[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dus_yld"]*pom$dus_yld[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdebt_gdp"]*pom$dpdebt_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dpdeficit_gdp"]*pom$dpdeficit_gdp[i] + coefficients(mdl_sr)[names(coefficients(mdl_sr))=="dbdp"]*pom$dbdp[i]
  pom$yld_kapica[i] <- pom$yld_kapica[i-1] + pom$dyld_kapica[i]
  pom$reziduali_kapica[i] <- pom$yld_kapica[i] - (pom$fix_ef[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="us_yld"]*pom$us_yld[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdebt_gdp"]*pom$pdebt_gdp[i] + coefficients(mdl_lr)[names(coefficients(mdl_lr))=="pdeficit_gdp"]*pom$pdeficit_gdp[i])
  
}


pom = pom %>% select(time,dyld,dyld_kapica) %>% gather(serija,iznos,2:3)
ggplot(pom,aes(x=time,y=iznos)) + geom_line(aes(color=serija)) + theme(legend.position = "top") + geom_vline(xintercept = as.numeric(as.Date("2018-07-01")), linetype=2)
