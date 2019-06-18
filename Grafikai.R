#Paketų instaliavimas
if(!require(eurostat)) install.packages("eurostat");require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse");require(tidyverse)
library(eurostat)
#kad grafike veiktų lietuviškos raidės
Sys.setlocale("LC_ALL","Lithuanian")

#Pirmas grafikas
ilc_di12 <- get_eurostat("ilc_di12",stringsAsFactors = FALSE)
df <- ilc_di12 %>% filter(geo %in% c("LT","EU"),
                          time>="2008-01-01")
#png(filename = "LTginiFrom2008.png",width = 9,height = 4,units = "in",res=200)
ggplot(df,aes(x=time,y=values))+
  geom_line(aes(col=geo),size=1)+
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year")+
  labs(title = "Gini koeficiento reikšmė Lietuvoje ir Europos Sąjungoje 2008 – 2017 metais",
       subtitle = "Šaltinis:Eurostat(ilc_di12)",
       x="Laikotarpis",
       y="Gini koeficiento reikšmė")
#dev.off()

#Antras grafikas
dg <- ilc_di12 %>% filter(geo %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
                          time=="2017-01-01")

#png(filename = "AllGini2017.png",width = 9,height = 4,units = "in",res=200)
ggplot(dg,aes(x=reorder(geo,values),y=values))+
  geom_bar(stat="identity",
           fill="steelblue")+
  geom_text(aes(label=values),vjust=-0.3,size=3)+
labs(title = "Gini koeficiento reikšmė Europos Sąjungos šalyse 2017 metais",
       subtitle = "Šaltinis:Eurostat(ilc_di12)",
       x="Šalys",
       y="Gini koeficiento reikšmė")
#dev.off()

#3 grafikas
dj <- ilc_di01 %>% filter(geo=="LT",
                          time=="2017-01-01",
                          currency=="EUR",
                          quantile %in% c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10"),
                          indic_il=="SHARE")
#png(filename="LTdeciles2017.png",width = 9,height = 4,units = "in",res=200)
ggplot(dj,aes(x=reorder(quantile,values),y=values))+
  geom_bar(stat="identity",
           fill="steelblue")+
  geom_text(aes(label=values),vjust=-0.3,size=3)+
  labs(title = "Disponuojamų pajamų pasiskirstymas deciliais",
       subtitle = "Šaltinis:Eurostat(ilc_di01)",
       x="Deciliai",
       y="Pajamų dalis (%)")
#dev.off()

#4 grafikas

ilc_di01 <- get_eurostat("ilc_di01",stringsAsFactors = FALSE)
dh <- ilc_di01 %>% filter(geo=="LT",
                          time=="2017-01-01",
                          currency=="EUR",
                          quantile %in% c("QU1","QU2","QU3","QU4","QU5"),
                          indic_il=="SHARE")
#png(filename="LTquantiles2017.png",width = 9,height = 4,units = "in",res=200)
ggplot(dh,aes(x=quantile,y=values))+
  geom_bar(stat="identity",
           fill="steelblue")+
  geom_text(aes(label=values),vjust=-0.3,size=3.5)+
  labs(title = "Disponuojamų pajamų pasiskirstymas kvintiliais",
       subtitle = "Šaltinis:Eurostat(ilc_di01)",
       x="Kvintiliai",
       y="Pajamų dalis (%)")
#dev.off()

#5 grafikas


ilc_peps01 <- get_eurostat("ilc_peps01",stringsAsFactors = FALSE)
dl <- ilc_peps01 %>% filter(geo %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
                          time=="2017-01-01",
                          unit=="PC",
                          age=="TOTAL",
                          sex=="T")
#png(filename="povertyrisk.png",width = 9,height = 4,units = "in",res=200)
ggplot(dl,aes(x=reorder(geo,values),y=values))+
  geom_bar(stat="identity",
           fill="steelblue")+
  geom_text(aes(label=values),vjust=-0.3,size=3)+
  labs(title = "Gyvetojų dalis, kuriems gręsia skurdas arba socialinė atskirtis 2017 metų duomenimis",
       subtitle = "Šaltinis:Eurostat(ilc_peps01)",
       x="Šalys",
       y="Gyventojų dalis (%)")
#dev.off()



