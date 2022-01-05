# 2. faza: Uvoz podatkov

library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#uvozimo podatke in jih uredimo
#1.tabela-VPIS V SREDNJE ŠOLE
tabela1.1 <- read_csv2("podatki/dijaki.csv", 
                      skip = 2, 
                      locale=locale(encoding="Windows-1250"),
                      col_types = cols(
                       .default = col_guess(),
                       STAROST = col_factor(),
                       LETNIK = col_factor()
                      )
                     )

colnames(tabela1.1)[1] <- "vrsta.izobrazevanja"
colnames(tabela1.1)[2] <-"letnik" 
colnames(tabela1.1)[3] <-"starost"


tabela1.1 <- tabela1.1 %>% 
  pivot_longer(
    cols = colnames(tabela1.1)[-1 : -3],
    names_to = "solsko.leto.spol",
    values_to = "stevilo.vpisanih"
  ) %>%
  separate(
    col = solsko.leto.spol,
    into = c("solsko.leto", "spol"),
    sep = " "
  )

tabela1.2 <- read_csv2("podatki/prebivalstvo_po_starosti.csv", skip = 2, 
                      locale=locale(encoding="Windows-1250"),
                      col_types = cols(
                        .default = col_guess(),
                        STAROST = col_factor()
                        )
                      )

colnames(tabela1.2)[1] <-"spol" 
colnames(tabela1.2)[2] <-"starost"

tabela1.2 <- tabela1.2 %>% 
  pivot_longer(
    cols = colnames(tabela1.2)[-1 : -2],
    names_to = "leto.slo",
    values_to = "stevilo.prebivalcev"
  ) %>%
  separate(
    col = leto.slo,
    into = c("leto", "slo"),
    sep = " "
  ) %>%
  select(-slo)%>%                      #izbrise stolpec slo
  mutate(leto=parse_number(leto)) %>%  #znebimo se polletne oznake H1 in H2, ostane samo leto
  group_by(spol, starost, leto) %>%    #zdruzimo po starosti spolu in letu, da v naslednjem koraku sestejemo
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))

#tabela, ki mi bo pomagala pretvorit solska leta in leta(mora biti dbl, ker je tako v tabela1.2)
pretvori.leta <- tibble(
  solsko.leto = c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15",
                  "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21"),
  leto = as.double(c("2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                     "2016", "2017", "2018", "2019", "2020")),
)

#združimo tabeli 1.1 in 1.2 in si pomagamo s pomožno tabelo:
tabela1 <- tabela1.1 %>%
  left_join(pretvori.leta, by = "solsko.leto")%>%
  left_join(tabela1.2, by = c("starost" = "starost", "spol" = "spol", "leto" = "leto")) %>%
  select(-leto)   #leta sem rabila samo za združitev sedaj jih ne potrebujem več, ker imam solska leta


tabela1[6] <- sapply(tabela1[6], as.numeric) #spremeni stolpec stevilo.vpisanih iz chr v dbl
tabela1[is.na(tabela1)] = 0                  #zamenja vse NA (v tej tabeli) v 0
#_________________________________________________________________________________
#2.tabela-MATURANTJE
tabela2.1 <- read_csv2("podatki/maturantje.csv", skip = 2,
                     locale=locale(encoding="Windows-1250"))

colnames(tabela2.1)[1] <-"vrsta.zavoda" 
colnames(tabela2.1)[2] <-"regija"

tabela2.1 <- tabela2.1 %>% 
  pivot_longer(
    cols = colnames(tabela2.1)[-1 : -2],
    names_to = "solsko.leto.vrsta.izobrazevanja",
    values_to = "stevilo.maturantov"
  ) 
#ze zacetek javlja napako, nevem kako naj popravim to
#%>%
separate(
    col = solsko.leto.vrsta.izobrazevanja,
    into = c("solsko.leto", "vrsta.izobrazevanja"),
    sep = " "
  )%>%
  select(-vrsta.zavoda)


tabela2.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"))
colnames(tabela2.2)[1] <-"spol" 
colnames(tabela2.2)[2] <-"starost"

tabela2.2 <- tabela2.2 %>% 
  pivot_longer(
    cols = colnames(tabela2.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  separate(
    col = leto.regija,
    into = c("leto", "regija"),
    sep = " "
  )%>%
  select(-starost)%>%
  mutate(leto = parse_number(leto)) %>% 
  group_by(leto, regija) %>%                 #spola ne potrebujemo več, ker ga ni niti v tabela2.1
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))

#združimo tabelo 2.1 in 2.2
tabela2 <- tabela2.1 %>%
  left_join(pretvori.leta, by = "solsko.leto")%>%
  left_join(tabela2.2, by = c("regija" = "regija", "leto" = "leto")) 

#_______________________________________________________________________________
#3.tabela-VPIS NA FAKULTETE
tabela3.1 <- read_csv2("podatki/studentje.csv", skip = 2, 
                     locale=locale(encoding="Windows-1250"))

colnames(tabela3.1)[1] <-"vrsta.izobrazevanja" 
colnames(tabela3.1)[2] <-"nacin.studija"
colnames(tabela3.1)[3] <-"spol" 
colnames(tabela3.1)[4] <-"kohezijska.regija"
tabela3.1[, -1:-4] <- sapply(tabela3.1[, -1:-4], as.character) #stolpec 2009/10 je edini bil dbl zato sem ga spremenila v chr  
                                                               #ampak ševedno javlja napako pri naslednji cevi
tabela3.1[is.na(tabela3.1)] = 0  #vse NA spremeni v 0

tabela3.1 <- tabela3.1 %>% 
  pivot_longer(
    cols = (tabela3.1)[-1:-4],
    names_to = "solsko.leto",
    values_to = "stevilo.vpisanih"
  )

#tabela ki pretvarja regije v zahodne oz vzhodne
regije.slo <- tibble(
  slo = c("Zahodna Slovenija", "Zahodna Slovenija", "Zahodna Slovenija", "Zahodna Slovenija", 
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", 
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Slovenija"),
  regija = c("Gorenjska", "Goriška", "Obalno-kraška", "Osrednjeslovenska","Pomurska", "Podravska", 
             "Koroška", "Savinjska", "Zasavska", "Posavska", "Jugovzhodna", "Primorsko-notranjska", "SLOVENIJA")
)

tabela3.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                         locale=locale(encoding="Windows-1250"))
colnames(tabela3.2)[1] <-"spol" 
colnames(tabela3.2)[2] <-"starost"

tabela3.2 <- tabela3.2 %>% 
  pivot_longer(
    cols = colnames(tabela3.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  separate(
    col = leto.regija,
    into = c("leto", "regija"),
    sep = " "
  )%>%
  select(-starost)%>%
  mutate(leto = parse_number(leto)) %>%
  left_join(regije.slo, by = c("regija" = "regija")) %>%     #potrebujemo samo za vzhodo in zahodo Slovenijo
  select(-regija) %>%                                        #zato odstranimo stolpec regija
  relocate(slo, .after = leto) %>%
  group_by(spol, leto, slo) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))  
  
  #zdruzimo tabela3.1 in tabela3.2
tabela3 <- tabela3.1 %>%
  left_join(pretvori.leta, by = "solsko.leto") %>%
  left_join(tabela3.2, by =c("spol" = "spol", "leto" = "leto", "kohezijska.regija" = "slo")) %>%
  select(-leto)

  

#_________________________________________________________________________
#4.tabela-DIPLOMANTI
tabela4.1 <- read_csv2("podatki/diplomanti.csv", skip = 2, 
                     #col_names = c(),
                     locale=locale(encoding="Windows-1250"))
tabela4.1 <- tabela4.1 %>% 
  pivot_longer(
    cols = colnames(tabela4.1)[-1 : -4],
    names_to = "leto",
    values_to = "stevilo.diplomantov"
  ) 

tabela4.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"))

colnames(tabela4.2)[1] <-"spol" 
colnames(tabela4.2)[2] <-"starost"

tabela4.2 <- tabela4.2 %>% 
  pivot_longer(
    cols = colnames(tabela4.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  separate(
    col = leto.regija,
    into = c("leto", "regija"),
    sep = " "
  )%>%
  select(-starost)%>%
  mutate(leto = parse_number(leto))%>%
  group_by(spol, leto) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev)/2) 

#zdruzimo tabela4.1 in tabela4.2
tabela4 <- tabela4.1 %>%
  left_join(tabela4.2, by = c("leto" = "leto", "spol" = "spol"))