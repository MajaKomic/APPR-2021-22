# 2. faza: Uvoz podatkov

library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(stringi)

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

tabela1.1 <- tabela1.1 %>% 
  pivot_longer(
    cols = colnames(tabela1.1)[-1 : -3],
    names_to = "solsko.leto.spol",
    values_to = "stevilo.vpisanih"
  ) %>%
  rename("vrsta.izobrazevanja" = "VRSTA IZOBRAŽEVANJA", 
         "letnik" = "LETNIK", 
         "starost" = "STAROST"
  ) %>%
  separate(
    col = solsko.leto.spol,
    into = c("solsko.leto", "spol"),
    sep = " "
  ) 

tabela1.1[6] <- sapply(tabela1.1[6], as.numeric) 
tabela1.1[is.na(tabela1.1)] = 0     #zamenja vse NA v 0
tabela1.1$solsko.leto <- as.factor(tabela1.1$solsko.leto)
tabela1.1$spol <- as.factor(tabela1.1$spol)

#tabela: stevilo prebivalcev po starosti
tabela1.2 <- read_csv2("podatki/prebivalstvo_po_starosti.csv", skip = 2, 
                      locale=locale(encoding="Windows-1250"),
                      col_types = cols(
                        .default = col_guess(),
                        STAROST = col_factor()
                        )
                      )

tabela1.2 <- tabela1.2 %>% 
  pivot_longer(
    cols = colnames(tabela1.2)[-1 : -2],
    names_to = "leto.slo",
    values_to = "stevilo.prebivalcev"
  ) %>%
  rename("spol" = "SPOL", 
         "starost" = "STAROST"
  ) %>%
  separate(
    col = leto.slo,
    into = c("leto", "slo"),
    sep = " "
  ) %>%
  select(-slo)%>%                      
  mutate(leto=parse_number(leto)) %>%  #znebimo se polletne oznake H1 in H2, ostane samo leto
  group_by(spol, starost, leto) %>%    
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))

#tabela za pretvorbo med solskim letom in letom
pretvori.leta1 <- tibble(
  solsko.leto = as.factor(c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15",
                  "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21")),
  leto = as.double(c("2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                     "2016", "2017", "2018", "2019", "2020")),
)

#združimo tabeli 1.1 in 1.2 in si pomagamo s pomožno tabelo:
tabela1 <- tabela1.1 %>%
  left_join(pretvori.leta1, by = "solsko.leto")%>%
  left_join(tabela1.2, by = c("starost" = "starost", "spol" = "spol", "leto" = "leto")) %>%
  select(-leto) %>%
  mutate(delez = stevilo.vpisanih / stevilo.prebivalcev * 100)


#_________________________________________________________________________________
#2.tabela-MATURANTJE
tabela2.1 <- read_csv2("podatki/maturantje.csv", skip = 2,
                     locale=locale(encoding="Windows-1250"))

tabela2.1[, -1:-2] <- sapply(tabela2.1[, -1:-2], as.numeric)

tabela2.1 <- tabela2.1 %>% 
  pivot_longer(
    cols = colnames(tabela2.1)[-1 : -2],
    names_to = "solsko.leto.vrsta.izobrazevanja",      
    values_to = "stevilo.maturantov"
  ) %>%
  rename("vrsta.zavoda" = "VRSTA ZAVODA", 
         "regija" = "STATISTIČNA REGIJA"
  ) %>%
  tidyr::extract(
    col = solsko.leto.vrsta.izobrazevanja,
    into = c("solsko.leto", "vrsta.izobrazevanja"),
    regex = "^([0-9]*/[0-9]*)(\\n*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*)"    
  )%>%
  select(-vrsta.zavoda)


#popravi odvečne presledke, ki so nastali pri uporabi regularnih izrazov
tabela2.1 <- tabela2.1 %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

tabela2.1$stevilo.maturantov <- as.numeric(tabela2.1$stevilo.maturantov)
tabela2.1$solsko.leto <- as.factor(tabela2.1$solsko.leto)
tabela2.1[is.na(tabela2.1)] = 0

tabela2.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"),
                      )

tabela2.2 <- tabela2.2 %>% 
  pivot_longer(
    cols = colnames(tabela2.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  rename("spol" = "SPOL",
         "starost" = "STAROST"
  ) %>%
  tidyr::extract(
    col = leto.regija,
    into = c("leto", "regija"),
    regex = "^([0-9]*[H][12])(\\n*\\s*[A-zčšžČŠŽ]*\\s*-*[A-zčšžČŠŽ]*)"  
  )%>%
  rowwise() %>%                           #pri tidyr::extract je v stolpcu regija
  mutate_all(funs(str_squish(.))) %>%     #nastal presledek pred vsako regijo,
  ungroup() %>%                           #tako se tega presledka znebim
  select(-starost)%>%
  mutate(leto = parse_number(leto)) %>% 
  group_by(leto, regija) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev)) 
  
tabela2.2[3] <- sapply(tabela2.2[3], as.numeric)

#tabela, ki mi bo pomagala pretvorit solska leta in leta
#ta tabela se razlikuje od tabele pretvori.leta2, ker vpis poteka septembra, torej 
#pretvroimo npr 2008/09 v 2008, matura je pa spomladi torej pretvorimo 2008/09 v 2009
pretvori.leta2 <- tibble(
  solsko.leto = c("2008/09", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15",
                  "2015/16", "2016/17", "2017/18", "2018/19", "2019/20"),
  leto = as.double(c("2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                     "2016", "2017", "2018", "2019", "2020")),
)

#združimo tabelo 2.1 in 2.2
tabela2 <- tabela2.1 %>%
  left_join(pretvori.leta2, by = "solsko.leto")%>%
  left_join(tabela2.2, by = c("regija" = "regija", "leto" = "leto")) %>%
  select(-leto) %>%
  mutate(delez.promil = stevilo.maturantov / stevilo.prebivalcev * 1000)


#_______________________________________________________________________________
#3.tabela-VPIS NA FAKULTETE
tabela3.1 <- read_csv2("podatki/studentje.csv", skip = 2, 
                     locale=locale(encoding="Windows-1250"))

tabela3.1[, -1:-4] <- sapply(tabela3.1[, -1:-4], as.numeric) 
tabela3.1[is.na(tabela3.1)] = 0  #vse NA spremeni v 0

tabela3.1 <- tabela3.1 %>% 
  pivot_longer(
    cols = colnames(tabela3.1)[-1:-4],
    names_to = "solsko.leto",
    values_to = "stevilo.vpisanih"
  ) %>%
  rename("vrsta.izobrazevanja" = "VRSTA IZOBRAŽEVANJA", 
         "nacin.studija" = "NAČIN ŠTUDIJA",
         "spol" = "SPOL", 
         "kohezijska.regija" = "KOHEZIJSKA REGIJA")

#tabela ki pretvarja regije v zahodne oz vzhodne
regije.slo <- tibble(
  slo = c("Zahodna Slovenija", "Zahodna Slovenija", "Zahodna Slovenija", "Zahodna Slovenija", 
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", 
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", "Slovenija"),
  regija = c("Gorenjska", "Goriška", "Obalno-kraška", "Osrednjeslovenska","Pomurska", "Podravska", 
             "Koroška", "Savinjska", "Zasavska", "Posavska", "Jugovzhodna Slovenija", "Primorsko-notranjska", "SLOVENIJA")
)

tabela3.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                         locale=locale(encoding="Windows-1250"))

tabela3.2 <- tabela3.2 %>% 
  pivot_longer(
    cols = colnames(tabela3.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  rename("spol" = "SPOL", 
         "starost" = "STAROST"
  ) %>%
  tidyr::extract(
    col = leto.regija,
    into = c("leto", "regija"),
    regex = "^([0-9]*[H][12])(\\n*\\s*[A-zčšžČŠŽ]*\\s*-*[A-zčšžČŠŽ]*)"     
  )%>%
  select(-starost)%>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup() %>%
  mutate(leto = parse_number(leto)) %>%
  left_join(regije.slo, by = c("regija" = "regija")) %>%     
  select(-regija)                                            
  
tabela3.2[3] <- sapply(tabela3.2[3], as.numeric) 

tabela3.2 <- tabela3.2 %>% 
  relocate(slo, .after = leto) %>%
  group_by(spol, leto, slo) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))  
  
#zdruzimo tabela3.1 in tabela3.2
tabela3 <- tabela3.1 %>%
  left_join(pretvori.leta1, by = "solsko.leto") %>%
  left_join(tabela3.2, by =c("spol" = "spol", "leto" = "leto", "kohezijska.regija" = "slo")) %>%
  select(-leto)%>%
  mutate(delez = stevilo.vpisanih / stevilo.prebivalcev * 100)

#_________________________________________________________________________
#4.tabela-DIPLOMANTI
tabela4.1 <- read_csv2("podatki/diplomanti.csv", skip = 2, 
                     #col_names = c(),
                     locale=locale(encoding="Windows-1250"))
tabela4.1[, -1:-3] <- sapply(tabela4.1[, -1:-3], as.numeric)
tabela4.1[is.na(tabela4.1)] = 0  #vse NA spremeni v 0

tabela4.1 <- tabela4.1 %>% 
  pivot_longer(
    cols = colnames(tabela4.1)[-1 : -3],
    names_to = "vrsta.zavoda.vrsta.izobrazevanja",
    values_to = "stevilo.diplomantov"
  )  %>%
  tidyr::extract(
    col = vrsta.zavoda.vrsta.izobrazevanja,
    into = c("vrsta.zavoda", "vrsta.izobrazevanja"),
    regex = "^([0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*
    [0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*\\s*
    [0-9A-zčšžČŠŽ]*\\s*[05]*[05]*%*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*)
    (\\n*\\s*[A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*[:punct:]*[12]*.*\\s*[0-9A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*[:punct:]*\\s*
    -*\\s*[A-zčšžČŠŽ]*\\s*[0-9A-zčšžČŠŽ]*)" 
  ) 

tabela4.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"))

tabela4.2 <- tabela4.2 %>% 
  pivot_longer(
    cols = colnames(tabela4.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  rename("spol" = "SPOL", 
         "starost" = "STAROST"
  ) %>%
  tidyr::extract(
    col = leto.regija,
    into = c("leto", "regija"),
    regex = "^([0-9]*[H][12])(\\n*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*)"          
  )%>%
  select(-starost)%>%
  mutate(leto = parse_number(leto))%>%
  group_by(spol, leto) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev)/2) 

#zdruzimo tabela4.1 in tabela4.2
tabela4 <- tabela4.1 %>%
  left_join(tabela4.2, by = c("leto" = "leto", "spol" = "spol"))