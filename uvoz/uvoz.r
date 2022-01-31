# 2. faza: Uvoz podatkov
source("lib/libraries.r")
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#_____________________________________________________________________________
#1.TABELA: VPIS V SREDNJE ŠOLE
tabela1.1 <- read_csv2("podatki/dijaki.csv", 
                      skip = 2, 
                      locale=locale(encoding="Windows-1250"),
                      col_types = cols(
                       .default = col_guess(),
                      STAROST = col_factor(),
                      LETNIK = col_factor())
                     )

tabela1.1 <- tabela1.1 %>% 
  pivot_longer(
    cols = colnames(tabela1.1)[-1 : -3],
    names_to = "solsko.leto.spol",
    values_to = "stevilo.vpisanih"
  ) %>%
  rename(
    "vrsta.izobrazevanja" = "VRSTA IZOBRAŽEVANJA", 
    "letnik" = "LETNIK", 
    "starost" = "STAROST" 
  ) %>%
  separate(
    col = solsko.leto.spol,
    into = c("solsko.leto", "spol"),
    sep = " " 
  ) 

tabela1.1[6] <- sapply(tabela1.1[6], as.numeric) 
tabela1.1[is.na(tabela1.1)] = 0     
tabela1.1$solsko.leto <- as.factor(tabela1.1$solsko.leto)
tabela1.1$spol <- as.factor(tabela1.1$spol)

#tabela: število prebivalcev po starosti
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
  dplyr::select(-slo) %>%                      
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

#združim tabeli 1.1 in 1.2 in si pomagam s pomožno tabelo:
tabela1 <- tabela1.1 %>%
  left_join(pretvori.leta1, by = "solsko.leto") %>%
  left_join(tabela1.2, by = c("starost" = "starost", "spol" = "spol", "leto" = "leto")) %>%
  dplyr::select(-leto) %>%
  mutate(delez = stevilo.vpisanih / stevilo.prebivalcev * 1000)


#_________________________________________________________________________________
#2.TABELA: MATURANTJE
tabela2.1 <- read_csv2("podatki/maturantje.csv", skip = 2,
                     locale=locale(encoding="Windows-1250"))

tabela2.1[, -1:-2] <- sapply(tabela2.1[, -1:-2], as.numeric)

tabela2.1 <- tabela2.1 %>% 
  pivot_longer(
    cols = colnames(tabela2.1)[-1 : -2],
    names_to = "solsko.leto.vrsta.izobrazevanja",      
    values_to = "stevilo.maturantov"
  ) %>%
  rename(
    "vrsta.zavoda" = "VRSTA ZAVODA", 
    "regija" = "STATISTIČNA REGIJA"
  ) %>%
  tidyr::extract(
    col = solsko.leto.vrsta.izobrazevanja,
    into = c("solsko.leto", "vrsta.izobrazevanja"),
    regex = "^([0-9]*/[0-9]*)(\\n*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*)"    
  ) %>%  
  dplyr::select(2, 3, 4, 5) %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

tabela2.1$stevilo.maturantov <- as.numeric(tabela2.1$stevilo.maturantov)
tabela2.1$solsko.leto <- as.factor(tabela2.1$solsko.leto)
tabela2.1[is.na(tabela2.1)] = 0

#tabela: število prebivalcev po regijah
tabela2.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"))

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
  dplyr::select(-starost) %>%
  rowwise() %>%                           #pri tidyr::extract je v stolpcu regija
  mutate_all(funs(str_squish(.))) %>%     #nastal presledek pred vsako regijo,
  ungroup()                               #tako se tega presledka znebim
  
tabela2.2[4] <- sapply(tabela2.2[4], as.numeric)
tabela2.2 <- tabela2.2 %>%
  mutate(leto = parse_number(leto)) %>% 
  group_by(leto, regija) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev)) 

#tabela, ki mi bo pomagala pretvorit solska leta in leta
#ta tabela se razlikuje od tabele pretvori.leta1, ker vpis poteka septembra, torej 
#pretvroimo npr 2008/09 v 2008, matura je pa spomladi torej pretvorimo 2008/09 v 2009
pretvori.leta2 <- tibble(
  solsko.leto = c("2008/09", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", 
                  "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20"),
  leto = as.double(c("2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                     "2016", "2017", "2018", "2019", "2020")),
)

#združim tabelo 2.1 in 2.2
tabela2 <- tabela2.1 %>%
  left_join(pretvori.leta2, by = "solsko.leto")%>%
  left_join(tabela2.2, by = c("regija" = "regija", "leto" = "leto")) %>%
  dplyr::select(-leto) %>%
  mutate(delez.promil = stevilo.maturantov / stevilo.prebivalcev * 1000)


#_______________________________________________________________________________
#3.TABELA: VPIS NA FAKULTETE
tabela3.1 <- read_csv2("podatki/studentje.csv", skip = 2, 
                     locale=locale(encoding="Windows-1250"))

tabela3.1[, -1:-4] <- sapply(tabela3.1[, -1:-4], as.numeric) 
tabela3.1[is.na(tabela3.1)] = 0  

tabela3.1 <- tabela3.1 %>% 
  pivot_longer(
    cols = colnames(tabela3.1)[-1:-4],
    names_to = "solsko.leto",
    values_to = "stevilo.vpisanih"
  ) %>%
  rename(
    "vrsta.izobrazevanja" = "VRSTA IZOBRAŽEVANJA", 
    "nacin.studija" = "NAČIN ŠTUDIJA",
    "spol" = "SPOL", 
    "kohezijska.regija" = "KOHEZIJSKA REGIJA")

#tabela ki pretvarja regije v zahodne oz vzhodne
regije.slo <- tibble(
  slo = c("Zahodna Slovenija", "Zahodna Slovenija", "Zahodna Slovenija", 
          "Zahodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija",
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija",
          "Vzhodna Slovenija", "Vzhodna Slovenija", "Vzhodna Slovenija", 
          "Slovenija"),
  regija = c("Gorenjska", "Goriška", "Obalno-kraška", "Osrednjeslovenska",
             "Pomurska", "Podravska", "Koroška", "Savinjska", "Zasavska", 
             "Posavska", "Jugovzhodna Slovenija", "Primorsko-notranjska", 
             "SLOVENIJA")
)

#tabela: prebivalstvo po regijah za V in Z Slovenijo
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
  dplyr::select(-starost)%>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup() %>%
  mutate(leto = parse_number(leto)) %>%
  left_join(regije.slo, by = c("regija" = "regija")) %>%     
  dplyr::select(-regija)                                            
  
tabela3.2[3] <- sapply(tabela3.2[3], as.numeric) 

tabela3.2 <- tabela3.2 %>% 
  relocate(slo, .after = leto) %>%
  group_by(spol, leto, slo) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev))  

#zdruzim tabela3.1 in tabela3.2
tabela3 <- tabela3.1 %>%
  left_join(
    pretvori.leta1, 
    by = "solsko.leto"
  ) %>%
  left_join(
    tabela3.2, 
    by =c("spol" = "spol", "leto" = "leto", "kohezijska.regija" = "slo")
  ) %>%
  dplyr::select(-leto)%>%
  mutate(delez = stevilo.vpisanih / stevilo.prebivalcev * 1000)

#_________________________________________________________________________
#4.tabela-DIPLOMANTI
tabela4.1 <- read_csv2("podatki/diplomanti.csv", skip = 2, 
                     locale=locale(encoding="Windows-1250"))

tabela4.1[, -1:-3] <- sapply(tabela4.1[, -1:-3], as.numeric)
tabela4.1[is.na(tabela4.1)] = 0  

tabela4.1 <- tabela4.1 %>% 
  pivot_longer(
    cols = colnames(tabela4.1)[-1 : -3],
    names_to = "vrsta.zavoda.vrsta.izobrazevanja",
    values_to = "stevilo.diplomantov"
  )%>%
  rename(
    "spol" = "SPOL", 
    "leto" = "LETO",
    "nacin.studija" = "NAČIN ŠTUDIJA")

tabela4.1$vrsta.zavoda.vrsta.izobrazevanja <- str_replace_all(
  tabela4.1$vrsta.zavoda.vrsta.izobrazevanja,
  c("Višje" = ", Višje","Visokošolsko" = ", Visokošolsko", 
    "Magistrsko" = ", Magistrsko","Specialistično" = ", Specialistično",
    "Doktorsko" = ", Doktorsko"))

tabela4.1 <- tabela4.1 %>%
  separate(
    col = vrsta.zavoda.vrsta.izobrazevanja,
    into = c("vrsta.zavoda", "vrsta.izobrazevanja"),
    sep = ","
  ) %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup() 

tabela4.1$vrsta.zavoda <- str_extract(tabela4.1$vrsta.zavoda,"[A-Za-z]+")
tabela4.1[is.na(tabela4.1)] = "Specialistično"
tabela4.1$leto <- as.double(tabela4.1$leto)
tabela4.1$stevilo.diplomantov <- as.double(tabela4.1$stevilo.diplomantov)

#tabela: prebivalstvo v Sloveniji
tabela4.2 <- read_csv2("podatki/prebivalstvo_po_regijah.csv", skip = 2, 
                       locale=locale(encoding="Windows-1250"))

tabela4.2 <- tabela4.2 %>% 
  pivot_longer(
    cols = colnames(tabela4.2)[-1 : -2],
    names_to = "leto.regija",
    values_to = "stevilo.prebivalcev"
  ) %>%
  rename(
    "spol" = "SPOL", 
    "starost" = "STAROST"
  ) %>%
  tidyr::extract(
    col = leto.regija,
    into = c("leto", "regija"),
    regex = "^([0-9]*[H][12])(\\n*\\s*[A-zčšžČŠŽ]*\\s*[A-zčšžČŠŽ]*)"          
  )%>%
  dplyr::select(-starost)%>%
  mutate(leto = parse_number(leto))%>%
  group_by(spol, leto) %>%                 
  summarise(stevilo.prebivalcev = sum(stevilo.prebivalcev)/2) 

#zdruzim tabela4.1 in tabela4.2
tabela4 <- tabela4.1 %>%
  left_join(
    tabela4.2, 
    by = c("leto" = "leto", "spol" = "spol")
  ) %>%
  left_join(
    pretvori.leta2,
    by = "leto"
  )%>%
  dplyr::select(-c(leto, vrsta.zavoda))%>%
  relocate(1, 3, 2, 6, 4, 5) %>%
  mutate(delez = stevilo.diplomantov / stevilo.prebivalcev * 1000)

