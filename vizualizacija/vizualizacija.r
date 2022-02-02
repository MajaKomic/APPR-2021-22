# 3. faza: Vizualizacija podatkov
source("lib/libraries.r")
#_______________________________________________________________________________
#1.GRAF: DINAMIKA VPISA 
tabela.graf1.1 <- tabela1 %>%
  filter(letnik == "1. letnik") %>%
  dplyr::select(1, 4, 6) %>%
  group_by(vrsta.izobrazevanja, solsko.leto) %>%    
  summarise(stevilo.vpisanih = sum(stevilo.vpisanih))

tabela.graf1.2 <- tabela3.1 %>%
  dplyr::select(1, 5, 6) %>%
  group_by(vrsta.izobrazevanja, solsko.leto) %>%    
  summarise(stevilo.vpisanih = sum(stevilo.vpisanih))

pomozna.tabela <- tibble(
  vrsta.izobrazevanja = c("Nižje poklicno", "Srednje poklicno", "Srednje splošno", 
            "Srednje tehniško in drugo strokovno",
            "Višje strokovno", "Visokošolsko strokovno (prejšnje)", 
            "Visokošolsko strokovno (1. bolonjska stopnja)",
            "Visokošolsko univerzitetno (1. bolonjska stopnja)", 
            "Visokošolsko univerzitetno (prejšnje)",
            "Magistrsko (2. bolonjska stopnja) - po končani 1. bolonjski stopnji",
            "Magistrsko (2. bolonjska stopnja) - enovito magistrsko", "Specialistično", 
            "Magistrsko (prejšnje)",
            "Doktorsko (prejšnje)", "Doktorsko (3. bolonjska stopnja)"),
  stopnja.izobrazbe = c("III. stopnja izobrazbe", "VI. stopnja izobrazbe",
                        "V. stopnja izobrazbe","V. stopnja izobrazbe",
           "VI. stopnja izobrazbe", "VI. stopnja izobrazbe","VI. stopnja izobrazbe",
           "VI. stopnja izobrazbe","VII. stopnja izobrazbe", "VII. stopnja izobrazbe",
           "VII. stopnja izobrazbe","VIII. stopnja izobrazbe",
           "VIII. stopnja izobrazbe", "VIII. stopnja izobrazbe","VIII. stopnja izobrazbe")
)

tabela.graf1 <- rbind(tabela.graf1.1, tabela.graf1.2) %>%
  left_join(pomozna.tabela, by = "vrsta.izobrazevanja") %>%
  relocate(2, 3, 4, 1)%>%
  dplyr::select(-vrsta.izobrazevanja)%>%
  group_by(stopnja.izobrazbe, solsko.leto) %>%
  summarise(stevilo.vpisanih = sum(stevilo.vpisanih))

graf1 <- ggplot(tabela.graf1) + 
  aes(
    x = solsko.leto, 
    y = stevilo.vpisanih, 
    group = stopnja.izobrazbe,
    colour = stopnja.izobrazbe
  ) + 
  geom_line() +                         
  geom_point() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust =  0),
    plot.title = element_text(hjust = 0.5) #naslov na sredini
  ) +
  labs(
    x = "Šolsko leto",
    y = "Število vpisanih",
    title = "DINAMIKA VPISA"
  ) +
  guides(colour = guide_legend(title="Stopnja izobrazbe:"))


#______________________________________________________________________________________
#2.GRAF: starostne skupine v letnikih
tabela.graf2 <- tabela1 %>%
  dplyr::select(1, 2, 3, 6) %>%                     
  group_by(vrsta.izobrazevanja, starost, letnik) %>%    
  summarise(povp.stevilo.vpisanih = ceiling(sum(stevilo.vpisanih)/12)) 

graf2 <-  ggplot(tabela.graf2) +
  aes(
    x = letnik, 
    y = povp.stevilo.vpisanih, 
    fill = starost
  ) +
  geom_bar( 
    stat='identity', 
    position = position_fill(reverse = TRUE)
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust =  0),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Letnik",
    y = "Povprečno število vpisanih na leto",
    title = "STAROSTNA SESTAVA SEKUNDARNEGA IZOBRAŽEVANJA"
  ) +
  facet_wrap(~vrsta.izobrazevanja, ncol = 2) +
  scale_fill_brewer(palette="PiYG") +
  guides(fill=guide_legend(title="Starost:")) 

#___________________________________________________________________________________________
#ZEMLJEVID: ŠTEVILO MATURANTOV NA 1000 PREBIVALCEV PO REGIJAH
#zemljevid Slovenije po regijah

source("lib/uvozi.zemljevid.r")
zemljevid <- uvozi.zemljevid("http://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/si/gadm36_SVN_shp.zip",
                             "gadm36_SVN_1", mapa = "zemljevidi", encoding="UTF-8")

zemljevid <- zemljevid %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84")) # pretvorimo v ustrezen format

loc <- locale(encoding="UTF-8")
for (col in names(zemljevid)) {
  if (is.character(zemljevid[[col]])) {
    zemljevid[[col]] <- zemljevid[[col]] %>% 
      parse_character(locale=loc)
  }
}

zemljevid@data
zemljevid.regije <- zemljevid
names(zemljevid.regije)
zemljevid.regije$NAME_1

zemljevid.regije$NAME_1 <- factor(zemljevid.regije$NAME_1)

lvls <- levels(zemljevid.regije$NAME_1)

# spremenim imena regij, da se ujemajo z zemljevidom, kratice spremenim nazaj v dolga imena
imena.regij.nazaj = tibble(
  regija = c("Gorenjska", "Goriška", "Jugovzhodna Slovenija", "Koroška", 
             "Obalno-kraška", "Osrednjeslovenska", "Podravska", "Pomurska", 
             "Spodnjeposavska", "Notranjsko-kraška", "Savinjska","Zasavska",
             "SLOVENIJA"),
  oznaka = c(
    "kr", "ng", "nm", "sg", "kp", "lj", "mb", "ms", "kk", "po", "ce", "za", "slo")
)

#podatki, ki jih bom prikazala na zemljeviu
tabela.zemljevid1.1 <- tabela2.2 %>%
  group_by(regija) %>%
  summarise(povp.st.prebivalcev = sum(stevilo.prebivalcev) / 12)

tabela.zemljevid1 <- tabela2.1 %>%
  dplyr::select(1, 2, 4) %>%
  group_by(regija, solsko.leto) %>%
  summarise(stevilo.maturantov = sum(stevilo.maturantov)) %>%
  group_by(regija)%>%
  summarise(povp.st.maturantov = sum(stevilo.maturantov)/12) %>%
  left_join(tabela.zemljevid1.1, by = c("regija" = "regija")) %>%
  mutate(promil = povp.st.maturantov/povp.st.prebivalcev * 1000) %>%
  filter(regija != "SLOVENIJA")%>%
  dplyr::select(1, 4) 
  
tabela.zemljevid1$regija <- str_replace_all(
  tabela.zemljevid1$regija, 
  c("Primorsko-notranjska" = "Notranjsko-kraška", "Posavska"= "Spodnjeposavska")
)

#narišem zemljevid
tmap_mode("plot")
zemljevid1 <- merge(
    x = tabela.zemljevid1, 
    y = imena.regij.nazaj, 
    by = "regija", all.x = TRUE
  ) %>% 
  dplyr::select(regija, promil) 

zemljevid1 <- merge(zemljevid.regije, 
                    zemljevid1, 
                    by.x = "NAME_1", 
                    by.y = "regija")

zem1 <- tm_shape(zemljevid1) + 
  tm_polygons(
    "promil" , 
    popup.vars = c("Število na 1000 prebivalcev: " = "promil"),
    style = "pretty", 
    palette="Blues",
    title="Število maturantov\nna 1000 prebivalcev"
  ) +  
  tm_layout(
    "Povprečno število maturantov na 1000 prebivalcev",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1) +
  tm_text("NAME_1", size = 2/3)


#_______________________________________________________________________________________
#4.GRAF: PRIMERJAVA REDNI IN IZREDNI ŠTUDIJ
tabela.graf4 <- tabela3.1 %>%
  dplyr::select(1, 2, 6) %>%
  left_join(pomozna.tabela, by = "vrsta.izobrazevanja") %>%
  dplyr::select(2, 3, 4) %>%
  group_by(stopnja.izobrazbe, nacin.studija) %>%
  summarise(stevilo.vpisanih = sum(stevilo.vpisanih)) 

tabela.graf4$nacin.stopnja.izobrazbe <- paste(tabela.graf4$nacin.studija,tabela.graf4$stopnja.izobrazbe)

#izračun procentov
pct <- round(100*tabela.graf4$stevilo.vpisanih/sum(tabela.graf4$stevilo.vpisanih))

graf4 <- ggplot(tabela.graf4) +
  aes(x = "", 
      y = -stevilo.vpisanih, 
      fill = nacin.stopnja.izobrazbe
  ) + 
  geom_bar(
    stat = "identity", 
    color = "black", width=1
  ) + 
  coord_polar("y", start=0) +
  geom_text(
    aes(label = paste0(pct, "%")), 
    position = position_stack(vjust = 0.5)
  )+
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Spectral") +
  labs(
    x = NULL, 
    y = NULL, 
    fill = "Način študija in\nstopnja izobrazbe:", 
    title = "VPIS V REDNI IN IZREDNI ŠTUDIJ"
  )+
  theme_void() 



#_____________________________________________________________________________________
#5.GRAF: Vpis na fakultete po končani srednji šoli na 1000 prebivalcev
tabela.graf5 <- tabela3 %>%
  filter(vrsta.izobrazevanja != "Doktorsko (3. bolonjska stopnja)", 
         vrsta.izobrazevanja != "Doktorsko (prejšnje)", 
         vrsta.izobrazevanja != "Magistrsko (2. bolonjska stopnja) - po končani 1. bolonjski stopnji",
         vrsta.izobrazevanja != "Magistrsko (prejšnje)",
         vrsta.izobrazevanja != "Specialistično") %>%
  dplyr::select(1, 3, 4, 5, 8) %>%
  group_by(spol, kohezijska.regija, solsko.leto) %>%
  summarise(promil = sum(delez))

tabela.graf5$spol.regija <- paste(tabela.graf5$spol,tabela.graf5$kohezijska.regija)

tabela.graf5$spol.regija <- str_replace_all(
  tabela.graf5$spol.regija,
  c("Moški Vzhodna Slovenija" = "Moški iz\nV Slovenije",
    "Moški Zahodna Slovenija" = "Moški iz\nZ Slovenije",
    "Ženske Vzhodna Slovenija" = "Ženske iz \nV Slovenije",
    "Ženske Zahodna Slovenija" = "Ženske iz\nZ Slovenije"))

graf5 <- ggplot(tabela.graf5) + 
  geom_point(
    aes(x = solsko.leto, 
        y = spol.regija, 
        size = promil, 
        colour = promil,
        text = promil)
  ) + 
  scale_size(breaks = seq(10, 22, by=1)) +
  coord_fixed() +
  theme_minimal()+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust =  0),
    plot.title = element_text(hjust = 0.5),
    legend.position="bottom"
  ) +
  labs(
    x = "Šolsko leto",
    y = "Spol in del Slovenije",
    title = "VPIS NA FAKULTETE PO KONČANI\nSREDNJI ŠOLI NA 1000 PREBIVALCEV"
  ) +
  scale_colour_gradientn(colours=terrain.colors(10)) +
  guides(
    size = guide_legend(title="Število vpisanih\nna 1000 prebivalcev"),
    colour = guide_legend(title="Število vpisanih\nna 1000 prebivalcev")
  ) 

#________________________________________________________________________________________
#6.GRAF: PRIMERJAVA POVPREČNEGA ŠTEVILA DIPLOMANTOV GLEDE NA VRSTO IZOBRAZEVANJA IN SPOL
#pomozna tabela
razdeli.imena <- tibble(
  vrsta.izobrazevanja = c("Višje strokovno", "Visokošolsko univerzitetno (prejšnje)", 
                          "Visokošolsko univerzitetno (1. bolonjska stopnja)", "Visokošolsko strokovno (prejšnje)",
                          "Visokošolsko strokovno (1. bolonjska stopnja)", "Magistrsko (prejšnje)",
                          "Magistrsko (2. bolonjska stopnja) - po končani 1. bolonjski stopnji",
                          "Magistrsko (2. bolonjska stopnja) - enovito magistrsko",
                          "Doktorsko (prejšnje)", "Doktorsko (3. bolonjska stopnja)"),
  nova = c("Višje strokovno", "Visokošolsko univerzitetno\n(prejšnje)", 
           "Visokošolsko univerzitetno\n(1. bolonjska stopnja)", "Visokošolsko strokovno\n(prejšnje)",
           "Visokošolsko strokovno\n(1.bolonjska stopnja)", "Magistersko (prejšnje)",
           "Magistersko (2. bolonjska stopnja)\n-po končani 1. bolonjski stopnji",
           "Magistersko (2. bolonjska stopnja)\n-enovitni magisterski študij",
           "Doktorsko (prejšnje)", "Doktorsko (3. bolonjska stopnja)")
)

tabela.graf6 <- tabela4 %>%
  dplyr::select(2, 3, 5)%>%
  filter(vrsta.izobrazevanja != "Specialistično") %>%
  left_join(razdeli.imena, by = "vrsta.izobrazevanja") %>%
  dplyr::select(-vrsta.izobrazevanja) %>%
  group_by(nova, spol) %>%
  summarise(stevilo.diplomantov = ceiling(sum(stevilo.diplomantov)/ 18)) 


graf6 <- ggplot(tabela.graf6) +
  aes(
    x = nova, 
    y = stevilo.diplomantov, 
    fill = spol
  ) +
  geom_bar(stat='identity')+
  coord_flip() +
  geom_text(
    aes(label=stevilo.diplomantov), 
    position=position_stack(vjust=0.5)
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic() +
  scale_fill_manual(values=c("#99CCFF", "#FF66CC")) +
  labs(
      x = "Vrsta izobraževanja",
      y = "Povprečno letno število diplomantov",
      title = "POVPREČNO ŠTEVILO DIPLOMANTOV\nGLEDE NA VRSTO IZOBRAZEVANJA IN SPOL"
    ) +
    guides(fill=guide_legend(title="Spol:")) 


