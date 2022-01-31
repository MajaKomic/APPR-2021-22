# 4. faza: Napredna analiza podatkov

#RAZVRŠČANJE V SKUPINE

#funkcije iz predavanj in vaj, ki sem jih uporabila v nadaljevanju:
hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  n = length(dendrogram$height) + 1  # število primerov in nastavitev parametra do
  if (is.null(do)) {
    do = n - 1
  }
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]) %>%
    mutate(dvisina = visina - lag(visina)) %>%         # sprememba višine
    mutate(               # ali se je intenziteta spremembe dovolj spremenila?
      koleno = lead(dvisina) - dvisina > eps)
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k, pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {   
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red") +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red") +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") + ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}


obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}

#________________________________________________________________________________
#uporabila bom podatke tabela2
tabela.skupine1.1 <- tabela2 %>%
  dplyr::select(1, 2, 6) %>%
  filter(regija != "SLOVENIJA") %>%
  group_by(regija, solsko.leto) %>%
  summarise(promil = sum(delez.promil)) 

tabela.skupine1.2 <- tabela.skupine1.1%>%
  pivot_wider(
    names_from = solsko.leto,
    values_from = promil
  )

#wardove razdalje:
regije <- tabela.skupine1.2[, 1] %>% unlist()
razdalje <- tabela.skupine1.2[, -1] %>% dist()
dendrogram <- razdalje %>% hclust(method = "ward.D") 

#tabela, za risanje skupin
regije.x.y <- as.tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(regije) %>%
  dplyr::select(regija = ...3, x = V1, y = V2)

#DENDROGRAM
#plot(dendrogram, labels = regije, ylab = "višina", main = NULL)

#HIERARHIČNO RAZVRŠČANJE V SKUPINE
#nariše graf in obarva kolena modro
graf.kolena <- dendrogram %>% hc.kolena() %>% diagram.kolena() #dobila sem dva oprimalna k: 2 in 3
#za k=2:
skupine.2 <- dendrogram %>%
  cutree(k = 2) %>%
  as.ordered()
diagram.skupine(regije.x.y, regije.x.y$regija, skupine.2, 2)
#za k=3:
skupine.3 <- dendrogram %>%
  cutree(k = 3) %>%
  as.ordered()
diagram.skupine(regije.x.y, regije.x.y$regija, skupine.3, 3)


#METODA K-TIH VODITELJEV:
#za r.km:
r.km <- tabela.skupine1.2[, -1] %>% obrisi(hc = FALSE)
opt.st.skupin.r.km <- obrisi.k(r.km)     #optimaleno k=2
diagram.obrisi(r.km) 

set.seed(123)
skupine <- tabela.skupine1.2[, -1] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered
#narišem graf in obkroži skupine:
diagram.skupine(regije.x.y, regije.x.y$regija, skupine, opt.st.skupin.r.km)

#za r.hc:
r.hc <- tabela.skupine1.2[, -1] %>% obrisi(hc = TRUE)
opt.st.skupin.r.hc <- obrisi.k(r.hc)     #optimaleno k=2
diagram.obrisi(r.hc) 

skupine <- tabela.skupine1.2[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = 2) %>%
  as.ordered()
#narišemo graf in obkroži skupine:
diagram.skupine(regije.x.y, regije.x.y$regija, skupine, opt.st.skupin.r.hc)

#funkcija, ki nariše graf za optimalno število kolen glede na nacin dolocanja
narisi <- function(nacin){
  if(nacin == "Hierarhično razvrščanje"){
    graf.kolena <- dendrogram %>% hc.kolena() %>% diagram.kolena()
    graf.kolena
  }else {
    r.km <- tabela.skupine1.2[, -1] %>% obrisi(hc = FALSE)   
    diagram.obrisi(r.km) 
  }
}

narisi("Hierarhično razvrščanje")
narisi("Metoda k-tih voditeljev")


#zemljevid za razvrščanje po skupinah z metodo HAC
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

#funkcija, ki nariše zemljevid glede na število izbranih skupin
zemljevid <- function(k){
  skupine.k <- dendrogram %>%
    cutree(k = k) %>%
    as.ordered()
  
  podatki.k <- regije.x.y %>%
    bind_cols(skupine.k) %>%
    rename(skupina = ...4) %>%
    dplyr::select(1, 4)
  
  podatki.k$regija <- str_replace_all(
    podatki.k$regija, 
    c("Primorsko-notranjska" = "Notranjsko-kraška", "Posavska"= "Spodnjeposavska")
  )
  
  # spremenim imena regij, da se ujemajo z zemljevidom, kratice spremenim nazaj v dolga imena
  imena.regij.nazaj = tibble(
    regija = c("Gorenjska", "Goriška", "Jugovzhodna Slovenija", "Koroška", 
               "Obalno-kraška", "Osrednjeslovenska", "Podravska", "Pomurska", 
               "Spodnjeposavska", "Notranjsko-kraška", "Savinjska","Zasavska",
               "SLOVENIJA"),
    oznaka = c(
      "kr", "ng", "nm", "sg", "kp", "lj", "mb", "ms", "kk", "po", "ce", "za", "slo")
  )
  
  tmap_mode("plot")
  zemljevid1 <- merge(
    x = podatki.k, 
    y = imena.regij.nazaj, 
    by = "regija", all.x = TRUE
  ) %>% 
    dplyr::select(regija, skupina) 
  
  zemljevid1 <- merge(zemljevid.regije, 
                      zemljevid1, 
                      by.x = "NAME_1", 
                      by.y = "regija")
  
  zem <- tm_shape(zemljevid1) + 
    tm_polygons(
      "skupina" , 
      style = "pretty", 
      palette="Blues",
      title="Skupina:"
    ) +  
    tm_layout(
      "",
      legend.title.size=1,
      legend.text.size = 0.8,
      legend.position = c("right","bottom"),
      legend.bg.color = "white",
      legend.bg.alpha = 1) +
    tm_text("NAME_1", size = 4/5)
  
  zem
  
}

#narišem zemljevid za k=2:
zem2 <- zemljevid(2)

#narišem zemljevid za k=3:
zem3 <- zemljevid(3)


#_______________________________________________________________________________
#NAPOVEDNI MODELI







