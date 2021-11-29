# Analiza podatkov s programom R - 2021/22
# Analiza izobraževanja v Sloveniji
## Maja Komic
Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika
### Opis
Analizirati želim, kako se skozi leta spreminja vpis glede na vrsto srednješolskega izobraževanja, ter primerjati koliko dijakov srednješolsko izobrazbo konča. Nato pa še vpis v terciarno izobraževanje glede na področje izobraževanja, ter koliko študentov vzpešno diplomira. Zanimalo me bo tudi koliko sredstev Slovenija nameni za izobraževanje in koliko sredstev se v te namene porabi.
V nadaljevanju bom analizirala še stopnjo izobraženosti po starostnih skupinah ter primerjala Slovenijo z ostalimi Evropskimi državami.

### Tabele
1.TABELA: Srednješolso izobraževanje v Sloveniji
*vrsta izobraževanja,
*letnik,
*spol,
*leto,
*število vpisanih v letnik
*število dijakov, ki je vzpešno zaključilo srednjo šolo.

2.TABELA: Terciarno izobraževanje v Sloveniji
*področje izobraževanja,
*vrsta izobraževanja,
*način študija,
*število vpisanih študentov,
*število diplomantov.

3.TABELA: Javni izdatki namenjeni v izobraževanje v Sloveniji
*raven izobraževanja,
*izdatki po namenu,
*leto,
*količina izdatkov,
*vrsta porabe,
*poraba.

4.TABELA: Sestava prebivalstva glede na stopnjo izobrazbe v Evropskih državah
*država,
*stopnja izobrazbe,
*starostna skupina,
*procent prebivalstva.

### Viri
Podatke sem dobila na naslednjihspletnih straneh:
*https://pxweb.stat.si/SiStat/sl 
*https://ec.europa.eu/eurostat


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
