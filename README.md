# Analiza podatkov s programom R - 2021/22
## Maja Komic
Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza izobraževanja v Sloveniji
### Opis
V tem projektu bom analizirala izobraževanje v Sloveniji. Zanimal me bo vpis v srednje šole in fakultete v primerjavi s številom maturantov in diplomantov. Ter izdatki države za izobraževanje in njihova poraba.

### Tabele
1.TABELA: Srednješolso izobraževanje v Sloveniji
* vrsta izobraževanja (<chr>),
* letnik (<chr>),
* spol (<fctr>),
* leto (<dbl>),
* število vpisanih v letnik (<dbl>),
* število dijakov, ki je vzpešno zaključilo srednjo šolo (<dbl>).

2.TABELA: Terciarno izobraževanje v Sloveniji
* področje izobraževanja (<chr>),
* vrsta izobraževanja (<chr>),
* način študija (<chr>),
* število vpisanih študentov (<dbl>),
* število diplomantov (<dbl>).

3.TABELA: Javni izdatki namenjeni v izobraževanje v Sloveniji
* raven izobraževanja (<chr>),
* izdatki po namenu (<chr>),
* leto (<dbl>),
* količina izdatkov (<dbl>),
* vrsta porabe (<chr>),
* poraba (<dbl>).

4.TABELA: Sestava prebivalstva glede na stopnjo izobrazbe v Evropskih državah
* država (<chr>),
* stopnja izobrazbe (<chr>),
* starostna skupina (<fctr>),
* procent prebivalstva (<dbl>).

### Viri
Podatke sem dobila na naslednjihspletnih straneh:
* https://pxweb.stat.si/SiStat/sl (podatki v obliki .csv)
* https://ec.europa.eu/eurostat (podatki v obliki .html)

### Plan dela
Analizirati želim, kako se skozi leta spreminja vpis glede na vrsto srednješolskega izobraževanja, ter primerjati koliko dijakov srednješolsko izobrazbo konča. Nato pa še vpis v terciarno izobraževanje glede na področje izobraževanja, ter koliko študentov vzpešno diplomira. Zanimalo me bo tudi koliko sredstev Slovenija nameni za izobraževanje in koliko sredstev se v te namene porabi.
V nadaljevanju bom analizirala še stopnjo izobraženosti po starostnih skupinah ter primerjala Slovenijo z ostalimi Evropskimi državami.

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
