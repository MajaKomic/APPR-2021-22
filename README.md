# Analiza podatkov s programom R - 2021/22
## Maja Komic
Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza izobraževanja v Sloveniji
### Opis

V tem projektu bom analizirala izobraževanje v Sloveniji. 
Zanimal me bo vpis v srednje šole in fakultete v primerjavi s številom maturantov in diplomantov. 

### Tabele

1.TABELA: Srednješolso izobraževanje v Sloveniji
* vrsta izobraževanja (chr),
* letnik (fct),
* starost (fct),
* spol (fctr),
* šolsko leto (chr),
* število vpisanih v letnik (dbl),
* delež dijakov na 1000 prebivalcev (dbl).

2.TABELA: Maturantje v Sloveniji
* vrsta izobraževanja (chr),
* regija (chr),
* šolsko leto (chr),
* število maturantov (dbl),
* delež maturantov na 1000 prebivalcev.

3.TABELA: Terciarno izobraževanje v Sloveniji
* vrsta izobraževanja (chr),
* način študija (chr),
* spol (chr),
* kohezijska regija (chr),
* šolsko leto (chr),
* število vpisanih študentov (dbl),
* delež študentov na 1000 prebivalcev (dbl).

4.TABELA: Diplomanti v Sloveniji
* vrsta izobraževanja (chr),
* način študija(chr),
* vrsta zavoda (chr),
* šolsko leto (chr),
* spol(chr),
* število diplomantov(dbl),
* delež na 1000 prebivalcev (dbl).

### Viri

Podatke sem dobila na naslednji spletni strani:
* https://pxweb.stat.si/SiStat/sl (podatki v obliki .csv)

### Plan dela

Analizirati želim, kako se skozi leta spreminja vpis glede na vrsto 
srednješolskega izobraževanja, ter primerjati koliko dijakov srednješolsko 
izobrazbo konča. Nato pa še vpis v terciarno izobraževanje glede na področje 
izobraževanja, ter koliko študentov vzpešno diplomira. 

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
