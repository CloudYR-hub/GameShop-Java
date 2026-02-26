#lazerinio atstuno matuoklio paklaidos:
# 0.01, -0.02, 0.04, 0.01. patikrinkite hipoteze, jog matuoklis nedaro
#sisteminiu paklaidu (jo paklaidu vidurkis 0) su reiksminguo lygmeniu alpha = 0.05
# laikydami, jog sigma - nezinomas

x<-c(0.01, -0.02, 0.04, 0.01)

#hipotezes tikrinimas
t.test(x)
#ats p-value 0.474 > 0.05, todel H0 - neatmestina, galime 
# laikyti, jog nedaro sisteminiu paklaidu

#2 uzd
# masyve msleep yra surinkta informacija apie miego iprocius
#stulpelyje sleep_total yra nurodytas vidutinis val sk per para
# kuri atitinkamas gyvunas praleidzia zzz
# aikydami kad duom pasiskirste per normaluji desni
# patikrinkite hipoteze, jog sleep_total vidurkis = 8h per para kai:
# a) reiksmingumo lygmuo alpha = 0.1 alternatyvi hipoteze Ha: m!=8
# b) reiksmingumo lygmuo alpha = 0.05, alt hipoteze Ha: m<8;
# c) reiksmingumo lygmuo alpha = 0.01, alt hipoteze Ha: m>8

install.packages("ggplot2")
library(ggplot2)

data(msleep) #duomeny masyvas

# sleep_total yra tik stulpelio vardas, o ne R duomeninis obj
#norint kad msleep stulpeliai taptu pasiekiami, reikia:
attach(msleep)

#hipoteziu tikrinimas
#a)
t.test(sleep_total, mu=8, conf.level = 0.9, alternative="two.sided")

#isvada p-value = 3.437e-06 yra mazesne uz 0.1, todel nuline hipoteze
#jog vidutine miego trukme per para lygi 8h per para atmetame
# galime teigti, jog zinduoliu miego trukme statistiskai
# reiksmingai skiriasi nuo 8h per para

#b)
t.test(sleep_total, mu = 8, conf.level=0.95, alternative = "less")
#isvada: p-value = 1 yra > uz 0.05, todel laikome, kad nera pagrindo
#atmesti nuline hipoteze, ir priimti alt hipoteze, jog zinduoliu
#miego trukme yra mazesne nei 8h per para

#c)
t.test(sleep_total, mu=8, conf.level = 0.99, alternative = "greater")
#isvada: p val < 0.01, todel nuline hipoteze, kad jog vid miego per para
# = 8 atmetame, galime teigti, jog miego trukme statistikai >8 val per para

#baigus darba su duomenu rinkiniu, tikslinga ji atjungti:
detach(msleep) # komanda detach() atjungs visus duomenu rinkinius

# hipoteze apie normaliojo skirstinio vidurkio lygybe skaiciui, kai dispersija
# yra zinoma

# 3 uzd
# gamybos proceso metu tirpalo pH turi buti lygiai 8.3. ismatuotos pH reiksmes
# pasiskirste pagal normaluji skirstini su zinomu standrtiniu nuokrypiu
# sigma=0.02. atlikus keturis matavimus, gautos pH: 8.31, 8.34, 8.32, 8.31.
# su reiksmingumo lygmeniu alpha = 0.05 patikrinkite hipoteze, ar tirpalo pH
# a) pasikeite b) padidejo c) sumazejo

x<-c(8.31, 8.34, 8.32, 8.31)
x

m0=8.3  # zinomas vidurkis
st_nuokr = 0.02 # zinomas standartinis nuokrypis
alpha = 0.05 # reiksmingumo lygmuo

library(DescTools)
# a) atvejis
ZTest(x, alternative = "two.sided", mu=m0, sd_pop = st_nuokr,conf.level = 1-alpha)
#isvada:
# kadangi p = 0.046 < alpha = 0.05, nuline hipoteze atmetame. laikome, jog
# ismatuotas tirpalo pH reiksmingai skiriasi nuo numatytos 8.3 reiksmes
# t.y. pH pasikeite

# b) atvejis
ZTest(x, alternative = "greater", mu=m0, sd_pop = st_nuokr, conf.level = 1-alpha)
#isvada:
# kadangi p = 0.02275 < alpha = 0.05, nuline hipoteze atmetame. laikome
# jog ismatuotas tirpalo pH yra didesnis uz 8.3 reiksme

# c) atvejis
ZTest(x, alternative = "less", mu=m0, sd_pop = st_nuokr, conf.level = 1-alpha)
# isvada
# kadangi p=0.9772 > alpha = 0.05, nuline hipoteze priimame, laikome, jog
# ismatuotas tirpalo pH reiksmingai nesiskiria nuo 8.3 reiksmes
# taciau gali buti ir kitu teisingu nuliniu hipoteziu

# suderinamumo hipotezes apie normaluji skirstini tikrinimas--------------------

# gamybos irenginys fasuoja gaminius pakeliais po 1000g. atsitiktinai pasverta
# 60 pakeliu. gauti sie svoriai, surasyto faile suderinamumo.......csv
# stulpelyje "normalusis". patikrinkite hipoteze, kad pakeliu svoris pasiskirstes
# pagal normaluji desni

duom = read.csv("Suderinamumo_hipotezes_imtys.csv")
x = duom$normalusis
x

n = length(x)
n

min = min(x)
min

max = max(x)
max

k=ceiling(log10(n)*3.32+1)
k

#ivertindami imties minimali ir maksimalia reiksmes bei grupavimo intervalu
# skaiciu, pirmojo intervalo pradzios a_pirm ir paskutiniojo intervalo galo
# a_pask taskus parenkame taip, kad intervalo plotis butu "grazus" skaicius.
# sios reiksmes turi buti k iek galima artimesnes min ir max reiksmems

a_pirm = 986
a_pask = 1014

#intervala [984; 1014) padalijame i k=7 dalis
#kintamajam bin priskiriame intervalu krastinius taskus

bin = seq(a_pirm, a_pask, length.out = k+1)
bin

# sudarome intervaliniu dazniu lentele. kintamasis o issaugo stebetuosius
# intervalinius daznius nubraizome dazniu histograma

o = hist(x, breaks = bin,
            plot = T,
            right = F,
            probability = T,
            main = "Histograma",
            ylab = "tankis")$counts

lines(density(x), col = "red", lw=3)

table(cut(x, breaks = bin, right = F))

# apibreziame nornmaliojo desnio parametru skaiciu s. ivertiname normaliojo desnio
# parametrus m ir sigma, apskaiciuodami imties vidurki ir standartini nuokrypi:

s=2
m = mean(x)
m
sigma = sd(x)
sigma

#normalusis atsitiktinis dydis igyja reiksmes nuo -inf iki inf, todel formaliai
# turime pakeisti pirmojo ir paskutiniojo intervalu krastinius taskus
#atnaujinti intervalu galai bin1

bin1 = c(-Inf, bin[2:k], Inf)
bin1

# skaiciuojame teorines tikimybes p, patikriname, ar apskaiciuotu tikimybiu suma
# lygi 1

p = c(pnorm(bin1[1:k+1],m,sigma)-pnorm(bin1[1:k],m,sigma))
p

sum(p)

# skaiciuojame tiketinuosius daznius E.
E = n*p
E

#apskaiciuojame gamma^2 statistikos realizacija
chi_kv_statistika <-sum((o-E)^2/E)
chi_kv_statistika

# apibreziame uzdavinio salygoje duota reiksmingumo lygmeni alpha. apskaiciuojame
# chi kvadrato skirstinio su k-s-1=7-2-1 laisves laipsniu 1-alpha=0.95 kvatntili
# chi_kv_kvantilis
alpha =0.05
chi_kv_kvantilis = qchisq(1-alpha, df = k-s-1)
chi_kv_kvantilis

#ats: kadangi 2.131<9.387, tai hipoteze, kad pakeliu svoris pasiskirstes pagal
# normaluji desni, nepriestarauja imties duomenims.

#antrasis budas, naudojant r funkcija chisq.test()
# chisq.test() turi du argumentus: stebetuju dazniu masyvas ir teoriniu skirstinio
# tikimybiu masyvas. (turime, cia o ir E atitinkamai)

test <- chisq.test(o, p=E/sum(E))
test

#turime atkreipti demesi, kad pagal nutylejima sis kriterijus turi k-1 laisves laipsniu
# skaiciu, taciau siam uzdaviniui spresti reikia apskaiciuoti chi kvadrato skirstinio
# kvantili su k-s-1 laipsniu skaiciumi
# si neatitikima isspresti pades sios eilutes:
test$parameter <-c(df=k-s-1)
test$p.value <- pchisq(test$statistic, df = test$parameter, lower.tail=FALSE)
test

#ats: kadangi pvalue didesni uz reiksmingumo lygmeni (0.711>0.05), tai hipoteze,
# kad atsitiktinis dydis pasiskirstes pagal normaluji desni, nepriestarauja imties
# duomenimis

#normalumui tikrinti naudojama kvantiliu palyginimo arba kvantiliu kvantiliu (QQ)
#diagrama, kurios x asyje iprastai atidedami teorinio modelio kvantiliai, o
#y asyje - empiriniu duomenu kvantiliai. x asyje gali buti tiek labiausiai jusu
#duomenis atitinkancio normaliojo skirstinio, tiek ir standartinio normaliojo 
#skirstinio kvantiliai. kuo QQ tasku issidestymas panasesnis i tiese, einancia ties
#grafo istrizaine, tuo duomenys geriau atitinka modeli. ypac svarbu, kad vienoje
# tieseje butu duomenu taskai, esantys tarp 1 ir 3 kvartiliu

install.packages("UsingR")
library(UsingR) #nera tokio?? idk
simple.eda(x)
-------------------------------------------------------------------------------------------------------------
#taskiniai parametru iverciai
# 1 uzd
# supakuoto gaminio net mase turi norm skirstini X ~ N(m,sigma^2)
#pasverus keturis, gauta imtis: 9.8, 9.9, 10.0, 10.3
# Raskite param m ir sigma^2 taskinius ivercius momentu ir
# didziausio tiketinumo metodais

x <- c(9.8, 9.9, 10.0, 10.3)

# momentu metodas (naudojant teorines iverciu formules)
m=mean(x)
m

n=length(x)
n

disp = ((n-1)/n)*var(x)
disp

# didziausio tiketinumo metodas (naudojant metodus is MASS)
install.packages("MASS")
library(MASS)

iverciai <- fitdistr(x,"normal")
iverciai

disp = 0.18708287^2
disp

# 2 uzd
# prietaiso vikimo trukme (sav) yra pasiskirsciusi pagal
# eksponentini desni, t. y. X ~ E(lambda)
# turimi duomenys apie trukmes: 1236, 335, 119, 76, 454
# rasti labda taskini iverti momentu ir didziausio tiketinumo momentais

x <-c(1236, 335, 119, 76, 454)

#momentu metodas
m=mean(x)
m

lambda = 1/m
lambda

# didziausio tiketinumo metodas
install.packages("MASS")
library(MASS)

fitdistr(x,'exponential')

# 3 uzd
# skrydzio trukme (h) yra pasiskirsciusi tolygiai, t. y. X~T(a,b)
# sesiu skrydziu trukmes: 8.00, 8.38, 9.11, 8.23, 8.88
# Raskite param a ir b taskinius ivercius didziausio tiketinumo metodu

x <-c(8.00, 8.38, 9.11, 8.23, 8.88)

# naudojant teorines iverciu formules:
a=min(x)
a
b=max(x)
b

# pasikliautinieji intervalai

# nustatant atstuma tarp dvieju gyvenvieciu atliekami keturi nepriklausomi matavimai
# gauti tokie rezultatai: 2235, 2244, 2256, 2265
# matavimo vidutine paklaida 40m
# su patikimumu 0.95 nustatykite matuojamo atstumo pasikliautinaji intervala
# laikome, kad matavimo rezultatai pasiskirste pagal norm. skirstini

# vidurkio pasikliautinasis intervalas, kai dispersija zinoma

x <-c(2235, 2244, 2256, 2265)

# pirmas budas (naudojant teorines formules)

n=length(x)
n

xVid <-mean(x)
xVid

sigma = 40
sigma

alfa = 0.05

p=1-alfa/2

#kvantilis
zp=qnorm(p)
zp

PI <-c(xVid-sigma*zp/sqrt(n), xVid+sigma*zp/sqrt(n))
PI

# antras metodas (naudojant DescTools funkcija Mean(I))
install.packages("DescTools")
library(DescTools)

MeanCI(x, sd = sigma, conf.level = 1-alfa)

#2 uzdavinys
#stebint normaluji atsitiktini dydi, kurio vidurkis ir dispersija yra
#nezinomi, uzrasyti duomenys: 0.6, 1.5, 1.4, 2, 1, 1.3. uzrasyti
#vidurkio 90% pasikliautinaji intervala

#vidurkio pasikliautinasis intervalas, kai dispersija nezinoma

x<-c(0.6, 1.5, 1.4, 2, 1, 1.3)

n=length(x);
n

xVid = mean(x)
xVid

alpha = 0.1
s = sd(x)
s

p=1-alpha/2
p
#kvantilis
tp=qt(p,n-1)
tp
PI <- c(xVid-s*tp/sqrt(n),xVid+s*tp/sqrt(n))
PI

#antras budas, naudojant t.test funkcija
t.test(x,conf.level = 1-alpha)$conf.int

#3 uzd
#stebint normaluji atsitiktini dydi, kurio vidurkis ir dispersija nezinomi
#gauti duomenys: -2, 1, 1, 0.5, 2. uzrasyti dispersijos 90%
#pasikliautinaji intervala

#dispersijos PI, kai vidurkis nezinomas (pasikliautinis intervalas)

x<-c(-2, 1, 1, 0.5, 2)
#1 budas, naudojant teorines formules

xVid = mean(x)
xVid
n=length(x)
n
alpha=0.1
S2=var(x) # pataisytoji dispersija
S2
p=alpha/2
p
#kvantiliai
chi2p_left=qchisq(1-p,n-1)
chi2p_left
chi2p_right=qchisq(p,n-1)
chi2p_right

PI<-c((n-1)*S2/chi2p_left,(n-1)*S2/chi2p_right)
PI

#antras budas
#OneTwoSamples paketas
install.packages("OneTwoSamples")
library(OneTwoSamples)
interval_var1(x, mu=Inf, alpha=alpha)

#treciasis budas (naudojant DescTools funkcija VarCI))
install.packages("DescTools")
library(DescTools)
VarCI(x, conf.level =1-alpha)

#4uzd
#stebint normaluji atsitiktini dydi, kurio vidurkis =4, o dispersija
#nezinoma, gauti duomenys: 3.5, 4, 5.2, 6, 6.1, 4.5, 4.8
#uzrasyti dispersijos 95% pasikliautinaji intervala

#dispersijos PI (pasikliautinasis intervalas), kai vidurkis zinomas
x<-c(3.5, 4, 5.2, 6, 6.1, 4.5, 4.8)

#duotasis vidurkis
m=4
n=length(x)
n
alpha = 0.05
p = alpha/2
p
S02 = sum((x-m)^2)/n #dispersija
S02
#kvantiliai
chi2p_left=qchisq(1-p,n)
chi2p_left
chi2p_right=qchisq(p,n)
chi2p_right

PI <-c(n*S02/chi2p_left, n*S02/chi2p_right)
PI

#kitas budas naudojant OneTwoSamples
install.packages("OneTwoSamples")
library(OneTwoSamples)

interval_var1(x,mu=m,alpha=alpha)

# 5uzd
# is 400 apklausu 25-45m amziaus miesto gyventoju, 144 nuomuojasi busta
# raskite tikimybes, jog sios grupes gyventojas nuomuojasi buusta,
# pasikliautiniaji intervala su 0.95 pasikliovimo lygmeniu (trys skaitm po kabl)

#pirmas budas (toerines formules)
k=144
n=400
# tikimybes ivertis
p=k/n
p
alfa=0.05
#kvantilis
zp=qnorm(1-alfa/2)
zp
# tikimybes pasikliautinasis intervalas
PI <-c(round(p-zp*sqrt(p*(1-p)/n),3), round(p+zp*sqrt(p*(1-p)/n),3))
PI

# antrasis budas (naudojant specialias funkcijas)
# pastaba: rstudio yra realizuota daugybe funkciju su binominio skirstinio
# parametru pasikliautinaisiais intervalais, taciau ju rezultatai gali
# skirtis priklausomai nuo naudojamo metodi. vienas is budu realizuoti musu teorinese
# paskaitose formules - naudoti desctools paketo BinomCI() funkcija, nurodant
# Waldo metoda

install.packages("DescTools")
library(DescTools)

#tikimybes taskinis ivertis ir pasikliautinasis intervalas
BinomCI(k,n, conf.level = 1 - alfa, method = "wald")
--------------------------------------------------------------------------------------------------------------
#lazerinio atstuno matuoklio paklaidos:
# 0.01, -0.02, 0.04, 0.01. patikrinkite hipoteze, jog matuoklis nedaro
#sisteminiu paklaidu (jo paklaidu vidurkis 0) su reiksminguo lygmeniu alpha = 0.05
# laikydami, jog sigma - nezinomas

x<-c(0.01, -0.02, 0.04, 0.01)

#hipotezes tikrinimas
t.test(x)
#ats p-value 0.474 > 0.05, todel H0 - neatmestina, galime 
# laikyti, jog nedaro sisteminiu paklaidu

#2 uzd
# masyve msleep yra surinkta informacija apie miego iprocius
#stulpelyje sleep_total yra nurodytas vidutinis val sk per para
# kuri atitinkamas gyvunas praleidzia zzz
# aikydami kad duom pasiskirste per normaluji desni
# patikrinkite hipoteze, jog sleep_total vidurkis = 8h per para kai:
# a) reiksmingumo lygmuo alpha = 0.1 alternatyvi hipoteze Ha: m!=8
# b) reiksmingumo lygmuo alpha = 0.05, alt hipoteze Ha: m<8;
# c) reiksmingumo lygmuo alpha = 0.01, alt hipoteze Ha: m>8

install.packages("ggplot2")
library(ggplot2)

data(msleep) #duomeny masyvas

# sleep_total yra tik stulpelio vardas, o ne R duomeninis obj
#norint kad msleep stulpeliai taptu pasiekiami, reikia:
attach(msleep)

#hipoteziu tikrinimas
#a)
t.test(sleep_total, mu=8, conf.level = 0.9, alternative="two.sided")

#isvada p-value = 3.437e-06 yra mazesne uz 0.1, todel nuline hipoteze
#jog vidutine miego trukme per para lygi 8h per para atmetame
# galime teigti, jog zinduoliu miego trukme statistiskai
# reiksmingai skiriasi nuo 8h per para

#b)
t.test(sleep_total, mu = 8, conf.level=0.95, alternative = "less")
#isvada: p-value = 1 yra > uz 0.05, todel laikome, kad nera pagrindo
#atmesti nuline hipoteze, ir priimti alt hipoteze, jog zinduoliu
#miego trukme yra mazesne nei 8h per para

#c)
t.test(sleep_total, mu=8, conf.level = 0.99, alternative = "greater")
#isvada: p val < 0.01, todel nuline hipoteze, kad jog vid miego per para
# = 8 atmetame, galime teigti, jog miego trukme statistikai >8 val per para

#baigus darba su duomenu rinkiniu, tikslinga ji atjungti:
detach(msleep) # komanda detach() atjungs visus duomenu rinkinius

# hipoteze apie normaliojo skirstinio vidurkio lygybe skaiciui, kai dispersija
# yra zinoma

# 3 uzd
# gamybos proceso metu tirpalo pH turi buti lygiai 8.3. ismatuotos pH reiksmes
# pasiskirste pagal normaluji skirstini su zinomu standrtiniu nuokrypiu
# sigma=0.02. atlikus keturis matavimus, gautos pH: 8.31, 8.34, 8.32, 8.31.
# su reiksmingumo lygmeniu alpha = 0.05 patikrinkite hipoteze, ar tirpalo pH
# a) pasikeite b) padidejo c) sumazejo

x<-c(8.31, 8.34, 8.32, 8.31)
x

m0=8.3  # zinomas vidurkis
st_nuokr = 0.02 # zinomas standartinis nuokrypis
alpha = 0.05 # reiksmingumo lygmuo

library(DescTools)
# a) atvejis
ZTest(x, alternative = "two.sided", mu=m0, sd_pop = st_nuokr,conf.level = 1-alpha)
#isvada:
# kadangi p = 0.046 < alpha = 0.05, nuline hipoteze atmetame. laikome, jog
# ismatuotas tirpalo pH reiksmingai skiriasi nuo numatytos 8.3 reiksmes
# t.y. pH pasikeite

# b) atvejis
ZTest(x, alternative = "greater", mu=m0, sd_pop = st_nuokr, conf.level = 1-alpha)
#isvada:
# kadangi p = 0.02275 < alpha = 0.05, nuline hipoteze atmetame. laikome
# jog ismatuotas tirpalo pH yra didesnis uz 8.3 reiksme

# c) atvejis
ZTest(x, alternative = "less", mu=m0, sd_pop = st_nuokr, conf.level = 1-alpha)
# isvada
# kadangi p=0.9772 > alpha = 0.05, nuline hipoteze priimame, laikome, jog
# ismatuotas tirpalo pH reiksmingai nesiskiria nuo 8.3 reiksmes
# taciau gali buti ir kitu teisingu nuliniu hipoteziu

# suderinamumo hipotezes apie normaluji skirstini tikrinimas--------------------

# gamybos irenginys fasuoja gaminius pakeliais po 1000g. atsitiktinai pasverta
# 60 pakeliu. gauti sie svoriai, surasyto faile suderinamumo.......csv
# stulpelyje "normalusis". patikrinkite hipoteze, kad pakeliu svoris pasiskirstes
# pagal normaluji desni

duom = read.csv("Suderinamumo_hipotezes_imtys.csv")
x = duom$normalusis
x

n = length(x)
n

min = min(x)
min

max = max(x)
max

k=ceiling(log10(n)*3.32+1)
k

#ivertindami imties minimali ir maksimalia reiksmes bei grupavimo intervalu
# skaiciu, pirmojo intervalo pradzios a_pirm ir paskutiniojo intervalo galo
# a_pask taskus parenkame taip, kad intervalo plotis butu "grazus" skaicius.
# sios reiksmes turi buti k iek galima artimesnes min ir max reiksmems

a_pirm = 986
a_pask = 1014

#intervala [984; 1014) padalijame i k=7 dalis
#kintamajam bin priskiriame intervalu krastinius taskus

bin = seq(a_pirm, a_pask, length.out = k+1)
bin

# sudarome intervaliniu dazniu lentele. kintamasis o issaugo stebetuosius
# intervalinius daznius nubraizome dazniu histograma

o = hist(x, breaks = bin,
            plot = T,
            right = F,
            probability = T,
            main = "Histograma",
            ylab = "tankis")$counts

lines(density(x), col = "red", lw=3)

table(cut(x, breaks = bin, right = F))

# apibreziame nornmaliojo desnio parametru skaiciu s. ivertiname normaliojo desnio
# parametrus m ir sigma, apskaiciuodami imties vidurki ir standartini nuokrypi:

s=2
m = mean(x)
m
sigma = sd(x)
sigma

#normalusis atsitiktinis dydis igyja reiksmes nuo -inf iki inf, todel formaliai
# turime pakeisti pirmojo ir paskutiniojo intervalu krastinius taskus
#atnaujinti intervalu galai bin1

bin1 = c(-Inf, bin[2:k], Inf)
bin1

# skaiciuojame teorines tikimybes p, patikriname, ar apskaiciuotu tikimybiu suma
# lygi 1

p = c(pnorm(bin1[1:k+1],m,sigma)-pnorm(bin1[1:k],m,sigma))
p

sum(p)

# skaiciuojame tiketinuosius daznius E.
E = n*p
E

#apskaiciuojame gamma^2 statistikos realizacija
chi_kv_statistika <-sum((o-E)^2/E)
chi_kv_statistika

# apibreziame uzdavinio salygoje duota reiksmingumo lygmeni alpha. apskaiciuojame
# chi kvadrato skirstinio su k-s-1=7-2-1 laisves laipsniu 1-alpha=0.95 kvatntili
# chi_kv_kvantilis
alpha =0.05
chi_kv_kvantilis = qchisq(1-alpha, df = k-s-1)
chi_kv_kvantilis

#ats: kadangi 2.131<9.387, tai hipoteze, kad pakeliu svoris pasiskirstes pagal
# normaluji desni, nepriestarauja imties duomenims.

#antrasis budas, naudojant r funkcija chisq.test()
# chisq.test() turi du argumentus: stebetuju dazniu masyvas ir teoriniu skirstinio
# tikimybiu masyvas. (turime, cia o ir E atitinkamai)

test <- chisq.test(o, p=E/sum(E))
test

#turime atkreipti demesi, kad pagal nutylejima sis kriterijus turi k-1 laisves laipsniu
# skaiciu, taciau siam uzdaviniui spresti reikia apskaiciuoti chi kvadrato skirstinio
# kvantili su k-s-1 laipsniu skaiciumi
# si neatitikima isspresti pades sios eilutes:
test$parameter <-c(df=k-s-1)
test$p.value <- pchisq(test$statistic, df = test$parameter, lower.tail=FALSE)
test

#ats: kadangi pvalue didesni uz reiksmingumo lygmeni (0.711>0.05), tai hipoteze,
# kad atsitiktinis dydis pasiskirstes pagal normaluji desni, nepriestarauja imties
# duomenimis

#normalumui tikrinti naudojama kvantiliu palyginimo arba kvantiliu kvantiliu (QQ)
#diagrama, kurios x asyje iprastai atidedami teorinio modelio kvantiliai, o
#y asyje - empiriniu duomenu kvantiliai. x asyje gali buti tiek labiausiai jusu
#duomenis atitinkancio normaliojo skirstinio, tiek ir standartinio normaliojo 
#skirstinio kvantiliai. kuo QQ tasku issidestymas panasesnis i tiese, einancia ties
#grafo istrizaine, tuo duomenys geriau atitinka modeli. ypac svarbu, kad vienoje
# tieseje butu duomenu taskai, esantys tarp 1 ir 3 kvartiliu

install.packages("UsingR")
library(UsingR) #nera tokio?? idk
simple.eda(x)
----------------------------------------------------------------------------------------------------------
# stebint atsitiktini dydi x buco gauta imtis, irasyta csv failo tolygusis
# stulpelyje. taikydami kolmogorovo kriteriju patikrinkite hipoteze
# kad duomenys pasiskirste pagal tolyguji desni X~T(0.2).

duom = read.csv("Suderinamumo_hipotezes_imtys.csv")
x = duom$tolygusis
x

#apskaiciuokime imtiews turi n, tolygiojo skirstinio parametrus a ir b
#surasome i variacine eilute
n = length(x)
n
a=0
b=2
#surasome i variacine eilute
x=sort(x)
x
#apskaiciuojame pasiskirstymo funkcijos reiksmes
FoX = punif(x,a,b)
FoX

#tarpiniai skaiciavimai Kolmogorovo statistikai D_n apskaiciuoti:
FnX = seq(1:n)/n #i/n
Fn1X = (seq(1:n)-1)/n #(i-1)/n
#apskaiciuojame skirtumus su teorine funkcija
Dp = FnX - FoX
Dm = FoX - Fn1X
# suformuojame tarpiniu rezultatu lentele
Lentele = cbind(x,FnX,Fn1X, FoX, Dp, Dm)
Lentele

# is lenteles isrenkame D_n^+ ir D_n^- reiksmes DP ir DM
DP = max(Dp)
DP
DM = max(Dm)
DM
#apskaiciuojame Kolmogorovo statistikos D_n reiksme Dn:
Dn = max(DP,DM)
Dn
# apibreziame uzdavinio salygoje duota reiksmingumo lygmeni alpha
alpha = 0.05
# pagal apytiksle formule apskaiciuojame Kolmogorovo skirstinio kvantili k:
k = sqrt(-log(alpha/2)/(2*n))-1/(6*n)
k
# ats: kadangi 0.08333<0.1725, tai hipoteze, kad atsitiktinis dydis pasiskirstes
# pagal tolyguji desni nepriestarauja imties duomenims

#antras budas naudojant funkcija ks.test()
# argumentai: imtis x, teorine pasiskirstymo funkcija y ir jos parametru reiksmes
ks.test(x,y="punif",a,b)
#ats: kadangi p reiksme didesne uz reiksmingumo lygmeni (0.799>0.05)
# tai hipoteze, kad atsitiktinis dydis pasiskirstes pagal tolyguji desni
# nepriestarauja imties duomenims
------------------------------------------------------------------------------------------------------------
# stebint atsitiktini dydi gauta imtis
# gauti duomenys surasyti csv failo stulpelyje eksponentinis
# patikrinkite hipoteze, kad duomenys pasisikirste pagal eksponentini desni

duom = read.csv("Suderinamumo_hipotezes_imtys.csv")
x = duom$eksponentinis
x

n = length(x)
n

min = min(x)
min

max = max(x)
max

k=ceiling(log10(n)*3.32+1)
k

#ivertindami imties minimali ir maksimalia reiksmes bei grupavimo intervalu
# skaiciu, pirmojo intervalo pradzios a_pirm ir paskutiniojo intervalo galo
# a_pask taskus parenkame taip, kad intervalo plotis butu "grazus" skaicius.
# sios reiksmes turi buti k iek galima artimesnes min ir max reiksmems

a_pirm = 0
a_pask = 3.5

#intervala (0;3.5) padaliname i k = 7 dalis
#bin priskiriame krastinius taskus
bin = seq(a_pirm, a_pask, length.out = k+1)
bin

# sudarome intervaliniu dazniu lentele. Kintamasis 0 issaugo stebetuosius
# intervalinius daznius. nubraizoma dazniu histograma:

o = hist(x, breaks = bin,
         plot = T,
         right = F,
         probability = T,
         main = "Histograma",
         ylab = "tankis")$counts

lines(density(x), col = "red", lw=3)

table(cut(x, breaks = bin, right = F))

# apibreziame eksponentinio desnio parametru skaiciu s. ivertiname eksponentinio
# desnio parametra lambda:

s = 1
lambda = 1/mean(x)
lambda

# taip pat reikia atsizvelgti i tai, kad eskponentinis atsitiktinis dydis
# pasiskirstes intervale (0;inf), todel paskutiniojo intervalo krastinis taskas
# pakeiciamas i inf. Taip suformuojamas paskutinysis intervalas [2;inf). visi
# intervalu krastiniai taskai saugomi masyve bin1

bin1 = c(bin[1:5], Inf)
bin1

# apibreziame naujaji grupavimo intervalu skaiciu k. suskaiciuojame naujus 
# intervalinius daznius o ir suformuojame naujuju intervaliniu dazniu lentele:

k = 5
o = hist(x, breaks = bin1, plot = F, right = F)$counts
table(cut(x, breaks = bin1, right = F))

#skaiciuojame teorines tikimybes p, patikriname, ar apskaiciuotu tikimybiu suma
# lygi 1

p = c(pexp(bin1[1:k+1], lambda) - pexp(bin1[1:k], lambda))
p
sum(p)

# skaiciuojame tiketinuosius daznius E
E = n*p
E

#apskaiciuojame raide^2 statistikos realizacija
chi_kv_statistika <-sum((o-E)^2/E)
chi_kv_statistika

#apibreziame uzdavinio salygoje duota reiksmingumo lygmeni alpha. apskaiciuojame
# chi kvadrato skirstinio su k-s-1=5-1-1 laisves laipsniu 1-alpha=0.95 kvantili
# chi_kv_kvantilis
alpha = 0.05

chi_kv_kvantilis = qchisq(1-alpha, df = k-s-1)
chi_kv_kvantilis

#isvada, kadangi 4.293<7.814 tai hipoteze, kad atsitiktinis dydis pasiskirstes pagal
# eksponentini desni nepriestarauja imties duomenims.
-----------------------------------------------------------------------------------------------------------
#duota imtis
x <- c(1, 1, 1, 4, 4, 5, 5, 4, 4, 5, 7, 6, 7, 6, 6, 10, 9)
x

dazniu_skirstinys <- table(x)
dazniu_skirstinys

plot(dazniu_skirstinys,type = "b",
     main = "dazniu skistinys",
     ylab = "dazniai",
     col = "red",
     pch = 21,
     bg = 2)

santykiniu_dazniu_skirstinys <- table(x)/length(x)
santykiniu_dazniu_skirstinys

plot(santykiniu_dazniu_skirstinys,type = "b",
     main = "santykiniu dazniu skistinys",
     ylab = "santykiniai dazniai",
     col = "red",
     pch = 21,
     bg = 2)

#variacine eilute
sort(x)

#imties turis
length(x)

min(x)
max(x)

#imties minmax reiksmes
range(x)

#imties plotis (isvestine)
diff(range(x))

#Imties moda
#PASTABA! Apskaičiuojant modą, turi būti suinstaliuotas paketas "modeest".
install.packages("modeest") #instaliuojame paketą "modeest"
library(modeest)
moda <- mfv(x)
moda

median(x)

#Apskaičiuojame pirmąjį, antrąjį ir trečiąjį kvartilius
quantile(x, c(0.25, 0.5, 0.75), type = 2)

#Apskaičiuojame kvartilinį plotį
KP <- IQR(x)
KP

#SUVESTINĖ - minimali reikšmė, pirmasis kvartilis, mediana, vidurkis, trečiasis kvartilis, maksimali reikšmė
summary(x, quantile.type = 2)

#Apskaičiuojame 0.17-kvantilį
quantile(x, 0.17, type = 2)

#Apskaičiuojame 0.2-kvantilį, 0.47-kvantilį, 0.73-kvantilį
quantile(x, c(0.2, 0.47, 0.73), type = 2)

#Penkiaskaitė suvestinė - minimali reikšmė, pirmasis kvartilis, mediana, trečiasis kvartilis, maksimali reikšmė
fivenum(x)

#Stačiakampė diagrama
boxplot(x, horizontal = TRUE, main = "Stačiakampė diagrama")

#Empirinis vidurkis
mean(x)

#Empirinė pataisytoji dispersija
disp_pat <- var(x)
disp_pat

#Empirinė dispersija
disp <- function(x){var(x)*(length(x)-1)/length(x)}
disp(x)

#Empirinis pataisytasis standartinis nuokrypis
standartas_pat <- sd(x)
standartas_pat 

#Empirinis standartinis nuokrypis
standartas <- sqrt(disp(x))
standartas

# antra uzd -------------------------------------------------------------------

#Nuskaitome failą "first.csv" į programą R
duom <- read.csv("first.csv")
x <-duom$X1
x

#Imties tūris
length(x)

#Stačiakampė diagrama
boxplot(x, horizontal = TRUE, main = "Stačiakampė diagrama")

#Apskaičiuojame negrupuotų duomenų empirines skaitines charakteristikas
#Empirinis vidurkis
mean(x)

#Empirinė pataisytoji dispersija
disp_pat <- var(x)
disp_pat

#Empirinė dispersija
disp <- function(x){var(x)*(length(x)-1)/length(x)}
disp(x)

#Empirinis pataisytasis standartinis nuokrypis
standartas_pat <- sd(x)
standartas_pat 

#Empirinis standartinis nuokrypis
standartas <- sqrt(disp(x))
standartas

#Apskaičiuojame grupavimo intervalų skaičių k=log10(n)*3,32+1
k <- ceiling(log10(length(x))*3.32+1)
k

#Histogramos sudarymui ir jos braižymui naudojama standartinė R funkcija hist.
#Kai kurie jos parametrai:
# x -- kiekybinio kintamojo reikšmių vektorius,
#breaks -- kintamojo x padalinimo į intervalus taškų vektorius,
#freq -- TRUE, nurodo, kad bus braižoma dažnių histograma,
#probability -- TRUE, nurodo, kad bus braižoma santykinių dažnių histograma,
#right -- TRUE, nurodo, kad histogramos intervalai yra (a, b] pavidalo,
#plot -- TRUE, nurodo, kad bus braižoma histograma.
hist(x, breaks = seq(min(x), max(x),length = k+1),
     probability = T,
     right = F,
     col = grey(0.9),
     ylab = "Santykiniai dažniai",
     main = "Histograma"
)

#Funkcija hist ne tik nubraižo histogramą, bet ir gali sukurti šiai histogramai
#nubraižyti reikalingų duomenų rinkinį. Iš jo galima sužinoti, kokie yra kintamojo
#dažniai intervaluose, šių intervalų padalinimo taškai, intervalų vidurio taškai ir pan.
#Kai kurie šio rinkinio parametrai:
#  histat$breaks -- intervalų padalinimo taškai;
#histat$mids -- intervalų vidurio taškai;
#histat$counts -- dažniai intervaluose.
histat <- hist(x, breaks = seq(min(x), max(x),length = k+1),
               probability = T,
               right = F,
               col = grey(0.9),
               ylab = "Santykiniai dažniai",
               main = "Histograma"
)
histat

#Nubraižo dažnių histogramą
plot(histat, ylab = "Dažniai")

#Apskaičiuojame grupuotų duomenų empirines skaitines charakteristikas
#Empirinis vidurkis
vidurkis_grup <- sum(histat$mids*histat$counts)/sum(histat$counts)
vidurkis_grup

#Empirinė dispersija
disp_grup <- sum((histat$mids^2)*histat$counts)/sum(histat$counts) - vidurkis_grup^2
disp_grup

#Empirinis standartinis nuokrypis
standartas <- sqrt(disp_grup)
standartas

#Empirinė pataisytoji dispersija
disp_grup_pat <- disp_grup*(length(x)/(length(x)-1))
disp_grup_pat

#Empirinis pataisytasis standartinis nuokrypis
standartas_grup_pat <- sqrt(disp_grup_pat)
standartas_grup_pat 