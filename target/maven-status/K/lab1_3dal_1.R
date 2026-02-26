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