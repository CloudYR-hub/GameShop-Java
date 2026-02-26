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