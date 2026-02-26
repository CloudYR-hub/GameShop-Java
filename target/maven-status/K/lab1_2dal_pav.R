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