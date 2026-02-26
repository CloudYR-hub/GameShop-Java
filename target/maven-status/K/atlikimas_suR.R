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