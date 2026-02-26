# Nurodoma varianto numeris (vartotojas įveda savo numerį, šiuo atveju 35).
N = 35  

# Nuskaitomi duomenys iš CSV failo. Failas turi būti darbiniame kataloge.
data = read.csv("duomenys_hipoteziu_tikrinimui.csv", header=TRUE)

# Iš pasirinktų duomenų stulpelio pagal varianto numerį paimami pirmieji 1000 įrašų.
imties_duomenys = data[1:1000, N]  

#1.1
# Apskaičiuojamas imties dydis (n), kuris yra duomenų kiekis imtyje.
imties_dydis <- length(imties_duomenys)
cat("Imties dydis (n):", imties_dydis, "\n")

# Randama imties mažiausia reikšmė.
minimumas <- min(imties_duomenys)
cat("Minimumas:", minimumas, "\n")

# Randama imties didžiausia reikšmė.
maksimumas <- max(imties_duomenys)
cat("Maksimumas:", maksimumas, "\n")

# Apskaičiuojamas imties plotis, kuris yra didžiausios ir mažiausios reikšmių skirtumas.
imties_plotis <- diff(range(imties_duomenys))
cat("Imties plotis:", imties_plotis, "\n")

# Apskaičiuojami imties kvartiliai (0%, 25%, 50%, 75%, 100% perskyros).
kvartiliai <- quantile(imties_duomenys)
cat("Kvartiliai:\n")
print(kvartiliai)

# Apskaičiuojamas interkvartilinis skirtumas (IQR) – skirtumas tarp 75% ir 25% kvartilių.
kvartiliu_skirtumas <- IQR(imties_duomenys)
cat("Kvartilių skirtumas (IQR):", kvartiliu_skirtumas, "\n")

# Apskaičiuojamas empirinis vidurkis (imties vidurkis).
empirinis_vidurkis <- mean(imties_duomenys)
cat("Empirinis vidurkis:", empirinis_vidurkis, "\n")

# Apskaičiuojama pataisytoji dispersija (imties dispersija).
pataisytoji_dispersija <- var(imties_duomenys)
cat("Pataisytoji dispersija:", pataisytoji_dispersija, "\n")

# Apskaičiuojamas pataisytasis standartinis nuokrypis (imties standartinis nuokrypis).
pataisytasis_standartinis_nuokrypis <- sd(imties_duomenys)
cat("Pataisytasis standartinis nuokrypis:", pataisytasis_standartinis_nuokrypis, "\n")

#1.2
# Sukuriama histograma, rodanti imties reikšmių pasiskirstymą.
hist(imties_duomenys, 
     main = "Histogramos braižymas", 
     xlab = "Reikšmės", 
     ylab = "Dažnis", 
     col = "lightblue", 
     border = "black")

# Sukuriama stačiakampė diagrama (boxplot), kuri parodo duomenų pasiskirstymo pagrindines savybes.
boxplot(imties_duomenys, 
        main = "Stačiakampė diagrama", 
        ylab = "Reikšmės", 
        col = "orange", 
        border = "brown")

# Skirstinio pasirinkimo komentarai remiantis grafine analize:
# - Histogramos simetriškumas ir varpo forma rodo galimą normalųjį skirstinį.
# - Jei reikšmės koncentruojasi mažesnių reikšmių link, tai gali būti eksponentinis skirstinys.
# - Vienodas pasiskirstymas rodo tolygųjį skirstinį.

# Tikrinama normalumo prielaida naudojant QQ diagramą.
qqnorm(imties_duomenys, main = "Normalumo QQ diagrama")
qqline(imties_duomenys, col = "red")

# Bibliotekos įdiegimas ir įkėlimas papildomai duomenų analizei.
install.packages("UsingR")
library(UsingR)

# Paprastos duomenų analizės funkcija.
simple.eda(imties_duomenys)

#1.3
# Bendrų statistinių parametrų apskaičiavimas:
mean_x <- mean(imties_duomenys)   # Imties vidurkis.
n <- length(imties_duomenys)      # Imties dydis.
sd_x <- sd(imties_duomenys)       # Standartinis nuokrypis.
min_x <- min(imties_duomenys)     # Minimumas.
max_x <- max(imties_duomenys)     # Maksimumas.

# Normaliojo skirstinio parametrų įvertinimas:
mu_hat <- mean_x  # Normaliojo skirstinio vidurkis (μ).
sigma_sq_hat <- ((n-1)/n)*var(imties_duomenys)  # Normaliojo skirstinio dispersija (σ^2).
cat("Normaliojo skirstinio parametrai:\n")
cat("  μ (vidurkis):", mu_hat, "\n")
cat("  σ^2 (dispersija):", sigma_sq_hat, "\n")

# Eksponentinio skirstinio parametrai:
lambda_hat <- 1 / mean_x  # Eksponentinio skirstinio intensyvumo parametras (λ).
cat("\nEksponentinio skirstinio parametras:\n")
cat("  λ (intensyvumas):", lambda_hat, "\n")

# Tolygiojo skirstinio parametrų įvertinimas:
a_hat <- min_x  # Tolygiojo skirstinio minimumas (a).
b_hat <- max_x  # Tolygiojo skirstinio maksimumas (b).
cat("\nTolygiojo skirstinio parametrai:\n")
cat("  a (minimumas):", a_hat, "\n")
cat("  b (maksimumas):", b_hat, "\n")

#1.4
# Tikrinama skirstinio hipotezė remiantis reikšmingumo lygiu (α = 0.05).
if (ks_test$p.value < alpha) {
  cat("\nAtmetame hipotezę, kad duomenys yra iš normalaus skirstinio.\n")
} else {
  cat("\nDuomenys yra iš normalaus skirstinio.\n")
}

if (ks_test_exp$p.value < alpha) {
  cat("\nAtmetame hipotezę, kad duomenys yra iš eksponentinio skirstinio.\n")
} else {
  cat("\nDuomenys yra iš eksponentinio skirstinio.\n")
}

if (ks_test_unif$p.value < alpha) {
  cat("\nAtmetame hipotezę, kad duomenys yra iš tolygaus skirstinio.\n")
} else {
  cat("\nDuomenys yra iš tolygaus skirstinio.\n")
}
