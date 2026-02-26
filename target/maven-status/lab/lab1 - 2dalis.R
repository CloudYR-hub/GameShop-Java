N=35  # Reikia įrašyti savo varianto numerį N
data=read.csv("normalusis_parametru_iverciams.csv",header=TRUE)  # Nuskaityti CSV failą su duomenimis (failas turi būti darbiniame kataloge)
pasikliovimo_lygmuo=data[1,N]  # Pasikliovimo lygmuo (γ=1- α) pagal varianto numerį
duomenys=data[2:1001,N]  # Analizuojami duomenys (imties reikšmės)

# 2.1. Grafikų braižymas ir savybių identifikavimas

set.seed(pasikliovimo_lygmuo)  # Nustatyti atsitiktinių skaičių generavimo sėklą pagal pasikliovimo lygmenį
# Histogramos braižymas
hist(duomenys, main = "Histograma", xlab = "Reikšmės", ylab = "Dažnis")  # Braižoma histograma duomenų pasiskirstymui vizualizuoti
# Histogramos savybė: simetriška forma aplink vidurkį rodo normalųjį skirstinį

# Stačiakampės diagramos (boxplot) braižymas
boxplot(duomenys, main = "Stačiakampė diagrama", ylab = "Reikšmės")  # Braižoma boxplot diagrama duomenų centrinei tendencijai ir išskirtinėms reikšmėms įvertinti
# Stačiakampės diagramos savybė: tarpkvantilinė sritis (dėžutė) yra simetriška, nėra stiprių išskirtinių reikšmių

# Q-Q grafiko braižymas
qqnorm(duomenys, main = "Q-Q grafikas")  # Q-Q grafikas tikrina, ar duomenys yra iš normaliojo skirstinio
qqline(duomenys, lwd = 2)  # Braižoma tiesė, rodanti idealų normalųjį skirstinį
# Q-Q grafiko savybė: duomenų taškai yra arti tiesės, tai rodo, kad skirstinys yra normalusis

# Įdiegiamas ir naudojamas paprastos duomenų analizės įrankis
#install.packages("UsingR")
library(UsingR)  # Įkelti biblioteką papildomoms funkcijoms
simple.eda(duomenys)  # Naudoti papildomas funkcijas duomenų analizei

# 2.2. Taškinių įverčių skaičiavimas
n <- length(duomenys)  # Imties dydis
vidurkis <- mean(duomenys)  # Apskaičiuoti imties vidurkį
dispersija <- ((n-1)/n)*var(duomenys)  # Apskaičiuoti imties dispersiją (koreguota)
cat("Vidurkis:", vidurkis, "\n")  # Atspausdinti vidurkį
cat("Dispersija:", dispersija, "\n")  # Atspausdinti dispersiją

# 2.3. Pasikliautinieji intervalai
alfa <- 0.05  # Reikšmingumo lygmuo
stderr <- sd(duomenys) / sqrt(n)  # Vidurkio standartinė paklaida

# Vidurkio pasikliautinasis intervalas
t_value <- qt(1 - alfa / 2, df = n - 1)  # Kritinė t reikšmė
mean_interval <- vidurkis + c(-1, 1) * t_value * stderr  # Vidurkio pasikliautinio intervalo skaičiavimas
cat("Vidurkio pasikliautinasis intervalas:", mean_interval, "\n")  # Atspausdinti vidurkio intervalą

# Dispersijos pasikliautinasis intervalas
chi2_bounds <- qchisq(c(alfa / 2, 1 - alfa / 2), df = n - 1)  # Kritinės chi-kvadrato reikšmės
variance_interval <- (n - 1) * dispersija / rev(chi2_bounds)  # Dispersijos pasikliautinio intervalo skaičiavimas
cat("Dispersijos pasikliautinasis intervalas:", variance_interval, "\n")  # Atspausdinti dispersijos intervalą

# 2.4. Hipotezių testavimas
m0 <- 3  # Nulinės hipotezės reikšmė
alfa <- 0.95  # Pasikliovimo lygmuo
# Hipotezė apie vidurkį (Ha: m < m0) su alternatyvia hipoteze, kad vidurkis yra mažesnis už 3
t.test(duomenys, mu = m0, conf.level = alfa, alternative = 'less')  # t-testas alternatyviai hipotezei patikrinti
# Rezultatas: jei p-reikšmė mažesnė už reikšmingumo lygį, atmetama nulinė hipotezė
# t-test rezultatai:
# t = -66.43: t statistikos reikšmė (testo dydis)
# df = 999: laisvės laipsnių skaičius
# p-value < 2.2e-16: p-reikšmė (tikimybė gauti tokius arba ekstremalesnius rezultatus, jei H₀ yra teisinga)
# Kadangi p-reikšmė yra daug mažesnė už reikšmingumo lygį (pvz., α = 0.05), nulinė hipotezė H₀ atmetama.
# Išvada: vidurkis statistiškai reikšmingai mažesnis už 3.

