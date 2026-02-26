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