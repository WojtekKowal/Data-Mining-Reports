dane <- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = TRUE)
summary(dane)
str(dane)
#1b)
#7043 obserwacje i 21 cech
#seniorCitizen,tenure,MonthlyCharges,TotalCharges sa numerical reszta factorial
# Tak jest cecha customerID, która jest identyfikatorem, wskazuje zawsze unikalna wartosc jest tak jakby kluczem wiec bezuzyteczna
dane[1] <- NULL
summary(dane)
#sa znalazłem tylko w TotalCharges, jako NA
colSums(is.na(dane))
dane$tenure[is.na(dane$TotalCharges)] # widać że same 0!
#NA sa tylko w total charges sa to nowi klienci, ktorzy jeszcze nic nie zaplacili wiec zastapimy to 0 (cos takiego by musialo tez w raporcie sie znalezc)
dane$TotalCharges[is.na(dane$TotalCharges)] <- 0
#nie widze nietypowych wartosci
summary(dane)
library(skimr)
# problem że seniorCitizen jest liczony numerycznie a to 0-1 wiec nie ma to sensu
dane$SeniorCitizen <- as.factor(dane$SeniorCitizen)
levels(dane$SeniorCitizen) <-c("No", "Yes")
skim(dane)
IQR(dane$tenure)
IQR(dane$MonthlyCharges)
IQR(dane$TotalCharges) #to jest wartosc 3 kwartylu - wartosc 1 kwartylu więc jakby różnica między "srodkowa polowa danych"
# teraz 2b) potrzebujemy biblioteki do wykresów
# kolory zmieniaj jak będzie lepiej pasować
library(ggplot2)
#najpierw numeryczne
#monthly charges, wykres1
ggplot(dane, aes(x = MonthlyCharges)) +
  geom_histogram(
    fill = "steelblue",
    color = "white",
    bins = 30
  ) +
  geom_text(
    stat = "bin",
    bins = 30,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 3 
  )+
  labs(
    title = "Histogram miesięcznych opłat",
    x = "Miesięczne opłaty",
    y = "Liczba klientów"
  ) +
  theme_minimal()
#histogram tenure, wykres 2
ggplot(dane, aes(x = tenure)) +
  geom_histogram(
    fill = "steelblue",
    color = "white",
    bins = 35
  ) +
  geom_text(
    stat = "bin",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 3,
  )+
  labs(
    title = "Histogram stażu klienta",
    x = "liczba miesięcy",
    y = "Liczba klientów"
  ) +
  theme_minimal()
#Histogram Total Charges, wykres 3
ggplot(dane, aes(x = TotalCharges)) +
  geom_histogram(
    fill = "steelblue",
    color = "white",
    bins = 30
  ) +
  geom_text(
    stat = "bin",
    bins = 30,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 3,
  )+
  labs(
    title = "Histogram całkowitych opłat",
    x = "Wysokość opłat",
    y = "Liczba klientów"
  ) +
  theme_minimal()
#kolor  też można zmienić
#boxtenure, wykres 4
ggplot(dane, aes(y = tenure)) +
  geom_boxplot(
    fill = "gray",
    color = "black",
  ) +
  labs(
    y= "Miesiące",
    title= "Wykres pudełkowy czasu trwania umowy",
  ) +
  theme_minimal()
#boxmonthly, wykres 5
ggplot(dane, aes(y = MonthlyCharges)) +
  geom_boxplot(
    fill = "gray",
    color = "black",
  ) +
  labs(
    y= "Miesiączne opłaty",
    title= "Wykres pudełkowy miesięcznych opłat",
  ) +
  theme_minimal()
#boxtotal, wykres 6
ggplot(dane, aes(y = TotalCharges)) +
  geom_boxplot(
    fill = "gray",
    color = "black",
  ) +
  labs(
    y= "Całkowite opłaty",
    title= "Wykres pudełkowy całkowitych opłat",
  ) +
  theme_minimal()
# koniec wykresów zmiennych ciągłych/numerycznych
#wykresy kategoryczne
#płeć, wykres 7
ggplot(dane, aes(x = gender, fill = gender)) +
  geom_bar(
    color = "black",
    ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Wykres kolumnowy płci klientów",
    x = "Płeć",
    y = "liczba klientów",
  ) +
  theme_classic()
#SeniorCitizen, wykres 8
ggplot(dane, aes(x = SeniorCitizen, fill = SeniorCitizen)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według statusu seniora",
    x = "Status seniora",
    y = "liczba klientów",
  ) +
  theme_classic()
#Partner, wykres 9
ggplot(dane, aes(x = Partner, fill = Partner)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania partnera",
    x = "Posiadanie partnera",
    y = "liczba klientów",
  ) +
  theme_classic()
#Dependents, wykres 10
ggplot(dane, aes(x = Dependents, fill = Dependents)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania osoby na utrzymaniu",
    x = "Posiadanie osoby na utrzymaniu",
    y = "liczba klientów",
  ) +
  theme_classic()
#PhoneService, wykres 11
ggplot(dane, aes(x = PhoneService, fill = PhoneService)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi telefonicznej",
    x = "Posiadanie usługi telefonicznej",
    y = "liczba klientów",
  ) +
  theme_classic()
#MultipleLines, wykres 12
ggplot(dane, aes(x = MultipleLines, fill = MultipleLines)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według liczby linii telefonicznych",
    x = "Posiadanie wielu linii telefonicznych",
    y = "liczba klientów",
  ) +
  theme_classic()
#InternetService, wykres 13
ggplot(dane, aes(x = InternetService, fill = InternetService)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi internetowej",
    x = "Posiadanie usługi internetowej",
    y = "liczba klientów",
  ) +
  theme_classic()
#DSL - Cyfrowa linia abonencka (Kabel telefoniczny), Fiber optic - światłowód wiadomo
#OnlineSecurity, wykres 14
ggplot(dane, aes(x = OnlineSecurity, fill = OnlineSecurity)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczeń internetowych",
    x = "Posiadanie zabezpeiczeń internetowych",
    y = "liczba klientów",
  ) +
  theme_classic()
#OnlineBackup, wykres 15
ggplot(dane, aes(x = OnlineBackup, fill = OnlineBackup)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania kopii zapasowej w chmurze",
    x = "Posiadanie kopii zapasowej w chmurze",
    y = "liczba klientów",
  ) +
  theme_classic()
#DeviceProtection, wykres 16
ggplot(dane, aes(x = DeviceProtection, fill = DeviceProtection)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczenia urządzenia",
    x = "Posiadanie zabezpieczenia urządzenia",
    y = "liczba klientów",
  ) +
  theme_classic()
#TechSupport, wykres 17
ggplot(dane, aes(x = TechSupport, fill = TechSupport)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania wsparcia technicznego",
    x = "Posiadanie wsparcia technicznego",
    y = "liczba klientów",
  ) +
  theme_classic()
#StreamingTV, wykres 18
ggplot(dane, aes(x = StreamingTV, fill = StreamingTV)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania telewizji strumieniowej",
    x = "Posiadanie telewizji strumieniowej",
    y = "liczba klientów",
  ) +
  theme_classic()
#StreamingMovies, wykres 19
ggplot(dane, aes(x = StreamingMovies, fill = StreamingMovies)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania filmów na żądanie",
    x = "Posiadanie filmów na żądanie",
    y = "liczba klientów",
  ) +
  theme_classic()
#Contract, wykres 20
ggplot(dane, aes(x = Contract, fill = Contract)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według długości umowy",
    x = "Posiadana długość umowy",
    y = "liczba klientów",
  ) +
  theme_classic()
#month-to-month że umowa na miesiąc co miesiac coś jak subskrypcja
#PaperlessBilling, wykres 21
ggplot(dane, aes(x = PaperlessBilling, fill = PaperlessBilling)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania elektronicznej faktury",
    x = "Posiadanie elektronicznej faktury",
    y = "liczba klientów",
  ) +
  theme_classic()
#PaymentMethod, wykres 22
ggplot(dane, aes(x = PaymentMethod, fill = PaymentMethod)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci metody płatności",
    x = "Wybrana metoda płatności",
    y = "liczba klientów",
  ) +
  theme_classic()
#Churn, wykres 23 - tym sie zajmuje wiec imo musi sie znalezc
ggplot(dane, aes(x = Churn, fill = Churn)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
 
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według pozostania przy firmie",
    x = "Pozostanie przy firmie",
    y = "liczba klientów",
  ) +
  theme_classic()
#teraz 2c)
#rozrzut tenure z monthlyCharges, wykres 24
ggplot(dane, aes(x=tenure,y=MonthlyCharges)) +
  geom_point(
    color = "firebrick2",
    alpha = 0.5 # to daje ta przezroczystosc - widac nachodzace krokpi, można usunac
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    size = 1.5,
  ) +
  labs(
    title = "Wykres rozrzutu stażu klienta i miesięcznych opłat",
    x = "Staż klienta",
    y = "Miesięczne opłaty",
  ) +
theme_classic()
#rozrzut tenure z totalcharges, wykres 25
ggplot(dane, aes(x=tenure,y=TotalCharges)) +
  geom_point(
    color = "steelblue",
    alpha = 0.5 # to daje ta przezroczystosc - widac nachodzace kropki, można usunac
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    size = 1.5,
    color ="firebrick",
  ) +
  labs(
    title = "Wykres rozrzutu stażu klienta i całkowitych opłat",
    x = "Staż klienta",
    y = "Całkowite opłaty",
  ) +
  theme_classic()
#rozrzut monthlycharges z totalcharges, wykres 26, nwm ile sensu on ma chociaż no widać że ci z samego konca rzadko odchodza
ggplot(dane, aes(x=MonthlyCharges,y=TotalCharges)) +
  geom_point(
    color = "green2",
    alpha = 0.5 # to daje ta przezroczystosc - widac nachodzace kropki, można usunac
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    size = 1.5,
    color ="black",
  ) +
  labs(
    title = "Wykres rozrzutu miesięcznych i całkowitych opłat",
    x = "Miesięczne opłaty",
    y = "Całkowite opłaty",
  ) +
  theme_classic()
#2d)
#Zinterpretuj otrzymane rezultaty, w szczególności odpowiadając na pytania:
# i)Jaki jest zakres możliwych wartości dla poszczególnych zmiennych?
# ii) W przypadku zmiennych ilościowych:
# ii1)Czy wszystkie zmienne mają rozkład symetryczny?
# ii2) Które cechy charakteryzują się największą zmiennoocią?
# iii) W przypadku zmiennych jakościowych:
# iii1) Co można powiedzieć o częstości przyjmowania poszczególnych kategorii?
#ODP:
#i) ???
min(dane$tenure)
max(dane$tenure)
min(dane$MonthlyCharges)
max(dane$MonthlyCharges)
min(dane$TotalCharges)
max(dane$TotalCharges)
##ii)
##1 monthly nie ma symetryzcnego skok w lewej częsci i w prawej, tenure wyglada na symteryczne U-modalny, Total charges nie symetryczny skośny prawostronnie
##2 to jest to skim(dane) co było blisko poczatku
# najwieksze odchylenie standardowe ma totalcharges ale, te dane sa innego rzedu wielkosci niż tenure i monthly wiec cieżko to tak porównywać
# w przypadku IQR widzimy również duże w przypadku Totalcharges
##iii1)
#brak czegos ciekawego w plci, malo seniorow, partnerzy podobnie jak plec, na utrzymaniu nie ma ponad 2/3 wiekszosc ma usluge telefoniczna
#wiekszosc nie ma wielu linii, najczesciej swiatlowod, rzadko zabezpieczenia internetowe, kopia zapasowa i zabezpiecznie podobnie bardzo telewizja strumieniowa i filmy pol na pol wsrod majacych internet, najpopularniejsza umowa jest miesiac do miesiaca
# 3 metody platnosci tak samo electronic check wiecej, duża cześć klientow zostaje ale jednak 1869/(5174+1869) to troche

#etap3
dane.zostali <- subset(dane, Churn=="No")
dane.odeszli <- subset(dane, Churn=="Yes")
skim(dane.zostali)
IQR(dane.zostali$tenure)
IQR(dane.zostali$MonthlyCharges)
IQR(dane.zostali$TotalCharges)
skim(dane.odeszli)
IQR(dane.odeszli$tenure)
IQR(dane.odeszli$MonthlyCharges)
IQR(dane.odeszli$TotalCharges)
#histogramy chyba lepiej pominac?
# No to ci co zostali
#boxtenure, wykres 27
ggplot(dane, aes(x= Churn, y = tenure, fill=Churn)) +
  geom_boxplot(
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "mediumblue", "Yes" = "firebrick2")
  ) +
  labs(
    y= "Miesiące",
    title= "Wykres pudełkowy czasu trwania umowy",
  ) +
  theme_minimal()
#boxmonthly, wykres 28
ggplot(dane, aes(x=Churn, y = MonthlyCharges, fill=Churn)) +
  geom_boxplot(
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "mediumblue", "Yes" = "firebrick2")
  ) +
  labs(
    y= "Miesiączne opłaty",
    title= "Wykres pudełkowy miesięcznych opłat",
  ) +
  theme_minimal()
#boxtotal, wykres 29
ggplot(dane, aes(x=Churn, y = TotalCharges, fill=Churn)) +
  geom_boxplot(
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "mediumblue", "Yes" = "firebrick2")
  ) +
  labs(
    y= "Całkowite opłaty",
    title= "Wykres pudełkowy całkowitych opłat",
  ) +
  theme_minimal()
# koniec wykresów zmiennych ciągłych/numerycznych
#wykresy kategoryczne
#płeć, wykres 30
ggplot(dane, aes(x = gender, fill = Churn)) +
  geom_bar(
    position ="dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    position = position_dodge(width = 0.9), 
    size = 4,
  )+
  labs(
    title = "Wykres kolumnowy płci klientów",
    x = "Płeć",
    y = "liczba klientów",
  ) +
  theme_classic()
#SeniorCitizen, wykres 31
ggplot(dane, aes(x = SeniorCitizen, fill = Churn)) +
  geom_bar(
    position ="dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    position = position_dodge(width = 0.9), 
    size = 4,
  )+
  labs(
    title = "Klienci według statusu seniora",
    x = "Status seniora",
    y = "liczba klientów",
  ) +
  theme_classic()
#Partner, wykres 32
ggplot(dane, aes(x = Partner, fill = Churn)) +
  geom_bar(
    position ="dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    position = position_dodge(width = 0.9),
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania partnera",
    x = "Posiadanie partnera",
    y = "liczba klientów",
  ) +
  theme_classic()
#Dependents, wykres 33
ggplot(dane, aes(x = Dependents, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania osoby na utrzymaniu",
    x = "Posiadanie osoby na utrzymaniu",
    y = "liczba klientów",
  ) +
  theme_classic()
#PhoneService, wykres 34
ggplot(dane, aes(x = PhoneService, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi telefonicznej",
    x = "Posiadanie usługi telefonicznej",
    y = "liczba klientów",
  ) +
  theme_classic()
#MultipleLines, wykres 35
ggplot(dane, aes(x = MultipleLines, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według liczby linii telefonicznych",
    x = "Posiadanie wielu linii telefonicznych",
    y = "liczba klientów",
  ) +
  theme_classic()
#InternetService, wykres 36 #!!! tu widac w fiber optic mocno!
ggplot(dane, aes(x = InternetService, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi internetowej",
    x = "Posiadanie usługi internetowej",
    y = "liczba klientów",
  ) +
  theme_classic()
#DSL - Cyfrowa linia abonencka (Kabel telefoniczny), Fiber optic - światłowód wiadomo
#OnlineSecurity, wykres 37 # tez cos widac
ggplot(dane, aes(x = OnlineSecurity, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczeń internetowych",
    x = "Posiadanie zabezpeiczeń internetowych",
    y = "liczba klientów",
  ) +
  theme_classic()
#OnlineBackup, wykres 38 tez chyba cos jest
ggplot(dane, aes(x = OnlineBackup, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania kopii zapasowej w chmurze",
    x = "Posiadanie kopii zapasowej w chmurze",
    y = "liczba klientów",
  ) +
  theme_classic()
#DeviceProtection, wykres 39
ggplot(dane, aes(x = DeviceProtection, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczenia urządzenia",
    x = "Posiadanie zabezpieczenia urządzenia",
    y = "liczba klientów",
  ) +
  theme_classic()
#TechSupport, wykres 40
ggplot(dane, aes(x = TechSupport, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania wsparcia technicznego",
    x = "Posiadanie wsparcia technicznego",
    y = "liczba klientów",
  ) +
  theme_classic()
#StreamingTV, wykres 41
ggplot(dane, aes(x = StreamingTV, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania telewizji strumieniowej",
    x = "Posiadanie telewizji strumieniowej",
    y = "liczba klientów",
  ) +
  theme_classic()
#StreamingMovies, wykres 42
ggplot(dane, aes(x = StreamingMovies, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania filmów na żądanie",
    x = "Posiadanie filmów na żądanie",
    y = "liczba klientów",
  ) +
  theme_classic()
#Contract, wykres 43
ggplot(dane, aes(x = Contract, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według długości umowy",
    x = "Posiadana długość umowy",
    y = "liczba klientów",
  ) +
  theme_classic()
#month-to-month że umowa na miesiąc co miesiac coś jak subskrypcja
#PaperlessBilling, wykres 44
ggplot(dane, aes(x = PaperlessBilling, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania elektronicznej faktury",
    x = "Posiadanie elektronicznej faktury",
    y = "liczba klientów",
  ) +
  theme_classic()
#PaymentMethod, wykres 45, tu widac w electronic check i jednak jest widoczne
ggplot(dane, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(
    position = "dodge",
    color = "black",
  ) +
  scale_fill_manual(
    values = c("No" = "cornflowerblue", "Yes" = "coral")
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci metody płatności",
    x = "Wybrana metoda płatności",
    y = "liczba klientów",
  ) +
  theme_classic()
# widać w 45,44,43,40,36,33?,31?,29,28,27 coś z tego imo, ale może coś więcej
#ogólnie mocno widać w tenure i np 43