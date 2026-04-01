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
    x = "ilość miesięcy",
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
#wykres boxplot tenure nwm czy to dobry pomysl, kolor też można zmienić
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
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Wykres kolumnowy płci klientów",
    x = "Płeć",
    y = "Ilość klientów",
  ) +
  theme_classic()
#SeniorCitizen, wykres 8
ggplot(dane, aes(x = SeniorCitizen, fill = SeniorCitizen)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według statusu seniora",
    x = "Status seniora",
    y = "Ilość klientów",
  ) +
  theme_classic()
#Partner, wykres 9
ggplot(dane, aes(x = Partner, fill = Partner)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania partnera",
    x = "Posiadanie partnera",
    y = "Ilość klientów",
  ) +
  theme_classic()
#Dependents, wykres 10
ggplot(dane, aes(x = Dependents, fill = Dependents)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania osoby na utrzymaniu",
    x = "Posiadanie osoby na utrzymaniu",
    y = "Ilość klientów",
  ) +
  theme_classic()
#PhoneService, wykres 11
ggplot(dane, aes(x = PhoneService, fill = PhoneService)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi telefonicznej",
    x = "Posiadanie usługi telefonicznej",
    y = "Ilość klientów",
  ) +
  theme_classic()
#MultipleLines, wykres 12
ggplot(dane, aes(x = MultipleLines, fill = MultipleLines)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według liczby linii telefonicznych",
    x = "Posiadanie wielu linii telefonicznych",
    y = "Ilość klientów",
  ) +
  theme_classic()
#InternetService, wykres 13
ggplot(dane, aes(x = InternetService, fill = InternetService)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania usługi internetowej",
    x = "Posiadanie usługi internetowej",
    y = "Ilość klientów",
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
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczeń internetowych",
    x = "Posiadanie zabezpeiczeń internetowych",
    y = "Ilość klientów",
  ) +
  theme_classic()
#OnlineBackup, wykres 15
ggplot(dane, aes(x = OnlineBackup, fill = OnlineBackup)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania kopii zapasowej w chmurze",
    x = "Posiadanie kopii zapasowej w chmurze",
    y = "Ilość klientów",
  ) +
  theme_classic()
#DeviceProtection, wykres 16
ggplot(dane, aes(x = DeviceProtection, fill = DeviceProtection)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania zabezpieczenia urządzenia",
    x = "Posiadanie zabezpieczenia urządzenia",
    y = "Ilość klientów",
  ) +
  theme_classic()
#TechSupport, wykres 17
ggplot(dane, aes(x = TechSupport, fill = TechSupport)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania wsparcia technicznego",
    x = "Posiadanie wsparcia technicznego",
    y = "Ilość klientów",
  ) +
  theme_classic()
#StreamingTV, wykres 18
ggplot(dane, aes(x = StreamingTV, fill = StreamingTV)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania telewizji strumieniowej",
    x = "Posiadanie telewizji strumieniowej",
    y = "Ilość klientów",
  ) +
  theme_classic()
#StreamingMovies, wykres 19
ggplot(dane, aes(x = StreamingMovies, fill = StreamingMovies)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania filmów na żądanie",
    x = "Posiadanie filmów na żądanie",
    y = "Ilość klientów",
  ) +
  theme_classic()
#Contract, wykres 20
ggplot(dane, aes(x = Contract, fill = Contract)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według długości umowy",
    x = "Posiadana długość umowy",
    y = "Ilość klientów",
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
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według posiadania elektronicznej faktury",
    x = "Posiadanie elektronicznej faktury",
    y = "Ilość klientów",
  ) +
  theme_classic()
#PaymentMethod, wykres 22
ggplot(dane, aes(x = PaymentMethod, fill = PaymentMethod)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci metody płatności",
    x = "Wybrana metoda płatności",
    y = "Ilość klientów",
  ) +
  theme_classic()
#Churn, wykres 23 - tym sie zajmuje wiec imo musi sie znalezc
ggplot(dane, aes(x = Churn, fill = Churn)) +
  geom_bar(
    color = "black",
  ) +
  geom_text(
    stat = "count",
    bins = 35,
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4,
  )+
  labs(
    title = "Klienci według pozostania przy firmie",
    x = "Pozostanie przy firmie",
    y = "Ilość klientów",
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
# SEKCJA WORK IN PROGRESS!
#2d)
#Zinterpretuj otrzymane rezultaty, w szczególności odpowiadając na pytania:
# i)Jaki jest zakres możliwych wartości dla poszczególnych zmiennych?
# ii) W przypadku zmiennych ilościowych:
# ii1)Czy wszystkie zmienne mają rozkład symetryczny?
# ii2) Które cechy charakteryzują się największą zmiennoocią?
# iii) W przypadku zmiennych jakościowych:
# iii1) Co można powiedzieć o częstości przyjmowania poszczególnych kategorii?