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
#nie widze nietypowych wartosci
summary(dane)
library(skimr)
# problem że seniorCitizen jest liczony numerycznie a to 0-1 wiec nie ma to sensu
dane$SeniorCitizen <- as.factor(dane$SeniorCitizen)
skim(dane)
# SEKCJA WORK IN PROGRESS!
# teraz 2b) potrzebujemy biblioteki do wykresów
library(ggplot2)
#najpierw numeryczne
ggplot(dane, aes(x = MonthlyCharges)) +
  geom_histogram(
    fill = "steelblue",
    color = "white",
    bins = 30
  ) +
  geom_text(
    stat = "bin",
    bins = 30,
    aes(label = after_stat(count), y = after_stat(count)),
    vjust = -0.5,
    size = 3 
  )+
  labs(
    title = "Histogram miesięcznych opłat",
    x = "Miesięczne opłaty",
    y = "Liczba klientów"
  ) +
  theme_minimal()
#histogram tenure
ggplot(dane, aes(x = tenure)) +
  geom_histogram(
    fill = "steelblue",
    color = "white",
    bins = 35
  ) +
  labs(
    title = "Histogram czasu trwania umowy",
    x = "ilość miesięcy",
    y = "Liczba klientów"
  ) +
  theme_minimal()
#wykres boxplot tenure nwm czy to dobry pomysl
ggplot(dane, aes(y = tenure)) +
  geom_boxplot(
    fill = "slateblue",
    color = "black",
  ) +
  labs(
    y= "Miesiące",
    title= "Rozkład czasu trwania umowy",
  ) +
  theme_minimal()
