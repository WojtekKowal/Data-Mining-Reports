
# EKSPLORACJA DANYCH – Zadanie 3

required_pkgs <- c("titanic", "cluster", "MASS", "ggplot2", "dplyr", "gridExtra")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(titanic)
library(cluster)
library(MASS)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Wspólny motyw dla wykresów
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold", size = 13),
                  legend.position = "bottom"))

#  (b) PRZYGOTOWANIE DANYCH

data("titanic_train", package = "titanic")
df <- titanic_train

# 1. Konwersja typów
df$Survived <- as.factor(df$Survived)
df$Pclass   <- as.ordered(df$Pclass) 
df$Sex      <- as.factor(df$Sex)
df$Embarked[df$Embarked == ""] <- NA 
df$Embarked <- as.factor(df$Embarked)

# 2. Usunięcie zmiennych pełniących rolę identyfikatorów
df <- df %>% dplyr::select(-PassengerId, -Name, -Ticket, -Cabin)

# 3. Obsługa braków danych
df_clean <- na.omit(df)

df_features_temp <- df_clean %>% dplyr::select(-Survived)
is_duplicate <- duplicated(df_features_temp)

# Zostawiamy tylko unikalne obserwacje
df_clean <- df_clean[!is_duplicate, ]

#  (c) REDUKCJA WYMIARU

df_features <- df_clean %>% dplyr::select(-Survived)

#dissimilarity matrix
dist_matrix <- daisy(df_features, metric = "gower")

# (Niemetryczne - isoMDS)
set.seed(73) 
mds_model <- isoMDS(dist_matrix, k = 2, trace = FALSE)

mds_coords <- mds_model$points

dist_mds <- dist(mds_coords)

par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))
plot(as.numeric(dist_matrix), as.numeric(dist_mds), 
     xlab = "Oryginalne odległości (Gower)", 
     ylab = "Odległości po mapowaniu MDS",
     main = "Diagram Sheparda",
     pch = 20, col = rgb(0, 0, 0, 0.4), cex = 0.6)
abline(0, 1, col = "red", lty = 2, lwd = 2)


#  (d) WIZUALIZACJA DANYCH

df_plot <- data.frame(
  MDS1 = mds_coords[, 1],
  MDS2 = mds_coords[, 2],
  Survived = df_clean$Survived,
  Sex      = df_clean$Sex,
  Pclass   = df_clean$Pclass
)

#wykres rozrzutu Survived
p_survived <- ggplot(df_plot, aes(x = MDS1, y = MDS2, color = Survived)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("0" = "#E41A1C", "1" = "#4DAF4A"), 
                     labels = c("Nie przeżył (0)", "Przeżył (1)")) +
  labs(title = "MDS - Klastrowanie a przetrwanie (Survived)",
       x = "Wymiar 1 (MDS)", y = "Wymiar 2 (MDS)", color = "Przeżycie:")

#wykres rozrzutu Sex
p_sex <- ggplot(df_plot, aes(x = MDS1, y = MDS2, color = Sex)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("male" = "#377EB8", "female" = "#984EA3")) +
  labs(title = "MDS - Podział na płeć (Sex)",
       x = "Wymiar 1 (MDS)", y = "Wymiar 2 (MDS)", color = "Płeć:")

#wykres rozrzutu Pclass
p_class <- ggplot(df_plot, aes(x = MDS1, y = MDS2, color = Pclass)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("1" = "#FF7F00", "2" = "#4DAF4A", "3" = "#377EB8")) +
  labs(title = "MDS - Podział na klasę (Pclass)",
       x = "Wymiar 1 (MDS)", y = "Wymiar 2 (MDS)", color = "Klasa:")

grid.arrange(p_survived, p_sex, p_class, ncol = 1, 
             top = grid::textGrob("Analiza wyników MDS (zbiór Titanic)", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))