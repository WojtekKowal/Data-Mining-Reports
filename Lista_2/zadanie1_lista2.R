library(arules)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(scales)

THEME <- theme_bw(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey40"),
        legend.position = "bottom")
theme_set(THEME)

PALETTE  <- c("setosa" = "#E41A1C", "versicolor" = "#377EB8", "virginica" = "#4DAF4A")
K        <- 3   #iczba przedziałów


#  (a) Wczytanie i przegląd danych

data(iris)

print(summary(iris))

features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
feat_lab  <- c(Sepal.Length = "Sepal Length [cm]", Sepal.Width  = "Sepal Width [cm]",
               Petal.Length = "Petal Length [cm]", Petal.Width  = "Petal Width [cm]")


#  (b) WYBÓR CECH

cat("\n── Statystyki opisowe (per gatunek) ──────────────────────────\n")
desc <- iris %>%
  group_by(Species) %>%
  summarise(across(all_of(features),
                   list(mean = ~round(mean(.), 2),
                        sd   = ~round(sd(.), 2),
                        min  = ~round(min(.), 2),
                        max  = ~round(max(.), 2)),
                   .names = "{.col}__{.fn}"),
            .groups = "drop")
print(as.data.frame(desc))

#rozkłady gęstości
df_long <- iris %>%
  pivot_longer(all_of(features), names_to = "Cecha", values_to = "Wartość") %>%
  mutate(Cecha = factor(Cecha, levels = features, labels = feat_lab))

p_density <- ggplot(df_long, aes(x = Wartość, fill = Species, colour = Species)) +
  geom_density(alpha = 0.28, linewidth = 0.7) +
  facet_wrap(~ Cecha, scales = "free", ncol = 2) +
  scale_fill_manual(values = PALETTE) +
  scale_colour_manual(values = PALETTE) +
  labs(title    = "Rozkłady gęstości cech (per gatunek)",
       subtitle = "Zachodzenie rozkładów = słabsza zdolność dyskryminacyjna",
       x = NULL, y = "Gęstość", fill = "Gatunek", colour = "Gatunek")
print(p_density)

# ── WYKRES 2 – boxploty z jitter (Narzędzie analizy jednowymiarowej) ────────
p_box <- ggplot(df_long, aes(x = Species, y = Wartość, fill = Species)) +
  geom_boxplot(alpha = 0.60, outlier.colour = NA) +
  geom_jitter(aes(colour = Species), width = 0.13, size = 0.8, alpha = 0.45) +
  facet_wrap(~ Cecha, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = PALETTE) +
  scale_colour_manual(values = PALETTE) +
  labs(title    = "Wykresy pudełkowe cech z nałożonymi punktami",
       subtitle = "Brak nakładania wąsów -> lepsza separacja klas (wizualna ocena dyskryminacji)",
       x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 18, hjust = 1))
print(p_box)

best_feat  <- "Petal.Length"
worst_feat <- "Sepal.Width"

cat(sprintf("\n  >> Najlepsza cecha: %s\n", best_feat))
cat(sprintf("  >> Najgorsza cecha: %s\n\n", worst_feat))

#  (c) DYSKRETYZACJA

do_discretize <- function(feat) {
  x <- iris[[feat]]
  
  #Equal Width
  d_ew <- discretize(x, method = "interval",  breaks = K)
  
  #equal Frequency
  d_ef <- discretize(x, method = "frequency", breaks = K)
  
  #k-means clustering
  d_km <- discretize(x, method = "cluster", breaks = K)
  
  #user-defined
  rng  <- range(x)
  span <- rng[2] - rng[1]
  d_ud <- cut(x, 
              breaks = c(-Inf, rng[1] + 0.28 * span, rng[1] + 0.62 * span, Inf),
              labels = c("niski", "sredni", "wysoki"))
  list(ew = d_ew, ef = d_ef, km = d_km, ud = d_ud)
}

disc_best  <- do_discretize(best_feat)
disc_worst <- do_discretize(worst_feat)

methods     <- c("ew", "ef", "km", "ud")
methods_lab <- c(ew = "Equal Width", ef = "Equal Frequency", 
                 km = "K-means",     ud = "User-defined")

plot_contingency <- function(disc_vec, title_str) {
  df  <- data.frame(Przedzial = factor(disc_vec), Gatunek = iris$Species)
  tbl <- as.data.frame(table(df)) %>%
    group_by(Przedzial) %>%
    mutate(Pct = Freq / sum(Freq)) %>%
    ungroup()
  
  ggplot(tbl, aes(x = Przedzial, y = Pct, fill = Gatunek)) +
    geom_col(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(Freq > 0, Freq, "")),
              position = position_stack(vjust = 0.5), 
              size = 3.4, colour = "white", fontface = "bold") +
    scale_fill_manual(values = PALETTE) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title_str, subtitle = "Struktura klas w przedziałach",
         x = "Przedział", y = "Udział", fill = "Gatunek") +
    theme(axis.text.x = element_text(angle = 22, hjust = 1, size = 8),
          legend.position = "bottom")
}

g3 <- lapply(methods, function(m) plot_contingency(disc_best[[m]], methods_lab[m]))
grid.arrange(grobs = g3, ncol = 2,
             top = grid::textGrob(
               paste("Dyskretyzacja najlepszej cechy:", best_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 14)))


g4 <- lapply(methods, function(m) plot_contingency(disc_worst[[m]], methods_lab[m]))
grid.arrange(grobs = g4, ncol = 2,
             top = grid::textGrob(
               paste("Dyskretyzacja najgorszej cechy:", worst_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 14)))

plot_lecture_breaks <- function(feat, disc_list) {
  x <- iris[[feat]]
  
  set.seed(73)
  y_rand <- runif(length(x), 0, 20)
  
  lapply(methods, function(m) {
    d <- disc_list[[m]]
    grp_med  <- sort(tapply(x, d, median))
    ord_lvls <- names(grp_med)
    bounds   <- numeric(0)
    for (i in 1:(length(ord_lvls) - 1)) {
      max_curr <- max(x[d == ord_lvls[i]], na.rm = TRUE)
      min_next <- min(x[d == ord_lvls[i + 1]], na.rm = TRUE)
      bounds   <- c(bounds, (max_curr + min_next) / 2)
    }
    
    ggplot(data.frame(x = x, y = y_rand, Gatunek = iris$Species), 
           aes(x = x, y = y, colour = Gatunek)) +
      geom_point(size = 1.8, alpha = 0.8) +
      geom_vline(xintercept = bounds, linetype = "dashed", 
                 colour = "grey20", linewidth = 0.8) +
      scale_colour_manual(values = PALETTE) +
      labs(title = methods_lab[m], x = feat_lab[feat], y = "") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 11, face = "bold"))
  })
}

ph_best  <- plot_lecture_breaks(best_feat,  disc_best)
ph_worst <- plot_lecture_breaks(worst_feat, disc_worst)

grid.arrange(grobs = ph_best, ncol = 2,
             top = grid::textGrob(
               paste("Wizualizacja podziałów", best_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 13)))

grid.arrange(grobs = ph_worst, ncol = 2,
             top = grid::textGrob(
               paste("Wizualizacja podziałów", worst_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 13)))

plot_heatmap <- function(disc_vec, feat, method_name) {
  tbl <- as.data.frame(table(Przedzial = factor(disc_vec), Gatunek = iris$Species))
  
  ggplot(tbl, aes(x = Gatunek, y = Przedzial, fill = Freq)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = Freq), size = 5.5, fontface = "bold", 
              colour = ifelse(tbl$Freq > 25, "white", "grey20")) +
    scale_fill_gradient(low = "#deebf7", high = "#08519c") +
    labs(title = paste(method_name, "–", feat),
         x = NULL, y = NULL, fill = "n") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

hm_best  <- lapply(methods, function(m) plot_heatmap(disc_best[[m]],  best_feat,  methods_lab[m]))
hm_worst <- lapply(methods, function(m) plot_heatmap(disc_worst[[m]], worst_feat, methods_lab[m]))

grid.arrange(grobs = hm_best,  ncol = 2,
             top = grid::textGrob(
               paste("Tabele krzyżowe", best_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 13)))

grid.arrange(grobs = hm_worst, ncol = 2,
             top = grid::textGrob(
               paste("Tabele krzyżowe", worst_feat),
               gp = grid::gpar(fontface = "bold", fontsize = 13)))

}