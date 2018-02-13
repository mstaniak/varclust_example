# Biblioteki
library(tidyverse)
library(varclust)
library(readr)
# Dane
may_2017 <- read_csv("~/Dokumenty/Projekty/varclust/may-2017.csv")
maj <- select_if(may_2017, is.numeric)
summarise_all(maj, function(x) sum(is.na(x)))
# Klasteryzacja
proba_var <- mlcc.bic(maj)
# Liczba klastrów
max(proba_var$segmentation)
# Jak wyglądają grupy?
colnames(maj)[proba_var$segmentation == 1]
colnames(maj)[proba_var$segmentation == 2]
colnames(maj)[proba_var$segmentation == 3]
colnames(maj)[proba_var$segmentation == 4]
colnames(maj)[proba_var$segmentation == 5]
colnames(maj)[proba_var$segmentation == 6]
colnames(maj)[proba_var$segmentation == 7]
save(proba_var, file = "proba_var.rda")
save(maj, file = "maj.rda")
