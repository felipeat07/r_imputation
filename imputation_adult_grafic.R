# Carregando bibliotecas necessárias
library(readr)
library(dplyr)
library(tidyr)
library(VIM)
library(mice)
library(ggplot2)

# Leitura dos dados
colunas <- c("age", "workclass", "fnlwgt", "education", "education_num", 
             "marital_status", "occupation", "relationship", "race", "sex", 
             "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

dados <- read_delim("C:/Users/felip.ELITEBOOK/Documents/r_imputation/adult_data/adult.data", 
                    delim = ",", col_names = colunas, trim_ws = TRUE)

# Função para inserção de NA
insert_mcar_column <- function(data, coluna, p = 0.1) {
  data_missing <- data
  idx <- sample(1:nrow(data), size = floor(p * nrow(data)))
  data_missing[idx, coluna] <- NA
  return(list(data_missing = data_missing, idx_na = idx))
}

# Função para calcular precisão
calculate_precision_single <- function(original, imputed, idx_na, coluna) {
  correct <- sum(original[[coluna]][idx_na] == imputed[[coluna]][idx_na])
  total <- length(idx_na)
  return(correct / total)
}

# Função de moda personalizada para KNN
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Variáveis categóricas de interesse
variaveis_categoricas <- c("education", "workclass", "marital_status", "occupation", "race", "sex")

# Lista para armazenar resultados
resultados_precisao <- data.frame()

# Loop pelas variáveis
for (col in variaveis_categoricas) {
  # Garantir que a coluna seja fator
  dados[[col]] <- as.factor(dados[[col]])
  
  # Inserir valores ausentes
  set.seed(123)
  mcar <- insert_mcar_column(dados, col, p = 0.1)
  data_missing <- mcar$data_missing
  idx_na <- mcar$idx_na
  
  # --- KNN ---
  knn_imp <- VIM::kNN(data_missing, variable = col, k = 5, imp_var = FALSE, metric = "Gower", catFun = moda)
  prec_knn <- calculate_precision_single(dados, knn_imp, idx_na, col)
  
  # --- SHD ---
  shd_imp <- VIM::hotdeck(data_missing, imp_var = FALSE)
  prec_shd <- calculate_precision_single(dados, shd_imp, idx_na, col)
  
  # --- MICE ---
 # mice_input <- data_missing
 # mice_input[] <- lapply(mice_input, function(x) as.factor(x))
 # mice_imp <- mice(mice_input, m = 1, method = "rf", seed = 123, printFlag = FALSE)
 # mice_complete <- complete(mice_imp, 1)
 # prec_mice <- calculate_precision_single(dados, mice_complete, idx_na, col)
  
  # Número de categorias
  n_categorias <- length(unique(na.omit(dados[[col]])))
  
  # Armazenar resultados
  resultados_precisao <- bind_rows(resultados_precisao,
                                   data.frame(Variavel = col, Metodo = "KNN", Precisao = prec_knn, Categorias = n_categorias),
                                   data.frame(Variavel = col, Metodo = "SHD", Precisao = prec_shd, Categorias = n_categorias),
                                  # data.frame(Variavel = col, Metodo = "MICE", Precisao = prec_mice, Categorias = n_categorias)
  )
}

# Ordenar variáveis pelo número de categorias
resultados_precisao <- resultados_precisao %>%
  mutate(Variavel = factor(Variavel, levels = unique(Variavel[order(Categorias)])))

# Plot
ggplot(resultados_precisao, aes(x = Variavel, y = Precisao, group = Metodo, color = Metodo)) +
  geom_line(aes(linetype = Metodo), position = position_dodge(width = 0.2)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_text(aes(label = Categorias), vjust = -1.2, size = 4, color = "gray20") +
  labs(
    title = "Precisão da imputação por variável (com diferentes números de categorias)",
    subtitle = "Métodos comparados: KNN, SHD e MICE",
    x = "Variável (número de categorias indicado no gráfico)",
    y = "Precisão",
    color = "Método"
  ) +
  theme_minimal(base_size = 14)
