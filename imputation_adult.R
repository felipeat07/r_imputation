library(readr)
library(dplyr)
library(tidyr)
library(VIM)
library(mice)
library(ggplot2)

colunas <- c(
  "age", "workclass", "fnlwgt", "education", "education_num", 
  "marital_status", "occupation", "relationship", "race", "sex", 
  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
)

dados <- read_delim("C:/Users/felip.ELITEBOOK/Documents/r_imputation/adult_data/adult.data", delim = ",", col_names = colunas, trim_ws = TRUE)
View(dados)


dominio <- unique(dados$education)
print(dominio)
print(length(dominio))


col_imputar <- "education"  

data_complete <- dados


# Inserindo valores ausentes artificialmente MCAR
insert_mcar_column <- function(data, coluna, p = 0.1) {
  data_missing <- data
  idx <- sample(1:nrow(data), size = floor(p * nrow(data)))
  data_missing[idx, coluna] <- NA
  return(data_missing)
}

data_missing <- insert_mcar_column(data_complete, col_imputar, p = 0.1)


# Imputação por Moda
mode_impute <- function(df) {
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
    }
  }
  return(df)
}

data_mode <- mode_impute(data_missing)


# Imputação por Hot Deck
data_shd <- VIM::hotdeck(data_missing, imp_var = FALSE)


# Imputação por KNN MODA E HAMMING
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data_knn <- VIM::kNN(
  data_missing,
  variable = col_imputar,
  k = 20,
  imp_var = FALSE,
  metric = "Gower",
  catFun = moda
)


# Imputação por MICE com Random Forest
data_missing_mice <- data_missing
data_missing_mice[] <- lapply(data_missing_mice, function(x) as.factor(x))
mice_data <- mice(data_missing_mice, m = 5, method = "rf", seed = 123)
data_mice <- complete(mice_data, 1)




# Avaliação de precisão
calculate_precision <- function(original, imputed, mask) {
  total <- 0
  correct <- 0
  for (col in names(original)) {
    na_idx <- which(is.na(mask[[col]]))
    total <- total + length(na_idx)
    correct <- correct + sum(original[[col]][na_idx] == imputed[[col]][na_idx])
  }
  return(correct / total)
}

precision_scores <- data.frame(
  Method = c("Mode", "SHD", "KNN"#, "MICE"
             ),
  Precision = c(
    calculate_precision(data_complete, data_mode, data_missing),
    calculate_precision(data_complete, data_shd, data_missing),
    calculate_precision(data_complete, data_knn, data_missing)#,
    #calculate_precision(data_complete, data_mice, data_missing)
  )
)

print(precision_scores)




# Tamanho do domínio da variável imputada
dominio_size <- length(unique(data_complete[[col_imputar]]))

# Precisão esperada aleatória
expected_random_precision <- 1 / dominio_size

# Função para calcular a precisão normalizada
calculate_normalized_precision <- function(precision, expected_precision) {
  return((precision - expected_precision) / (1 - expected_precision))
}

# Calculando as precisões normalizadas
normalized_precision_scores <- data.frame(
  Method = precision_scores$Method,
  Normalized_Precision = sapply(precision_scores$Precision, function(p) {
    calculate_normalized_precision(p, expected_random_precision)
  })
)

print(normalized_precision_scores)




###########################################  Analise gráfica


##################### BARRAS para dominio 16
# Adicionando coluna de precisão aleatória esperada
precision_scores$ExpectedRandom <- 1 / dominio_size

# Preparando o DataFrame para o gráfico
precision_long <- precision_scores %>%
  pivot_longer(cols = c("Precision", "ExpectedRandom"),
               names_to = "Tipo",
               values_to = "Precisao") %>%
  mutate(Tipo = recode(Tipo,
                       "Precision" = "Precisão Observada",
                       "ExpectedRandom" = "Precisão Aleatória"))

# Plotando
ggplot(precision_long, aes(x = Method, y = Precisao, fill = Tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparação entre Precisão Observada e Aleatória",
    x = "Método de Imputação",
    y = "Precisão",
    fill = "Tipo de Precisão"
  ) +
  scale_fill_manual(values = c("Precisão Observada" = "steelblue", "Precisão Aleatória" = "salmon")) +
  theme_minimal(base_size = 14)
#####################


##################### MODA
# Função que reduz o domínio agrupando categorias menos frequentes em "Outro"
reduzir_dominio <- function(coluna, n_top) {
  top_levels <- names(sort(table(coluna), decreasing = TRUE))[1:n_top]
  nova_coluna <- ifelse(coluna %in% top_levels, coluna, "Outro")
  return(factor(nova_coluna))
}

# Função principal para calcular precisão
simular_reducao_dominio <- function(data, coluna, tamanhos_dominios, p_missing = 0.1) {
  resultados <- data.frame()
  
  for (dom_size in tamanhos_dominios) {
    # Reduz o domínio
    data_reduzido <- data %>% mutate(educ_reduzido = reduzir_dominio(.data[[coluna]], dom_size))
    
    # Introduz NA
    set.seed(123 + dom_size)
    amostra <- data_reduzido
    idx_na <- sample(1:nrow(amostra), size = floor(p_missing * nrow(amostra)))
    valor_real <- amostra$educ_reduzido[idx_na]
    amostra$educ_reduzido[idx_na] <- NA
    
    # Imputa com moda
    moda <- names(sort(table(amostra$educ_reduzido), decreasing = TRUE))[1]
    imputado <- amostra$educ_reduzido
    imputado[idx_na] <- moda
    
    # Calcula precisao observada
    correto <- sum(imputado[idx_na] == valor_real)
    total <- length(idx_na)
    precisao_obs <- correto / total
    
    # Precisao aleatoria teorica
    n_dom <- length(unique(na.omit(data_reduzido$educ_reduzido)))
    precisao_aleat <- 1 / n_dom
    
    resultados <- rbind(resultados, data.frame(
      Dominio = n_dom,
      Precisao_Observada = precisao_obs,
      Precisao_Aleatoria = precisao_aleat
    ))
  }
  
  return(resultados)
}

# Executa a análise
tamanhos <- 2:16
resultados <- simular_reducao_dominio(dados, "education", tamanhos)

# Transforma em formato longo
resultados_long <- pivot_longer(resultados,
                                cols = c("Precisao_Observada", "Precisao_Aleatoria"),
                                names_to = "Tipo", values_to = "Precisao")

# Plota
ggplot(resultados_long, aes(x = Dominio, y = Precisao, color = Tipo)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Precisão da imputação com MODA ao reduzir o domínio de 'education'",
    x = "Tamanho do domínio (após redução)",
    y = "Precisão",
    color = "Tipo de Precisão (método: Moda)"
  ) +
  theme_minimal(base_size = 14)
#####################


##################### KNN
# Função que reduz o domínio agrupando categorias menos frequentes em "Outro"
reduzir_dominio <- function(coluna, n_top) {
  top_levels <- names(sort(table(coluna), decreasing = TRUE))[1:n_top]
  nova_coluna <- ifelse(coluna %in% top_levels, coluna, "Outro")
  return(factor(nova_coluna))
}

# Função principal para calcular precisão com imputação por KNN
simular_reducao_dominio_knn <- function(data, coluna, tamanhos_dominios, p_missing = 0.1) {
  resultados_knn <- data.frame()
  
  for (dom_size in tamanhos_dominios) {
    # Reduz o domínio
    data_reduzido <- data %>% mutate(educ_reduzido = reduzir_dominio(.data[[coluna]], dom_size))
    
    # Introduz NA
    set.seed(123 + dom_size)
    amostra <- data_reduzido
    idx_na <- sample(1:nrow(amostra), size = floor(p_missing * nrow(amostra)))
    valor_real <- amostra$educ_reduzido[idx_na]
    amostra$educ_reduzido[idx_na] <- NA
    
    # Imputa com KNN e a função de moda
    data_knn <- VIM::kNN(
      amostra,
      variable = "educ_reduzido",
      k = 5,  # Ajustando o valor de K para ser menor
      imp_var = FALSE,
      metric = "Gower",
      catFun = moda
    )
    
    imputado_knn <- data_knn$educ_reduzido
    
    # Calcula precisão observada
    correto_knn <- sum(imputado_knn[idx_na] == valor_real)
    total_knn <- length(idx_na)
    precisao_obs_knn <- correto_knn / total_knn
    
    # Precisão aleatória teórica
    n_dom_knn <- length(unique(na.omit(data_reduzido$educ_reduzido)))
    precisao_aleat_knn <- 1 / n_dom_knn
    
    # Armazena os resultados
    resultados_knn <- rbind(resultados_knn, data.frame(
      Dominio = n_dom_knn,
      Precisao_Observada = precisao_obs_knn,
      Precisao_Aleatoria = precisao_aleat_knn
    ))
  }
  
  return(resultados_knn)
}

# Definindo os tamanhos do domínio
tamanhos <- 2:16

# Executa a análise para KNN
resultados_knn <- simular_reducao_dominio_knn(dados, "education", tamanhos)

# Transforma em formato longo
resultados_knn_long <- pivot_longer(resultados_knn,
                                    cols = c("Precisao_Observada", "Precisao_Aleatoria"),
                                    names_to = "Tipo", values_to = "Precisao")

# Plota KNN
ggplot(resultados_knn_long, aes(x = Dominio, y = Precisao, color = Tipo)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Precisão da imputação por KNN ao reduzir o domínio de 'education'",
    x = "Tamanho do domínio (após redução)",
    y = "Precisão",
    color = "Tipo de Precisão (método: KNN com Moda)"
  ) +
  theme_minimal(base_size = 14)

#####################


