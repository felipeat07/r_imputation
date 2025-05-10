library(dplyr)
library(tidyr)
library(VIM)
library(mice)
library(missForest)
library(caret)
library(e1071)
library(naivebayes)
library(randomForest)


# Gerando dados artificiais semelhantes ao dataset do artigo

#BUSCAR RELAÇÂO ENTRE DOMINIO DO DADO CATEGORICO E QUALIDADE DE IMPUTAÇÂO

set.seed(42)
n <- 5000

data_complete <- data.frame(
  Religion = sample(c("Catholic", "Muslim", "Protestant", "Other"), n, replace = TRUE),
  Diabetes = sample(c("Yes", "No"), n, replace = TRUE),
  HIV = sample(c("Yes", "No"), n, replace = TRUE),
  AgeGroup = sample(c("<20", "<35", "≥35"), n, replace = TRUE),
  MaritalStatus = sample(c("Married", "Unmarried"), n, replace = TRUE),
  Education = sample(c("No formal education", "Primary", "Lower secondary", "Upper secondary","Post-secondary non-tertiary", "Tertiary - Bachelor", "Tertiary - Master", "Tertiary - Doctorate"), n, replace = TRUE),
  Gravidity = sample(c("Primi", "Multi", "Grand"), n, replace = TRUE),
  SMM = sample(c("Yes", "No"), n, replace = TRUE)
)


# Inserindo valores ausentes do tipo MCAR apenas na coluna Education
insert_mcar_education <- function(data, p = 0.1) {
  data_missing <- data
  idx <- sample(1:nrow(data), size = floor(p * nrow(data)))
  data_missing[idx, "Diabetes"] <- NA
  return(data_missing)
}

data_missing <- insert_mcar_education(data_complete, p = 0.1)



# IMPUTAÇÃO POR MODA

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



#IMPUTAÇÃO SEQUENCIAL HOT DECK

data_shd <- VIM::hotdeck(data_missing, imp_var = FALSE)



# IMPUTAÇÃO POR KNN

# Função para moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Imputação apenas da coluna Education com KNN + Hamming + Moda
data_knn <- VIM::kNN(
  data_missing,
  variable = "Diabetes",   # imputa coluna education
  k = 20,
  imp_var = FALSE,          # não adiciona colunas auxiliares
  metric = "Gower",         # usa Hamming internamente para categóricas
  catFun = moda             # moda para imputação categórica
)



# IMPUTAÇÃO POR MISSFOREST

# Faz uma cópia para evitar modificar o original
data_missing_rf <- data_missing
# Converte apenas nessa cópia
data_missing_rf[] <- lapply(data_missing_rf, function(x) as.factor(x))
# Aplica missForest
data_rf <- missForest(data_missing_rf)$ximp



# IMPUTAÇÃO POR MICE (com random forest)

data_missing_mice <- data_missing
data_missing_mice[] <- lapply(data_missing_mice, function(x) as.factor(x))

mice_data <- mice(data_missing_mice, m = 5, method = "rf", seed = 123)
data_mice <- complete(mice_data, 1)



#AVALIAÇÃO DA PRECISAO GLOBAL

calculate_precision <- function(original, imputed, mask) {
  total <- 0
  correct <- 0
  for (col in names(original)[-ncol(original)]) {
    na_idx <- which(is.na(mask[[col]]))
    total <- total + length(na_idx)
    correct <- correct + sum(original[[col]][na_idx] == imputed[[col]][na_idx])
  }
  return(correct / total)
}

precision_scores <- data.frame(
  Method = c("Mode", "SHD", "KNN", "RF", "MICE"),
  Precision = c(
    calculate_precision(data_complete, data_mode, data_missing),
    calculate_precision(data_complete, data_shd, data_missing),
    calculate_precision(data_complete, data_knn, data_missing),
    calculate_precision(data_complete, data_rf, data_missing),
    calculate_precision(data_complete, data_mice, data_missing)
  )
)

print(precision_scores)














#AVALIAADO POR CLASSIFICADORES

evaluate_model <- function(data, method_name = "Método") {
  data$SMM <- as.factor(data$SMM)
  idx <- createDataPartition(data$SMM, p = 0.75, list = FALSE)
  train <- data[idx, ]
  test <- data[-idx, ]
  
  # Modelos
  models <- list(
    "Logistic" = train(SMM ~ ., data = train, method = "glm", family = "binomial"),
    "NaiveBayes" = train(SMM ~ ., data = train, method = "naive_bayes"),
    "SVM" = train(SMM ~ ., data = train, method = "svmRadial"),
    "RandomForest" = train(SMM ~ ., data = train, method = "rf")
  )
  
  results <- sapply(models, function(model) {
    prob <- predict(model, newdata = test, type = "prob")[, 2]
    ref <- ifelse(test$SMM == "Yes", 1, 0)
    return(roc_auc <- pROC::auc(pROC::roc(ref, prob)))
  })
  
  return(data.frame(Method = method_name, t(results)))
}



library(pROC)

results <- rbind(
  evaluate_model(data_mode, "Mode"),
  evaluate_model(data_shd, "SHD"),
  evaluate_model(data_knn, "KNN"),
  evaluate_model(data_rf, "RF"),
  evaluate_model(data_mice, "MICE")
)

print(results)




