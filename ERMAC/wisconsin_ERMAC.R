# Instalar e carregar pacotes necessários
install.packages(c("caret", "e1071", "randomForest", "glmnet", "reticulate"))
library(caret)
library(tools)
library(e1071)
library(randomForest)
library(glmnet)
library(reticulate)

# Carregar a biblioteca optuna do Python
py_install("optuna")
# options(reticulate.py_flush_output = FALSE) # só se precisar
optuna <- import("optuna")

# Carregar os dados a partir dos arquivos CSV
X_train <- read.csv("C:/Users/ana_v/OneDrive/Área de Trabalho/Doutorado/ERMAC/X_train.csv", header = TRUE)
y_train <- read.csv("C:/Users/ana_v/OneDrive/Área de Trabalho/Doutorado/ERMAC/y_train.csv", header = TRUE)
X_test <- read.csv("C:/Users/ana_v/OneDrive/Área de Trabalho/Doutorado/ERMAC/X_test.csv", header = TRUE)
y_test <- read.csv("C:/Users/ana_v/OneDrive/Área de Trabalho/Doutorado/ERMAC/y_test.csv", header = TRUE)

# Converter `y_train` e `y_test` em fatores
y_train <- as.factor(y_train[, 1])
y_test <- as.factor(y_test[, 1])

# Combinar X_train e y_train para o treinamento
train_data <- cbind(X_train, Diagnosis = y_train)

# Função objetivo para otimizar o SVM
objective_svm <- function(trial) {
  # Sugerir valores para os hiperparâmetros
  svc_c <- trial$suggest_float("svc_c", 1e-5, 1e2, log = TRUE)
  svc_kernel <- trial$suggest_categorical("svc_kernel", c("linear", "polynomial", "radial", "sigmoid"))
  
  # Configuração do método e grade de tuning
  method <- switch(svc_kernel,
                   "linear" = "svmLinear",
                   "polynomial" = "svmPoly",
                   "radial" = "svmRadial",
                   "sigmoid" = "svmRadialSigma")  # sigmoid usa o mesmo método do radial no caret
  
  # Adicionar `sigma` apenas se for necessário
  if (svc_kernel %in% c("polynomial", "radial", "sigmoid")) {
    sigma <- trial$suggest_float("sigma", 1e-5, 1e2, log = TRUE)
    tune_grid <- expand.grid(C = svc_c, sigma = sigma)
  } else {
    tune_grid <- expand.grid(C = svc_c)
  }
  
  # Treinar o modelo
  model <- tryCatch({
    train(Diagnosis ~ ., data = train_data,
          method = method,
          trControl = trainControl(method = "cv", number = 10),
          tuneGrid = tune_grid)
  }, error = function(e) return(NULL))  # Retornar NULL caso o modelo falhe
  
  # Verificar se o modelo foi treinado com sucesso
  if (is.null(model)) return(NA)
  
  # Retornar a acurácia média
  return(mean(model$results$Accuracy))
}

# Função objetivo para otimizar o Random Forest
objective_rf <- function(trial) {
  rf_n_estimators <- trial$suggest_int("rf_n_estimators", 50, 200)
  rf_max_depth <- trial$suggest_int("rf_max_depth", 10, 50)
  rf_min_samples_split <- trial$suggest_int("rf_min_samples_split", 2, 20)
  
  # Treinar o modelo Random Forest
  model <- randomForest(Diagnosis ~ ., data = train_data,
                        ntree = rf_n_estimators,
                        maxnodes = rf_max_depth,
                        nodesize = rf_min_samples_split)
  
  # Calcular acurácia no conjunto de treino
  predictions <- predict(model, newdata = train_data)
  acc <- mean(predictions == train_data$Diagnosis)
  
  return(acc)
}

# Função objetivo para otimizar a Regressão Logística
objective_lr <- function(trial) {
  lr_c <- trial$suggest_loguniform("lr_c", 1e-5, 1e2)
  lr_solver <- trial$suggest_categorical("lr_solver", c("newton-cg", "lbfgs", "liblinear", "saga"))
  
  # Ajustar o modelo com glmnet
  model <- train(Diagnosis ~ ., data = train_data,
                 method = "glmnet",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = expand.grid(alpha = 0, lambda = 1 / lr_c))
  
  # Retornar a acurácia média
  return(mean(model$results$Accuracy))
}

# Otimização com Optuna
# SVM
study_svm <- optuna$create_study(direction = "maximize")
study_svm$optimize(objective_svm, n_trials = 10)

# Random Forest
study_rf <- optuna$create_study(direction = "maximize")
study_rf$optimize(objective_rf, n_trials = 10)

# Regressão Logística
study_lr <- optuna$create_study(direction = "maximize")
study_lr$optimize(objective_lr, n_trials = 10)

# Melhores parâmetros encontrados
best_params_svm <- study_svm$best_params
best_params_rf <- study_rf$best_params
best_params_lr <- study_lr$best_params

cat("Melhores parâmetros para o SVM:", paste(names(best_params_svm), best_params_svm, collapse = ", "), "\n")
cat("Melhores parâmetros para o Random Forest:", paste(names(best_params_rf), best_params_rf, collapse = ", "), "\n")
cat("Melhores parâmetros para a Regressão Logística:", paste(names(best_params_lr), best_params_lr, collapse = ", "), "\n")

# Avaliação final nos dados de teste
evaluate_model <- function(model, X_test, y_test) {
  predictions <- predict(model, newdata = X_test)
  confusion <- confusionMatrix(predictions, y_test)
  print(confusion)
}

# Modelos otimizados
# Modelo SVM Linear com os melhores parâmetros
model_svm <- train(Diagnosis ~ ., 
                   data = train_data, 
                   method = "svmLinear",  # Usando SVM com kernel linear
                   tuneGrid = expand.grid(C = best_params_svm$svc_c))  # Somente o parâmetro C
evaluate_model(model_svm, X_test, y_test)

# Modelo Random Forest com os melhores parâmetros
model_rf <- randomForest(Diagnosis ~ ., 
                         data = train_data,
                         ntree = best_params_rf$rf_n_estimators,
                         maxnodes = best_params_rf$rf_maxnodes,
                         nodesize = best_params_rf$rf_nodesize)
evaluate_model(model_rf, X_test, y_test)

# Modelo de Regressão Logística com os melhores parâmetros
model_lr <- train(Diagnosis ~ ., 
                  data = train_data, 
                  method = "glmnet",  # Método fixo para regressão logística
                  tuneGrid = expand.grid(alpha = 0, lambda = 1 / best_params_lr$lr_c))
evaluate_model(model_lr, X_test, y_test)