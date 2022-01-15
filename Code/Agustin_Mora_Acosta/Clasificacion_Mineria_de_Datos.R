### Agustín Mora Acosta
## Máster en Ciencia de Datos e Ingeniería de Computadores
## Mineria de Datos: Clasificación y Preprocesamiento
## Universidad de Granada

# Importamos las librerías necesarias para realizar el trabajo planteado
require(tidyverse)
require(caret)
require(RWeka)
require(pROC)
require(ggplot2)

################################################################################
################################## Línea Base ##################################
################################################################################

# Establecemos una semilla aleatoria
set.seed(1)


# Definimos un método para cargar los datos con los tipos adecuados
load_data_processed = function(data_path, remove_id = TRUE) {
  
  # Cargamos los datos crudos
  data = read.csv(data_path, header=TRUE, sep=',', na.strings = c('?','', 'NA'))
  
  # Quitamos el id en caso de que el parámetro remove_id sea TRUE
  if(remove_id == TRUE){
    data = data %>%
      select(-respondent_id)
  }
  
  # Transformamos los datos para adecuar los tipos
  data = data %>%
    mutate(
      h1n1_concern = factor(h1n1_concern, levels = c("Not at all concerned", "Not very concerned", "Somewhat concerned", "Very concerned"), ordered = TRUE),
      h1n1_knowledge = factor(h1n1_knowledge, levels = c("No knowledge", "A little knowledge", "A lot of knowledge"), ordered = TRUE),
      behavioral_antiviral_meds = as.logical(behavioral_antiviral_meds),
      behavioral_avoidance = as.logical(behavioral_avoidance),
      behavioral_face_mask = as.logical(behavioral_face_mask),
      behavioral_wash_hands = as.logical(behavioral_wash_hands),
      behavioral_large_gatherings = as.logical(behavioral_large_gatherings),
      behavioral_outside_home = as.logical(behavioral_outside_home),
      behavioral_touch_face = as.logical(behavioral_touch_face),
      doctor_recc_h1n1 = as.logical(doctor_recc_h1n1),
      doctor_recc_seasonal = as.logical(doctor_recc_seasonal),
      chronic_med_condition = as.logical(chronic_med_condition),
      child_under_6_months = as.logical(child_under_6_months),
      health_worker = as.logical(health_worker),
      health_insurance = as.logical(health_insurance),
      opinion_h1n1_vacc_effective = factor(opinion_h1n1_vacc_effective, levels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_h1n1_risk = factor(opinion_h1n1_risk, levels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      opinion_h1n1_sick_from_vacc = factor(opinion_h1n1_sick_from_vacc, levels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
      opinion_seas_vacc_effective = factor(opinion_seas_vacc_effective, levels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_seas_risk = factor(opinion_seas_risk, levels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      opinion_seas_sick_from_vacc = factor(opinion_seas_sick_from_vacc, levels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
      age_group = factor(age_group, levels = c("18 - 34 Years", "35 - 44 Years", "45 - 54 Years", "55 - 64 Years", "65+ Years"), ordered = TRUE),
      education = factor(education, levels = c("< 12 Years", "12 Years", "Some College", "College Graduate"), ordered = TRUE),
      race = as.factor(race),
      sex = as.factor(sex),
      income_poverty = factor(income_poverty, levels = c("Below Poverty", "<= $75,000, Above Poverty", "> $75,000"), ordered = TRUE),
      marital_status = as.factor(marital_status),
      rent_or_own = as.factor(rent_or_own),
      employment_status = as.factor(employment_status),
      hhs_geo_region = as.factor(hhs_geo_region),
      census_msa = as.factor(census_msa),
      household_adults = as.ordered(household_adults),
      household_children = as.ordered(household_children),
      employment_industry = as.factor(employment_industry),
      employment_occupation = as.factor(employment_occupation)
    )
}

# Definimos un método para cargar los datos que tienen menos caracteristicas tras una selección
load_data_selected_features = function(data_path, remove_id = TRUE) {
  
  # Cargamos los datos crudos
  data = read.csv(data_path, header=TRUE, sep=',', na.strings = c('?','', 'NA'))
  
  # Quitamos el id en caso de que el parámetro remove_id sea TRUE
  if(remove_id == TRUE){
    data = data %>%
      select(-respondent_id)
  }
  
  # Transformamos los datos para adecuar los tipos
  data = data %>%
    mutate(
      behavioral_antiviral_meds = as.logical(behavioral_antiviral_meds),
      behavioral_avoidance = as.logical(behavioral_avoidance),
      behavioral_face_mask = as.logical(behavioral_face_mask),
      behavioral_wash_hands = as.logical(behavioral_wash_hands),
      behavioral_large_gatherings = as.logical(behavioral_large_gatherings),
      behavioral_outside_home = as.logical(behavioral_outside_home),
      behavioral_touch_face = as.logical(behavioral_touch_face),
      doctor_recc_h1n1 = as.logical(doctor_recc_h1n1),
      doctor_recc_seasonal = as.logical(doctor_recc_seasonal),
      chronic_med_condition = as.logical(chronic_med_condition),
      child_under_6_months = as.logical(child_under_6_months),
      health_worker = as.logical(health_worker),
      health_insurance = as.logical(health_insurance),
      opinion_h1n1_vacc_effective = factor(opinion_h1n1_vacc_effective, levels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_h1n1_risk = factor(opinion_h1n1_risk, levels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      opinion_seas_vacc_effective = factor(opinion_seas_vacc_effective, levels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_seas_risk = factor(opinion_seas_risk, levels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      age_group = factor(age_group, levels = c("18 - 34 Years", "35 - 44 Years", "45 - 54 Years", "55 - 64 Years", "65+ Years"), ordered = TRUE),
      employment_industry = as.factor(employment_industry),
      employment_occupation = as.factor(employment_occupation)
    )
}

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos los datos de entrenamiento y de test con selección de características
X_train = load_data_selected_features("~/X_train_noNAmanual_NAindicators_featureselection55.csv", remove_id = FALSE)
X_test = load_data_selected_features("~/X_test_noNAmanual_NAindicators_featureselection55.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train = X_train %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))

# Instanciamos el modelo Ripper con los datos de entrenamiento 
model.Ripper = JRip(class~., X_train)
summary(model.Ripper)

# Realizamos predicciones sobre el conjunto de evaluación
model.Ripper.pred = predict(model.Ripper, newdata = X_test, type = "probability")
model.Ripper.pred

# Rellenamos el submission_format
submission_format['h1n1_vaccine'] = model.Ripper.pred[, 'Vaccinated (Both)'] + model.Ripper.pred[, 'Vaccinated (H1N1)']
submission_format['seasonal_vaccine'] = model.Ripper.pred[, 'Vaccinated (Both)'] + model.Ripper.pred[, 'Vaccinated (Seasonal)']

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_baseline_nofeatureselection.csv", row.names = FALSE)


################################################################################
################### Línea Base (Dos Problemas Binarios) ########################
################################################################################

# Creamos dos conjuntos de datos con cada vacuna
X_train['class_h1n1'] = as.factor(y_train$h1n1_vaccine)
X_train['class_seas'] = as.factor(y_train$seasonal_vaccine)

# Definimos en caret el train control
fit.control = trainControl(method="cv", number = 5, verboseIter = TRUE)

# Entrenamos el modelo con validación cruzada y con diferentes parámetros para la vacuna h1n1
train.model.h1n1 = caret::train(class_h1n1 ~ .-class_seas, data=X_train, method="JRip",
                        trControl = fit.control, tuneLength = 2)

# Mostramos el resumen del entrenamiento para la vacuna h1n1
train.model.h1n1

# Entrenamos el modelo con validación cruzada y con diferentes parámetros para la vacuna estacional
train.model.seas = caret::train(class_seas ~ .-class_h1n1, data=X_train, method="JRip",
                                trControl = fit.control, tuneLength = 2)

# Mostramos el resumen del entrenamiento para la vacuna seaonal
train.model.seas

# Realizamos predicciones sobre ambos targets utilizando el conjunto de datos de evaluación
predicted.prob.h1n1 = predict(train.model.h1n1$finalModel, 
    newdata=as.data.frame(model.matrix(~., X_test)), type = "probability")
predicted.prob.h1n1[,"1"]
predicted.prob.seas = predict(train.model.seas$finalModel, 
    newdata=as.data.frame(model.matrix(~., X_test)), type = "probability")
predicted.prob.seas[,"1"]

# Rellenamos el submission_format
submission_format['h1n1_vaccine'] = predicted.prob.h1n1[,"1"]
submission_format['seasonal_vaccine'] = predicted.prob.seas[,"1"]

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_dual_baseline_nofeatureselection.csv", row.names = FALSE)


################################################################################
##################################### OVO ######################################
################################################################################

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train = X_train %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))
 
# Obtenemos todas las parejas posibles de clases que existen
class_pairs = list()
class_pairs[[1]] = c(levels(X_train$class)[1], levels(X_train$class)[2])
class_pairs[[2]] = c(levels(X_train$class)[1], levels(X_train$class)[3])
class_pairs[[3]] = c(levels(X_train$class)[1], levels(X_train$class)[4])
class_pairs[[4]] = c(levels(X_train$class)[2], levels(X_train$class)[3])
class_pairs[[5]] = c(levels(X_train$class)[2], levels(X_train$class)[4])
class_pairs[[6]] = c(levels(X_train$class)[3], levels(X_train$class)[4])

# Creamos una función que dado lo siguiente:
#  - Un conjunto de datos de entrenamiento con una columna 'class' con las clases a predecir
#  - El conjunto de datos de evaluación a predecir
#  - Un vector de etiquetas con las que filtrar los datos para entrenar el modelo
# Devuelve un vector con la probabilidad de la primera clase a predecir
calculate_probability_ovo_model = function(train_data, test_data, filtered_classes) {

  # Filtramos el conjunto de datos por las librerias especificadas
  X_train_filtered = train_data %>%
    filter(class %in% filtered_classes)

  # Redefinimos el factor para no generar problemas con Caret
  X_train_filtered = X_train_filtered %>%
    mutate(class = factor(class, levels = filtered_classes))
 
  # Definimos en caret el train control
  fit.control = trainControl(method="cv", number = 5, verboseIter = TRUE)
  
  # Entrenamos el modelo con validación cruzada y con diferentes parámetros para la vacuna h1n1
  train.model.filtered = caret::train(class ~ ., data=X_train_filtered, method="JRip",
                                      trControl = fit.control, tuneLength = 2)
  
  # Realizamos predicciones sobre el conjunto de datos de test
  predicted.prob = predict(train.model.filtered$finalModel, newdata=as.data.frame(model.matrix(~., test_data)), type = "probability")
  predicted.prob[, filtered_classes[1]]
  
}

# Lanzamos este método para cada una de las parejas de clases que hemos planteado
predicted.ovo = lapply(class_pairs, function(pair) calculate_probability_ovo_model(X_train, X_test, pair))
predicted.ovo


# Definimos el número de clases
K = 4

# Declaramos un vector de etiquetas con los nombres de las probabilidades obtenidas
probabilities_names = unlist(lapply(class_pairs, function(pair) paste(pair[1], pair[2], sep=" Vs. ")))

# Creamos dos vectores para almacenar los resultados para cada una de las vacunas
h1n1.probabilities = c()
seas.probabilities = c()

# Iteramos sobre cada uno de los elementos de las predicciones devueltas por los modelos OVO
for(prediction.index in 1:length(predicted.ovo[[1]])){

  # Obtenemos un array con las predicciones para la instancia en concreto
  probabilities_vector = unlist(lapply(predicted.ovo, function(predictions) predictions[prediction.index]))
  probabilities_array = array(probabilities_vector, dim=c(1,(K*(K-1)/2)))
  colnames(probabilities_array) = probabilities_names
  
  # Obtenemos la matriz con las probabilidades
  Q = matrix(0,K,K)
  Q[lower.tri(Q)] = 1 - probabilities_array
  Qt = t(Q)
  Q[upper.tri(Q)] = 1 - Qt[upper.tri(Qt)]
  diag(Q) = rowSums(Q)
  Q = Q / (K-1)
  
  # Creamos un vector de probabilidades para cada clase inicial que sume 1
  p = rbeta(K,1,1)
  p = p/sum(p)
  
  # Actualizamos el vector de probabilidades hasta que el equilibrio ha sido alcanzado utilizando
  # como referencia: Probability Estimates for Multi-class Classification by Pairwise Coupling, Wu et al. (2003)
  for(i in 1:1000) p = Q%*%p
  
  # Formateamos un poco el vector de probabilidades resultante
  final.probability = as.vector(t(p))
  names(final.probability) = levels(X_train$class)
  
  # Calculamos la probabilidad de que haya tomado ambas vacunas siguiendo el formato establecido
  h1n1.vaccine.prob = final.probability['Vaccinated (H1N1)'] + final.probability['Vaccinated (Both)']
  seas.vaccine.prob = final.probability['Vaccinated (Seasonal)'] + final.probability['Vaccinated (Both)']
  
  # Añadimos estas probabilidades a los vectores en los que las almacenamos
  h1n1.probabilities = append(h1n1.probabilities, h1n1.vaccine.prob)
  seas.probabilities = append(seas.probabilities, seas.vaccine.prob)
}

# Añadimos al submission format las predicciones de probabilidad 
submission_format['h1n1_vaccine'] = h1n1.probabilities
submission_format['seasonal_vaccine'] = seas.probabilities

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_ovo_nofeatureselection.csv", row.names = FALSE)


################################################################################
################################## Boosting ####################################
################################################################################

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Creamos dos conjuntos de datos con cada vacuna
X_train['class'] = as.factor(y_train$h1n1_vaccine)

# Definimos una función que dados los siguientes parámetros:
#  - Un conjunto de datos de entrenamiento con una columna 'class' con la clase a predecir
#  - Un conjunto de datos de evaluación sobre el que realizar predicciones
#  - El número de clasificadores débiles que considerar en este metaalgoritmo
# Instancia un modelo de Boosting utilizando como clasificador JRip y realiza predicciones
# sobre el conjunto de datos de evaluación especificado.
boosting_JRip_model = function(train, test, n.weak.learners = 100, probability.calibration = "friedman") {
  
  # Declaramos las variables necesarias para ejecutar el boosting
  models.trained = 0
  
  # Definimos los pesos iniciales para nuestro conjunto de datos de entrenamiento
  weigths = rep(c(1/dim(train)[1]), dim(train)[1])
  
  # Creamos un vector para almacenar las importancias de los clasificadores débiles
  weak.learners.importance = c()
  
  # Creamos una lista en la que almacenar 
  weak.learners.test.predictions = list()
  
  # Iteramos mientras queden modelos que entrenar
  while(models.trained < n.weak.learners){
    
    print(paste("[Boosting Model] Entrenando clasificador débil número ", models.trained+1, "...", sep = ""))
    
    # En el caso de que sea el primer modelo utilizamos como datos el conjunto de entrenamientp
    if(models.trained == 0){
      sample.indexes = 1:nrow(train)
      iteration.data = train
    }else{
      # En el caso contrario, realizamos una muestra ponderada del conjunto de datos de
      # entrenamiento utilizando los pesos calculados
      sample.indexes = sample(seq_len(nrow(train)), nrow(train), prob=weigths)
      iteration.data = train[sample.indexes, ]
    }
    
    # Entrenamos el modelo sobre el conjunto de datos de entrenamiento 
    model.Ripper = JRip(class~., iteration.data)
    
    # Realizamos predicciones sobre el propio conjunto de datos
    model.Ripper.pred = predict(model.Ripper, newdata = iteration.data)
    
    # Calculamos el error del clasificador débil
    training.error = 0
    for(training.index in 1:length(model.Ripper.pred)){
      
      # Obtenemos el índice de esta observación al realizar el muestreo
      sample.idx = sample.indexes[training.index]
      
      # Calculamos el error para esta observación de la muestra y la añadimos a nuestros datos
      training.error = training.error + weigths[sample.idx]*as.integer(model.Ripper.pred[training.index] == train$class[sample.idx])
    }
    training.error = training.error / sum(weigths)
    
    # Calculamos el alpha para actualizar los pesos
    alpha = log((1 - training.error) / training.error)
    
    # Añadimos la importancia de este clasificador a la estructura de datos designada para esta labor
    weak.learners.importance = c(weak.learners.importance, alpha)

    # Actualizamos los pesos de las observaciones
    for(training.index in 1:length(model.Ripper.pred)){
      
      # Obtenemos el índice de esta observación al realizar el muestreo
      sample.idx = sample.indexes[training.index]
      
      # Calculamos el nuevo peso para esta observación del conjunto de datos
      weight.computed = weigths[sample.idx]*exp(alpha*as.integer(model.Ripper.pred[training.index] == train$class[sample.idx]))
      
      # Modificamos el peso en el vector original
      weigths[sample.idx] = weight.computed
    }
    
    print(paste("[Boosting Model] Realizando predicciones con el clasificador débil número ", models.trained+1, "...", sep = ""))
    
    # Realizamos con este clasificador débil predicciones sobre el conjunto de datos de evaluación
    model.Ripper.test.pred = predict(model.Ripper, newdata = test)
    
    # Añadimos estas predicciones a la lista de resultados
    weak.learners.test.predictions[[models.trained + 1]] = model.Ripper.test.pred
    
    print(paste("[Boosting Model] Entrenamiento y evaluación del clasificador débil número ", models.trained+1, " completado.", sep = ""))
  
    # Añadimos una unidad a los modelos ya entrenados
    models.trained = models.trained + 1
  }
  
  print("[Boosting Model] Agregando la salida de los modelos...")
  
  # Calculamos la salida del modelo para cada observación del conjunto de datos de evaluación
  final.output.boosting = c()
  for(test.observation.index in 1:nrow(test)) {
    
    # Para cada uno de los clasificadores, multiplicamos su salida por la importancia del clasificador
    # output.boosting = sapply(1:n.weak.learners, function(weak.model.index) 
    #  weak.learners.importance[weak.model.index] * ifelse(weak.learners.test.predictions[[weak.model.index]][test.observation.index] == 1, 1, -1))
    # Añadimos el output final de este modelo a un vector
    #final.output.boosting = c(final.output.boosting, sum(unlist(output.boosting)))

    # Obtenemos las predicciones de los diferentes clasificadores para esta observación
    predictions.for.observation = unlist(lapply(weak.learners.test.predictions, function(predictions) predictions[test.observation.index]))
    
    # Calculamos la probabilidad haciendo uso del paper de Friedman
    fx = 0.5 * log((sum(predictions.for.observation == 1)/length(predictions.for.observation))
                   / (sum(predictions.for.observation == 0)/length(predictions.for.observation)))
    boosting.prob = 1 / (1 + exp(-2*fx))
    
    # Añadimos la predicción de probabilidad para esta observación en nuestra estructura de datos
    final.output.boosting = c(final.output.boosting, boosting.prob)
  }
  
  print("[Boosting Model] Modelo de Boosting instanciado y evaluado con éxito.")

  final.output.boosting

}

# Lanzamos el Boosting para este target
boosting.results.h1n1 = boosting_JRip_model(X_train, X_test, n.weak.learners = 128)
boosting.results.h1n1

# Cambiamos la clase del conjunto de datos de entrenamiento por la relativa a la vacuna estacional
X_train['class'] = as.factor(y_train$seasonal_vaccine)

# Lanzamos el Boosting con los mismos parámetros que el anterior
boosting.results.seas = boosting_JRip_model(X_train, X_test, n.weak.learners = 128)
boosting.results.seas

# Rellenamos el submission_format
submission_format['h1n1_vaccine'] = boosting.results.h1n1
submission_format['seasonal_vaccine'] = boosting.results.seas
submission_format

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_boosting_nofeatureselection.csv", row.names = FALSE)


################################################################################
########################### Boosting Cross-Validation ##########################
################################################################################

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Creamos dos conjuntos de datos con cada vacuna
X_train['class'] = as.factor(y_train$h1n1_vaccine)

# Definimos la validación cruzada sobre nuestros datos
folds = caret::createMultiFolds(X_train$class, k = 5, times = 1)

# Definimos una estructura para almacenar las métricas de cada prueba
cv.result = c()

# Definimos los parámetros de n.weak.learner a probar
parameter.to.try = c(16, 32, 64, 128, 256)

# Iteramos sobre los distintos parámetros de n.weak.learners a probar
for(parameter in parameter.to.try){
  
  print(paste("   [Boosting CV] Lanzando validación cruzada con el siguiente valor de n.weak.learners: ", parameter, sep=""))
  
  # Definimos una estructura de datos en la que almacenar la métrica de evaluación para cada fold
  fold.evaluation = c()
  
  # Recorremos los folds de la validación cruzada
  for(rowIndex in (folds)){
    
    # Obtenemos los datos para esta iteración del conjunto de datos
    train.cv = X_train[rowIndex,]
    test.cv = X_train[-rowIndex,]
    
    # Extraemos del fold de evaluación la clase
    y.real = test.cv$class
    
    # Eliminamos del fold la clase
    test.cv$class = NULL
    
    # Lanzamos el modelo de boosting con los parámetros consecuentes
    cv.boosting.results = boosting_JRip_model(train.cv, test.cv, n.weak.learners = parameter)
    
    # Evaluamos las predicciones realizadas por nuestro modelo de boosting
    roc.obj = roc(y.real, cv.boosting.results)
    
    # Añadimos la métrica correspondiente en nuestra estructura 
    fold.evaluation = c(fold.evaluation, auc(roc.obj))
    
  }
  
  # Obtenemos la media para este parámetro y la añadimos a nuestros resultados
  cv.result = c(cv.result, sum(fold.evaluation)/length(fold.evaluation))

  print(paste("   [Boosting CV] Concluida validación cruzada con el parámetro: ", parameter, sep=""))
  print(paste("   [Boosting CV] Resultado de la validación cruzada - AUC ROC: ",  sum(fold.evaluation)/length(fold.evaluation), sep=""))
}

# Mostramos los resultados de la validación cruzada
cv.result
# [1] 0.7436371 0.7525712 0.7586733 0.7651374 0.7673431

# Creamos una gráfica para mostrar los resultados
cv.result.df = as.data.frame(cv.result)
cv.result.df = cv.result.df %>%
  mutate(n.weak.learner = parameter.to.try)
cv.result.df
ggplot(data = cv.result.df, aes(x=n.weak.learner, y=cv.result)) +
         geom_line(linetype = "dashed", color = "deepskyblue3") +
         geom_point(color = "deepskyblue4") +
         xlab("Número de Clasificadores Débiles") +
         ylab("AUC ROC Media en CV") + 
         ggtitle("Evolución del AUC ROC Respecto al Número de Clasificadores Débiles en Boosting") + 
         scale_y_continuous(limits = c(0.74, 0.77), breaks = c(0.74, 0.75, 0.76, 0.77))


################################################################################
################################### Boosting OVO ###############################
################################################################################


# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAknn.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAknn.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train = X_train %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))

# Obtenemos todas las parejas posibles de clases que existen
class_pairs = list()
class_pairs[[1]] = c(levels(X_train$class)[1], levels(X_train$class)[2])
class_pairs[[2]] = c(levels(X_train$class)[1], levels(X_train$class)[3])
class_pairs[[3]] = c(levels(X_train$class)[1], levels(X_train$class)[4])
class_pairs[[4]] = c(levels(X_train$class)[2], levels(X_train$class)[3])
class_pairs[[5]] = c(levels(X_train$class)[2], levels(X_train$class)[4])
class_pairs[[6]] = c(levels(X_train$class)[3], levels(X_train$class)[4])

# Creamos una función que dado lo siguiente:
#  - Un conjunto de datos de entrenamiento con una columna 'class' con las clases a predecir
#  - El conjunto de datos de evaluación a predecir
#  - Un vector de etiquetas con las que filtrar los datos para entrenar el modelo
# Devuelve un vector con la probabilidad de la primera clase a predecir
calculate_probability_ovo_boosting_model = function(train_data, test_data, filtered_classes, n.weak.learners = 256) {
  
  # Filtramos el conjunto de datos por las librerias especificadas
  X_train_filtered = train_data %>%
    filter(class %in% filtered_classes)
  
  # Redefinimos el factor para no generar problemas con Caret
  X_train_filtered = X_train_filtered %>%
    mutate(class = factor(class, levels = filtered_classes))
  
  # Transformamos la clase a 0 y 1
  X_train_filtered = X_train_filtered %>%
    mutate(class = ifelse(class == filtered_classes[1], 1, 0))
  
  # Lo volvemos a convertir en un factor
  X_train_filtered = X_train_filtered %>%
    mutate(class = as.factor(class))
  
  # print(X_train_filtered$class)
  
  # Creamos el modelo de boosting y evaluamos sobre el conjunto de evaluación
  boosting.result = boosting_JRip_model(X_train_filtered, test_data, n.weak.learners = n.weak.learners)
  boosting.result
}

# Lanzamos este método para cada una de las parejas de clases que hemos planteado
predicted.ovo = lapply(class_pairs, function(pair) calculate_probability_ovo_boosting_model(X_train, X_test, pair, n.weak.learners = 256))
predicted.ovo

# Definimos el número de clases
K = 4

# Declaramos un vector de etiquetas con los nombres de las probabilidades obtenidas
probabilities_names = unlist(lapply(class_pairs, function(pair) paste(pair[1], pair[2], sep=" Vs. ")))

# Creamos dos vectores para almacenar los resultados para cada una de las vacunas
h1n1.probabilities = c()
seas.probabilities = c()

# Iteramos sobre cada uno de los elementos de las predicciones devueltas por los modelos OVO
for(prediction.index in 1:length(predicted.ovo[[1]])){
  
  # Obtenemos un array con las predicciones para la instancia en concreto
  probabilities_vector = unlist(lapply(predicted.ovo, function(predictions) predictions[prediction.index]))
  probabilities_array = array(probabilities_vector, dim=c(1,(K*(K-1)/2)))
  colnames(probabilities_array) = probabilities_names
  
  # Obtenemos la matriz con las probabilidades
  Q = matrix(0,K,K)
  Q[lower.tri(Q)] = 1 - probabilities_array
  Qt = t(Q)
  Q[upper.tri(Q)] = 1 - Qt[upper.tri(Qt)]
  diag(Q) = rowSums(Q)
  Q = Q / (K-1)
  
  # Creamos un vector de probabilidades para cada clase inicial que sume 1
  p = rbeta(K,1,1)
  p = p/sum(p)
  
  # Actualizamos el vector de probabilidades hasta que el equilibrio ha sido alcanzado utilizando
  # como referencia: Probability Estimates for Multi-class Classification by Pairwise Coupling, Wu et al. (2003)
  for(i in 1:1000) p = Q%*%p
  
  # Formateamos un poco el vector de probabilidades resultante
  final.probability = as.vector(t(p))
  names(final.probability) = levels(X_train$class)
  
  # Calculamos la probabilidad de que haya tomado ambas vacunas siguiendo el formato establecido
  h1n1.vaccine.prob = final.probability['Vaccinated (H1N1)'] + final.probability['Vaccinated (Both)']
  seas.vaccine.prob = final.probability['Vaccinated (Seasonal)'] + final.probability['Vaccinated (Both)']
  
  # Añadimos estas probabilidades a los vectores en los que las almacenamos
  h1n1.probabilities = append(h1n1.probabilities, h1n1.vaccine.prob)
  seas.probabilities = append(seas.probabilities, seas.vaccine.prob)
}

# Añadimos al submission format las predicciones de probabilidad 
submission_format['h1n1_vaccine'] = h1n1.probabilities
submission_format['seasonal_vaccine'] = seas.probabilities
submission_format

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_ovo_boosting256_featureselection55_manualimputationwithindicators.csv", row.names = FALSE)


################################################################################
################################### Random Forest ##############################
################################################################################

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Creamos dos conjuntos de datos con cada vacuna
X_train['class'] = as.factor(y_train$h1n1_vaccine)

# Definimos la validación cruzada sobre nuestros datos
folds = caret::createMultiFolds(X_train$class, k = 7, times = 1)

# Definimos una estructura para almacenar las métricas de cada prueba
cv.result = c()

# Definimos los parámetros de n.weak.learner a probar
parameter.to.try = c(8, 16, 32, 64, 128, 256, 512)

# Iteramos sobre los distintos parámetros de n.weak.learners a probar
for(parameter in parameter.to.try){
  
  print(paste("   [RandomForest CV] Lanzando validación cruzada con el siguiente valor de n.weak.learners: ", parameter, sep=""))
  
  # Definimos una estructura de datos en la que almacenar la métrica de evaluación para cada fold
  fold.evaluation = c()
  
  # Recorremos los folds de la validación cruzada
  for(rowIndex in (folds)){
    
    # Obtenemos los datos para esta iteración del conjunto de datos
    train.cv = X_train[rowIndex,]
    test.cv = X_train[-rowIndex,]
    
    # Extraemos del fold de evaluación la clase
    y.real = test.cv$class
    
    # Eliminamos del fold la clase
    test.cv$class = NULL
    
    # Creamos un DataFrame para almacenar las predicciones
    prob.prediction.df = data.frame(X0 = rep(0.0, dim(test.cv)[1]), X1 = rep(0.0, dim(test.cv)[1]))
    
    # Iteramos sobre el número de clasificadores débiles a construir
    for (i in 1:parameter){
      
      # Obtenemos una muestra aleatoria del 5% de nuestros datos de entrenamiento
      iteration.data = train.cv %>%
        sample_frac(.05)
      
      # Obtenemos una muestra de las columnas y filtramos el conjunto de datos
      filtered.features = sample(length(colnames(iteration.data %>% select(-class))), 6) 
      iteration.data = iteration.data %>%
        select(c(filtered.features, class))

      # Lanzamos el entrenamiento
      model.Ripper = JRip(class~., iteration.data)
      
      # Realizamos predicciones sobre el propio conjunto de datos
      model.Ripper.pred = predict(model.Ripper, newdata = test.cv, type = "probability")
      
      # Añadimos la probabilidad predecida
      prob.prediction.df = prob.prediction.df + (model.Ripper.pred / parameter)
    }
    
    # Evaluamos las predicciones realizadas por nuestro modelo de boosting
    roc.obj = roc(y.real, prob.prediction.df$X1, levels = c(0,1), direction = "<")
    
    # Añadimos la métrica correspondiente en nuestra estructura 
    fold.evaluation = c(fold.evaluation, auc(roc.obj))
    
  }
  
  # Obtenemos la media para este parámetro y la añadimos a nuestros resultados
  cv.result = c(cv.result, sum(fold.evaluation)/length(fold.evaluation))
  
  print(paste("   [Boosting CV] Concluida validación cruzada con el parámetro: ", parameter, sep=""))
  print(paste("   [Boosting CV] Resultado de la validación cruzada - AUC ROC: ",  sum(fold.evaluation)/length(fold.evaluation), sep=""))
}

# Mostramos los resultados de la validación cruzada
cv.result
# [1] 0.7436371 0.7525712 0.7586733 0.7651374 0.7673431
# [1] 0.7200215 0.7501394 0.7704004 0.7901843 0.7964664 0.8056829 0.8061095

# Creamos una gráfica para mostrar los resultados
cv.result.df = as.data.frame(cv.result)
cv.result.df = cv.result.df %>%
  mutate(n.weak.learner = parameter.to.try)
cv.result.df
ggplot(data = cv.result.df, aes(x=n.weak.learner, y=cv.result)) +
  geom_line(linetype = "dashed", color = "deepskyblue3") +
  geom_point(color = "deepskyblue4") +
  xlab("Número de Clasificadores Débiles") +
  ylab("AUC ROC Media en CV") + 
  ggtitle("Evolución del AUC ROC Respecto al Número de Clasificadores Débiles en RandomForest") + 
  scale_y_continuous(limits = c(0.72, 0.81), breaks = c(0.72, 0.74, 0.76, 0.78, 0.80))



################################################################################
################################ Random Forest OVO #############################
################################################################################

# Cargamos los datos de entrenamiento y de test
X_train = load_data_processed("~/X_train_noNAmice.csv", remove_id = FALSE)
X_test = load_data_processed("~/X_test_noNAmice.csv", remove_id = FALSE)

# Cargamos el formato de submission y las etiquetas correspondientes a las clases del conjunto de entrenamiento
y_train = read.csv("training_set_labels.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))
submission_format = read.csv("submission_format.csv", header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train = X_train %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))

# Obtenemos todas las parejas posibles de clases que existen
class_pairs = list()
class_pairs[[1]] = c(levels(X_train$class)[1], levels(X_train$class)[2])
class_pairs[[2]] = c(levels(X_train$class)[1], levels(X_train$class)[3])
class_pairs[[3]] = c(levels(X_train$class)[1], levels(X_train$class)[4])
class_pairs[[4]] = c(levels(X_train$class)[2], levels(X_train$class)[3])
class_pairs[[5]] = c(levels(X_train$class)[2], levels(X_train$class)[4])
class_pairs[[6]] = c(levels(X_train$class)[3], levels(X_train$class)[4])

# Creamos una función que dado lo siguiente:
#  - Un conjunto de datos de entrenamiento con una columna 'class' con las clases a predecir
#  - El conjunto de datos de evaluación a predecir
#  - Un vector de etiquetas con las que filtrar los datos para entrenar el modelo
# Devuelve un vector con la probabilidad de la primera clase a predecir
calculate_probability_ovo_random_forest_model = function(train_data, test_data, filtered_classes, n.weak.learners = 256) {
  
  # Filtramos el conjunto de datos por las librerias especificadas
  X_train_filtered = train_data %>%
    filter(class %in% filtered_classes)
  
  # Redefinimos el factor para no generar problemas con Caret
  X_train_filtered = X_train_filtered %>%
    mutate(class = factor(class, levels = filtered_classes))
  
  # Transformamos la clase a 0 y 1
  X_train_filtered = X_train_filtered %>%
    mutate(class = ifelse(class == filtered_classes[1], 1, 0))
  
  # Lo volvemos a convertir en un factor
  X_train_filtered = X_train_filtered %>%
    mutate(class = as.factor(class))
  
  # Creamos un DataFrame para almacenar las predicciones
  prob.prediction.df = data.frame(X0 = rep(0.0, dim(test_data)[1]), X1 = rep(0.0, dim(test_data)[1]))
  
  # Iteramos sobre el número de clasificadores débiles a construir
  for (i in 1:n.weak.learners){
    
    # Obtenemos una muestra aleatoria del 5% de nuestros datos de entrenamiento
    iteration.data = X_train_filtered %>%
      sample_frac(.05)
    
    # Obtenemos una muestra de las columnas y filtramos el conjunto de datos
    filtered.features = sample(length(colnames(iteration.data %>% select(-class))), 6) 
    iteration.data = iteration.data %>%
      select(c(filtered.features, class))
    
    # Lanzamos el entrenamiento
    model.Ripper = JRip(class~., iteration.data)
    
    # Realizamos predicciones sobre el propio conjunto de datos
    model.Ripper.pred = predict(model.Ripper, newdata = test_data, type = "probability")
    
    # Añadimos la probabilidad predecida
    prob.prediction.df = prob.prediction.df + (model.Ripper.pred / n.weak.learners)
  }
  
  # Devolvemos la probabilidad de la clase
  prob.prediction.df$X1

}

# Lanzamos este método para cada una de las parejas de clases que hemos planteado
predicted.ovo = lapply(class_pairs, function(pair) calculate_probability_ovo_random_forest_model(X_train, X_test, pair, n.weak.learners = 256))
predicted.ovo


# Definimos el número de clases
K = 4

# Declaramos un vector de etiquetas con los nombres de las probabilidades obtenidas
probabilities_names = unlist(lapply(class_pairs, function(pair) paste(pair[1], pair[2], sep=" Vs. ")))

# Creamos dos vectores para almacenar los resultados para cada una de las vacunas
h1n1.probabilities = c()
seas.probabilities = c()

# Iteramos sobre cada uno de los elementos de las predicciones devueltas por los modelos OVO
for(prediction.index in 1:length(predicted.ovo[[1]])){
  
  # Obtenemos un array con las predicciones para la instancia en concreto
  probabilities_vector = unlist(lapply(predicted.ovo, function(predictions) predictions[prediction.index]))
  probabilities_array = array(probabilities_vector, dim=c(1,(K*(K-1)/2)))
  colnames(probabilities_array) = probabilities_names
  
  # Obtenemos la matriz con las probabilidades
  Q = matrix(0,K,K)
  Q[lower.tri(Q)] = 1 - probabilities_array
  Qt = t(Q)
  Q[upper.tri(Q)] = 1 - Qt[upper.tri(Qt)]
  diag(Q) = rowSums(Q)
  Q = Q / (K-1)
  
  # Creamos un vector de probabilidades para cada clase inicial que sume 1
  p = rbeta(K,1,1)
  p = p/sum(p)
  
  # Actualizamos el vector de probabilidades hasta que el equilibrio ha sido alcanzado utilizando
  # como referencia: Probability Estimates for Multi-class Classification by Pairwise Coupling, Wu et al. (2003)
  for(i in 1:1000) p = Q%*%p
  
  # Formateamos un poco el vector de probabilidades resultante
  final.probability = as.vector(t(p))
  names(final.probability) = levels(X_train$class)
  
  # Calculamos la probabilidad de que haya tomado ambas vacunas siguiendo el formato establecido
  h1n1.vaccine.prob = final.probability['Vaccinated (H1N1)'] + final.probability['Vaccinated (Both)']
  seas.vaccine.prob = final.probability['Vaccinated (Seasonal)'] + final.probability['Vaccinated (Both)']
  
  # Añadimos estas probabilidades a los vectores en los que las almacenamos
  h1n1.probabilities = append(h1n1.probabilities, h1n1.vaccine.prob)
  seas.probabilities = append(seas.probabilities, seas.vaccine.prob)
}

# Añadimos al submission format las predicciones de probabilidad 
submission_format['h1n1_vaccine'] = h1n1.probabilities
submission_format['seasonal_vaccine'] = seas.probabilities
submission_format

# Guardamos en disco los resultados
write.csv(submission_format,"~/Submission_Formats/submission_format_ovo_rf256_nofeatureselection.csv", row.names = FALSE)
