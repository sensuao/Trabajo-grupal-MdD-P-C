### Agustín Mora Acosta
## Máster en Ciencia de Datos e Ingeniería de Computadores
## Mineria de Datos: Clasificación y Preprocesamiento
## Universidad de Granada

# Importamos las librerías necesarias para realizar el preprocesamiento
require(tidyverse)
require(ggplot2)
require(naniar)
require(mice)
require(missForest)
library(NoiseFiltersR)
library(themis)
library(ggcorrplot)
library(FSelectorRcpp)
library(DescTools)
require(VIM)
require(caret)
require(RANN)

# Establecemos una semilla aleatoria
set.seed(1)

# Definimos un método para cargar los datos con los tipos adecuados
load_data = function(data_path, remove_id = TRUE) {
  
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
      h1n1_concern = factor(h1n1_concern, levels = c(0, 1, 2, 3), labels = c("Not at all concerned", "Not very concerned", "Somewhat concerned", "Very concerned"), ordered = TRUE),
      h1n1_knowledge = factor(h1n1_knowledge, levels = c(0, 1, 2), labels = c("No knowledge", "A little knowledge", "A lot of knowledge"), ordered = TRUE),
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
      opinion_h1n1_vacc_effective = factor(opinion_h1n1_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_h1n1_risk = factor(opinion_h1n1_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      opinion_h1n1_sick_from_vacc = factor(opinion_h1n1_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
      opinion_seas_vacc_effective = factor(opinion_seas_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
      opinion_seas_risk = factor(opinion_seas_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
      opinion_seas_sick_from_vacc = factor(opinion_seas_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
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

# Definimos otro método para cargar los datos que ya han sido procesados (Y por lo tanto son diferentes)
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


# 1. Cargamos los datos del conjunto de datos de entrenamiento y de evaluación
X_train = load_data('training_set_features.csv')
X_test= load_data('test_set_features.csv')


# 2. Preprocesamiento de los Datos

# 2.1. Visualización, Limpieza, Transformación de los Datos

# Mostramos las primeras filas de los datos de entrenamiento
head(X_train)

# Mostramos la estructura de los datos
str(X_train)

# Mostramos un resumen de los datos
summary(X_train)

# 2.2. Valores Perdidos

# 2.2.1. MICE

# Calculamos el ratio de valores nulos para todas las variables del conjunto de datos
ratio.nulos = colSums(is.na(X_train_tf))/nrow(X_train_tf)
ratio.nulos

# Obtenemos un primer conjunto de datos con los valores nulos que no nos sirve
X_train.noNA = X_train_tf %>%
  na.omit()
X_train.noNA

# Mostramos visualizaciones para conocer los patrones de valores nulos
gg_miss_upset(X_train_tf)
mice::md.pattern(X_train_tf)

# Definimos el imputador para nuestro conjunto de datos
imputer = mice(X_train_tf, maxiter = 5, m = 1,
               meth = c(
                 "polr", "polr", "logreg.boot", "logreg.boot", "logreg.boot", "logreg.boot",
                 "logreg.boot", "logreg.boot", "logreg.boot", "logreg.boot", "logreg.boot", "logreg.boot",
                 "logreg.boot", "logreg.boot", "logreg.boot", "polr", "polr", "polr",
                 "polr", "polr", "polr", "polr", "polr", "polyreg",
                 "polyreg", "polr", "polyreg", "polyreg", "polyreg", "polyreg",
                 "polyreg", "polr", "polr", "polyreg", "polyreg"
               ), nnet.MaxNWts = 5000, seed = 1
               )


# Mostramos un resumen del imputador
summary(imputer)

# Obtenemos los datos imputados
X_train_noNA = complete(imputer)

# Mostramos el número de valores vacios en este nuevo conjunto de datos
sum(is.na(X_train_noNA))

# Imputamos los valores sobre el conjunto de datos de evaluación con el imputador entrenado con el conjunto de entrenamiento
X_test_noNA = mice.reuse(imputer, X_test_tf, maxit = 5)

# Guardamos en memoria estos dos conjuntos de datos
write.csv(X_train_noNA,"~/X_train_noNAmice.csv", row.names = FALSE)
write.csv(X_test_noNA[[1]],"~/X_test_noNAmice.csv", row.names = FALSE)



# 2.2.2. MissForest

# Unimos los datos de entrenamiento y test
X = rbind(X_train, X_test)

# Definimos el imputador e imputamos los datos que faltan
imputer = missForest(X, ntree = 5)

# Mostramos los valores imputados
imputer$ximp

# Separamos de nuevo los conjuntos de datos
X_train_noNA = imputer$ximp[1:26707,]
X_test_noNA = imputer$ximp[26708:nrow(imputer$ximp),]

# Guardamos en memoria estos dos conjuntos de datos
write.csv(X_train_noNA,"~/X_train_noNAmissforest.csv", row.names = FALSE)
write.csv(X_test_noNA,"~/X_test_noNAmissforest.csv", row.names = FALSE)



# 2.2.3. Imputación Manual o Heurística

# Imputamos por heuristica
X_train$education[is.na(X_train$education)] = "< 12 Years"
X_train$income_poverty[is.na(X_train$income_poverty)] = "Below Poverty"
X_train$marital_status[is.na(X_train$marital_status)] = "Not Married"
X_train$rent_or_own[is.na(X_train$rent_or_own)] = "Rent"
X_train$employment_status[is.na(X_train$employment_status)] = "Unemployed"
X_train$health_insurance[is.na(X_train$health_insurance)] = 0

# Eliminamos el ID
X_train = X_train %>%
  select(-respondent_id)
    
# Convertimos las variables a factor
X_train = X_train %>%
  mutate(
    h1n1_concern = factor(h1n1_concern, levels = c(0, 1, 2, 3), labels = c("Not at all concerned", "Not very concerned", "Somewhat concerned", "Very concerned"), ordered = TRUE),
    h1n1_knowledge = factor(h1n1_knowledge, levels = c(0, 1, 2), labels = c("No knowledge", "A little knowledge", "A lot of knowledge"), ordered = TRUE),
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
    opinion_h1n1_vacc_effective = factor(opinion_h1n1_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
    opinion_h1n1_risk = factor(opinion_h1n1_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
    opinion_h1n1_sick_from_vacc = factor(opinion_h1n1_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
    opinion_seas_vacc_effective = factor(opinion_seas_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
    opinion_seas_risk = factor(opinion_seas_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
    opinion_seas_sick_from_vacc = factor(opinion_seas_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
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

# Sustituimos los NAs restantes por la moda
for (i in 1:dim(X_train)[2]) {
  X_train[i][is.na(X_train[i])] = sapply(X_train[i], function(x) Mode(x, na.rm=TRUE))
}

# Sustituimos los valores por heurística al igual que hicimos antes
X_test$education[is.na(X_test$education)] = "< 12 Years"
X_test$income_poverty[is.na(X_test$income_poverty)] = "Below Poverty"
X_test$marital_status[is.na(X_test$marital_status)] = "Not Married"
X_test$rent_or_own[is.na(X_test$rent_or_own)] = "Rent"
X_test$employment_status[is.na(X_test$employment_status)] = "Unemployed"
X_test$health_insurance[is.na(X_test$health_insurance)] = 0

# Convertimos las variables a factor
X_test = X_test %>%
  mutate(
    h1n1_concern = factor(h1n1_concern, levels = c(0, 1, 2, 3), labels = c("Not at all concerned", "Not very concerned", "Somewhat concerned", "Very concerned"), ordered = TRUE),
    h1n1_knowledge = factor(h1n1_knowledge, levels = c(0, 1, 2), labels = c("No knowledge", "A little knowledge", "A lot of knowledge"), ordered = TRUE),
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
    opinion_h1n1_vacc_effective = factor(opinion_h1n1_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
    opinion_h1n1_risk = factor(opinion_h1n1_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
    opinion_h1n1_sick_from_vacc = factor(opinion_h1n1_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
    opinion_seas_vacc_effective = factor(opinion_seas_vacc_effective, levels = c(1, 2, 3, 4, 5), labels = c("Not at all effective", "Not very effective", "Don't know", "Somewhat effective", "Very effective"), ordered = TRUE),
    opinion_seas_risk = factor(opinion_seas_risk, levels = c(1, 2, 3, 4, 5), labels = c("Very Low", "Somewhat low", "Don't know", "Somewhat high", "Very high"), ordered = TRUE),
    opinion_seas_sick_from_vacc = factor(opinion_seas_sick_from_vacc, levels = c(1, 2, 3, 4, 5), labels = c("Not at all worried", "Not very worried", "Don't know", "Somewhat worried", "Very worried"), ordered = TRUE),
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

# Sustituimos los NAs restantes por la moda
for (i in 1:dim(X_test)[2]) {
  X_test[i][is.na(X_test[i])] = sapply(X_test[i], function(x) Mode(x))
}

# Guardamos en memoria estos dos conjuntos de datos
write.csv(X_train,"~/X_train_noNAmanual.csv", row.names = FALSE)
write.csv(X_test,"~/X_test_noNAmanual.csv", row.names = FALSE)

############ Etiquetas sobre la imputación ##################

# Cargamos los datos que ya han sido imputados
X_train_noNA = load_data_processed('~/X_train_noNAmanual.csv', remove_id = FALSE)
X_test_noNA = load_data_processed('~/X_test_noNAmanual.csv', remove_id = TRUE)

# Añadimos nuevas columnas en train que indican si el valor de esa columna ha sido imputado o no
for(colname in colnames(X_train)){
  X_train_noNA[[paste(colname, "_imp", sep = "")]] = is.na(X_train[[colname]])
}

# Añadimos nuevas columnas en test que indican si el valor de esa columna ha sido imputado o no
for(colname in colnames(X_test)){
  X_test_noNA[[paste(colname, "_imp", sep = "")]] = is.na(X_test[[colname]])
}

# Guardamos en disco estos nuevos datos
write.csv(X_train_noNA,"~/X_train_noNAmanual_NAindicators.csv", row.names = FALSE)
write.csv(X_test_noNA,"~/X_test_noNAmanual_NAindicators.csv", row.names = FALSE)




# 2.2.4. KNN Imputer

sapply(X_train, as.numeric)

# Imputamos los datos del conjunto de datos de entrenamiento
X_train_noNA = kNN(X_train, k = 7)
X_train_noNA
# Imputamos los datos del conjunto de datos de evaluación
X_test_noNA = kNN(X_test, k = 7)

knnImputer = preProcess(X_train %>% select(-respondent_id), method = c("knnImpute"), k = 7)
X_train_noNA = predict(knnImputer, X_train %>% select(-respondent_id))
X_test_noNA = predict(knnImputer, X_test %>% select(-respondent_id))
X_train_noNA

# Guardamos en disco ambos conjuntos de datos
write.csv(X_train_noNA,"~/X_train_noNAknn.csv", row.names = FALSE)
write.csv(X_test_noNA,"~/X_test_noNAknn.csv", row.names = FALSE)

# 2.3. Eliminación de Ruido

# Cargamos los datos que ya han sido imputados
X_train_noNA = load_data_processed('~/X_train_noNAmanual_NAindicators.csv', remove_id = FALSE)
X_test_noNA = load_data_processed('~/X_test_noNAmanual_NAindicators.csv', remove_id = FALSE)

# Cargamos las clases de los datos de entrenamiento
y_train = read.csv('training_set_labels.csv', header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train_noNA['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train_noNA = X_train_noNA %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))

# Lanzamos IPF para eliminar las instancias con ruido
noise.filter = IPF(class~., X_train_noNA)

# Obtenemos el conjunto de datos sin ruido
X_train_noNA_noise = noise.filter$cleanData

# Guardamos la clase
new_y_train = X_train_noNA_noise$class

# Eliminamos la clase
X_train_noNA_noise$class = NULL

# Guardamos en disco estos datos
write.csv(X_train_noNA_noise,"~/X_train_noNAmanual_NAindicators_noise.csv", row.names = FALSE)



# 2.4. Selección de Características

# Cargamos los datos que ya han sido imputados
X_train_noNA = load_data_processed('~/X_train_noNAmanual_NAindicators.csv', remove_id = FALSE)
X_test_noNA = load_data_processed('~/X_test_noNAmanual_NAindicators.csv', remove_id = FALSE)

# Cargamos las clases de los datos de entrenamiento
y_train = read.csv('training_set_labels.csv', header=TRUE, sep=',', na.strings = c('?','', 'NA'))

# Modificamos las clases para crear una única variable multiclase y la añadimos a nuestros datos de entrenamiento
X_train_noNA['class'] = strtoi(paste(y_train$h1n1_vaccine, y_train$seasonal_vaccine, sep=""), base = 2)

# Convertimos esta clase en un factor
X_train_noNA = X_train_noNA %>%
  mutate(class = factor(class, levels = c(0, 1, 2, 3), labels = c("Unvaccinated", "Vaccinated (Seasonal)", "Vaccinated (H1N1)", "Vaccinated (Both)")))

# Mostramos la matriz de correlación entre nuestras variables
model.matrix(~0+., data=X_train_noNA) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F,method = "square", type="upper", lab=FALSE, lab_size=1)

# Calculamos la information gain para todas las variables de nuestros datos
pesosG <- FSelectorRcpp::information_gain(class ~ ., X_train_noNA)

# Ordenamos y nos quedamos con las 20 mejores que visualizamos
visual <- head(pesosG %>% arrange(desc(importance)), 70)
ggplot(data=visual, aes(x=reorder(attributes, -importance), y=importance)) +
  geom_bar(fill="cornflowerblue", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Filtramos el Data Frame quedándonos con las características que mejores resultados han aportado
best.features <- FSelectorRcpp::cut_attrs(pesosG, k=55)
best.features

# Filtramos el conjunto de datos de entrenamiento y el de test
X_train_selectedfeatures = X_train_noNA %>% select(-class) %>% select(best.features)

# Realizamos esto mismo sobre el conjunto de datos de evaluación
X_test_selectedfeatures = X_test_noNA %>% select(best.features)

# Guardamos en disco estos conjuntos de datos
write.csv(X_train_selectedfeatures,"~/X_train_noNAmanual_NAindicators_featureselection55.csv", row.names = FALSE)
write.csv(X_test_selectedfeatures,"~/X_test_noNAmanual_NAindicators_featureselection55.csv", row.names = FALSE)
