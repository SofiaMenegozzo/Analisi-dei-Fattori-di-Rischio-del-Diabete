# LIBRERIE UTILIZZATE 
library(tidyverse)
library(corrplot)
library(pROC)
library(caret)
library(ggplot2)
library(broom)
library(reshape2)
library(car)
library(pscl)
library(dplyr)
library(ResourceSelection)
library(PRROC)
library(smotefamily)

#IMPORTAZIONE DEL DATASET 
df<-read.csv("~/Downloads/clean_diabetes_v2.csv")
head(df)
str(df)
dim(df)
colSums(is.na(df))
summary(df) 

# Media e deviazione standard
df %>%
  summarise( bmi_mean = mean(bmi), bmi_sd = sd(bmi), age_mean = mean(age), age_sd = sd(age), phys_health_mean = mean(physical_health),  ment_health_mean = mean(mental_health) )

#  DISTRIBUZIONE TARGET
table(df$diabetes)
prop.table(table(df$diabetes)) * 100

ggplot(df, aes(x = factor(diabetes))) +
  geom_bar(fill = "steelblue") +
  labs(title = "distirbuzione variabile diabete",  x = "Diabete (0 = No, 1 = Sì)",  y = "Conteggio" ) +
  theme_minimal()

# CONFRONTO TRA DIABETICI E NON
group_stats <- df %>%
  group_by(diabetes) %>%
  summarise( count = n(), bmi_mean = mean(bmi), bmi_sd = sd(bmi),  age_mean = mean(age), general_health_mean = mean(general_health),
             physical_health_mean = mean(physical_health), mental_health_mean = mean(mental_health) )
print(group_stats)

#bmi vs diabete 
ggplot(df, aes(x = factor(diabetes), y = bmi, fill = factor(diabetes))) +
  geom_boxplot() +
  labs( title = "BMI e diabete", x = "Diabete", y = "BMI" ) +
  theme_minimal()
# Media BMI per gruppo
df %>% 
  group_by(diabetes) %>%
  summarise(media_bmi = mean(bmi))
# DENSITY PLOT BMI
ggplot(df, aes(x = bmi, fill = factor(diabetes))) +
  geom_density(alpha = 0.4) +
  labs(title = "BMI e diabete", x = "BMI", y = "Densità") +
  theme_minimal()

#età vs diabete 
ggplot(df, aes(x = factor(diabetes), y = age, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(  title = "Età e diabete",x = "Diabete", y = "Classe età" ) +
  theme_minimal()

#salute generale vs diabete 
ggplot(df, aes(x = factor(diabetes), y = general_health, fill = factor(diabetes))) +
  geom_boxplot() +
  labs( title = "Salute generale e diabete", x = "Diabete", y = "General Health" ) +
  theme_minimal()

# Funzione per grafici percentuali
plot_binary <- function(var_name) {
  df %>%
    group_by(.data[[var_name]], diabetes) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[var_name]]) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ggplot(aes(x = factor(.data[[var_name]]), y = percent, fill = factor(diabetes))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs( title = paste("Diabete vs", var_name), x = var_name, y = "Percentuale", fill = "Diabete") +
    theme_minimal()
}

# differenza percentuale fattori di rischio 
risk_factors <- c( "high_bp", "high_chol", "smoker", "stroke", "heart_disease", "physical_activity", "diff_walk")
for (var in risk_factors) { cat("\n====================================\n")
  cat("Variabile:", var, "\n")
  tab <- table(df[[var]], df$diabetes)
  print(tab)
  perc <- prop.table(tab, margin = 1)*100
  cat("\nPercentuali:\n")
  print(round(perc,2))
}

# RISCHIO DI DIABETE (%)
# pressione alta
df %>%
  group_by(high_bp) %>%
  summarise( diabetes_rate = mean(diabetes)*100, n = n() )

# Colesterolo alto
df %>%
  group_by(high_chol) %>%
  summarise( diabetes_rate = mean(diabetes)*100 )

# Fumo
df %>%
  group_by(smoker) %>%
  summarise( diabetes_rate = mean(diabetes)*100 )

# Attività fisica
df %>%
  group_by(physical_activity) %>%
  summarise( diabetes_rate = mean(diabetes)*100 )

# grafici fattori rischio
plot_binary("high_bp")
plot_binary("high_chol")
plot_binary("smoker")
plot_binary("stroke")
plot_binary("heart_disease")
plot_binary("physical_activity")
plot_binary("diff_walk")

# GRAFICO COMPARATIVO DEI FATTORI DI RISCHIO
risk_vars <- c("high_bp", "high_chol", "smoker",
               "stroke", "heart_disease",
               "physical_activity", "diff_walk")
risk_summary <- map_df(risk_vars, function(v){ df %>%
    group_by(.data[[v]]) %>%
    summarise(  diabetes_rate = mean(diabetes) * 100 ) %>%
    mutate( variable = v, category = .data[[v]] ) })
risk_summary$category <- ifelse(risk_summary$category == 1,
                                "Si", "No")
ggplot(risk_summary,
       aes(x = variable, y = diabetes_rate, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs( title = "Percentuale di soggetti diabetici per fattore di rischio",
        x = "Fattori di rischio", y = "% soggetti diabetici", fill = "Presenza del fattore" ) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal(base_size = 13) +
  theme( axis.text.x = element_text(angle = 30, hjust = 1))


# GRAFICO COMPARATIVO DEI FATTORI DI RISCHIO SOGGETTI NON DIABETICI
risk_vars <- c("high_bp", "high_chol", "smoker",
               "stroke", "heart_disease",
               "physical_activity", "diff_walk")
risk_summary <- map_df(risk_vars, function(v){ df %>%
    group_by(.data[[v]]) %>%
    summarise(  diabetes_rate = mean(diabetes) * 100 ) %>%
    mutate( variable = v, category = .data[[v]] ) })
risk_summary$category <- ifelse(risk_summary$category == 0,
                                "Si", "No")
ggplot(risk_summary,
       aes(x = variable, y = diabetes_rate, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs( title = "Percentuale di soggetti NON diabetici per fattore di rischio",
        x = "Fattori di rischio", y = "% soggetti NON diabetici", fill = "Presenza del fattore" ) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal(base_size = 13) +
  theme( axis.text.x = element_text(angle = 30, hjust = 1))

# MATRICE CORRELAZIONE
cor_matrix <- cor(df); cor_matrix
corrplot( cor_matrix, method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.5 )
# Correlazioni con diabete
cor(df)
cor_diabetes <- cor_matrix[, "diabetes"]
sort(cor_diabetes, decreasing = TRUE)
corrplot( cor_matrix, method = "color", type = "upper")

# CORRELAZIONE con variabili piu importanti
selected_vars <- df %>%
  select(diabetes, bmi, age, high_bp, high_chol, general_health, physical_health, diff_walk, income, education)
cor_matrix_small <- cor(selected_vars)
corrplot(cor_matrix_small, method = "color", type = "upper", addCoef.col = "black", tl.cex = 0.8)

# ANALISI STATISTICA
# Media variabili per gruppo
df %>%
  group_by(diabetes) %>%
  summarise( bmi_mean = mean(bmi), age_mean = mean(age), gen_health_mean = mean(general_health),
             mental_health_mean = mean(mental_health), physical_health_mean = mean(physical_health) )

# TEST STATISTICI
# T-test BMI
t.test(bmi ~ diabetes, data = df)
# T-test età
t.test(age ~ diabetes, data = df)
# Chi-quadro pressione alta
table_bp <- table(df$high_bp, df$diabetes)
chisq.test(table_bp)
# Chi-quadro colesterolo alto
table_chol <- table(df$high_chol, df$diabetes)
chisq.test(table_chol)

# REGRESSIONE LOGISTICA
model <- glm( diabetes ~ high_bp + high_chol + bmi + smoker + stroke + heart_disease + physical_activity + fruits + veggies +
                heavy_alcohol + general_health + mental_health + physical_health + diff_walk + sex + age + education + income,
              data = df, family = "binomial" )
summary(model)
vif(model)  #multicollinearità

# PREDIZIONI 
pseudo_r2 <- pR2(model)["McFadden"]
pseudo_r2
model_aic <- AIC(model)
model_aic

# Probabilità predette e ROC CURVE
prob <- predict(model, type = "response")
roc_obj <- roc(df$diabetes, prob)
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve - Modello Logistico")
abline(a = 0, b = 1, lty = 2, col = "red")

# AUC
auc_value <- auc(roc_obj)
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)))

#. PRECISION-RECALL CURVE
# Probabilità predette
prob <- predict(model, type = "response")
# Precision Recall curve
pr <- pr.curve(scores.class0 = prob[df$diabetes == 1],
               scores.class1 = prob[df$diabetes == 0], curve = TRUE)
plot(pr, main = "Precision-Recall Curve", col = "blue", lwd = 2)

pred_class <- ifelse(prob > 0.5, 1, 0)
mean(pred_class == df$diabetes)
pred_factor <- factor(pred_class)
real_factor <- factor(df$diabetes)

# Matrice di confusione
conf_matrix <- confusionMatrix( pred_factor, real_factor, positive = "1")
conf_matrix

# metriche modello 
accuracy  <- conf_matrix$overall["Accuracy"] ; accuracy
precision <- conf_matrix$byClass["Pos Pred Value"]; precision
recall    <- conf_matrix$byClass["Sensitivity"]; recall
f1_score  <- 2 * ((precision * recall) / (precision + recall)); f1_score

# prob > 0,3 è migliore di 0,5, con quest'ultimo valore si sta ottimizzando implicitamente l’accuracy invece della capacità di trovare diabetici
# con prob > 0,5 Accuracy = 83% sembra buona ma Recall = 18% ( in un problema medico, perdere l’82% dei diabetici è un limite importante)
pred_class3 <- ifelse(prob > 0.3, 1, 0)
mean(pred_class3 == df$diabetes)
pred_factor3 <- factor(pred_class3)
real_factor3 <- factor(df$diabetes)
# Matrice di confusione
conf_matrix3 <- confusionMatrix(
  pred_factor3,
  real_factor3,
  positive = "1"
)
conf_matrix3
accuracy3  <- conf_matrix3$overall["Accuracy"]; accuracy3
precision3 <- conf_matrix3$byClass["Pos Pred Value"]; precision3
recall3    <- conf_matrix3$byClass["Sensitivity"]; recall3
f1_score3  <- 2 * ((precision3 * recall3) / (precision3 + recall3)); f1_score3

# tabella riassuntiva 
model_eval <- data.frame( Metric = c("PseudoR2", "AIC", "Accuracy", "Precision", "Recall", "F1 Score", "AUC"),
                          Value = c( round(as.numeric(pseudo_r2), 3), round(model_aic, 2),
                                     round(accuracy, 3), round(precision, 3), round(recall, 3),
                                     round(f1_score, 3), round(as.numeric(auc_value), 3) ) )
model_eval
# visualizzazione matrice di confusione in prob > 0,3
cm <- confusionMatrix(factor(ifelse(prob > 0.3,1,0)), factor(df$diabetes), positive = "1")
cm_table <- as.data.frame(cm$table)
ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "grey", high = "blue") +
  labs(title = "Confusion Matrix (threshold = 0.3)") +
  theme_minimal()

# TRAIN / TEST SPLIT 70/30
set.seed(123)
train_index <- createDataPartition(df$diabetes,
                                   p = 0.7,
                                   list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]
# modello sui dati train
model_split <- glm(
  diabetes ~ high_bp + high_chol + bmi + smoker +
    stroke + heart_disease + physical_activity +
    fruits + veggies + heavy_alcohol +
    general_health + mental_health +
    physical_health + diff_walk +
    sex + age + education + income,
  data = train_data,
  family = "binomial"
)
summary(model_split)
# predizioni sul test set
prob_test <- predict(model_split,
                     newdata = test_data,
                     type = "response")
pred_test <- ifelse(prob_test > 0.3, 1, 0)
#matrice di confusione
conf_test <- confusionMatrix(
  factor(pred_test),
  factor(test_data$diabetes),
  positive = "1"
)
conf_test
# ROC curve
roc_test <- roc(test_data$diabetes, prob_test)
plot(roc_test,
     col = "blue",
     lwd = 3,
     main = "ROC Curve - Test Set")
abline(a = 0, b = 1,
       lty = 2,
       col = "red")
auc(roc_test)

# CROSS VALIDATION
set.seed(123)
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
# target come fattore
df_cv <- df
df_cv$diabetes <- factor(
  ifelse(df_cv$diabetes == 1, "Yes", "No")
)
cv_model <- train(
  diabetes ~ high_bp + high_chol + bmi + smoker +
    stroke + heart_disease + physical_activity +
    fruits + veggies + heavy_alcohol +
    general_health + mental_health +
    physical_health + diff_walk +
    sex + age + education + income,
  data = df_cv,
  method = "glm",
  family = "binomial",
  
  metric = "ROC",
  trControl = ctrl
)
cv_model
#ASSENZA DI BILANCIAMENTO CLASSI
# OVERSAMPLING
set.seed(123)
over_data <- upSample(
  x = df %>% select(-diabetes),
  y = factor(df$diabetes),
  yname = "diabetes"
)
table(over_data$diabetes)
model_over <- glm(
  diabetes ~ .,
  data = over_data,
  family = "binomial"
)
summary(model_over)
#UNDERSAMPLING
set.seed(123)
under_data <- downSample(
  x = df %>% select(-diabetes),
  y = factor(df$diabetes),
  yname = "diabetes"
)
table(under_data$diabetes)
model_under <- glm(
  diabetes ~ .,
  data = under_data,
  family = "binomial"
)
summary(model_under)
# SMOTE
df_smote <- df
df_smote$diabetes <- as.factor(df_smote$diabetes)
# separo features e target
X <- subset(df_smote, select = -diabetes)
y <- df_smote$diabetes
# SMOTE
smote_result <- SMOTE(
  X = X,
  target = y,
  K = 5,
  dup_size = 2
)
# ricostruisco dataset
smote_data <- data.frame(
  smote_result$data
)
# rinomino target
colnames(smote_data)[ncol(smote_data)] <- "diabetes"
smote_data$diabetes <- as.factor(smote_data$diabetes)
table(smote_data$diabetes)
# logistic regression
model_smote <- glm(
  diabetes ~ .,
  data = smote_data,
  family = "binomial"
)
summary(model_smote)

# CLASS WEIGHTS
# calcolo pesi
weights <- ifelse(
  df$diabetes == 1,
  nrow(df) / (2 * sum(df$diabetes == 1)),
  nrow(df) / (2 * sum(df$diabetes == 0))
)
model_weighted <- glm(
  diabetes ~ high_bp + high_chol + bmi + smoker +
    stroke + heart_disease + physical_activity +
    fruits + veggies + heavy_alcohol +
    general_health + mental_health +
    physical_health + diff_walk +
    sex + age + education + income,
  
  data = df,
  family = "binomial",
  weights = weights
)
summary(model_weighted)
# CONFRONTO MODELLI
results <- data.frame(
  Model = c("Base", "Oversampling",
            "Undersampling",
            "SMOTE",
            "Weighted"),
  AUC = c(
    auc(roc(df$diabetes,
            predict(model, type="response"))),
    
    auc(roc(over_data$diabetes,
            predict(model_over, type="response"))),
    
    auc(roc(under_data$diabetes,
            predict(model_under, type="response"))),
    
    auc(roc(smote_data$diabetes,
            predict(model_smote, type="response"))),
    
    auc(roc(df$diabetes,
            predict(model_weighted,
                    type="response")))
  )
)
results


# Odds Ratio
odds_ratio <- exp(coef(model)); odds_ratio
or_df <- data.frame( Variable = names(odds_ratio), Odds_Ratio = odds_ratio)
or_df <- or_df %>%
  arrange(desc(Odds_Ratio))
or_df
exp(cbind(OR = coef(model), confint(model)))

# IMPORTANZA VARIABILI
coeff <- data.frame( Variable = names(coef(model)), Coefficient = coef(model) )
coeff <- coeff[order(abs(coeff$Coefficient), decreasing = TRUE), ] ; coeff 
# IMPORTANZA VARIABILI grafico 
coeff_df <- data.frame( Variable = names(coef(model)), Coef = coef(model) ) %>%
  filter(Variable != "(Intercept)") %>%
  arrange(abs(Coef))
ggplot(coeff_df, aes(x = reorder(Variable, abs(Coef)), y = Coef)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importanza variabili (coeff. logistica)", x = "Variabili", y = "Coefficiente") +
  theme_minimal()

# FOREST PLOT ODDS RATIO
# Odds ratio + confidence intervals
or_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
# Rimuovi intercetta
or_results <- or_results %>%
  filter(term != "(Intercept)")
# Forest plot
ggplot(or_results, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_log10() +
  labs( title = "Forest Plot delle Odds Ratio", x = "Variabili",  y = "Odds Ratio (scala log)" ) +
  theme_minimal(base_size = 13)

#CALIBRATION PLOT per verificare se le probabilità predette sono realistiche
# Hosmer-Lemeshow test
hoslem.test(df$diabetes, fitted(model))
calib <- data.frame( observed = df$diabetes, predicted = fitted(model))
calib$bin <- cut(calib$predicted,  breaks = 10)
calibration <- calib %>%
  group_by(bin) %>%
  summarise( mean_pred = mean(predicted), mean_obs = mean(observed) )
ggplot(calibration, aes(x = mean_pred, y = mean_obs)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(  title = "Calibration Plot", x = "Probabilità predetta",  y = "Probabilità osservata" ) +
  theme_minimal(base_size = 13)

# Odds Ratio + IC95%
or_table <- tidy(model,
                 exponentiate = TRUE,
                 conf.int = TRUE)
# Rinomina colonne
or_table <- or_table %>%
  select(term,
         estimate,
         conf.low,
         conf.high,
         p.value)
colnames(or_table) <- c(
  "Variabile",
  "Odds_Ratio",
  "IC95_low",
  "IC95_high",
  "p_value"
)
# Ordina per importanza effetto
or_table <- or_table %>%
  arrange(desc(abs(log(Odds_Ratio))))
print(or_table)

# Standardizzazione variabili continue
df_std <- df %>%
  mutate(
    bmi = scale(bmi),
    age = scale(age),
    general_health = scale(general_health),
    physical_health = scale(physical_health),
    mental_health = scale(mental_health),
    income = scale(income),
    education = scale(education)
  )
model_std <- glm(
  diabetes ~ high_bp + high_chol + bmi + smoker +
    stroke + heart_disease + physical_activity +
    fruits + veggies + heavy_alcohol +
    general_health + mental_health +
    physical_health + diff_walk +
    sex + age + education + income,
  data = df_std,
  family = "binomial"
)
summary(model_std)


# analisi heavy alchol 
df %>%
  group_by(heavy_alcohol) %>%
  summarise(
    age_mean = mean(age),
    bmi_mean = mean(bmi),
    diabetes_rate = mean(diabetes)*100
  )

model_no_alcohol <- glm(
  diabetes ~ high_bp + high_chol + bmi + smoker +
    stroke + heart_disease + physical_activity +
    fruits + veggies +
    general_health + mental_health +
    physical_health + diff_walk +
    sex + age + education + income,
  data = df,
  family = "binomial"
)
summary(model_no_alcohol)


model_interaction <- glm(
  diabetes ~ heavy_alcohol * age +
    high_bp + high_chol + bmi +
    sex + income,
  data = df,
  family = "binomial"
)
summary(model_interaction)