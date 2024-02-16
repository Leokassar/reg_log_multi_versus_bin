# Imports

library(dplyr)
library(tidyr)
library("nnet")
library("stargazer")
library("jtools")
library("fastDummies")
library("ROCR")
library("plotly")

# Leitura da base

df  <- read.csv("credit_score.csv")

summary(df)

# Exclusão de variáveis desenecessárias/redundantes

# Name -> Dropar - redundante com Customer_ID
# SSN -> Dropar - redundate com Occupation
# Monthly_Inhand_Salary -> Dropar -  redundância com Annual_income e existência de NAs


df = subset(df, select = -c(Name, SSN,  Monthly_Inhand_Salary, Credit_History_Age, Type_of_Loan))

summary(df)

## Análise Exploratória e Preparação dos Dados


# Criação da variável resposta do modelo logístico binário
df$target_binaria <- ifelse(df$Credit_Score == "Poor", 1, 0)
table(df$target_binaria)

# Tratamento da variável resposta "Credit_Score"
df$Credit_Score <- factor(df$Credit_Score)
table(df$Credit_Score)

# Tratamento variável "Age"
df$Age <- gsub("_", "", df$Age)
df$Age <- as.numeric(df$Age)
hist(df$Age)
df$Age <- ifelse((df$Age >= 110) | (df$Age < 18), median(df$Age), df$Age)

# Tratamento variável "Occupation"
df$Occupation <- factor(df$Occupation)
table(df$Occupation)

# Tratamento variável "Annual_Income"
df$Annual_Income <- gsub("_", "", df$Annual_Income)
df$Annual_Income <- as.numeric(df$Annual_Income)
summary(df$Annual_Income)
quantile(df$Annual_Income , probs = c(0.99), na.rm=TRUE)
quantile(df$Annual_Income , probs = c(0.95), na.rm=TRUE)

# Tratamento variável Num_Bank_Accounts
hist(df$Num_Bank_Accounts)
df$Num_Bank_Accounts <- ifelse((df$Num_Bank_Accounts >= 20) | (df$Num_Bank_Accounts < 0),
                                      median(df$Num_Bank_Accounts), df$Num_Bank_Accounts)

# Tratamento variável Num_Credit_Card
summary(df$Num_Credit_Card)
quantile(df$Num_Credit_Card , probs = c(0.99), na.rm=TRUE)
quantile(df$Num_Credit_Card , probs = c(0.95), na.rm=TRUE)
df$Num_Credit_Card <- ifelse((df$Num_Credit_Card > 10), 10, df$Num_Credit_Card)

# Tratamento Interest_Rate
summary(df$Interest_Rate)
quantile(df$Interest_Rate , probs = c(0.99), na.rm=TRUE)
quantile(df$Interest_Rate , probs = c(0.95), na.rm=TRUE)
quantile(df$Interest_Rate , probs = c(0.975), na.rm=TRUE)
df$Interest_Rate <- ifelse((df$Interest_Rate > 34), 34, df$Interest_Rate)

# Tratamento variável Num_of_Loan
df$Num_of_Loan <- as.numeric(df$Num_of_Loan)
summary(df$Num_of_Loan)
quantile(df$Num_of_Loan, probs = c(0.95), na.rm=TRUE)
quantile(df$Num_of_Loan, probs = c(0.99), na.rm=TRUE)
df$Num_of_Loan <- ifelse((df$Num_of_Loan > 9) | (df$Num_of_Loan < 0),
                               median(df$Num_of_Loan), df$Num_of_Loan)
df <- df %>% dplyr::mutate(across(Num_of_Loan, ~replace_na(., median(., na.rm=TRUE))))

# Tratamento Delay_from_due_date
summary(df$Delay_from_due_date)
df$Delay_from_due_date <- ifelse((df$Delay_from_due_date < 0), 0, df$Delay_from_due_date)

# Tratamento Num_of_Delayed_Payment
df$Num_of_Delayed_Payment <- as.numeric(df$Num_of_Delayed_Payment)
summary(df$Num_of_Delayed_Payment)
df$Num_of_Delayed_Payment <- ifelse((df$Num_of_Delayed_Payment < 0), 0, df$Num_of_Delayed_Payment)
quantile(df$Num_of_Delayed_Payment, probs = c(0.95), na.rm=TRUE)
quantile(df$Num_of_Delayed_Payment, probs = c(0.99), na.rm=TRUE)
df$Num_of_Delayed_Payment <- ifelse((df$Num_of_Delayed_Payment > 27), 27, df$Num_of_Delayed_Payment)
df <- df %>% dplyr::mutate(across(Num_of_Delayed_Payment, ~replace_na(., median(., na.rm=TRUE))))

# Changed_Credit_Limit
df$Changed_Credit_Limit <- as.numeric(df$Changed_Credit_Limit)
summary(df$Changed_Credit_Limit)
df$Changed_Credit_Limit <- ifelse((df$Changed_Credit_Limit < 0), 0, df$Changed_Credit_Limit)
df <- df %>% dplyr::mutate(across(Changed_Credit_Limit, ~replace_na(., median(., na.rm=TRUE))))

# Tratamento variável Credit_Mix
df$Credit_Mix <- factor(df$Credit_Mix)
table(df$Credit_Mix)

# Tratamento Outstanding_Debt 
df$Outstanding_Debt <- gsub("_", "", df$Outstanding_Debt)
df$Outstanding_Debt <- as.numeric(df$Outstanding_Debt)

# Credit_Utilization_Ratio 
hist(df$Credit_Utilization_Ratio)

# Tratamento Payment_of_Min_Amount
df$Payment_of_Min_Amount <- factor(df$Payment_of_Min_Amount)
table(df$Payment_of_Min_Amount)

# Total_EMI_per_month 
hist(df$Total_EMI_per_month)
summary(df$Total_EMI_per_month)
quantile(df$Total_EMI_per_month, probs = c(0.99), na.rm=TRUE)
quantile(df$Total_EMI_per_month, probs = c(0.95), na.rm=TRUE)

# Tratamento Amount_invested_monthly 
df$Amount_invested_monthly <- as.numeric(df$Amount_invested_monthly)
df <- df %>% dplyr::mutate(across(Amount_invested_monthly, ~replace_na(., median(., na.rm=TRUE))))

# Payment_Behaviour
df$Payment_Behaviour <- factor(df$Payment_Behaviour)
table(df$Payment_Behaviour)

# Tratamento Monthly_Balance 
df$Monthly_Balance <- as.numeric(df$Monthly_Balance)
df <- df %>% dplyr::mutate(across(Monthly_Balance, ~replace_na(., median(., na.rm=TRUE))))

# Tratamento Num_Credit_Inquiries
df$Num_Credit_Inquiries <- ifelse(df$Num_Credit_Inquiries > 20 ,
                                      20, df$Num_Credit_Inquiries)
df <- df %>% dplyr::mutate(across(Num_Credit_Inquiries, ~replace_na(., median(., na.rm=TRUE))))

summary(df)

# Dummização
df_dummies <- dummy_columns(.data = df,
                                    select_columns = c("Occupation",
                                                       "Credit_Mix", 
                                                       "Payment_of_Min_Amount",
                                                       "Payment_Behaviour"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)



# 80% do tamanho da amostra
smp_size <- floor(0.8 * nrow(df_dummies))
smp_size

# Definindo seed para reproducibilidade
set.seed(123)
train_ind <- sample(seq_len(nrow(df_dummies)), size = smp_size)

# Divisão dos dados em treino e teste
df_treino <- df_dummies[train_ind, ]
df_teste <- df_dummies[-train_ind, ]

summary(df_treino)
##############################################################################
#           ENSAIO 01 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO            #
##############################################################################

log_binario <- glm(formula = target_binaria ~ . -ID -Customer_ID -Month -Credit_Score, 
                      data = df_treino, 
                      family = "binomial")

#Parâmetros do binário
summary(log_binario)

# Outras maneiras de apresentar os outputs do modelo

# Funções 'summ'  do pacote 'jtools' 
summ(log_binario, confint = T, digits = 3, ci.width = .95)

# Função 'stargazer' do pacote 'stargazer'
stargazer(log_binario, nobs = T, type = "text") # mostra o valor de Log-Likelihood

#Extração do valor de Log-Likelihood (LL)
logLik(log_binario)

##############################################################################
# ENSAIO 02 - Excluindo variáveis que não passam no teste ao nível de significância de 0.1 #
##############################################################################
# Excluindo a variável Occupation por simplicidade
log_binario2 <- glm(formula = target_binaria ~ . -ID -Customer_ID -Month -Credit_Score
                    -Credit_Utilization_Ratio -Total_EMI_per_month 
                    -Occupation_Accountant -Occupation_Architect -Occupation_Developer
                    -Occupation_Engineer -Occupation_Entrepreneur -Occupation_Lawyer 
                    -Occupation_Manager -Occupation_Mechanic -Occupation_Musician
                    -Occupation_Scientist -Occupation_Teacher -Payment_Behaviour_Low_spent_Large_value_payments,
                   data = df_treino, 
                   family = "binomial")


summary(log_binario2)
logLik(log_binario2)

# Outras maneiras de apresentar os outputs do modelo

#Funções 'summ' do pacote 'jtools'
summ(log_binario2, confint = T, digits = 3, ci.width = .95)

# Função 'stargazer' do pacote 'stargazer'
stargazer(log_binario2, nobs = T, type = "text") # mostra o valor de Log-Likelihood

# Probabilidades preditas
df_teste$prob_pred <- (predict(object = log_binario2, df_teste, type = "response"))

# Predições multi-classes a posteriori - percentis definidos após saída do modelo multinomial
df_teste$predicao <- ifelse(df_teste$prob_pred < quantile(df_teste$prob_pred, probs = c(0.1864), na.rm=TRUE),
                            "Good",
                            ifelse(df_teste$prob_pred < quantile(df_teste$prob_pred, probs = c(0.7444), na.rm=TRUE),
                                   "Standard", "Poor"))

# Matriz de confusão multi-classe em termos percentuais
table(df_teste$Credit_Score, df_teste$predicao)/20000

# Matriz de confusão binária
df_teste$predicao_binaria <- ifelse(df_teste$prob_pred < quantile(df_teste$prob_pred, probs = c(0.7444)), 0, 1)
table(df_teste$target_binaria, df_teste$predicao_binaria)/20000

############################################################################
#           ENSAIO 03 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO MULTINOMIAL           #
##############################################################################
#Apontando a categoria de referência
df_treino$Credit_Score <- relevel(df_treino$Credit_Score, 
                                        ref = "Poor")

#Estimação do modelo - função multinom do pacote nnet
modelo_multinomial <- multinom(formula = Credit_Score ~ . -target_binaria -ID -Customer_ID -Month -Credit_Score
                               -Credit_Utilization_Ratio -Total_EMI_per_month 
                               -Occupation_Accountant -Occupation_Architect -Occupation_Developer
                               -Occupation_Engineer -Occupation_Entrepreneur -Occupation_Lawyer 
                               -Occupation_Manager -Occupation_Mechanic -Occupation_Musician
                               -Occupation_Scientist -Occupation_Teacher -Payment_Behaviour_Low_spent_Large_value_payments,
                            data = df_treino)

#Parâmetros do modelo_atrasado
summary(modelo_multinomial)

#Outra maneira de apresentar os outputs do modelo - função stargazer do pacote stargazer 
stargazer(modelo_multinomial, nobs=T, type="text")

#LL do modelo_atrasado
logLik(modelo_multinomial)

#A função summ do pacote jtools não funciona para objetos de classe 'multinom'. Logo,
#vamos definir uma função Qui2 para se extrair a estatística geral do modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#Estatística geral do modelo_atrasado
Qui2(modelo_multinomial)

#Para calcular as estatísticas z de Wald, há que se dividir os valores da 
#seção 'Coefficients' pelos valores da seção 'Std. Errors.' Assim, temos que:  

zWald_modelo_multinomial <- (summary(modelo_multinomial)$coefficients / 
                            summary(modelo_multinomial)$standard.errors)

zWald_modelo_multinomial

#Porém, ainda faltam os respectivos p-values. Assim, os valores das probabilidades 
#associadas às abscissas de uma distribuição normal-padrão é dada pela função
#pnorm(), considerando os valores em módulo - abs(). Após isso, multiplicamos 
#por dois os valores obtidos para considerar os dois lados da distribuição
#normal padronizada (distribuição bicaudal). Desta forma, temos que:
round((pnorm(abs(zWald_modelo_multinomial), lower.tail = F) * 2), 4)

#Fazendo predições para o modelo_multinomial
df_teste$pred_mult <- (predict(object = modelo_multinomial, 
                                df_teste, 
                                type = "class"))

# Predições binárias
df_teste$pred_mult2 <- ifelse(df_teste$pred_mult == "Poor", 1, 0)

# Matriz de confusão multi-classe em termos percentuais
table(df_teste$Credit_Score, df_teste$pred_mult)/20000
# 25,56% classificados como Poor; 18,64% como Good; 55,8% como Standard

# Matriz de confusão binária
table(df_teste$pred_mult2, df_teste$target_binaria)

