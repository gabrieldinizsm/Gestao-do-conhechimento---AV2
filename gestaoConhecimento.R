# Fonte de dados https://catalog.data.gov/dataset/traffic-data

setwd("C:/Users/user/Desktop/Scripts R/VT Gestao Conhecimento")

# 1.1 - Descreva aqui o que sua base de dados guarda.

# R: Dados sobre acidentes de trânsito no condado de Monroe, Flórida.

# 1.2 - Após tratada, criar uma pasta no seu GoogleDrive e disponibilizar o link do arquivo de sua base 
#(csv, xlsx, etc) para ser importado no seu código.

# R: https://drive.google.com/file/d/1sBgN67BL9xNKt5-vjdYEyuOtQeGnd50L/view?usp=sharing

# 2.1 - Importar os modulos python para machine learn e carregar o arquivo

library(nnet)
library(UBL)
library(class)

baseDados <- read.csv2("C:/Users/user/Desktop/Scripts R/VT Gestao Conhecimento/acidentes.csv")

View (baseDados)

nomesColunas <- c("idAcidente", "ano", "mes", "diaSemana", "finalSemana", 
                  "hora", "tipoColisao", "gravidadeFerimento", "fatorPrimario", 
                  "localidade", "latitude", "longitude")

colnames(baseDados) <- nomesColunas

View (baseDados)

# 2.2 - Exibir os primeiros registros desta tabela.

head(baseDados)

#2.3 - De que se trata esse banco de dados? e que tipo de previsão pode ser feito com ele?

unique(baseDados$gravidadeFerimento)

# R: Informação sobre acidentes de trânsito e algumas características
# R: Dado um acidente tentaremos prever sua gravidade (gravidadeFerimento)

# 3.1 - Quantas "features" têm nessa base de dados?

baseDadosV2 <- baseDados[, c(-1, -10, -11, -12)]

dim(baseDadosV2)

# R: 8 variáveis sendo 7 features e 1 target. 

baseDadosV2 <- baseDadosV2[!(baseDadosV2$finalSemana==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$hora==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$tipoColisao==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$fatorPrimario==""), ] 
baseDadosV2 <- na.omit(baseDadosV2)

any(is.null(baseDadosV2))
any(is.na(baseDadosV2))

# 3.2 - Quantas observações têm nessa base de dados?

nrow(baseDadosV2)

#R: 52594 

baseDadosV3 <- baseDadosV2[order(runif(dim(baseDadosV2)[1])),] 

View(baseDadosV3)

indiceLinhas = 0.75 * nrow(baseDadosV3) 

baseTreino <- baseDadosV3[1:indiceLinhas,]
baseTeste <- baseDadosV3[(indiceLinhas+1):nrow(baseDadosV3) ,]

baseTreino$gravidadeFerimento <- as.factor(baseTreino$gravidadeFerimento)
baseTreino$ano <- as.factor(baseTreino$ano)
baseTreino$finalSemana <- as.factor(baseTreino$finalSemana)
baseTreino$mes <- as.factor(baseTreino$mes)
baseTreino$hora <- as.factor(baseTreino$hora)
baseTreino$diaSemana <- as.factor(baseTreino$diaSemana)
baseTreino$fatorPrimario <- as.factor(baseTreino$fatorPrimario)
baseTreino$tipoColisao <- as.factor(baseTreino$tipoColisao)

baseTeste$gravidadeFerimento <- as.factor(baseTeste$gravidadeFerimento)
baseTeste$ano <- as.factor(baseTeste$ano)
baseTeste$finalSemana <- as.factor(baseTeste$finalSemana)
baseTeste$mes <- as.factor(baseTeste$mes)
baseTeste$hora <- as.factor(baseTeste$hora)
baseTeste$diaSemana <- as.factor(baseTeste$diaSemana)
baseTeste$fatorPrimario <- as.factor(baseTeste$fatorPrimario)
baseTeste$tipoColisao <- as.factor(baseTeste$tipoColisao)

str(baseTreino)

table(baseTeste$gravidadeFerimento)

# Balanceando as classes através do oversample

memory.limit(99999999999)

baseTreinoBalanceada <- AdasynClassif(gravidadeFerimento ~ ano + mes + diaSemana + finalSemana
                                      + hora + tipoCOlisao + gravidadeFerimento + fatorPrimario
                                      , baseTreino, dist = 'Overlap')

table(baseTreinoBalanceada$gravidadeFerimento)

# 4.1 - Faça uma previsão, usando o algoritmo de LogisticRegression

modeloRegressao <- multinom(gravidadeFerimento ~ ., 
                            data = baseTreinoBalanceada,
                            maxit = 1000,
                            MaxNWts = 500)

baseTesteV2 <- baseTeste

baseTesteV2$previsoesClasses <- predict(modeloRegressao, newdata = baseTesteV2, "class")

View(baseTesteV2)

# Questão 5

#4.1 - Acurácia usando o algoritmo de KNN (com 1 vizinho, k=1)

baseTreinoBalanceada$gravidadeFerimento <- as.numeric(baseTreinoBalanceada$gravidadeFerimento)
baseTreinoBalanceada$ano <- as.numeric(baseTreinoBalanceada$ano)
baseTreinoBalanceada$finalSemana <- as.numeric(baseTreinoBalanceada$finalSemana)
baseTreinoBalanceada$mes <- as.numeric(baseTreinoBalanceada$mes)
baseTreinoBalanceada$hora <- as.numeric(baseTreinoBalanceada$hora)
baseTreinoBalanceada$diaSemana <- as.numeric(baseTreinoBalanceada$diaSemana)
baseTreinoBalanceada$fatorPrimario <- as.numeric(baseTreinoBalanceada$fatorPrimario)
baseTreinoBalanceada$tipoColisao <- as.numeric(baseTreinoBalanceada$tipoColisao)

baseTeste$gravidadeFerimento <- as.numeric(baseTeste$gravidadeFerimento)
baseTeste$ano <- as.numeric(baseTeste$ano)
baseTeste$finalSemana <- as.numeric(baseTeste$finalSemana)
baseTeste$mes <- as.numeric(baseTeste$mes)
baseTeste$hora <- as.numeric(baseTeste$hora)
baseTeste$diaSemana <- as.numeric(baseTeste$diaSemana)
baseTeste$fatorPrimario <- as.numeric(baseTeste$fatorPrimario)
baseTeste$tipoColisao <- as.numeric(baseTeste$tipoColisao)

target_category <- baseTreinoBalanceada[ , 7]

test_category <-  baseTeste[ , 7]

modeloKnnv1 <- knn(baseTreinoBalanceada, baseTeste, cl = target_category, k = 1)

tabelaKnn1 <- table(modeloKnnv1, test_category)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(tabelaKnn1)

# 4.2 - Acurácia usando o algoritmo de KNN (com 5 vizinho, k=5)

modeloKnnV5 <- knn (train = baseTreinoBalanceada, test = baseTeste, target_category , k = 5)

tabelaKnnV5 <- table(modeloKnnV5, test_category)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(tabelaKnnV5)

# 4.3 - Acurácia usando o algoritmo de LogisticRegression

tabelaClassificacaoTeste <- table(baseTeste$gravidadeFerimento, baseTesteV2$previsoesClasses)

round((sum(diag(tabelaClassificacaoTeste)) / sum(tabelaClassificacaoTeste)) * 100, 2)

# 4.4 - De acordo com seus resultados anteriores, qual dos 3 métodos é mais eficiente?

print(accuracy(tabelaKnn1))





