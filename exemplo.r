library(ggplot2)
library(gridExtra)
getwd()

# Carrega a planilha no Rstudio

atividades = read.csv(file.choose(), header = TRUE, sep = ";")

atividades

# Comandos para ver alguns status da planilha

str(atividades)

names(atividades)

# For para transformar as colunas do excel de string para numerico

colunas_a_converter <- c("Ativ01", "Ativ02", "Ativ03", "Ativ04", "Ativ05")

for (colunas in colunas_a_converter){
  
  atividades[[colunas]] <- as.numeric(atividades[[colunas]])
  
}

# Media das notas das atividades recebidas

medias <- colMeans(atividades[, 1:5])

medias_df <- data.frame(Atividade = colnames(atividades)[1:5], Media = medias)

ggplot(medias_df, aes(x = Atividade, y = Media)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  labs(x = "Atividade", y = "Média das Notas", title = "Média das Notas por Atividade")

grafico1 <- ggplot(medias_df, aes(x = Atividade, y = Media)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  labs(x = "Atividade", y = "Média das Notas", title = "Média das Notas por Atividade")

# Quantidade de Atividades recebidas

quantidades_por_coluna <- numeric(length = ncol(atividades))

for (i in 1:ncol(atividades)) {
  coluna <- atividades[, i]
  quantidades_por_coluna[i] <- sum(coluna != 0)
}

quantidades_df <- data.frame(Atividade = colnames(atividades), Quantidade = quantidades_por_coluna)

ggplot(quantidades_df, aes(x = Atividade, y = Quantidade)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Atividade", y = "Quantidade", title = "Atividades Recebidas (Excluindo 0)")

grafico2 <- ggplot(quantidades_df, aes(x = Atividade, y = Quantidade)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Atividade", y = "Quantidade", title = "Atividades Recebidas (Excluindo 0)")

grid.arrange(grafico1, grafico2, ncol = 2)

# Definição final entre as medias das atividades 1 e 5

somatorio_ativ03 = sum(atividades$Ativ03)
media_Ativ03 = (somatorio_ativ03/(quantidades_por_coluna[3]))

somatorio_ativ04 = sum(atividades$Ativ04)
media_Ativ04 = (somatorio_ativ04/(quantidades_por_coluna[4]))

media_abs <- data.frame(med = c(media_Ativ03,media_Ativ04),
                        Catt = c("Media Atv 3", "Media Atv 4"),
                        cor = c("red", "blue"))

ggplot(media_abs, aes(x = Catt, y = med, fill = cor)) +
  geom_bar(stat = "identity") +
  labs(x = "Atividades", y = "Media", title = "Comparação entre as atividades") +
  scale_fill_identity()

#Atividade sala vs atividade casa

somatorio_ativ01 = sum(atividades$Ativ01)
media_Ativ01 = (somatorio_ativ01/(quantidades_por_coluna[1]))

somatorio_ativ02 = sum(atividades$Ativ02)
media_Ativ02 = (somatorio_ativ02/(quantidades_por_coluna[2]))

somatorio_ativ03 = sum(atividades$Ativ03)
media_Ativ03 = (somatorio_ativ03/(quantidades_por_coluna[3]))

somatorio_ativ04 = sum(atividades$Ativ04)
media_Ativ04 = (somatorio_ativ04/(quantidades_por_coluna[4]))

somatorio_ativ05 = sum(atividades$Ativ05)
media_Ativ05 = (somatorio_ativ05/(quantidades_por_coluna[5]))

media_sala = (media_Ativ01 + media_Ativ02 + media_Ativ03)/3

media_casa = (media_Ativ04 + media_Ativ05)/2

media_casa_sala <- data.frame(med = c(media_sala,media_casa),
                        Catt = c("Media Atv Sala", "Media Atv Casa"),
                        cor = c("blue", "red"))

ggplot(media_casa_sala, aes(x = Catt, y = med, fill = cor)) +
  geom_bar(stat = "identity") +
  labs(x = "Atividades", y = "Media", title = "Comparação entre casa e sala") +
  scale_fill_identity()


