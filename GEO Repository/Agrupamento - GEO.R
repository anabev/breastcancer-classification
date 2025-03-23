# 1. Leitura e Preprocessamento dos Dados
# Carregar o arquivo CSV com os dados
dados <- read.csv("C:/Users/ana_v/OneDrive/Documentos/Repositórios/GSE25066.csv")
# Remover a coluna de subtipo para deixar apenas os dados de expressão gênica
dados_exp <- dados[, -c(1,2)]  # Remover as colunas desnecessárias (subtipo e outra coluna)

# 2. Cálculo das Matrizes de Distância
# Calcular a distância Euclidiana entre as amostras, escalando os dados primeiro
dist_euc <- dist(scale(dados_exp), method = "euclidean")
# Calcular a distância baseada na correlação de Pearson entre as amostras
dist_cor <- as.dist(1 - cor(t(dados_exp), method = "pearson"))

# 3. Construção dos Dendrogramas
# Usando o método de agrupamento hierárquico Ward.D2 para criar dendrogramas com base nas duas métricas de distância
hc_euc <- hclust(dist_euc, method = "ward.D2")
hc_cor <- hclust(dist_cor, method = "ward.D2")

# 4. Definição do Número Ideal de Clusters Usando o Método de Mojena
# Função para calcular o número de clusters ideal com base no ponto de corte de Mojena
mojena_cut <- function(hc, k = 1.5) {
  heights <- hc$height  # Altura dos pontos de corte
  mean_h <- mean(heights)  # Média das alturas
  sd_h <- sd(heights)  # Desvio padrão das alturas
  cutoff <- mean_h + k * sd_h  # Definir o ponto de corte
  num_clusters <- sum(heights > cutoff) + 1  # Determinar o número de clusters
  return(num_clusters)
}

# Determinar o número ideal de clusters para cada tipo de distância
num_clusters_euc <- mojena_cut(hc_euc, k = 1.25)
num_clusters_cor <- mojena_cut(hc_cor, k = 1.25)

cat("Número ideal de clusters (Euclidiana - Mojena):", num_clusters_euc, "\n")
cat("Número ideal de clusters (Correlação de Pearson - Mojena):", num_clusters_cor, "\n")

# 5. Realização do Clustering e Cálculo da Pureza
# Criar clusters a partir dos dendrogramas usando o número ideal de clusters
dados$Subtype_num <- as.numeric(factor(dados$type))  # Transformar o subtipo em variável numérica
clusters_euc <- cutree(hc_euc, k = num_clusters_euc)
clusters_cor <- cutree(hc_cor, k = num_clusters_cor)

# Função para calcular a pureza dos clusters (baseada na classe predominante dentro de cada cluster)
calc_pureza <- function(clusters, labels) {
  tab <- table(clusters, labels)  # Tabela de contagem de clusters e rótulos reais
  pureza <- sum(apply(tab, 1, max)) / length(labels)  # Calcular a pureza
  return(pureza)
}

# Calcular a pureza para ambos os métodos de distância
pureza_euc <- calc_pureza(clusters_euc, dados$Subtype_num)
pureza_cor <- calc_pureza(clusters_cor, dados$Subtype_num)

cat("Pureza (Euclidiana - Mojena):", pureza_euc, "\n")
cat("Pureza (Correlação de Pearson - Mojena):", pureza_cor, "\n")

# 6. Visualização dos Dendrogramas e Gráfico de Pureza
# Gerar os dendrogramas para visualização
library(ggplot2)
plot(hc_euc, main = "Dendrogram - Euclidean Distance", xlab = "", sub = "", cex = 0.6)
plot(hc_cor, main = "Dendrogram - Pearson Correlation", xlab = "", sub = "", cex = 0.6)

# Criar um gráfico de barras para comparar a pureza dos clusters
pureza_df <- data.frame(
  Tipo = c("Euclidean", "Pearson Correlation"),
  Pureza = c(pureza_euc, pureza_cor)
)

ggplot(pureza_df, aes(x = Tipo, y = Pureza, fill = Tipo)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster Purity", y = "Purity", x = "Distance Method") +
  theme(legend.position = "none")

# 7. Matrizes de Distribuição de Frequência
# Calcular e exibir as matrizes de distribuição de frequência para os clusters gerados
freq_dist_euc <- table(clusters_euc, dados$Subtype_num)
cat("\nFrequency distribution matrix (Euclidean):\n")
print(freq_dist_euc)

freq_dist_cor <- table(clusters_cor, dados$Subtype_num)
cat("\nFrequency distribution matrix (Pearson Correlation):\n")
print(freq_dist_cor)

# 8. Determinação da Classe Predominante em Cada Cluster
# Identificar a classe predominante em cada cluster com base na matriz de frequência
predominant_class_euc <- apply(freq_dist_euc, 1, function(x) names(which.max(x)))
predominant_class_cor <- apply(freq_dist_cor, 1, function(x) names(which.max(x)))

# Criar uma tabela com os resultados do clustering, incluindo a pureza e a classe predominante
resultados <- data.frame(
  Método = c(rep("Euclidean", num_clusters_euc), rep("Pearson Correlation", num_clusters_cor)),
  Cluster = c(1:num_clusters_euc, 1:num_clusters_cor),
  Classe_Predominante = c(predominant_class_euc, predominant_class_cor),
  Pureza = c(rep(pureza_euc, num_clusters_euc), rep(pureza_cor, num_clusters_cor))
)

cat("\nClustering Results:\n")
print(resultados)

# Exibir a tabela com os resultados de clustering utilizando 'kable'
library(knitr)
kable(resultados, caption = "Clustering Results with Purity and Predominant Class")

# 9. Análise Bayesianas e Curvas de Probabilidade
# Calcular a probabilidade a posteriori para cada cluster, com base nas distribuições de frequência
posterior_euc <- prop.table(freq_dist_euc, margin = 1)
cat("\nPosterior probabilities (Euclidean):\n")
print(posterior_euc)

posterior_cor <- prop.table(freq_dist_cor, margin = 1)
cat("\nPosterior probabilities (Pearson Correlation):\n")
print(posterior_cor)

# Gerar as curvas de probabilidade para os clusters baseados na distância de Pearson

library(ggplot2)
dimnames(freq_dist_cor) <- list(clusters_cor = as.character(1:33), classes = as.character(1:5))
posterior_cor <- prop.table(freq_dist_cor, margin = 1)
posterior_cor_df <- as.data.frame(as.table(posterior_cor))

colnames(posterior_cor_df) <- c("Cluster", "True_Class", "Probability")
ggplot(posterior_cor_df, aes(x = True_Class, y = Probability, color = factor(Cluster))) +
  geom_line(aes(group = Cluster)) +
  geom_point() +
  labs(title = "Probability Curves - Pearson Correlation", 
       x = "True Class", 
       y = "Probability") +
  theme_minimal()

# Gerar as curvas de probabilidade para os clusters baseados na distância Euclidiana
dimnames(freq_dist_euc) <- list(clusters_euc = as.character(1:33), classes = as.character(1:5))
posterior_euc <- prop.table(freq_dist_euc, margin = 1)
posterior_euc_df <- as.data.frame(as.table(posterior_euc))

colnames(posterior_euc_df) <- c("Cluster", "True_Class", "Probability")
ggplot(posterior_euc_df, aes(x = True_Class, y = Probability, color = factor(Cluster))) +
  geom_line(aes(group = Cluster)) +
  geom_point() +
  labs(title = "Probability Curves - Euclidean", 
       x = "True Class", 
       y = "Probability") +
  theme_minimal()


################

# Função para calcular a pureza de cada cluster individualmente
calc_pureza_por_cluster <- function(clusters, labels) {
  tab <- table(clusters, labels)  # Tabela de contagem de clusters e rótulos reais
  pureza_por_cluster <- apply(tab, 1, function(x) max(x) / sum(x))  # Pureza de cada cluster
  return(pureza_por_cluster)
}

# Calcular pureza por cluster para os dois métodos de distância
pureza_por_cluster_euc <- calc_pureza_por_cluster(clusters_euc, dados$Subtype_num)
pureza_por_cluster_cor <- calc_pureza_por_cluster(clusters_cor, dados$Subtype_num)

# Exibir a pureza por cluster
cat("\nPureza por cluster (Euclidiana):\n")
print(pureza_por_cluster_euc)

cat("\nPureza por cluster (Correlação de Pearson):\n")
print(pureza_por_cluster_cor)


#########

library(ggplot2)
library(reshape2)
library(patchwork)

# Criar as matrizes de frequência
freq_dist_cor <- table(clusters_cor, dados$Subtype_num)
freq_dist_euc <- table(clusters_euc, dados$Subtype_num)

# Converter para data frame
df_freq_cor <- as.data.frame(freq_dist_cor)
colnames(df_freq_cor) <- c("Cluster", "Subtype", "Frequency")

df_freq_euc <- as.data.frame(freq_dist_euc)
colnames(df_freq_euc) <- c("Cluster", "Subtype", "Frequency")

# Heatmap Pearson Correlation
p1 <- ggplot(df_freq_cor, aes(x = Subtype, y = Cluster, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Frequency Distribution (Pearson Correlation)",
       x = "Subtype",
       y = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap Euclidean
p2 <- ggplot(df_freq_euc, aes(x = Subtype, y = Cluster, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Frequency Distribution (Euclidean)",
       x = "Subtype",
       y = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
