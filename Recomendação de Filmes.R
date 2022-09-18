'''O projeto tem como objetivo separar filmes com bases nos seus gêneros e assim conseguir recomendar filmes de um mesmo gênero'''
''' Para isso vou utilizar a técnica de clusterização'''
getwd()
#importação dos dados
filmes <- read.csv('movies.csv')
filmes
filmes_transform <- read.csv2('movies_transf.csv')
filmes_transform

#Normalização dos dados
install.packages('dplyr')
library(dplyr)
filmes_transform <- filmes_transform %>%
                  select(-movieId, -titulo)
dados_normalizados <-data.frame(scale(filmes_transform))

#Criação dos clusters
set.seed(1987)
resultado_cluster <- kmeans(dados_normalizados, centers = 3)
resultado_cluster$cluster
View(resultado_cluster$centers)
resultado_cluster$withinss #os resultados dentro de um mesmo cluster estão bem heterogêneos
resultado_cluster$size #tamanho dos cluster 1 - 6797 itens, 2 - 158, 3 - 1637
#Ou seja, a distribuição dos itens entre os clusters foi bem desigual

#Visualização dos clusters
install.packages('cluster')
library(cluster)
clusplot(x= filmes_transform, resultado_cluster$cluster,
         color = TRUE, shade= TRUE)
install.packages('fpc')
library(fpc)
plotcluster(x= dados_normalizados, resultado_cluster$cluster,
            ignorenum = T)

#Classificações nos Clusters
centros <- resultado_cluster$centers
install.packages('reshape2')
library(reshape2)
centros2 <-melt(centros)
View(centros2)
colnames(centros2) <- c('cluster', 'genero', 'centro')
centros2$cluster <- as.factor(centros2$cluster)
install.packages('ggplot2')
library(ggplot2)
ggplot(data = centros2) +
  geom_bar(aes(x=genero, y=centro, fill=cluster), stat= 'identity')+
             facet_grid(cluster ~ .)

#Melhorias nos clusters
#Método Elbow
range_k <- c(1:25)
soma_quadrados <- 0
set.seed(1987)
for(i in range_k){
  cluster <- kmeans(dados_normalizados, centers = i, nstart = 25)
  soma_quadrados[i] <- sum(cluster$withinss)
}
plot(range_k, soma_quadrados, type = 'b',
     xlab = 'número de clusters',
     ylab = 'soma dos quadrados')
axis(side = 1, at= range_k, labels = range_k)
#Usando o método Elbow descobrimos que a quantidade ideal de clusters é 5
abline(v=5, col = 'red')
#Técnica Silhouette
range_k <- c(2:15)
medias_silhouete <- c(0)
set.seed(1987)
for (i in range_k) {
  clusters <- kmeans(dados_normalizados, centers = i)
  silhouete <- silhouette(clusters$cluster, dist(dados_normalizados))
  medias_silhouete[i] <- mean(silhouete[,3])
}
medias_silhouete
plot(medias_silhouete, type = 'b',
     xlab = 'numero de clusters',
     ylab = 'média silhouette')
axis(side = 1, at=range_k, labels = range_k)

#Criação da Recomendação de Filmes
set.seed(1987)
resultado_cluster <- kmeans(dados_normalizados,centers = 12)
centros <- resultado_cluster$centers
centros2 <- melt(centros)
colnames(centros2) <- c('cluster', 'genero', 'centro')
centros2$cluster <- as.factor(centros2$cluster)
ggplot(data = centros2) +
  geom_bar(aes(x = genero, y = centro, fill = cluster),
  stat = 'identity') +
  facet_grid(cluster~.)
filmes$cluster <- resultado_cluster$cluster
#exibindo uma lista de filmes 'parecidos' com o filme dado - nesse caso Toy Story
agrupamento <- filmes[filmes$title == 'Toy Story (1995)', 'cluster']
agrupamento
filmes[filmes$cluster == agrupamento, 'title'] 
  sample(10)
  
#Cluster Hierarquico
matriz_dist <- dist(centros)
matriz_dist
clust_h <- hclust(matriz_dist)
clust_h
plot(clust_h)
