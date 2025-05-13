library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
clientes<-
sparsematrix<-readRDS("Datos/Transformados/matriz_recomendacion.RDS")
dim(sparsematrix)
row
matriz<-as(sparsematrix,"Matrix")
modelo<-WRMF$new(
  rank = 20,                # Número de factores latentes
  lambda = 0.1,             # Regularización
  max_iter = 10,            # Máximo de iteraciones
  nthread = parallel::detectCores() - 1,  # Paralelización
  use_float = FALSE,        # Usar precisión double
  non_negative = TRUE       # Restricción de factores no negativos
)

user_emb<-modelo$fit_transform(matriz)
su<-similarity(as(user_emb,"realRatingMatrix"))
item_emb <- modelo$components # matriz de factores de los items
item_emb ; item_emb_n <- t(modelo$components)  
su2<-similarity(as(item_emb,"realRatingMatrix"))

modulo <- function(x){
  if (is.matrix(x)){ 
    sqrt(rowSums(x^2))
  } else {
    sqrt(sum(x^2))  
  }}
user_emb_n <- user_emb/modulo(user_emb) # normalizar
item_emb_n_n <- item_emb/modulo(item_emb) # normalizar
item_emb_n <- t(item_emb)/modulo(t(item_emb)) # normalizar y transponer


item_emb_n %*% user_emb_n[rownames(user_emb_n)=="u1",]%>%   
  sort(decreasing=TRUE, index.return = T)
