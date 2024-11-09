install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

data <- read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep = ";", fileEncoding = "latin1")

data$cui <- format(data$cui, scientific =FALSE)

data$nota[data$nota == "SDE"] <- -1
data$final[data$final == "SDE"] <- -1
data$final[data$final == "NSP"] <- -1

data$nombre1 <- sapply(strsplit(data$nombre, " "), `[`, 1)
data$nombre2 <- sapply(strsplit(data$nombre, " "), `[`, 2)

genero("LUIS")
genero("EMILY")

data$genero <- genero(data$nombre1)

subset(data, is.na(data$genero))

data$genero <- ifelse(is.na(data$genero), genero(data$nombre2), data$genero)

data[77, "genero"] <- "male"
data[113, "genero"] <- "male"
data[119, "genero"] <- "female"
data[120, "genero"] <- "male"
data[179, "genero"] <- "female"
data[185, "genero"] <- "male"
data[202, "genero"] <- "male"
data[225, "genero"] <- "male"
data[250, "genero"] <- "male"
data[276, "genero"] <- "female"
data[363, "genero"] <- "female"
data[473, "genero"] <- "female"
data[487, "genero"] <- "male"
data[566, "genero"] <- "male"

data$genero <- ifelse(data$genero == "male", 1, 2)

data$anio_carne <- substr(data$carne, start=1, stop=4)

subset(data, anio_carne > 8000)

data$anio_carne <- ifelse(data$anio_carne > 8000, as.numeric(substr(data$anio_carne, 1,2))+1900, data$anio_carne)

data$edad <- as.integer(data$anio) - as.integer(data$anio_carne) +18

data$municipio <- substr(data$cui, nchar(data$cui) -1, nchar(data$cui))
data$departamento <- substr(data$cui, nchar(data$cui) -3, nchar(data$cui)-2)

data_apriori <- data[, c("lab", "zona", "final", "nota", "anio", "sem", "genero", "edad", "municipio", "departamento")]

reglas <- apriori(data_apriori, parameter = list(support=0.2, confidence = 0.5))

inspect(reglas[0:100])

inspect(reglas[100:200])

data_apriori2 <- subset(data_apriori, genero ==1)
data_apriori3 <- subset(data_apriori, genero ==2)

reglas2 <- apriori(data_apriori2, parameter = list(support=0.2, confidence = 0.5))

data_apriori2 <- data_apriori2[, -7]

inspect(reglas2[0:100])

data_apriori3 <- data_apriori3[, -7]
reglas3 <- apriori(data_apriori3, parameter = list(support=0.2, confidence = 0.5))
inspect(reglas3[0:100])
inspect(reglas3[100:200])

cluster <- kmeans(data_apriori, centers=4)

install.packages("ggplot2")
library(ggplot2)

ggplot(data_apriori, aes(x = edad, y = zona, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=edad, y = zona), color = "black", size=4, shape=17)+
  labs(title = "Edad vs Zona")+
  theme_minimal()

install.packages("ggalt")
library(ggalt)

ggplot(data_apriori, aes(x = edad, y = zona, color = as.factor(cluster$cluster), shape = as.factor(cluster$cluster))) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(cluster$centers),                    
             aes(x = edad, y = zona), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = cluster$cluster, fill = as.factor(cluster$cluster)), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Edad vs zona", x = "Edad", y = "Zona") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )



dataps <- subset(data_apriori, sem==1)
clusteps <- kmeans(dataps, centers=4)
ggplot(dataps, aes(x = edad, y = zona, color = as.factor(clusteps$cluster), shape = as.factor(clusteps$cluster))) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(clusteps$centers),                    
             aes(x = edad, y = zona), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = clusteps$cluster, fill = as.factor(clusteps$cluster)), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Edad vs zona - 1er Sem", x = "Edad", y = "Zona") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )


datass <- subset(data_apriori, sem==2)
clustess <- kmeans(datass, centers=4)

ggplot(datass, aes(x = edad, y = zona, color = as.factor(clustess$cluster), shape = as.factor(clustess$cluster))) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(clustess$centers),                    
             aes(x = edad, y = zona), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = clustess$cluster, fill = as.factor(clustess$cluster)), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Edad vs zona - 2do Sem", x = "Edad", y = "Zona") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )
  
