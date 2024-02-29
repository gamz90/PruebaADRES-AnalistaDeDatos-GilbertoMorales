#install.packages(c('sqldf','dplyr','tidyverse','devtools','ggplot2','hrbrthemes'))
#devtools::install_github("r-lib/conflicted")
library(sqldf)
library(tidyverse)
library(dplyr)
library(conflicted)
library(ggplot2)
library(hrbrthemes)
library(data.table)
hrbrthemes::import_roboto_condensed()

# Normalizacion min-max
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Conexion a la base de datos generada previamente
db <- dbConnect(SQLite(), dbname='Adres.sqlite')

# Se haran analisis sobre la frecuencia relativa de los tipos de prestadores por cada mil habitantes
resultados <- c()
# Primero se hara una unica extraccion de informacion de Municipios, la cual se concatena con la informacion extraida en cada analisis individual de prestadores
query <- "SELECT Departamento, Municipio, Poblacion, Irural, Region FROM Municipios"
infoMunicipios <- dbGetQuery(db, query)


# Primer análisis se hace sobre el conteo total de prestadores por municipio
query <- "SELECT depa_nombre, muni_nombre, COUNT(codigo_habilitacion) FROM Prestadores GROUP BY depa_nombre, muni_nombre;"
resultadosAct <- dbGetQuery(db, query)
colnames(resultadosAct)[1] = "Departamento"
colnames(resultadosAct)[2] = "Municipio"
colnames(resultadosAct)[3] = "Conteo"
resultadosAct <- full_join(x=resultadosAct,y=infoMunicipios, by=c("Departamento","Municipio"))
resultadosAct$Conteo <-resultadosAct$Conteo %>% replace_na(0)
resultadosAct$PrestadoresPorDiezMilPersonas <- 10000*resultadosAct$Conteo/resultadosAct$Poblacion
resultadosAct <- resultadosAct[order(resultadosAct$Departamento,resultadosAct$Municipio ),]
resultadosAct <- resultadosAct[resultadosAct$Municipio != 'Guachené', ]

# Se generan las gráficas de Análisis

png(file="data/output/Porcentaje acumulado de municipios por prestadores por habitante.png", width=600, height=350)
ggplot(resultadosAct, aes(x=PrestadoresPorDiezMilPersonas)) + geom_step(stat="ecdf", aes(y= ..y..*100)) +scale_x_continuous(name ="Prestadores por cada 10.000 personas") + scale_y_continuous(name ="Porcentaje acumulado de Municipios")
dev.off()

png(file="data/output/Indice ruralidad VS Prestadores por habitante.png", width=600, height=350)
ggplot(resultadosAct, aes(x=Irural, y=PrestadoresPorDiezMilPersonas, color=Region, shape=Region)) + geom_point(size=1) + scale_y_continuous(name ="Prestadores por cada 10.000 personas") + scale_x_continuous(name ="Indice de Ruralidad")
dev.off()

resultadosAct <- resultadosAct %>% mutate(dummy=1) %>% spread(key=Region,value=dummy, fill=0)
colnames(resultadosAct)[which(names(resultadosAct) == "Región Caribe")] <- "Caribe"
colnames(resultadosAct)[which(names(resultadosAct) == "Región Centro Oriente")] <- "Centro_Oriente"
colnames(resultadosAct)[which(names(resultadosAct) == "Región Centro Sur")] <- "Centro_Sur"
colnames(resultadosAct)[which(names(resultadosAct) == "Región Eje Cafetero")] <- "Eje_Cafetero"
colnames(resultadosAct)[which(names(resultadosAct) == "Región Llano")] <- "Llano"
colnames(resultadosAct)[which(names(resultadosAct) == "Región Pacífico")] <- "Pacifico"
resultadosAct$PrestadoresPorDiezMilPersonas <- min_max_normalize(resultadosAct$PrestadoresPorDiezMilPersonas)
resultadosAct$Poblacion <- min_max_normalize(resultadosAct$Poblacion)
resultadosAct$Irural <- min_max_normalize(resultadosAct$Irural)
lmA = lm(PrestadoresPorDiezMilPersonas~ Poblacion + Irural + Caribe + Centro_Oriente + Centro_Sur + Eje_Cafetero + Llano, data = resultadosAct) 
summary(lmA) #Review the results





# Se hace un segundo análisis verificando el nivel de los prestadores
query <- "SELECT depa_nombre, muni_nombre, nivel, COUNT(codigo_habilitacion) FROM Prestadores GROUP BY depa_nombre, muni_nombre,nivel;"
resultadosAct2 <- dbGetQuery(db, query)
colnames(resultadosAct2)[1] = "Departamento"
colnames(resultadosAct2)[2] = "Municipio"
colnames(resultadosAct2)[3] = "Nivel"
colnames(resultadosAct2)[4] = "Conteo"
resultadosAct2 <- full_join(x=resultadosAct2,y=infoMunicipios, by=c("Departamento","Municipio"))
resultadosAct2$Conteo <-resultadosAct2$Conteo %>% replace_na(0)
resultadosAct2$PrestadoresPorDiezMilPersonas <- 10000*resultadosAct2$Conteo/resultadosAct2$Poblacion
resultadosAct2 <- resultadosAct2[order(resultadosAct2$Departamento,resultadosAct2$Municipio ),]
resultadosAct2 <- resultadosAct2[resultadosAct2$Municipio != 'Guachené', ]

resultadosAct2 <- resultadosAct2[resultadosAct2$Conteo >0, ]
resultadosAct2 <- resultadosAct2[,!names(resultadosAct2) %in% c("PrestadoresPorDiezMilPersonas", "Departamento", "Municipio", "Region","Poblacion")]

# Se analiza el total de prestadores en cada nivel de ruralidad según el nivel (1,2,3) del prestador
resultadosAct3 <- resultadosAct2 %>% group_by(Irural,Nivel) %>%  summarise(Prestadores=sum(Conteo))
resultadosAct3$Nivel <- as.character(resultadosAct3$Nivel) 
resultadosAct3 <- drop_na(resultadosAct3)
png(file="data/output/Prestadores por Nivel.png", width=600, height=350)
ggplot(resultadosAct3, aes(fill=Nivel, y=Prestadores, x=Irural)) + 
  geom_bar(position="stack", stat="identity")  + scale_y_continuous(name ="Número de prestadores") + scale_x_continuous(name ="Indice de Ruralidad")
dev.off()

# Se analiza el promedio de prestadores por municio en cada nivel de ruralidad según el nivel (1,2,3) del prestador
resultadosAct2 <- resultadosAct2 %>% group_by(Irural,Nivel) %>%  summarise(Prestadores=mean(Conteo))
resultadosAct2$Nivel <- as.character(resultadosAct2$Nivel) 
resultadosAct2 <- drop_na(resultadosAct2)
png(file="data/output/Prestadores promedio por Nivel.png", width=600, height=350)
ggplot(resultadosAct2, aes(fill=Nivel, y=Prestadores, x=Irural)) + 
  geom_bar(position="stack", stat="identity")  + scale_y_continuous(name ="Número de prestadores") + scale_x_continuous(name ="Indice de Ruralidad")
dev.off()


names(resultadosAct)
dbDisconnect(db)            # Close connection
