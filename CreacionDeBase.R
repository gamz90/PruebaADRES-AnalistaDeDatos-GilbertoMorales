#install.packages(c('sqldf','readxl','stringr'))
# Esta libreria usa SQLite para crear las bases de datos.
library('sqldf')
# Lectura de archivos de excel 
library('readxl')
# Edicion de textos
library('stringr')

# Se accede a la base de datos, en caso de que no exista la crea.
db <- dbConnect(SQLite(), dbname='Adres.sqlite')

# Si ya se corrio al menos 1 vez el codigo, quitar los comentarios para borrar las tablas generadas previamente
dbRemoveTable(db, "Municipios")
dbRemoveTable(db, "Prestadores")


# Se carga el primer libro de la base de datos de Municipios
Municipios <- read_excel("data/input/Municipios.xlsx")
## Correcciones al texto:

### Se eliminan los caracteres especiales dentro de las cadenas de texto
Municipios$Departamento <- gsub("[[:punct:]]", "", Municipios$Departamento) 
Municipios$Municipio <- gsub("[[:punct:]]", "", Municipios$Municipio) 
### Se convierten espacios en blanco múltiples a sencillos
Municipios$Departamento <- gsub("\\s+", " ", Municipios$Departamento) 
Municipios$Municipio <- gsub("\\s+", " ", Municipios$Municipio) 
### Se eliminan espacios vacios al inicio y final del texto
Municipios$Departamento <- trimws( Municipios$Departamento)
Municipios$Municipio <- trimws( Municipios$Municipio) 
### Se capitalizan los textos, dejando la primera letra en mayúscula y el resto en minúscula de cada palabra
Municipios$Departamento <- str_to_title( Municipios$Departamento)
Municipios$Municipio <- str_to_title( Municipios$Municipio)
### Se adaptan los nombres para que sean equivalentes entre ambas bases, considerando el nombre real de cada departamento y municipio
Municipios$Departamento<-replace(Municipios$Departamento, Municipios$Departamento %in% ('Bogotá D C'), ('Bogotá DC'))
Municipios$Departamento<-replace(Municipios$Departamento, Municipios$Departamento %in% ("San Andrés"), ("San Andrés Y Providencia")) 
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Bogotá D C'), ('Bogotá'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Guachene'), ('Guachené'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Buena Vista'), ('Buenavista'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Dibula'), ('Dibulla'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Chivolo'), ('Chibolo'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Pueblo Viejo'), ('Puebloviejo'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Acacias'), ('Acacías'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Valle De Guamez'), ('Valle Del Guamuez'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Carmen De Apicala'), ('Carmen De Apicalá'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('Rio Blanco'), ('Rioblanco'))
Municipios$Municipio<-replace(Municipios$Municipio, Municipios$Municipio %in% ('San Pablo'), ('San Pablo De Borbur'))
#### Se hace una separación para corregir el nombre de San Luis de Palenque en Casanare, pues se obtiene que el nombre no era el correcto sino se había asignado el nombre de un municipio boyacence (San Luis de Gaceno)
Mun1 <- Municipios[Municipios$Departamento == 'Casanare', ]
Mun1$Municipio <- replace(Mun1$Municipio, Mun1$Municipio %in% c('San Luis De Gaceno'), ('San Luis De Palenque'))
Mun2 <- Municipios[Municipios$Departamento != 'Casanare', ]
Municipios <- rbind(Mun1,Mun2)
rm(Mun1)
rm(Mun2)
### Se ordena el dataframe por departamento y municipio
Municipios <- Municipios[order(Municipios$Departamento,Municipios$Municipio ),]

## Estructura de la tabla
str(Municipios) 
## Nombres de las columnas
names(Municipios) 

## Se crea la tabla de Municipios dentro de la base de datos
dbWriteTable(conn = db, name = "Municipios", value = Municipios, row.names = FALSE)
## Se elimina la tabla de municipios al no ser requerida en ningún otro paso dentro del proceso.
rm(Municipios)

# Se carga el segundo libro de la base de datos de Prestadores
Prestadores <- read_excel('data/input/Prestadores.xlsx')
## Se hacen homologaciones de texto para poder hacer conexiones entre las bases de datos

### Se eliminan los caracteres especiales dentro de las cadenas de texto
Prestadores$depa_nombre <- gsub("[[:punct:]]", "", Prestadores$depa_nombre) 
Prestadores$muni_nombre <- gsub("[[:punct:]]", "", Prestadores$muni_nombre) 
### Se capitalizan los textos, dejando la primera letra en mayúscula y el resto en minúscula de cada palabra
Prestadores$depa_nombre <- str_to_title( Prestadores$depa_nombre)
Prestadores$muni_nombre <- str_to_title( Prestadores$muni_nombre)

### Se corrijen los nombres de los departamentos para los municipios especiales donde aparece el nombre de la ciudad reportada en el campo de departamento
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ('Bogotá Dc'), ('Bogotá DC')) 
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ("Barranquilla"), ("Atlántico"))
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ("Buenaventura"), ("Valle Del Cauca"))
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ("Cali"), ("Valle Del Cauca"))
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ("Cartagena"), ("Bolívar"))
Prestadores$depa_nombre <- replace(Prestadores$depa_nombre, Prestadores$depa_nombre %in% ("Santa Marta"), ("Magdalena"))

### Se adaptan los nombres para que sean equivalentes entre ambas bases, considerando el nombre real de cada municipio
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Guatape'), ('Guatapé'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Sonson'), ('Sonsón'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Turbana'), ('Turbaná'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Paez'), ('Páez'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Magüi'), ('Magüí'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Tumaco'), ('San Andrés De Tumaco'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Calarca'), ('Calarcá'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Chima'), ('Chimá'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Sincé'), ('San Luis De Sincé'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('Vistahermosa'), ('Vista Hermosa'))
Prestadores$muni_nombre <- replace(Prestadores$muni_nombre, Prestadores$muni_nombre %in% ('San Pablo'), ('San Pablo De Borbur'))

#### Se hace una separación en Antioqui para completar el nombre de San Andrés de Cuerquía y que sea compatible con la base de Municipios
Pres1 <- Prestadores[Prestadores$depa_nombre == 'Antioquia', ]
Pres1$muni_nombre <- replace(Pres1$muni_nombre, Pres1$muni_nombre %in% c('San Andrés'), ('San Andrés De Cuerquía'))
Pres2 <- Prestadores[Prestadores$depa_nombre != 'Antioquia', ]
Prestadores <- rbind(Pres1,Pres2)
rm(Pres1)
rm(Pres2)

Prestadores <- Prestadores[order(Prestadores$depa_nombre,Prestadores$muni_nombre ),]

## Estructura de la tabla
str(Prestadores)
## Nombres de las columnas
names(Prestadores) 

# Se crea la tabla de Prestadores dentro de la base de datos
dbWriteTable(conn = db, name = "Prestadores", value = Prestadores, row.names = FALSE)

## Se elimina la tabla al no ser requerida
rm(Prestadores)

# Listado de tablas en db
dbListTables(db)
#dbReadTable(db,"Municipios")
#dbReadTable(db,"Prestadores")

# Se cierra la conexion
dbDisconnect(db)
rm(db)
