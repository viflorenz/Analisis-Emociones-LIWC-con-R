##Extraccion de datos y bbdd
library(rtweet)
#guardar llaves api
api_key <- "XXX"
api_secret_key <- "XXX"
access_token <- "XXX"
access_token_secret <- "XXX"

#autenticacion 
token <- create_token(
  app = "Taller Telar",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

get_token()

#Extraccion datos
usuario <- c("gabrielboric") #acá se pueden poner múltiples usuarios
tuits <- get_timeline(usuario, #usuario
                      n=300,   #cantidad de tuits
                      include_rts = FALSE,retryonratelimit = TRUE)

tweets.df <- as.data.frame(tuits) #dataframe original

library(tidyverse)
#Construccion BBDD
#limpiar texto
tweets.df$full_text <- tolower(tweets.df$full_text) #a minuscula
tweets.df$full_text <- gsub("[[:punct:]]", "", tweets.df$full_text) #sacar puntuacion, se mantienen las palabras con tilde
tweets.df$full_text <- gsub("[[:digit:]]", "", tweets.df$full_text) #sacar numeros
tweets.df$full_text <- gsub("\\s+", " ", str_trim(tweets.df$full_text)) #sacar espacios extra
tweets.df$full_text <- gsub("http.*","",tweets.df$full_text)
tweets.df$full_text <- gsub("https.*","",tweets.df$full_text)#sacar links
tweets.df$full_text <- gsub("#\\w+","",tweets.df$full_text)
tweets.df$full_text <- gsub("@\\w+","",tweets.df$full_text)#sacar los hashtags y usuarios
tweets.df$full_text <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets.df$full_text)#sacar textos con numeros
tweets.df$full_text <- gsub("[^\x20-\x7E,á,é,í,ó,ú,ñ]", "", tweets.df$full_text) #linea anterior se desconfiguro, este es el de github
#View(tweets.df)

tweets.df <- select(tweets.df, created_at, id_str, full_text)

#Filtrar por palabras clave
# BBDD <- tweets.df %>% 
#   filter(str_detect(full_text, "convencionconstitucional|convenciónconstitucional|convención|convencion|constitucional|constitu
# yentes|constituyente|convencionconstituyente|
# convenciónconstituyente|nuevaconstitucion|nuevaconstitución|nueva constitución|nueva
# constitucion|constitución|constitucion|proceso constituyente|procesoconstituyente|convención
# constituyente|convención constitucional|convencion constituyente|convencion
# constitucional|reforma
# constitucional|convencioncl|convencióncl|convencionales|constitucionales|chileconstitucion|chileco
# nstitución|proceso constitucional|chileconstituyente|chileconvencion|chileconvención|chile
# constituyente|reformaconstitucional")) %>%
#   filter(!grepl("acusación constitucional",full_text)) %>%
#   filter(!grepl("tribunal constitucional",full_text))

##Analisis de emociones
#Emolex
#http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#install.packages("syuzhet")
library(syuzhet)
library(quanteda)

word.df <- as.vector(tweets.df) #agrupa todas las observaciones por cada variable

nrc <- get_nrc_sentiment(word.df$full_text, #aplicacion emolex
                                language = "spanish")

nrc <- cbind(tweets.df, nrc) 

corpus <- corpus(tweets.df$full_text) #dataframe a corpus
corpus_tokenizado <- tokens(corpus, what = "word") #sin más argumentos pq limpiamos antes

#tokenizar para el WC y sacar %
toks <- quanteda::tokens(corpus_tokenizado, split_hyphens = TRUE)

# WC (word count)
WC <- quanteda::ntoken(toks)

nrc.df <- cbind(nrc,WC)
nrc.df <- nrc.df %>% 
  relocate(WC, .before=anger) |> 
  select(created_at, full_text, WC, negative, positive)

#eliminar hora en created_at
nrc.df <- nrc.df |> 
  separate(created_at,sep = " ",into = c("created_at","eliminar")) |> 
  select(-c("eliminar"))

library(lubridate)#especificar columna date
nrc.df$created_at <- as_date(nrc.df$created_at)

#bbdd suma diarios 
sumaWCnrc <- nrc.df |> 
  group_by(created_at) |> 
  summarise(WC = sum(WC))

sumaEmoPosnrc <- nrc.df |> 
  group_by(created_at) |> 
  summarise(Positive = sum(positive))

sumaEmoNegnrc<- nrc.df |> 
  group_by(created_at) |> 
  summarise(Negative = sum(negative))

nrcsum <- inner_join(sumaWCnrc, sumaEmoPosnrc)
nrcsum <- inner_join(nrcsum, sumaEmoNegnrc)


#%s
nrcsum <- nrcsum |> 
  mutate(Positive_por = (Positive)/WC) |> 
  mutate(Negative_por = (Negative)/WC) 

#descargar archivo proporción diaria emolex
write.csv(nrcsum, "Analisis emociones Emolex.csv") #csv
library(writexl)
write_xlsx(nrcsum, "Analisis emociones Emolex.xlsx") #excel

library(scales)#especificar %s
# nrcsum$Positive_por  <- round(nrcsum$Positive_por, digits = 2)
# nrcsum$Negative_por  <- round(nrcsum$Negative_por, digits = 2)

#peaks fechas
#Positive_por: 2022-09-03; 2022-10-02; 2022-09-19
#Negative_por: 2022-09-13; 2022-08-31; 2022-08-27

fecha_pos <- which(nrcsum$created_at %in% as.Date(c("2022-09-03","2022-10-02","2022-09-19")))
fecha_neg <- which(nrcsum$created_at %in% as.Date(c("2022-09-13","2022-08-31","2022-08-27")))


ggplot(nrcsum, aes(x = created_at)) + 
  geom_line(aes(y = round(Positive_por,2), color = 'Emociones Positivas',group=1)) + 
  geom_line(aes(y = round(Negative_por,2), color = 'Emociones Negativas',group=2)) +
  labs(title="Análisis de Emociones de la cuenta de tuiter del presidente @gabrielboric",
       subtitle="Análisis diario con el diccionario NRC \nFechas peak",
       y="Porcentaje emociones",
       x="Día") + 
  scale_y_continuous(labels = percent)+
  geom_vline(xintercept= as.numeric(nrcsum$created_at[fecha_pos]),
             color = "dark blue", size = 1)+
  annotate("text", x = as.Date("2022-09-02") , y = -0.005, label = "3/9", angle=90, size=3)+
  annotate("text", x = as.Date("2022-10-01") , y = -0.005, label = "2/10", angle=90, size=3)+
  annotate("text", x = as.Date("2022-09-18") , y = -0.005, label = "19/9", angle=90, size=3)+
  
  geom_vline(xintercept= as.numeric(nrcsum$created_at[fecha_neg]),
             color = "dark red", size = 1)+
  annotate("text", x = as.Date("2022-09-12") , y = -0.005, label = "13/9", angle=90, size=3)+
  annotate("text", x = as.Date("2022-08-31") , y = -0.005, label = "1/9", angle=90, size=3)+
  annotate("text", x = as.Date("2022-08-26") , y = -0.005, label = "27/10", angle=90, size=3)


##Analisis de emociones
#LIWC
#install.packages("quanteda")
library(quanteda)
#subir diccionario desde el wd y leerlo como tal
liwcdict <- dictionary(file = "C:/Users/viflo/Documents/Viflorenz/Códigos R/Spanish_LIWC2007_Dictionary.txt",
                       format = "LIWC")

corpus <- corpus(tweets.df$full_text) #dataframe a corpus

corpus_tokenizado <- tokens(corpus, what = "word") #sin más argumentos pq limpiamos antes

dfm <- dfm(corpus_tokenizado, remove = stopwords("spanish"))

#aplicacion del diccionario
liwcdfm <- dfm_lookup(dfm, dictionary = liwcdict) 
liwcdfm.df <- as.data.frame(liwcdfm)
liwcdfm.df <- select(liwcdfm.df, c(Afect, EmoPos, EmoNeg))#columnas de interés
liwcdfm.df <- cbind(tweets.df,liwcdfm.df)#combinar la bbdd de texto c/ analisis

#tokenizar para el WC y sacar %
toks <- quanteda::tokens(corpus_tokenizado, split_hyphens = TRUE)

# WC (word count)
WC <- quanteda::ntoken(toks)

liwcdfm.df <- cbind(liwcdfm.df,WC)
liwcdfm.df <- liwcdfm.df %>% 
  relocate(WC, .before=Afect) 
#View(liwcdfm.df)#hasta acá, análisis de emociones (liwc) con puntuaciones listo

#Análisis de emociones porcentual por dia
#le cambie el nombre a liwcdfm.df para no reescribir lo anterior por error

#eliminar hora en created_at
bbdd <- liwcdfm.df |> 
  separate(created_at,sep = " ",into = c("created_at","eliminar")) |> 
  select(-c("eliminar"))

library(lubridate)#especificar columna date
bbdd$created_at <- as_date(bbdd$created_at)

#bbdd suma diarios 
sumaWC <- bbdd |> 
  group_by(created_at) |> 
  summarise(WC = sum(WC))

sumaAfect <- bbdd |> 
  group_by(created_at) |> 
  summarise(Afect = sum(Afect))

sumaEmoPos <- bbdd |> 
  group_by(created_at) |> 
  summarise(EmoPos = sum(EmoPos))

sumaEmoNeg<- bbdd |> 
  group_by(created_at) |> 
  summarise(EmoNeg = sum(EmoNeg))

bbddsum <- inner_join(sumaWC, sumaAfect)
bbddsum <- inner_join(bbddsum, sumaEmoPos)
bbddsum <- inner_join(bbddsum, sumaEmoNeg)

#%s
bbddsum <- bbddsum |> 
  mutate(Afect_por = (Afect)/WC) |> 
  mutate(EmoPos_por = (EmoPos)/WC) |> 
  mutate(EmoNeg_por = (EmoNeg)/WC)


library(scales)#especificar %s
bbddsum$Afect_por  <- round(bbddsum$Afect_por, digits = 2)
bbddsum$EmoPos_por  <- round(bbddsum$EmoPos_por, digits = 2)
bbddsum$EmoNeg_por  <- round(bbddsum$EmoNeg_por, digits = 2)

#graficar
ggplot(bbddsum, aes(x = created_at)) + 
  geom_line(aes(y = EmoPos, color = 'Emociones Positivas',group=1)) + 
  geom_line(aes(y = EmoNeg, color = 'Emociones Negativas',group=2)) +
  labs(title="Análisis de Emociones de la cuenta de tuiter del presidente @gabrielboric",
       subtitle="Análisis diario con el diccionario LIWC",
       y="Frecuencia emociones",
       x="Día")

ggplot(bbddsum, aes(x = created_at)) + 
  geom_line(aes(y = EmoPos_por, color = 'Emociones Positivas',group=1)) + 
  geom_line(aes(y = EmoNeg_por, color = 'Emociones Negativas',group=2)) +
  labs(title="Análisis de Emociones de la cuenta de tuiter del presidente @gabrielboric",
       subtitle="Análisis diario con el diccionario LIWC",
       y="Porcentaje emociones",
       x="Día") + 
  scale_y_continuous(labels = percent)

#peaks fechas
#emopos_por: 2022-09-13; 2022-08-20; 2022-09-04
#emoneg_por: 2022-10-02; 2022-09-01; 2022-08-22

fecha_emopos <- which(bbddsum$created_at %in% as.Date(c("2022-09-13","2022-08-20","2022-09-04")))
fecha_emoneg <- which(bbddsum$created_at %in% as.Date(c("2022-10-02","2022-09-01","2022-08-22")))


ggplot(bbddsum, aes(x = created_at)) + 
  geom_line(aes(y = EmoPos_por, color = 'Emociones Positivas',group=1)) + 
  geom_line(aes(y = EmoNeg_por, color = 'Emociones Negativas',group=2)) +
  labs(title="Análisis de Emociones de la cuenta de tuiter del presidente @gabrielboric",
       subtitle="Análisis diario con el diccionario LIWC \nFechas peak",
       y="Porcentaje emociones",
       x="Día") + 
  scale_y_continuous(labels = percent)+
  geom_vline(xintercept= as.numeric(bbddsum$created_at[fecha_emopos]),
             color = "dark blue", size = 1)+
  annotate("text", x = as.Date("2022-09-12") , y = -0.005, label = "13/9", angle=90, size=3)+
  annotate("text", x = as.Date("2022-08-19") , y = -0.005, label = "20/8", angle=90, size=3)+
  annotate("text", x = as.Date("2022-09-03") , y = -0.005, label = "4/9", angle=90, size=3)+
  
  geom_vline(xintercept= as.numeric(bbddsum$created_at[fecha_emoneg]),
             color = "dark red", size = 1)+
  annotate("text", x = as.Date("2022-10-01") , y = -0.005, label = "2/10", angle=90, size=3)+
  annotate("text", x = as.Date("2022-08-31") , y = -0.005, label = "1/9", angle=90, size=3)+
  annotate("text", x = as.Date("2022-08-21") , y = -0.005, label = "22/10", angle=90, size=3)
