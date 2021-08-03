######################################
# ANALIZANDO MENSAJE DE LA NACION 2021
#####################################



rm(list = ls())

setwd("D:/Proyectos/8. Mensaje de la Nacion 2021/")

#library(Rcpp)
library(pdftools)
library(lubridate)
library(readr)
library(rtweet)
library(tidyverse)
library(lubridate)
library(igraph)

#install.packages("ggraph")
library(ggraph)
library(tm)
library(SnowballC)
library(wordcloud2)
library(dplyr)
library(readr)
library(dplyr)

#install.packages("wordcloud2")
#install.packages("ggraph")

mensaje <- pdf_text("Mensaje_a_la_nacion_presidente_Pedro_Castillo.pdf.pdf")
#mensaje



# Limpieza ----------------------------------------------------------------


mensaje <-  mensaje %>% 
  paste0(., collapse = " ") 


#eliminar los numero de paginas y salto de linea


mensaje<-gsub("[[:digit:]]*\\\r", "", mensaje)
#mensaje

mensaje<-gsub("\\\n", " ", mensaje)
#mensaje


#eliminar el titulo:
mensaje <- gsub('DISCURSO DE ASUNCIÓN DEL PRESIDENTE DE LA REPÚBLICA,                 JOSÉ PEDRO CASTILLO TERRONES                         28 DE JULIO DE  ','',mensaje)

#Eliminar espacios en blancos, tabs, etc
mensaje = gsub("[ |\t]{2,}", " ", mensaje)  # Remove tabs
mensaje = gsub("^ ", "", mensaje)  # Leading blanks
mensaje = gsub(" $", "", mensaje)  # Lagging blanks
mensaje = gsub(" +", " ", mensaje) # General spaces 
mensaje = gsub("[[:cntrl:]]", " ", mensaje) # saltos de linea y tabulaciones


#eliminar stopword
mensaje = tolower(mensaje) #convertimos todo a minuscula
mensaje = removeWords(mensaje, words = stopwords("spanish"))


library(readxl)
stopwords_es_1 = read_excel("CustomStopWords.xlsx")
names(stopwords_es_1) = c("Token","Fuente")
stopwords_es_2 = tibble(Token=tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es = rbind(stopwords_es_1, stopwords_es_2)
stopwords_es = stopwords_es[!duplicated(stopwords_es$Token),]
remove(stopwords_es_1, stopwords_es_2)

stopwords_es[sample(nrow(stopwords_es),size = 10, replace = F),]


#no voy a eliminar los puntos para poder sacar los bigramas
#mensaje = removePunctuation(mensaje)

a <- stopwords("spanish")
a <- data.frame(a)

mensaje = removeNumbers(mensaje) 
mensaje = stripWhitespace(mensaje)

mensaje<-gsub("^[[:alnum:]]*", "", mensaje)

#mensaje<-gsub("([.])[[:punct:]]", "", mensaje,perl=T)


#mensaje2 <- mensaje



# Conteo de palabras ------------------------------------------------------

#bag of word

library(tidytext)

mensaje <- data.frame(mensaje_text=mensaje)

#eliminar mas stopword

# token_mensaje = token_mensaje %>%
#   anti_join(stopwords_es)

#stemm
token_mensaje0 <- mensaje %>% unnest_tokens(output = Token, input = mensaje_text)  %>%
  anti_join(stopwords_es)
token_mensaje0 <- token_mensaje0 %>% 
  mutate(stem = wordStem(Token, "spanish"))
token_mensaje0

token_mensaje0$Token <- token_mensaje0$stem

token_mensaje <- token_mensaje0[,-2] 
token_mensaje <- data.frame(Token=token_mensaje)



#lemma
#se demora y tuve problema con una la palabra raiz sustantivo suma
# lemma -------------------------------------------------------------------

ini <- Sys.time()

library(stringi)
library(rvest)
lematiza = function( frase ){
  #frase <- 'Bolivia'
  palabra = gsub( " ", "+", frase )
  base.url = paste0( 
    "https://www.lenguaje.com/cgi-bin/lema.exe?edition_field=", 
    palabra,"&B1=Lematizar")
  Sys.sleep(1)
  lemma = read_html(base.url, encoding = "latin1") %>% 
    html_node(css = "div div div div div li") %>% 
    html_text(trim = T)
  #Sys.sleep(2)
  #lemma <- gsub("Ã","i",lemma)
  #lemma <- gsub('"',"",lemma)
  #lemma <- gsub('Raiz del sustantivo suma.',"nose",lemma)
  
  #Encoding(lemma)
  #lemma <- iconv(lemma, from = "UTF-8",to = "latin1")
  
  #length(lemma)
  #lemma = ifelse(lemma=='RaÃz del sustantivo \"suma\"',frase,lemma)
  lemma = ifelse(substr(lemma,22,30)=='\"suma\".',frase,lemma)
  
  return(lemma)
  
  
}
lematiza('comimos')
lematiza('Bolivia')

token_mensaje0 <- mensaje %>% unnest_tokens(output = Token, input = mensaje_text)
token_mensaje0 <- token_mensaje0 %>% mutate(stem = wordStem(Token, "spanish"))
#token_mensaje0


token_mensaje0 <- token_mensaje0 %>% 
  # tail(10) %>% 
  mutate(lemma_dict = sapply(Token,lematiza)) %>%
  select(Token, stem, lemma_dict)

fin <- Sys.time()

tmpo_lemma <- fin - ini
tmpo_lemma

#demoro 1.8horas

#write_rds(token_mensaje0,"token_mensaje.rds")

#token_mensaje0 <- read_rds("token_mensaje.rds")

token_mensaje0 %>% group_by(lemma_dict) %>%
  summarise(cant = n()) %>% arrange(-cant)


token_mensaje <- token_mensaje0[,3]
token_mensaje <- data.frame(Token=token_mensaje)

#guardar base



# Frecuencia de palabras --------------------------------------------------

#tabla para hacer los graficos
tb1 <- token_mensaje %>% group_by(Token) %>%
  summarise(count=n()) #%>% subset(count > 13)

boxplot(tb1$count)
summary(tb1$count)


png(filename="Imagen/FreqWord_PC2021.png",width=1000, height=700)
#tabla de frecuencia
tb1  %>% subset(count > 13) %>% ggplot(aes(reorder(Token, count),
                                           count,label = count,fill=count)) + 
  geom_bar(stat = 'identity') +coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  geom_text(aes(label=count), hjust=1.5, colour="white", size=4.5)+
  theme(axis.text = element_text(size=11.5))+
  labs(
    x = NULL, y = NULL,
    title = "Mensaje de la Nación 2021 - Pedro Castillo",
    subtitle = "Palabras más usadas (> 13 veces)",
    caption = "\nFuente: PDF - https://www.gob.pe/institucion/presidencia/mensajes-a-la-nacion"
  )

tb1
dev.off()


# Nube de palabras --------------------------------------------------------

#nube de palabras

wordcloud2(data=tb1, color='random-dark',shape = "circle",size = .35)



# RAKE --------------------------------------------------------------------


library(udpipe)

#token_mensaje0 <- mensaje %>% unnest_tokens(output = Token, input = mensaje_text)
token_mensajekey <- token_mensaje %>% mutate(presidente = 'PedroCastillo')

mensaje_keywords = keywords_rake(x = token_mensajekey, term = "Token", group = "presidente", relevant = token_mensajekey$Token, ngram_max = 3,n_min = 4)

head(mensaje_keywords)




# LDA ---------------------------------------------------------------------


mensaje3 <- VCorpus(VectorSource(token_mensaje$Token))
#mensaje3 <- Corpus(VectorSource(token_mensaje$Token))
mensaje3 <- VCorpus(VectorSource(mensaje$mensaje_text))

### limpieza usando la librerÃ?a TM ### 
mensaje4 <- tm_map(mensaje3, removeWords, stopwords("spanish"))
mensaje4 <- tm_map(mensaje4, removeNumbers)



mensaje4 <- tm_map(mensaje4, stemDocument)

#Nube de Palabras
dtm = DocumentTermMatrix(mensaje4)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 

freq = colSums(as.matrix(dtm))
length(freq)

plot = data.frame(words = names(freq), count = freq)



#plot = subset(plot, plot$count > 20) #creating a subset of words having more than 100 frequency

# wordcloud2::letterCloud(plot,word = "Joy + Bangla", wordSize = 0.5,color='random-light' , backgroundColor="black")
# wordcloud2( plot,figPath = "corazon.png",size = 1,backgroundColor = "black")

library(ggrepel)
library(topicmodels)
library(ldatuning)

#install.packages("ldatuning")

# Selección del número óptimo de tópicos
necronomicon_ntopics = FindTopicsNumber(dtm,
                                        topics = seq(from = 3, to = 10, by = 1),
                                        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                        method = "Gibbs",
                                        control = list(seed = 123),
                                        mc.cores = 4,
                                        verbose = TRUE)


FindTopicsNumber_plot(necronomicon_ntopics)

#install.packages("topicmodels")
ini <- Sys.time()

library(topicmodels)
#LDA model with 5 topics selected
lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 5 topics selected
lda_4 = LDA(dtm, k = 4, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 6 topics selected
lda_6 = LDA(dtm, k = 6, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 2 topics selected
lda_2 = LDA(dtm, k = 2, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 10 topics selected
lda_10 = LDA(dtm, k = 10, method = 'Gibbs', 
             control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                            thin = 500, burnin = 4000, iter = 2000))
fin <- Sys.time()

tespera <- fin - ini
tespera

#Top 10 terms or words under each topic
top10terms_4 = as.matrix(terms(lda_4,10))
top10terms_5 = as.matrix(terms(lda_5,10))
top10terms_2 = as.matrix(terms(lda_2,10))
top10terms_10 = as.matrix(terms(lda_10,10))
top10terms_6 = as.matrix(terms(lda_6,10))


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}



top10terms_5
top10terms_6

write.excel(top10terms_5)
write.excel(top10terms_6)

#install.packages("tidyverse",dependencies = T)
#install.packages("tidyr",dependencies = T)

#install.packages("tidyverse",dependencies = T)
library(tidyverse)
library(tidyr)
library(tidytext)
#install.packages("dplyr")
# Obtención de matriz beta
necronomicon_beta_lda = tidy(lda_5, matrix = "beta")

png(filename="Imagen/LDA_Topic_5PC2021.png",width=1000, height=700)

# Gráfico de palabras más probables por tópico
necronomicon_beta_lda %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ggplot(aes(x=reorder_within(term, beta, topic), beta, fill = as.factor(topic))) +
  scale_x_reordered()+
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  coord_flip()+
  labs(x="Palabra")+
  theme(text=element_text(size=11))
dev.off()


#para 6

necronomicon_beta_lda = tidy(lda_6, matrix = "beta")

png(filename="Imagen/LDA_Topic_6PC2021.png",width=1000, height=700)

# Gráfico de palabras más probables por tópico
necronomicon_beta_lda %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ggplot(aes(x=reorder_within(term, beta, topic), beta, fill = as.factor(topic))) +
  scale_x_reordered()+
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  coord_flip()+
  labs(x="Palabra")+
  theme(text=element_text(size=11))
dev.off()


# BIGRAMA -----------------------------------------------------------------

#bigrama

bigram_mensaje <- mensaje %>% unnest_tokens(output = Token, input = mensaje_text, token = 'ngrams', n=2, to_lower = F) #%>%



library(igraph)

bigrama_grafo_mensaje =bigram_mensaje %>% mutate(presidente = 'PedroCastillo') %>%
  separate(Token, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% c(stopwords_es$Token)) %>%
  filter(!palabra2 %in% c(stopwords_es$Token)) %>%
  
  count(palabra1, palabra2, sort = TRUE) %>% 
  filter(n >= 4) 

bigrama_grafo_mensaje2 <- bigrama_grafo_mensaje %>%
  graph_from_data_frame()


bigrama_grafo_mensaje2

write.excel(bigrama_grafo_mensaje)

bigrama_grafo_mensaje$bigrama <- paste0(bigrama_grafo_mensaje$palabra1," ",
                                        bigrama_grafo_mensaje$palabra2)


bigrama_grafo_mensaje

png(filename="Imagen/Bigrama_PC2021.png",width=1000, height=700)

p1 <-bigrama_grafo_mensaje %>% ggplot(aes(reorder(bigrama, n),
           n,label = n,fill=n)) + 
  geom_bar(stat = 'identity') +coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  geom_text(aes(label=n), hjust=1.5, colour="white", size=4.5)+
  theme(axis.text = element_text(size=11.5))+
  labs(
    x = NULL, y = NULL,
    title = "Bigramas más usados",
    
    caption = "\nElaboracion: EdwinChirre")
p1
dev.off()



# ini <- sys.time()
# set.seed(1234)
# 
# a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
# 
# ggraph(bigrama_grafo_mensaje2, layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
#                  arrow = a, end_cap = circle(.07, 'inches')) +
#   geom_node_point(color = "lightblue", size = 3) +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#   theme_void()
# 
# fin <- sys.time()
# tm_bigram <- fin - ini

#ojo para bi grama y tri grama no se borra los stopwords


#trigrama

trigram_mensaje <- mensaje %>% unnest_tokens(output = Token, input = mensaje_text, token = 'ngrams', n=3, to_lower = F) #%>%

library(igraph)

trigrama_grafo_mensaje =trigram_mensaje %>% mutate(presidente = 'PedroCastillo') %>%
  separate(Token, c("palabra1", "palabra2","palabra3"), sep = " ") %>%
  filter(!palabra1 %in% c(stopwords_es$Token)) %>%
  filter(!palabra2 %in% c(stopwords_es$Token)) %>%
  filter(!palabra3 %in% c(stopwords_es$Token)) %>%
  
  count(palabra1, palabra2,palabra3, sort = TRUE) %>% 
  filter(n >= 2) 

trigrama_grafo_mensaje1 <- trigrama_grafo_mensaje %>%
  graph_from_data_frame()


trigrama_grafo_mensaje1

write.excel(trigrama_grafo_mensaje)


trigrama_grafo_mensaje$trigrama <- paste0(trigrama_grafo_mensaje$palabra1," ",
                                         trigrama_grafo_mensaje$palabra2," ",trigrama_grafo_mensaje$palabra3)


trigrama_grafo_mensaje


png(filename="Imagen/Trigrama_PC2021.png",width=1000, height=700)

p2<-trigrama_grafo_mensaje %>% ggplot(aes(reorder(trigrama, n),
                                     n,label = n,fill=n)) + 
  geom_bar(stat = 'identity') +coord_flip() +
  theme_minimal() +  
  theme(plot.title = element_text(face = "bold", size = 12)) +
  geom_text(aes(label=n), hjust=1.5, colour="white", size=4.5)+
  theme(axis.text = element_text(size=11.5))+
  labs(
    x = NULL, y = NULL,
    title = "Trigramas más usados",
    
    caption = "\nElaboracion: EdwinChirre") 
p2
dev.off()



# ANALISIS DE SENTIMIENTOS ------------------------------------------------

#antes del steam y lemma


download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()



#### Cambio de nombre para poder hacer join con las palabras de los tweets
names (afinn)[1] = "lemma_dict"

mensaje_afinn = token_mensaje0 %>%
  inner_join(afinn, ., by = "lemma_dict") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))


tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

png(filename="Imagen/AnalisisSentimiento_PC2021.png",width=1000, height=700)

mensaje_afinn %>%
  group_by(Tipo) %>%
  summarise(n=n()) %>% 
  ggplot() +
  aes(Tipo, (n/nrow(mensaje_afinn))*100, fill = Tipo) +
  geom_col() + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Postivos / Negativos  Palabras utilizadas en el discurso",
       subtitle =paste0(round((nrow(mensaje_afinn) / nrow(token_mensaje0))*100),"% palabras analizadas"))+ tema_graf +
  ylab("% de palabras") +
  xlab ("Sentimiento")

dev.off()

png(filename="Imagen/AnalisisSentimiento_puntutacion_PC2021.png",width=1000, height=700)


mensaje_afinn %>%  group_by(lemma_dict) %>%
  
  summarize(occurences = n(),
            
            Puntuacion = sum(Puntuacion)) %>%
  
  top_n(20, abs(Puntuacion)) %>%
  
  mutate(Palabras = reorder(lemma_dict, Puntuacion)) %>%
  
  head(20) %>%
  
  ggplot(aes(Palabras, Puntuacion, fill = Puntuacion > 0)) +
  
  geom_col(show.legend = FALSE) +
  
  coord_flip() + theme_bw() + ggtitle("Analisis de sentimientos - Puntuaciones de las palabras")
dev.off()


png(filename="Imagen/AnalisisSentimiento_palabras_PC2021.png",width=1000, height=700)

mensaje_afinn %>% 
  count(lemma_dict, Tipo, sort = TRUE) %>%
  ungroup() %>%
  
  select(lemma_dict,Tipo,n) %>%
  group_by(Tipo) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(Palabra = reorder(lemma_dict,n)) %>%
  ggplot(aes(Palabra, n, fill = Tipo)) +
  geom_col(show.legend = FALSE) + #title("Palabras por tipo de sentimiento") +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(y = "Palabras por tipo de sentimiento",
       x = NULL) +
  coord_flip()

dev.off()


# LDA final ---------------------------------------------------------------

necronomicon_beta_lda = tidy(lda_5, matrix = "beta")

necronomicon_beta_lda$topic <- ifelse(
  necronomicon_beta_lda$topic == 1,
  "Salud integral y mejor",
  ifelse(
    necronomicon_beta_lda$topic == 2,
    "Plan económico y créditos",
    ifelse(
      necronomicon_beta_lda$topic == 3,
      "Asamblea Constituyente",
      ifelse(
        necronomicon_beta_lda$topic == 4,
        "Educación, cultura y nuevo ministerio",
        "Gobierno descentralizado"
        
      )
      
    )
    
  )
)

png(filename="Imagen/LDA_Topic_5_finalPC2021.png",width=1000, height=700)

# Gráfico de palabras más probables por tópico
necronomicon_beta_lda %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ggplot(aes(x=reorder_within(term, beta, topic), beta, fill = as.factor(topic))) +
  scale_x_reordered()+
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  coord_flip()+
  labs(x="Palabra")+
  theme(text=element_text(size=11))
dev.off()

################### FIN 



