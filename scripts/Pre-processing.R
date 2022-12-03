## Autores: Juan José Rincón , Juan Andres Ospina, Juanita Chacón 
## Descripción: Desarrollo 4 problem set /Big Data and Machine Leanring for applied economics
## Universidad de los Andes 2022-2
## Creation Date: 26/11/2022
####################################

#### setting the work space #### 

rm(list=ls())

dir_set <- function(){
  if(Sys.info()["user"]=="JuanJose"){
    setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Problems Set/ProblemSet4-BDML-Uniandes-20222")
  }
  else if(Sys.info()["user"]=="PC-PORTATIL"){
    setwd("C:/Users/PC-PORTATIL/OneDrive/Documentos/GitHub/ProblemSet4-BDML-Uniandes-20222")
  }
  else{
    setwd("C:/Users/ja.ospinap/Downloads/ProblemSet4-BDML-Uniandes-20222")
  }
}


dir_set()
pacman::p_load(tidyverse,rio,glue,hexbin,patchwork,vip,ggrepel,stringi,tidytext)
pacman::p_load(stopwords,emoji,stringi,tm,rvest)



# importar los datos ------------------------------------------------------


Train<-read.csv(file = file.path("stores/train.csv"))

Test<-read.csv(file = file.path("stores/test.csv"))



### Pre-processing ### ----------------------------------------------------------

stopwords_es <- stopwords::stopwords("es", source = "snowball")
stopwords_es <- tolower(stopwords_es)
stopwords_es <- stri_trans_general(str = stopwords_es, id = "Latin-ASCII")


# Vamos a crear una función para lematizar (que es lenta como un hpta)
lematiza = function(frase){
  print(frase)
  # Se reemplazan los espacios con +
  query <- gsub(" ", "+", frase)
  url_base <- "https://www.lenguaje.com/cgi-bin/lema.exe?edition_field="
  url_final <- paste0(url_base, query,"&B1=Lematizar")
  lemma <- read_html(url_final, encoding = "latin1") %>%
    html_nodes('div') %>% 
    tail(1) %>% 
    html_nodes("li") %>% 
    html_text2() %>% 
    tail(1)
  # lemma <- read_html(url_final, encoding = "latin1") %>% 
  #   html_node(css = "div div div div div li") %>% 
  #   html_text() 
  # lemma <- gsub("La palabra:", "", lemma)
  # lemma <- gsub("tiene los siguientes lemas:", "", lemma)
  # error <- "\r\n     Palabra no encontrada\r\n     Palabra no encontrada"
  # lemma <- ifelse(lemma == error, frase, lemma)
  if (length(lemma) == 0) {
    return(frase)
  } else {
    lemma <- str_split(lemma, "\\n")[[1]][1]
    return(lemma)
  }
}


# Normalizar texto --------------------------------------------------------

Train<-Train%>%
  mutate(text = tolower(text),
         text = stri_trans_general(str=text,id= "Latin-ASCII"),
         text = gsub(" https.+","",text),
         text = gsub("https.+","",text),
         text = removeWords(text, stopwords_es),
         text = str_replace_all(text, "[^[:alnum:]]", " "),
         text = gsub("[0-9]+","",text),
         text = gsub("\\s+"," ",text),
         #emoji_in_text = ifelse(emoji_detect(text, negate = FALSE),1,0),
         #emoji_count = emoji_count(text),
         #emoji = emoji_extract(text)
         )



Train_text <- Train %>%
  unnest_tokens(output = token, input = text)%>%
  count(token, sort = TRUE)

diccionario_lemmatizador <- data.frame(corpus = unique(Train_text$token))

diccionario_lemmatizador1<-data.frame(corpus = diccionario_lemmatizador[1:1000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador1[1,2]<-diccionario_lemmatizador1[1,1]

diccionario_lemmatizador2<-data.frame(corpus = diccionario_lemmatizador[1001:2000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador3<-data.frame(corpus = diccionario_lemmatizador[2001:3000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador4<-data.frame(corpus = diccionario_lemmatizador[3001:4000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador4<-data.frame(corpus = diccionario_lemmatizador[4001:5000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador5<-data.frame(corpus = diccionario_lemmatizador[5001:6000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador6<-data.frame(corpus = diccionario_lemmatizador[6001:7000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador7<-data.frame(corpus = diccionario_lemmatizador[7001:8000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador8<-data.frame(corpus = diccionario_lemmatizador[8001:9000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador9<-data.frame(corpus = diccionario_lemmatizador[9001:10000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador10<-data.frame(corpus = diccionario_lemmatizador[10001:11000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador11<-data.frame(corpus = diccionario_lemmatizador[11001:12000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador12<-data.frame(corpus = diccionario_lemmatizador[12001:13000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador13<-data.frame(corpus = diccionario_lemmatizador[13001:14000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador14<-data.frame(corpus = diccionario_lemmatizador[14001:15000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador15<-data.frame(corpus = diccionario_lemmatizador[15001:16000,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador16<-data.frame(corpus = diccionario_lemmatizador[16001:16835,] ) %>%
  mutate(lemma = sapply(corpus, lematiza))

diccionario_lemmatizador_append<-bind_rows(diccionario_lemmatizador1,diccionario_lemmatizador2,diccionario_lemmatizador3,diccionario_lemmatizador4,
                                           diccionario_lemmatizador5,diccionario_lemmatizador6,diccionario_lemmatizador7,diccionario_lemmatizador8,
                                           diccionario_lemmatizador9,diccionario_lemmatizador10,diccionario_lemmatizador11,diccionario_lemmatizador12,
                                           diccionario_lemmatizador13,diccionario_lemmatizador14,diccionario_lemmatizador15,diccionario_lemmatizador16)

saveRDS(diccionario_lemmatizador_append,"stores/diccionario_lemmatizador_append.rds")
##### frecuencia 

words <- Train %>%
  unnest_tokens(output = word, input = text) %>%
  filter(!word %in% stopwords_es)%>%
  filter(!word %in% c("hoy","dia","solo",))

top_words <- words %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(0:10)) %>%
  slice_max(n, n = 100) %>%
  pull(word)


count_words_ <- Most_frequent_words %>%
  count(word, name) %>%
  complete(word, name, fill = list(n = 0)) ## expandir todas las posibles combinaciones


count_words <- Train %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(get_stopwords("es"),"word")%>%
  count(word, name, sort = TRUE) %>%
  complete(word, name, fill = list(n = 0))


word_freqs <- count_words_ %>%
  group_by(name) %>%
  mutate(name_sum = sum(n),
         proportion = n / name_sum) %>%
  ungroup() %>%
  filter(word %in% top_words)






top_words_Lopez<-filter(count_words,name=="Lopez")%>%
  order_by(n)

words_by_name<-count_words %>%
  group_by(name)
  
###########

Train$text[996]

install.packages("emoji")
library(emoji)

rank <- emoji_count(Train$text)

emoji_name

emoji_extract(Train$text[952])

text =Train$text[952]
emoji = emoji_extract_all(text)
for (e in 1:length(emoji[[1]])){ text=gsub(emoji[[1]][e],"",text)}




gsub(emoji_extract(Train$text[952]),"",Train$text[952])
