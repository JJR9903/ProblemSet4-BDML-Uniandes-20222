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
pacman::p_load(stopwords,emoji)

# importar los datos ------------------------------------------------------


Train<-read.csv(file = file.path("stores/train.csv"))

Test<-read.csv(file = file.path("stores/test.csv"))



### Pre-processing ### ----------------------------------------------------------


# Normalizar texto --------------------------------------------------------

Train<-Train%>%
  mutate(text = toupper(text),
         text = stri_trans_general(str=text,id= "Latin-ASCII"),
         text = gsub("[0-9]+","",text),
         text = gsub("\n"," ",text),
         text = gsub("[::punct::]","",text),
         text = gsub(" HTTPS.+","",text),
         text = gsub("\\s\\s"," ",text),
         text = gsub("#","",text),
         emoji_in_text = ifelse(emoji_detect(text, negate = FALSE),1,0),
         emoji_count = emoji_count(text),
         emoji = emoji_extract(text),
         #text = gsub(emoji,"",text),
         )

Train$text[1000]

Most_frequent_words <- Train %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(get_stopwords("es"),"word")%>%
  count(word, sort = TRUE)


count_words <- Train %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(get_stopwords("es"),"word")%>%
  count(word, name) %>%
  complete(word, name, fill = list(n = 0))

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
