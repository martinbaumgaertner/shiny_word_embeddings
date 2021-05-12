library(doc2vec)
library(tidyverse)
library(word2vec)
library(lubridate)
library(umap)

doc2vec_package<-function(path){
  name=strsplit(path,split="/") %>% 
    unlist() %>% 
    tail(1) %>% 
    str_split(pattern="\\.",simplify = T) %>% 
    .[,1]
  model <- read.paragraph2vec(file = paste0(path))
  word_embedding <- as.matrix(model, which = "words")
  doc_embedding <- as.matrix(model, which = "docs")
  saveRDS(word_embedding,paste0("bs4dash/data/models_matrix/","wordemb_cb_",name,".Rds"))
  saveRDS(doc_embedding,paste0("bs4dash/data/models_matrix/","docemb_cb_",name,".Rds"))
}

word2vec_package<-function(path){
  name=strsplit(path,split="/") %>% 
    unlist() %>% 
    tail(1) %>% 
    str_split(pattern="\\.",simplify = T) %>% 
    .[,1]
  model <- read.word2vec(file = paste0(path))
  word_embedding <- as.matrix(model, which = "words")
  saveRDS(word_embedding,paste0("bs4dash/data/models_matrix/","wordemb_cb_",name,".Rds"))
}

glove_cb<-function(path){
  name=strsplit(path,split="/") %>% 
    unlist() %>% 
    tail(1) %>% 
    str_split(pattern="\\.",simplify = T) %>% 
    .[,1]
  word_embedding<-readRDS(paste0(path)) %>% 
    remove_rownames() %>%
    column_to_rownames(var = 'word') %>% 
    as.matrix()
  saveRDS(word_embedding,paste0("bs4dash/data/models_matrix/","wordemb_cb_",name,".Rds"))
}

textdata_glove<-function(){
  word_embedding<-textdata::embedding_glove6b(dimensions=300) %>%
    remove_rownames() %>%
    column_to_rownames(var = 'token') %>% 
    as.matrix()
  
  saveRDS(word_embedding,paste0("bs4dash/data/models_matrix/","wordemb_web_textdata_glove300.Rds"))
}

word2vec_web<-function(){
  word_embedding<-read_table2("bs4dash/data/models/GoogleNews-vectors-negative300-SLIM.txt", 
              col_names = FALSE, skip = 1) %>% 
    .[-234111,] %>% #remove word INVICTUS because of data error
    rename_at(vars(paste0("X",2:301)), function(x) paste0("V",1:300)) %>% 
    rename(word=X1)%>%
    remove_rownames() %>%
    column_to_rownames(var = 'token') %>% 
    as.matrix()
  
  saveRDS(word_embedding,paste0("bs4dash/data/models_matrix/","wordemb_web_google_word2vec.Rds"))
}
  
apply(list.files("bs4dash/data/models") %>% 
        as_tibble() %>% 
        filter(str_detect(value,"doc2vec")) %>% 
        mutate(value=paste0("bs4dash/data/models/",value)),
      1,doc2vec_package)
apply(list.files("bs4dash/data/models/") %>% 
        as_tibble() %>% 
        filter(str_detect(value,"word2vec")) %>% 
        mutate(value=paste0("bs4dash/data/models/",value)),
      1,word2vec_package)
apply(list.files("bs4dash/data/models/") %>% 
        as_tibble() %>% 
        filter(str_detect(value,"glove")) %>% 
        mutate(value=paste0("bs4dash/data/models/",value)),
      1,glove_cb)
# textdata_glove()
# word2vec_web()

doc_data<-lapply(list.files("bs4dash/data/models_matrix/",full.names = T,pattern="docemb"),
                 function (x) readRDS(x))
names(doc_data)<-str_remove_all(list.files("bs4dash/data/models_matrix/",pattern="docemb"),".Rds")
uma<-umap(doc_data[[1]])
docs<-uma$layout %>% 
  as_tibble(rownames = "doc_id")

dataset<-readRDS("bs4dash/data/models/dataset.Rds")

umap_data<-left_join(docs,dataset,by="doc_id") %>% 
  filter(language=="en") %>% 
  select(-title,-speaker_position,-event,-text,-text_colloc,-language,-location,-release_date,-country_code,-currency_code,-chapter,-link) %>% 
  mutate(year=year(date))

saveRDS(umap_data,paste0("bs4dash/data/models/umap_data.Rds"))

bs4dash

