
library(tidyverse)
library(shinythemes)
library(htmltools)
library(formattable)
library(shiny)
library(umap)
library(ggvis)
library(lubridate)


# data<-lapply(list.files("Shiny_word_embeddings/data/models_matrix/",full.names = T,pattern="wordemb"),
#              function (x) readRDS(x))
# names(data)<-str_remove_all(list.files("Shiny_word_embeddings/data/models_matrix/",pattern="wordemb"),".Rds")
# 
# data_doc<-lapply(list.files("Shiny_word_embeddings/data/models_matrix/",full.names = T,pattern="docemb"),
#                  function (x) readRDS(x))
# names(data_doc)<-str_remove_all(list.files("Shiny_word_embeddings/data/models_matrix/",pattern="docemb"),".Rds")
# dataset<-readRDS("Shiny_word_embeddings/data/models/dataset.Rds")

data<-lapply(list.files("data/models_matrix/",full.names = T,pattern="wordemb"),
             function (x) readRDS(x))
names(data)<-str_remove_all(list.files("data/models_matrix/",pattern="wordemb"),".Rds")

data_doc<-lapply(list.files("data/models_matrix/",full.names = T,pattern="docemb"),
             function (x) readRDS(x))
names(data_doc)<-str_remove_all(list.files("data/models_matrix/",pattern="docemb"),".Rds")

dataset<-readRDS("data/models/dataset.Rds")

umap_data<-readRDS(paste0("data/models/umap_data.Rds"))

collocations<-readRDS(paste0("data/models/collocations.Rds"))

ui <-navbarPage("Whatever it takes - to understand a central banker",
               #theme = shinytheme("slate"),
               theme = bslib::bs_theme(bootswatch = "flatly"),
               tabPanel("Similarity",
                        # tags$head(
                        #     tags$style(mycss)
                        #     ),
                        sidebarLayout(
                            sidebarPanel(width=0.3,
                                textInput("searchword", "Word", value = "inflation", width = NULL, placeholder = NULL),
                                selectInput("model", "Model:",
                                            names(data)),
                                sliderInput("top_n",
                                            "Number of Words:",
                                            min = 1,
                                            max = 20,
                                            value = 5)
                                ),
                            mainPanel(
                                formattableOutput("similarity_table")
                                )
                            )
                        ),
               tabPanel("Umap",
                        sidebarLayout(
                            sidebarPanel(width=0.2,
                                           h4("Filter"),
                                sliderInput("year", "Year released", 1977, 2021, value = c(1977, 2021),
                                            sep = ""),
                                selectInput("type", "Type",
                                            c("All",unique(umap_data$type))),
                                selectInput("cb", "central bank",
                                            c("All",unique(umap_data$cb))),
                                selectInput("currency", "currency",
                                            c("All",unique(umap_data$currency))),
                                textInput("speaker", "Speaker name (e.g., Mario Draghi)"),
                                selectInput("color", "Color",
                                            c("currency","speaker","cb","type","year"))
                                ),
                                mainPanel(
                                       ggvisOutput("umap_plot"),
                                       )
                        )
               ),
               tabPanel("Collocations",
                        fluidPage(
                                DT::dataTableOutput("collocation")
                        )
               ),
               tabPanel("Doc2Vec",
                        sidebarLayout(
                            sidebarPanel(width=0.2,
                                         textInput("searchword_doc", "Word", value = "inflation", width = NULL, placeholder = NULL),
                                         selectInput("model_doc", "Model:",names(data_doc),selected=names(data_doc)[1]),
                                         radioButtons("plot_type","Plot type:",c("Point","Smooth"),selected = "Smooth"),
                                         h4("Filter"),
                                         sliderInput("year_doc", "Year released", 1977, 2021, value = c(2000, 2021),
                                                     sep = ""),
                                         selectInput("type_doc", "Type",
                                                     c("All",unique(dataset$type))),
                                         selectInput("cb_doc", "central bank",
                                                     c("All",unique(dataset$cb))),
                                         selectInput("currency_doc", "currency",
                                                     c("All",unique(dataset$currency)),selected="Euro"),
                                         textInput("speaker_doc", "Speaker name (e.g., Mario Draghi)"),
                                         selectInput("color_doc", "Color",
                                                     c("currency","speaker","cb","type","year"))
                            ),
                            mainPanel(
                                textOutput("word"),
                                ggvisOutput("doc_plot")
                            )
                        )
               )
               )

# model<-"docemb_cb_fulldoc2vecPVDM300"
# word<-"testdd;Draghi"
# word<-"testddddddd"

server <- function(input, output) {
    
    most_similar_to_word<-function(model,word,n_output){
        word<-tolower(word)
        rownumber<-which(rownames(model)==word)
        if(length(rownumber)==0){
            message("Word does not exist in Corpus")
            return(NA)
        }else{
            input<-as.matrix(t(model[rownumber,]))
            return(top_embeddings(model,input,n_output))
        }
    }
    top_embeddings<-function(model,input,n_output){
        as_tibble(text2vec::sim2(model,input),rownames="Word") %>%
            arrange(desc(V1)) %>%
            slice(-1) %>%
            top_n(n_output,V1) %>%
            mutate(Rank=1:n_output) %>% 
            rename(Similarity=V1) %>% 
            relocate(Rank)
    }
    doc_embeddings_similar<-function(model,input){
        as_tibble(text2vec::sim2(model,t(input)),rownames="doc") %>%
            rename(Similarity=V1,
                   doc_id=doc)
    }

    doc_embeddings_word<-function(model,word){
        
        if(str_detect(word,";")){
            word<-c(str_split(word,";",simplify = T))
        }
        
        word<-tolower(word)
        doc_model<-data_doc[[model]]
        word_model<-data[[str_replace(model,"docemb","wordemb")]]
        rownumber<-which(rownames(word_model)%in%word)
        if(length(rownumber)==0){
            message("Word does not exist in Corpus")

            similar<-tibble(doc_id=paste0("doc_",1:10),
                                    Similarity=0,
                            word=word)%>% 
                left_join(dataset %>%
                              select(doc_id,date,type,speaker,cb,currency)%>% 
                              mutate(year=lubridate::year(date)),
                          by="doc_id")
            return(similar)
        }else{
            if(length(rownumber)==1){
                input<-as.matrix(t(word_model[rownumber,]))
            }else{
                input<-as.matrix(word_model[rownumber,])
            }
            
            sim_list<-list()
            for(i in 1:nrow(input)){
                sim_list[[i]]<-doc_embeddings_similar(doc_model,input[i,])
            }
            names(sim_list)<-rownames(word_model)[rownames(word_model)%in%word]
            
            similar<-sim_list%>%
                reduce(inner_join, by = "doc_id")%>%
                rename_with(~paste0("Similarity_",names(sim_list)),-c("doc_id"))%>% 
                pivot_longer(!doc_id,names_to = "word",names_prefix = "Similarity_",values_to = "Similarity")%>% 
                left_join(dataset %>%
                              select(doc_id,date,type,speaker,cb,currency)%>% 
                              mutate(year=lubridate::year(date)),
                          by="doc_id") 
            
            return(similar)
        }
    }
    tooltip<-function(x){
        if (is.null(x)) return(NULL)
        if (is.null(x$doc_id)) return(NULL)
        
        # Pick out the movie with this ID
        text_all <- isolate(umap_filter())
        text <- text_all[text_all$doc_id == x$doc_id, ]
        
        paste0("<b>", text$type, "</b><br>",
               text$currency, "<br>",
               text$speaker, "<br>",
               format(text$date, big.mark = ",", scientific = FALSE)
        )
    }
    

    similarity_table <- reactive({
        dat<-data[[input$model]]
        a<-most_similar_to_word(dat,input$searchword,input$top_n)%>%
            formattable(align = c("c","c","r"),
                        list(`Rank` = formatter("span", style = ~ style(font.weight = "bold")), 
                             `Word` = formatter("span", style = ~ style(font.weight = "bold")),
                             `Similarity` = color_bar("#FA614B")))
            
        return(a)
    })
    output$similarity_table <- renderFormattable({similarity_table()})
    
    umap_filter  <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
        # Apply filters
        m <- umap_data %>%
            filter(year >= minyear,
                year <= maxyear)

        # Optional: filter by Type
        if (input$type != "All") {
            Type <- input$type
            m <- m %>% filter(type == Type)
        }
        # Optional: filter by Speaker
        if (input$speaker!= "") {
            Speaker <- input$speaker
            m <- m %>% filter(speaker == Speaker)
        }
        # Optional: filter by Cb
        if (input$cb!= "All") {
            Cb <- input$cb
            m <- m %>% filter(cb == Cb)
        }
        # Optional: filter by Currency
        if (input$currency!= "All") {
            Currency <- input$currency
            m <- m %>% filter(currency == Currency)
        }
        
        m <- as.data.frame(m)
        
        m
    })
    vis <- reactive({
        fill_variable<- prop("fill",as.name(input$color))

        umap_filter%>% 
            ggvis(~V1,~V2) %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                         fill = fill_variable, key := ~doc_id)%>%
            add_tooltip(tooltip, "hover") %>% 
            hide_legend('fill')%>%
            add_axis("x", title = "Dimension 1") %>%
            add_axis("y", title = "Dimension 2") %>%
            set_options(width = 2000, height = 1100)
    })
    vis %>% bind_shiny("umap_plot")
    
    output$collocation = DT::renderDataTable({collocations %>% 
            select(-count_nested,-length)})

    doc_sim<-reactive({
        
        model<-input$model_doc
        word<-input$searchword_doc
        
        similar<-doc_embeddings_word(model,word)
            minyear <- input$year_doc[1]
            maxyear <- input$year_doc[2]
            
            # Apply filters
            m <- similar %>%
                filter(year >= minyear,
                       year <= maxyear)

            # Optional: filter by Type
            if (input$type_doc != "All") {
                Type <- input$type_doc
                m <- m %>% filter(type == Type)
            }
            # Optional: filter by Speaker
            if (input$speaker_doc!= "") {
                Speaker <- input$speaker_doc
                m <- m %>% filter(speaker == Speaker)
            }
            # Optional: filter by Cb
            if (input$cb_doc!= "All") {
                Cb <- input$cb_doc
                m <- m %>% filter(cb == Cb)
            }
            # Optional: filter by Currency
            if (input$currency_doc!= "All") {
                Currency <- input$currency_doc
                m <- m %>% filter(currency == Currency)
            }
            
            m <- as.data.frame(m)
            
            # if(nrow(m)==0){
            #     return(data.frame("doc_id"=c(1,2,3,4,5,6),"word"=c("wrong_input",),"Similarity"=c(1,1,1,1,1,1),"date"=c(1:6)))
            # }else{
            #     return(m)
            # }
        m
    })

    vis_doc <- reactive({
        
        if(input$plot_type=="Point"){
                fill_variable<- prop("fill",as.name(input$color_doc))

                doc_sim%>%
                    ggvis(~date,~Similarity) %>%
                    layer_points(size := 50, size.hover := 200,
                                 fillOpacity := 0.2, fillOpacity.hover := 0.5,
                                 fill = fill_variable, key := ~doc_id)%>%
                    add_tooltip(tooltip, "hover") %>%
                    hide_legend('fill')%>%
                    add_axis("x", title = "Date") %>%
                    add_axis("y", title = "Similarity") %>%
                    set_options(width = 2000, height = 1100)
            }else{
                doc_sim%>%
                    ggvis(~date,~Similarity) %>%
                    group_by(word) %>%
                    layer_smooths(stroke=~word)%>%
                    add_axis("x", title = "Date") %>%
                    add_axis("y", title = "Similarity") %>%
                    set_options(width = 2000, height = 1100)
                }
    })
    vis_doc %>% bind_shiny("doc_plot")
    output$word <- renderText({ paste("You have selected:", input$searchword_doc, "(",nrow(doc_sim()),"Datapoints)") })
     
}

shinyApp(ui = ui, server = server)
