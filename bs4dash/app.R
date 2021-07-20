library(bs4Dash)
library(ggvis)
library(formattable)
library(plotly)
library(tidyverse)
library(echarts4r)
library(fresh)
# data<-lapply(list.files("bs4dash/data/models_matrix/",full.names = T,pattern="wordemb"),
#              function (x) readRDS(x))
# names(data)<-str_remove_all(list.files("bs4dash/data/models_matrix/",pattern="wordemb"),".Rds")
# 
# data_doc<-lapply(list.files("bs4dash/data/models_matrix/",full.names = T,pattern="docemb"),
#                  function (x) readRDS(x))
# names(data_doc)<-str_remove_all(list.files("bs4dash/data/models_matrix/",pattern="docemb"),".Rds")
# dataset<-readRDS("bs4dash/data/models/dataset.Rds")

data<-lapply(list.files("data/models_matrix/",full.names = T,pattern="wordemb"),
             function (x) readRDS(x))
names(data)<-str_remove_all(list.files("data/models_matrix/",pattern="wordemb"),".Rds")

data_doc<-lapply(list.files("data/models_matrix/",full.names = T,pattern="docemb"),
                 function (x) readRDS(x))
names(data_doc)<-str_remove_all(list.files("data/models_matrix/",pattern="docemb"),".Rds")

dataset<-readRDS("data/models/dataset.Rds")

umap_data<-readRDS(paste0("data/models/umap_data.Rds"))

collocations<-readRDS(paste0("data/models/collocations.Rds"))

echarts_dark_theme <- list(
    options = '{
    "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
    "backgroundColor": "#343a40", 
    "textStyle": {
        color: "#fff"
    }
  }',
  name = "dark_theme"
)

ui <- dashboardPage(
    fullscreen = TRUE,
    header=dashboardHeader(
        title = dashboardBrand(
            title = HTML("Whatever it takes to<br/>understand a central banker"),
            color = "danger",
            href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
            #image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
            opacity = 0.8
        ),
        status="danger"
        ),
    dashboardSidebar(skin = "light",
                     status = "danger",
                     # title = "Whatever it takes to understand a central banker",
                     # titleWidth = 450,
                     brandColor = "primary",
                     url = "https://www.google.fr",
                     src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
                     elevation = 3,
                     opacity = 0.8,
                     bs4SidebarMenu(
                         sidebarHeader("Applications"),
                         menuItem("Similarity",tabName = "Similarity",icon = icon("list",lib = "font-awesome")),
                         menuItem("Collocations",tabName = "Collocations",icon = icon("link",lib = "font-awesome")),
                         menuItem("Umap",tabName = "Umap",icon = icon("map",lib = "font-awesome")),
                         menuItem("Doc2Vec",tabName = "Doc2Vec",icon = icon("chart-line",lib = "font-awesome")),
                         #sidebarHeader("Contact"),
                         menuItem("Paper",tabName = "Info",icon = icon("id-card"))
                     )),
    controlbar = dashboardControlbar(
        id = "controlbar",
        skin = "light",
        width = 350,
        #title = "My right sidebar",
        collapsed = T,
        controlbarMenu(
        #    controlbarItem(id="Similarity",
        #        div(class = "p-3", skinSelector())),
        controlbarItem(
            tabName = "Filter",
            id="Filter",
            sliderInput("year", "Year released", 1977, 2021, value = c(1977, 2021),sep = ""),
            selectInput("type", "Type",c("All",unique(umap_data$type))),
            selectInput("cb", "central bank",c("All",unique(umap_data$cb))),
            selectInput("currency", "currency",c("All",unique(umap_data$currency))),
            textInput("speaker", "Speaker name",placeholder = "e.g. Mario Draghi"),
            selectInput("color", "Color",c("currency","speaker","cb","type","year"))
        )
        )
        ),
    dashboardBody(
        e_theme_register(echarts_dark_theme$options, name = echarts_dark_theme$name),
        skin = "light",
        bs4TabItems(
            bs4TabItem(
                tabName = "Similarity",
                fluidRow(
                    box(title="Filter",
                        closable = F,
                        labelStatus="primary",
                        width=2,
                        textInput("searchword", "Word", value = "inflation", width = NULL, placeholder = NULL),
                        selectInput("model", "Model:",names(data)),
                        sliderInput("top_n","Number of Words:",min = 1,max = 20,value = 5)
                        ),
                    box(title="Similarity",
                        closable = F,
                        width=10,
                        formattableOutput("similarity_table"),
                        "The table shows the most similar terms to the target word according to the cosine distance of the underlying word embeddings")
                    )
                ),
            bs4TabItem(
                tabName = "Collocations",
                fluidPage(
                    DT::dataTableOutput("collocation"),
                    p("Collocations based on:"), 
                    p("Blaheta, D., & Johnson, M. (2001, July). Unsupervised learning of multi-word verbs. In Proceedings of the 39th Annual Meeting of the ACL (pp. 54-60)."),
                    p("Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018). “quanteda: An R package for the quantitative analysis of textual data.” Journal of Open Source Software, 3(30), 774. doi: 10.21105/joss.00774, https://quanteda.io.")
                    )
                ),
            bs4TabItem(
                tabName = "Umap",
                fluidRow(
                    box(title="Umap decomposition of documents",
                        width=12,
                        height="90vh",
                        overflow = F,
                        ggvisOutput("umap_plot"),
                        p("UMAP decomposition based on:"),
                        p("McInnes, L., Healy, J., & Melville, J. (2018). Umap: Uniform manifold approximation and projection for dimension reduction. arXiv preprint arXiv:1802.03426.")
                    )
                )
            ),
            bs4TabItem(
                tabName = "Doc2Vec",
                fluidRow(
                    box(width = 1,
                        textInput("searchword_doc", "Word", value = "inflation", width = NULL, placeholder = NULL),
                        selectInput("model_doc", "Model:",names(data_doc),selected=names(data_doc)[1]),
                        radioButtons("plot_type","Plot type:",c("Point","Smooth"),selected = "Smooth")
                        ),
                    box(title="Document similarity with selected word",
                        width=11,
                        height="90vh",
                        overflow = F,
                        textOutput("word"),
                        ggvisOutput("doc_plot")
                        )
                )
                ),
            bs4TabItem(
              tabName = "Info",
              fluidRow(
                    box(title ="Paper", 
                            "Baumgärtner, Martin & Zahner, Johannes (2021). Whatever it takes to understand a central banker - Embedding their words using neural networks.",
                            icon = icon("credit-card")),
                    status = "primary"
                    )#,
              # userBox(
              #   id = "userbox",
              #   title = userDescription(
              #     title = "Johannes Zahner",
              #     subtitle = "Research Assistant Philipps University Marburg",
              #     type = 2,
              #     image = "https://scholar.googleusercontent.com/citations?view_op=view_photo&user=3ZPPM8EAAAAJ&citpid=1",
              #   ),
              #   status = "primary",
              #   gradient = TRUE,
              #   background = "primary",
              #   boxToolSize = "xl",
              #   footer = "The footer here!",
              #   collapsible = FALSE
              # )
              )
            )
            
        ),
    dashboardFooter(
        fixed = FALSE,
        left = a(
            #href = "https://twitter.com/divadnojnarg",
            #target = "_blank", "@DivadNojnarg"
        ),
        right = "2021"
    )
    )



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
    similarity_table <- reactive({
        dat<-data[[input$model]]
        a<-most_similar_to_word(dat,input$searchword,input$top_n)%>%
            formattable(align = c("c","c","r"),
                        list(`Rank` = formatter("span", style = ~ formattable::style(font.weight = "bold")),
                             `Word` = formatter("span", style = ~ formattable::style(font.weight = "bold")),
                             `Similarity` = color_bar("#dc3545")))
        
        return(a)
    })
    output$similarity_table <- renderFormattable({similarity_table()})
    
    output$collocation = DT::renderDataTable({collocations %>% 
            select(-count_nested,-length)})
    
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
    umap_filter  <- reactive({
        minyear <- input$year[1]
        maxyear <- input$year[2]

        m <- umap_data %>%
            filter(year >= minyear,
                   year <= maxyear)
        
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
            ggvis::hide_legend('fill')%>%
            add_axis("x", title = "Dimension 1") %>%
            add_axis("y", title = "Dimension 2") %>%
            set_options(width = "auto", height = "auto")
    })
    vis %>% bind_shiny("umap_plot")
    
    doc_embeddings_similar<-function(model,input){
        as_tibble(text2vec::sim2(model,t(input)),rownames="doc") %>%
            rename(Similarity=V1,
                   doc_id=doc)
    }
    observeEvent(input$searchword_doc, {
        word<-input$searchword_doc
        model<-input$model_doc
        if(str_detect(word,";")){
            word<-c(str_split(word,";",simplify = T))
        }
        word<-tolower(word)
        doc_model<-data_doc[[model]]
        word_model<-data[[str_replace(model,"docemb","wordemb")]]
        rownumber<-which(rownames(word_model)%in%word)
        if(length(word)!=length(rownumber)){
            showNotification(ui = paste(Sys.time(), "up to one word not in dataset"), duration = 5, closeButton = FALSE, id = "previousWarningMessage", type = "warning")
        }
    })
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
                            word=word[1])%>% 
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
    doc_sim<-reactive({
        
        model<-input$model_doc
        word<-input$searchword_doc
        
        similar<-doc_embeddings_word(model,word)
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
        # Apply filters
        m <- similar %>%
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
        
        if(nrow(m)==0){
            return(data.frame("doc_id"=c(1,2,3,4,5,6),"word"=c("wrong_input"),"Similarity"=c(1,1,1,1,1,1),"date"=c(1:6)))
        }else{
            return(m)
        }
        #m
    })
    
    vis_doc <- reactive({
        
        if(input$plot_type=="Point"){
            fill_variable<- prop("fill",as.name(input$color))
            
            doc_sim%>%
                ggvis(~date,~Similarity) %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5,
                             fill = fill_variable, key := ~doc_id)%>%
                add_tooltip(tooltip, "hover") %>%
                ggvis::hide_legend('fill')%>%
                add_axis("x", title = "Date") %>%
                add_axis("y", title = "Similarity") %>%
                set_options(width = "auto", height = "auto")
        }else{
            doc_sim%>%
                ggvis(~date,~Similarity) %>%
                group_by(word) %>%
                layer_smooths(stroke=~word)%>%
                add_axis("x", title = "Date") %>%
                add_axis("y", title = "Similarity") %>%
                set_options(width = "auto", height = "auto")
        }
    })
    vis_doc %>% bind_shiny("doc_plot")
    output$word <- renderText({ paste("You have selected:", input$searchword_doc, "(",nrow(doc_sim()),"Datapoints)") })
}

 shinyApp(ui, server)
