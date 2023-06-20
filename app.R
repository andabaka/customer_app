# CUSTOMER SEGMENTATION APP

# Libraries ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)
library(plotly)
library(DT)
library(dplyr)







# Import data ----
mall_customers_tbl<-readr::read_csv("customers.csv")
mall_customers_tbl <- mall_customers_tbl %>% 
    mutate(Spol = Spol %>% as.factor())

source(file = "functions.R")




# USER INTERFACE ----
ui <- shiny::fluidPage(
    title = "customer segmentation App",
    div(
        class = "container",
        id = "page",
        h1(class = "page-header", tags$small("customer segmentation"), "App",
           icon(class = "fa-1x fa-flip-horizontal pull-right", "cart-shopping"))
    
    ),
    
    # CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # JS ----
    shinyjs::useShinyjs(),
    
    
    
    div(
        class = "container",
        id = "application_ui",
        column(
            width = 5,
            wellPanel(
                fluidRow(
                div(
                    id = "input_main",
                    h4("Opisna statistika"),
                    column(width = 6,
                    pickerInput(
                        inputId  = "picker_hist", 
                        label    = h5("Graf frekvencija"), 
                        choices  = c("Dob", "Godišnji_Prihodi", "Potrošački_Kod"), 
                        selected = "Dob", 
                        multiple = FALSE,
                        options  = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3")
                    )),
                    column(width = 6,
                    pickerInput(
                        inputId  = "picker_box", 
                        label    = h5("Box-Plot graf: Spol"), 
                        choices  = c("Dob", "Godišnji_Prihodi", "Potrošački_Kod"), 
                        selected = "Dob", 
                        multiple = FALSE,
                        options  = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3")
                        )
                     )
                    )
                )
                ),
            br(),
               
            div(
                id = "plots",
                    div(
                        class = "panel",
                        div(
                            class = "panel-header",
                            h4("Graf frekvencija")
                        ),
                        div(
                            class = "panel-body bg-primary",
                            plotOutput(outputId = "plot_1")
                       
                    )),
                    br(),
                        div(
                            class = "panel",
                            div(
                                class = "panel-header",
                                h4("Box-Plot graf")
                            ),
                            div(
                                class = "panel-body bg-primary",
                                plotOutput(outputId = "plot_2")
                            )
                        )
                    
            )     
        ),
        column(
            width = 7,
            wellPanel(
                fluidRow(
                div(
                    id = "cluster_analysis",
                h4("Klaster analiza (k-means algoritam)"),
                      column(
                          width = 5,
            pickerInput(
                inputId = "k_means",
                label = h5("Potrošački kod + "),
                choices = c("Dob", "Godišnji_Prihodi"),
                selected = c("Dob", "Godišnji_Prihodi"),
                multiple = TRUE,
                options  = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3")
            )
            ),
            div(
                class = "pull-right",
            column(
                width = 5,
                actionButton(inputId = "reset", label = "Reset", icon = icon("rotate-right")
                             ))
                
            ))
            
            )),
            br(),
            div(
                class = "panel",
                div(
                    class = "panel-header",
                    h4("Vizualizacija klastera (UMAP-2D projekcija)")
                ),
                div(
                    class = "panel-body bg-primary",
                    plotlyOutput(outputId = "plotly_1")
                )
            ),
            br(),
            div(
                class = "panel",
                div(
                    class = "panel-header",
                    h4("Interaktivna tablica sa klasterima")
                    ),
                div(
                    class = "panel-body bg-primary",
                    DTOutput(outputId = "table")
                
                )
            )
        )
    ),
    hr(),
    div(
        class = "container",
        id = "footer",
        h5(class = "pull-right", "Source Code",
           a(href = "https://github.com/andabaka", icon("github", class = "pull-right", style = "color: #414a37"),target = "_blank")),
        h4(tags$small("by"), "marijana andabaka",
           a(href = "https://mandabaka.netlify.app/",
                  icon("chart-simple", style = "color: #414a37"), target = "_blank"),
           a(href = "www.linkedin.com/in/marijana-andabaka-9a66831a4", icon("linkedin", style = "color: #414a37"), target = "_blank")
           ) 
          ),
    br()
)

server <- function(input, output, session){
    
   
    
    mall_customers_tbl_react <- reactive(
     mall_customers_tbl
    )
    
   
    
   
    # Update cluster input settings after reset
    observeEvent(eventExpr = input$reset,
                 handlerExpr = {
                     updatePickerInput(
                         session = session,
                         inputId = "k_means",
                         selected = c("Dob", "Godišnji_Prihodi")
                     )
                 })
    
    
  
    
    
    k_means_results <- reactive({
        mall_customers_tbl_react() %>% k_mean_fn(k_means = input$k_means, seed = 123) %>% 
            augment(mall_customers_tbl_react())
            
    })
    
   
        
    
    umap_results <- reactive({
        mall_customers_tbl_react() %>% umap_fn(k_means = input$k_means)
    })
        
   
     
   umap_plot <- reactive({
       k_means_results() %>% left_join(umap_results()) %>% 
           mutate(lable_text = str_glue("ID Kupca: {`ID Kupca`}
                                Spol: {Spol}
                                Dob: {Dob}
                                Godišnji prihodi: {Godišnji_Prihodi}
                                Potrošački kod: {Potrošački_Kod}
                                Cluster: {.cluster}"))
       })
    
    output$plotly_1 <-renderPlotly({
        if (is.null(input$k_means)){
            return(NULL)} 
       g <- umap_plot() %>% 
            ggplot(aes(x, y, color = .cluster)) + 
            geom_point(aes(text = lable_text)) + 
            theme_tq() +
            scale_color_tq() + 
            labs(title = "") +
            theme(legend.position = "none")
        g <- ggplotly(g, tooltip = "text")
      
    })
    
   
    
    
    output$table <-renderDataTable({
        if (is.null(input$k_means)){
            return(NULL)} 
        k_means_results() %>% 
            select(`ID Kupca`, .cluster, Dob, Spol, Godišnji_Prihodi, Potrošački_Kod)  
       }, options = list(autoWidth = FALSE,
                         pageLength = 8,
                         scrollX = TRUE,
                         scrollY = TRUE), selection = "single",
       rownames = FALSE)
    
   
    
    
    output$plot_1 <- renderPlot({
        
       g <- mall_customers_tbl_react() %>% 
            ggplot(aes_string(input$picker_hist))+
            geom_histogram(fill = "#414a37")+
            theme_tq() +
            labs(y = "Frekvencija")
       if(identical(input$picker_hist, "Godišnji_Prihodi")){
           g <- g + labs(x = "Godišnji Prihodi (1000$)")
       } else if(identical(input$picker_hist, "Potrošački_Kod")){
           g <- g + labs(x = "Potrošački Kod (1-100)")    
            }
        g
    })
    
    output$plot_2 <- renderPlot({
        
       g <- mall_customers_tbl_react() %>% 
            ggplot(aes_string(input$picker_box, "Spol", fill= "Spol"))+
            geom_boxplot(color = c("#414a37", "#5b0b15"),alpha= 0.3) +
            coord_flip() +
            theme_tq()+
            scale_fill_manual(values = c("#414a37", "#5b0b15"))+
            theme(legend.position = "none")
       
        if(identical(input$picker_box, "Godišnji_Prihodi")){
            g <- g + labs(x = "Godišnji Prihodi (1000$)")
        } else if(identical(input$picker_box, "Potrošački_Kod")){
            g <- g + labs(x = "Potrošački Kod (1-100)")    
        }
        g
    })
}

shinyApp(ui, server)
    
    