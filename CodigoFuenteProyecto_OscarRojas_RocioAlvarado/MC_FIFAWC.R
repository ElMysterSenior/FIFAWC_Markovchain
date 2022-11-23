#IMPORTAR LIBRERIAS
library(shiny)
library(markovchain)

#CARGAR DATOS, DATOS ORGANIZADOS POR EQUIPOS PARA VER SUS DIAGRAMAS DE ESTADOS INDIVIDUALES Y EN CONJUNTO (POSIBILIDAD DE QUE TODOS LOS EQUIPOS 
#TERMINEN EN ALGUN ESTADO)
mdatagral <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Resultados.csv",header = TRUE,fill = TRUE)
mdataarg <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Argentina.csv",header = TRUE,fill = TRUE)
mdatabel <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Belgica.csv",header = TRUE,fill = TRUE)
mdatabra <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Brasil.csv",header = TRUE,fill = TRUE)
mdatadina <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Dinamarca.csv",header = TRUE,fill = TRUE)
mdataesp <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Espana.csv",header = TRUE,fill = TRUE)
mdatafra <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Francia.csv",header = TRUE,fill = TRUE)
mdataing <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Inglaterra.csv",header = TRUE,fill = TRUE)
mdataita <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Italia.csv",header = TRUE,fill = TRUE)
mdatamex <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Mexico.csv",header = TRUE,fill = TRUE)
mdatapb <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_PaisesBajos.csv",header = TRUE,fill = TRUE)
mdatapor <- read.csv("C:\\Users\\oscar\\OneDrive\\Escritorio\\MC_Portugal.csv",header = TRUE,fill = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COPA MUNDIAL FIFA: CADENAS DE MARKOV"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          helpText("Se muestra la transicion de transicion donde se busca saber la probabilidad de que un equipo dentro
                   del TOP 10 FIFA no participe, se quede en fase de grupos, pase a octavos, pase a cuartos, pase a semifinales,
                   pase a la final o quede campeon, esto dentro de los numero de pasos que el usuario seleccione"),
          
          selectInput(inputId = "dataset",
                      label = "Elija un equipo:",
                      choices = c("BRASIL", "BELGICA", "ARGENTINA", "FRANCIA",
                                  "INGLATERRA","ITALIA","ESPAÑA","PAISES BAJOS","PORTUGAL","DINAMARCA",
                                  "MEXICO","TODOS")),
          
          
          sliderInput("steps",
                      "Número de pasos:",
                      min = 1,
                      max = 20,
                      value = 10),
          
          helpText("Al elegir número de pasos nos mostrará otra matriz",
                   "donde calculará nuevas probabilidades, estan corresponden,",
                   "a la probabilidad n mundiales después.")
        ),
        

        
        mainPanel(
          h3(textOutput("caption", container = span)),
          verbatimTextOutput("summary"),
          helpText("Numero de mundiales que le tomaria al equipo volver a estar en alguno de los siguentes estados:"),
          verbatimTextOutput("recurrence"),
         
          
          
          helpText("DIAGRAMA DE TRANSICION DE ESTADOS"),
          
          plotOutput(outputId = "chainplot", height = "500px"),
          
          verbatimTextOutput("st"),
          
          tableOutput("view"),
        )
    )
)


server <- function(input, output) {

  datasetInput <- reactive({
    switch(input$dataset,
           "BRASIL" = mdatabra,
           "BELGICA" = mdatabel,
           "ARGENTINA" = mdataarg,
           "FRANCIA" = mdatafra,
           "INGLATERRA" = mdataing,
           "ITALIA" = mdataita,
           "ESPAÑA" = mdataesp,
           "PAISES BAJOS" = mdatapb,
           "PORTUGAL" = mdatapor,
           "DINAMARCA" = mdatadina,
           "MEXICO" = mdatamex,
           "TODOS" = mdatagral)
  })
  
  
  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    Pe<- dataset$Clasificatorias
    fit <- markovchainFit(data = Pe,confidencelevel = 0.95)
    print(fit$estimate)
    
  })
  
  output$recurrence <- renderPrint({
    dataset <- datasetInput()
    Pe<- dataset$Clasificatorias
    fit <- markovchainFit(data = Pe,confidencelevel = 0.95)
    meanRecurrenceTime(fit$estimate)
    
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = 50)
  })
  
  output$chainplot <- renderPlot({
    dataset <- datasetInput()
    Pe<- dataset$Clasificatorias
    fit <- markovchainFit(data=Pe,confidencelevel = 0.95)
    plot(fit$estimate)
  })
  
  output$st <- renderPrint({
    dataset <- datasetInput()
    Pe<- dataset$Clasificatorias
    fit <- markovchainFit(data=Pe,confidencelevel = 0.95)
    a<- fit$estimate^input$steps
    print("Probabilidad con n cantidad de pasos: ")
    print(a)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
