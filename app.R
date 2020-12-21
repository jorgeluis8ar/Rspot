cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4)
setwd("~/Documents/Universidad/Taller de R/Proyecto/RSpot")

library(shiny)
library(ggplot2)
dta = readRDS(file = 'Datos/base.rds')
dta$artist_name <- tolower(dta$artist_name)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Explorando la API de Spotify"),
    # showTab(),    
    # Sidebar panel for inputs ----
    sidebarPanel(
        
        # Input: Selector for variable to plot against mpg ----
        selectInput("band", "Band:", 
                    c("Artic Monkeys" = "arctic monkeys",
                      "Alice in Chains" = "alice in chains",
                      "Avenged Sevenfold" = "avenged sevenfold",
                      "Audioslave" = "audioslave",
                      "Bon Jovi" = "bon jovi",
                      "Drake" = "drake",
                      "Guns N' Roses" = "guns n' roses",
                      "Interpol" = "interpol",
                      "Kendrick Lamar" = "kendrick lamar",
                      "Metallica" = "metallica",
                      "Muse" = "muse",
                      "Nirvana" = "nirvana",
                      "Pearl Jam" = "pearl jam",
                      "Pink Floyd" = "pink floyd",
                      "Queen" = "queen",
                      "Radiohead" = "radiohead",
                      "Soda Stereo" = "soda stereo",
                      "Stone Temple Pilots" = "stone temple pilots",
                      "Vicente Fernández" = "vicente fernández",
                      "Vilma Palma e Vampiros" = "vilma palma e vampiros")),
        
        # Input: Checkbox for whether outliers should be included ----
        # checkboxInput("outliers", "Show outliers", TRUE)
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        plotOutput("bandas"),
        textOutput(outputId = "desc"),
        tags$a(href = "https://developer.spotify.com",
               "Fuente: Spotify API", target = "_blank")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bandas <- renderPlot({
    
        base <- subset(dta,dta$artist_name==input$band)
        
        ggplot(data = base,aes(x = valence, y = energy, colour = album_name)) + geom_point() + 
            geom_vline(xintercept = .5) + geom_hline(yintercept = .5) +
            theme_bw() + theme(legend.position = 'bottom')+ xlim(0,1) + ylim(0,1) +
            labs(colour = "Nombre del Albúm", x = "Valencia", y = "Energía")+
            annotate(geom="text", x=0.05, y=0.05, label="Triste/Deprimente",
                     color="black")+
            annotate(geom="text", x=0.05, y=0.95, label="Turbulenta/Enojada",
                     color="black")+
            annotate(geom="text", x=0.95, y=0.95, label="Feliz/Alegre",
                     color="black")+
            annotate(geom="text", x=0.95, y=0.05, label="Chill/Tranquila",
                     color="black")
    })
    # output$info <- renderText({
    #     paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    # })

}

# Run the application 
shinyApp(ui = ui, server = server)
