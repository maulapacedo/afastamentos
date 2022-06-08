#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Carregar Pacotes

pacman::p_load(shiny, data.table, dplyr, ggplot2, plotly, shinyWidgets, shinydashboard)

#Setar diretorio
setwd("~/COBRADI/mes_referencia")

#Abrir dados 
afastamentos_total <- list.files(path = "C:/Users/E3072167/Documents/COBRADI/mes_referencia",
                                 full.names = TRUE) |> 
  lapply(read.csv, header = TRUE, sep = ";", dec = ",",  stringsAsFactors = FALSE) |> 
  bind_rows()

media_afastamentos <- afastamentos_total |> 
                        group_by(Valor_rendimento_liquido) |> 
                        dplyr::summarize(qtd_afastamentos = n() ) |> 
                        summarize(media_valor_rendimento_liquido = mean(Valor_rendimento_liquido)) |> 
                        as.numeric()
#Titulo
cabecalho <- dashboardHeader(title = "Dashboard COBRADI")

#Barra Lateral 
barra_lateral <- dashboardSidebar(width = "250px",
                                  sidebarMenu(
                                    menuItem("Início",
                                             tabName = "inicio"),
                                    menuItem("Dashboard",
                                             tabName = "dashboard",
                                             icon = icon("dashboard", verify_fa = FALSE)),
                                    menuItem("Informações",
                                             tabName = "infos",
                                             icon = icon("info-circle"))
                                  ))

#Painel Principal 
painel_principal <- dashboardBody(
  tabItems(
    tabItem(tabName = "infos",
            h1("Informações"),
            infoBox(title = "Contato",
                    icon = icon("envelope-square"),
                    subtitle = "Para mais informações e/ou feedback
                    entre em contato: cobradi@ipea.gov.br [Respondemos em até 24h]")),
    tabItem(tabName = "dashboard",
            fluidRow(
              valueBox(subtitle = "Afastamentos",
                       value = nrow(afastamentos_total),
                       icon = icon("database")),
              infoBox(title = "", subtitle = "Afastamentos por Ano",
                      value = media_afastamentos,
                      icon = icon("list")),
              
              valueBoxOutput(outputId = "qtdUf")
              ),
            fluidRow(
              column(width = 12,
                     box(title = "Filtros", width = "100%",
                         column(width = 12,
                                box(width = "100%",
                                    awesomeCheckboxGroup(inline = TRUE,
                                                         inputId = "select_UF",
                                                         label = "Estados:",
                                                         choices = c("TODOS",
                                                                     unique(
                                                                       afastamentos_total$UF_da_UPAG_de_vinculacao)),
                                                         selected = "TODOS"))
                          ), 
                          column(width = 6,
                                   box(width = "100%",
                                       dateRangeInput(inputId = "data_abertura",
                                                      label =  "Data Abertura:", format = "dd-mm-yyyy",
                                                      start = min(as.Date(afastamentos_total$Ano_Mes_inicio_afastamento)),
                                                      end   = max(as.Date(afastamentos_total$Ano_Mes_inicio_afastamento)))
                                )
                          ), 
                          column(width = 6,
                                 box(width = "100%",
                                     selectizeInput(inputId = "afastamento",
                                                    label = "Tipo de Afastamento:",
                                                    choices = c("TODOS", unique(afastamentos_total$Descricao_do_afastamento)),
                                                    multiple = T, options = list(maxItems = 5),
                                                    selected = "TODOS")))
                        )
                  )
            ),#FIM fluidrow1
            fluidRow(
              column(
                width = 12,
                box(width = "100%",
                    plotlyOutput(outputId = "data", width = "100%"),
                    textOutput(outputId = "descData")
                    )
              )
            ), #FIM fluidrow2
            fluidRow(
              column(
                width = 6,
                box(width = "100%",
                    plotlyOutput(outputId = "porcentagem_afastamentos_descricao"))
              ),
              column(
                width = 6,
                box(width = "100%",
                    plotlyOutput(outputId = "afastamentos_por_ano"))
              )
            ), #FIM fluidrow3
            fluidRow(
              column(width = 12,
                     box(width = "100%", title = "Afastamentos por UF",
                         plotlyOutput(outputId = "UF"),
                         textOutput(outputId = "descUf")
          )
        )
      )
    )
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
