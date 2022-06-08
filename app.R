#
# This is a Shiny web application. You can run the application by clicking
# the "Run App" button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Carregar Pacotes
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinydashboard)


#Setar diretorio
setwd("~/COBRADI/cobradi/test/ALURA/dashboard_com_r-aula2/procon")

#Abrir Dados
dados <- fread("dados_limpos.csv", encoding = "UTF-8")
media_chamados <- dados |>
                    group_by(anocalendario) |>
                    summarize(qtd_chamados = n() ) |>
                    summarize(media_chamado_ano = mean(qtd_chamados)) |>
                    as.numeric()

cabecalho <- dashboardHeader(title = "Dashboard PROCONs")
barra_lateral <- dashboardSidebar(width = "250px",
                                  sidebarMenu(
                                    menuItem("Dashboard",
                                             tabName = "dashboard",
                                             icon = icon("dashboard")),
                                    menuItem("Informacoes",
                                             tabName = "infos",
                                             icon = icon("info-circle"))
                                  ))

painel_principal <- dashboardBody(
  tabItems(
    tabItem(tabName = "infos",
            h1("Informacoes"),
            infoBox(title = "Contato",
                    icon = icon("envelope-square"),
                    subtitle = "Para mais informacoes e/ou feedback
                    entre em contato: paula.barros@ipea.gov.br")),
    tabItem(tabName = "dashboard",
            fluidRow(
              valueBox(subtitle = "Registros",
                       value = nrow(dados),
                       icon = icon("database")),

              infoBox(title = "", subtitle = "Reclamcoes por Ano",
                      value = media_chamados,
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
                                                         label =  "Estados:",
                                                         choices = c("TODOS",unique(dados$UF)),
                                                         selected = "TODOS"))
                         ),
                         column(width = 6,
                                box(width = "100%",
                                    dateRangeInput(inputId = "data_abertura",
                                                   label =  "Data Abertura:",format = "dd-mm-yyyy",
                                                   start = min(as.Date(dados$DataAbertura)),
                                                   end   = max(as.Date(dados$DataAbertura)))
                                )
                         ),
                         column(width = 6,
                                box(width = "100%",
                                    selectizeInput(inputId = "assunto",
                                                   label = "Descricao Assunto:",
                                                   choices = c("TODOS", unique(dados$DescricaoAssunto)),
                                                   multiple = T, options = list(maxItems = 5),
                                                   selected = "TODOS")))
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(width = "100%",
                         plotlyOutput(outputId = "data", width = "100%"),
                         textOutput(outputId = "descData")
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     box(width = "100%",
                         plotlyOutput(outputId = "atendida"
                         )
                     )
              ),
              column(width = 6,
                     box(width = "100%",
                         plotlyOutput(outputId = "atendidaAno")
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(width = "100%", title = "Reclamacoes por UF",
                         plotlyOutput(outputId = "uf"),
                         textOutput(outputId = "descUf")
          )
        )
      )
    )
  ), #FIM TAB ITEMS
) #FINAL MAIN PAINEL

ui <- dashboardPage(header = cabecalho,
                    sidebar = barra_lateral,
                    body = painel_principal)


## front-end (tela que sera mostrada para o usuario)
ui2 <- fluidPage(
  ## titulo da pagina
  titlePanel("Dashboard PROCON"),
  sidebarLayout(
    sidebarPanel(
      ## caixa de selecao UF
      checkboxGroupInput(inputId = "select_UF",
                         label =  "Estado:",
                         choices = c("TODOS",unique(dados$UF)),
                         selected = "TODOS"),

      ##calendario para selecionar
      dateRangeInput(inputId = "data_abertura",
                     label =  "Data Abertura:",format = "dd-mm-yyyy",
                     start = min(as.Date(dados$DataAbertura)),
                     end   = max(as.Date(dados$DataAbertura))),

      selectizeInput(inputId = "assunto",
                  label = "Descricao Assunto",
                  choices = c("TODOS", unique(dados$DescricaoAssunto)),
                  multiple = T, options = list(maxItems = 5),
                  selected = "TODOS")

    ),
    mainPanel(
      ## grafico de linhas
      plotlyOutput(outputId = "data", width = "100%"),

      ## texto descritivo do grafico de linhas
      textOutput(outputId = "descData"),

      ## grafico UF
      plotlyOutput(outputId = "uf"),

      ## texto descritivo do grafico UF
      textOutput(outputId = "descUf"),

      ## grafico ATENDIDA
      plotlyOutput(outputId = "atendida"),

      ## grafico ATENDIDA ANO
      plotlyOutput(outputId = "atendidaAno"),

    )
  )
)

## back-end (o que o sistema ira executar para retornar para o usuario, front-end)
server <- function(input, output, session) {

  dados_selecionados <- reactive({
    ## filtro UF
    print(input)
    if (! "TODOS" %in% input$select_UF){
      dados <- dados %>% filter(UF %in% input$select_UF)
    }
    #Filtro Assunto
    if(! "TODOS" %in% input$assunto){
      dados <- dados |>
        filter(DescricaoAssunto %in% input$assunto)
    }
    ## filtro DATA
    dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] &
                                as.Date(DataAbertura) <= input$data_abertura[2])
    dados

  })

  ## grafico de linhas ano-mes
  output$data <- renderPlotly({
    ano_mes <- data.frame(table(format(as.Date(dados_selecionados()$DataAbertura),
                                       "%Y-%m"))) %>% rename(Data = Var1, Qtd=Freq)
    ano_mes$Data <- as.Date(paste(ano_mes$Data,"01",sep = "-"))

    ggplotly(
      ggplot(data = ano_mes, aes(Data, Qtd)) +
        geom_line(group = 1) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        ggtitle("Quantidade de Reclamacoes por Ano-Mes") +
        scale_x_date(date_labels = "%b-%Y",breaks = "6 month")
    )
  })

  ## grafico UF
  output$uf   <- renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) +
        geom_bar(fill = "blue",stat = "identity") +
        coord_flip() +
        xlab("UF") + #ylab("Quantidade") +
        theme_bw() +
        ggtitle("Quantidade de Reclamacoes por UF"),
      tooltip = "text"
    )
  })

  ## grafico atendida
  output$atendida <- renderPlotly({
    ggplotly(
      ggplot(dados_selecionados()) +
        geom_bar(aes(Atendida),fill = c("red","green"),stat = "count") +
        ylab("Quantidade") +
        theme_bw() +
        ggtitle("Quantidade de Chamados Atendidos")
    )
  })

  ## grafico atendida por ano
  output$atendidaAno <- renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$anocalendario,dados_selecionados()$Atendida)) %>%
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() +
        geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                 stat = "identity",position = position_dodge2()) +
        theme_bw() +
        ggtitle("Quantidade de Reclamacoeses Atendidas(nao) por Ano")
    )
  })

  ## retornando texto para cada campo em especifico
  output$descData <- renderText({
    paste("Grafico com a quantidade de reclamacoes feitas entre:",
          min(dados_selecionados()$DataAbertura),"-",
          max(dados_selecionados()$DataAbertura))
  })
  output$descUf   <- renderText({
    estados <- paste(unique(dados_selecionados()$UF),collapse = ", ")
    paste("Grafico com a quantidade de reclamacoes feitas por UF: ",estados)
  })

  output$descAtendida    <- renderText({"Grafico com a quantidade de reclamacoes atendidas e nao atendidas"})

  output$descAtendidaAno <- renderText({"Grafico com a quantidade de reclamacoes atendidas e nao atendidas por Ano"
    })

  output$qtdUf <- renderValueBox({
    valueBox(value = length(unique(dados_selecionados()$UF)),
             subtitle = "UFs Selecionadas", icon = icon("map-maker")
             )
    })
}

shinyApp(ui, server)

runApp(list(ui = ui, server = server),launch.browser = TRUE)
