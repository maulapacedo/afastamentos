#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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
library(readr)
library(lubridate)
library(DT)
library(gt)
library(devtools)
library(htmlTable)
library(kableExtra)
library(magrittr)
library(skimr)
library(gtsummary)


#Setar diretório
require("knitr")
opts_knit$set(root.dir = "~/COBRADI/mes_referencia")

#TRATAR BASE DE DADOS AFASTAMENTOS

#Abrir Dados  
afastamentos_total <- list.files(
  path = "C:/Users/E3072167/Documents/COBRADI/mes_referencia",
  full.names = TRUE) |> 
  lapply(read.csv, header = TRUE, sep = ";", dec = ",",  stringsAsFactors = FALSE) |> 
  bind_rows()

#Eliminar variáveis desnecessárias para análise
afastamentos_total <- afastamentos_total %>% 
  select(-c(Nome, CPF, X))

#Eliminar os valores faltantes do data frame
sapply(afastamentos_total, function(x) sum (is.na(x)))
afastamentos_total <- afastamentos_total[complete.cases(afastamentos_total),]
sapply(afastamentos_total, function(x) sum (is.na(x)))

#Eliminar os valores nulos
afastamentos_total[afastamentos_total==0] <- NA

afastamentos_1 <- afastamentos_total[complete.cases(
  afastamentos_total), ]

#Alterar Mês Início Afastamento e Ano Mês Referência para o formato de data padrão e, posteriormente, inserir dia
afastamentos_1$Ano_Mes_inicio_afastamento <- parse_date_time(
  afastamentos_1$Ano_Mes_inicio_afastamento, "ym")
afastamentos_1$Ano_Mes_referencia <- parse_date_time(
  afastamentos_1$Ano_Mes_referencia, "ym")

#Salvar Mês e Ano em colunas separadas
afastamentos_2 <- afastamentos_1 |>  
  mutate(Ano_inicio_afastamento = format(Ano_Mes_inicio_afastamento, "%Y"),
         Mes_inicio_afastamento = format(Ano_Mes_inicio_afastamento, "%m"))

#Adicionar coluna do ´RENDIMENTO POR HORA´ (Remuneração por Hora Relativa)
afastamentos_2 <- afastamentos_2 |> 
                  mutate(Rendimento_Liquido_Hora = ((Valor_rendimento_liquido/21)/8))

#Eliminando espaços da variável Descrição do Afastamento
afastamentos_2$Descricao_do_afastamento <- trimws(afastamentos_2$Descricao_do_afastamento, which = c("both"))

#Selecionar Ano
afastamentos_2021 <- afastamentos_2 |>  
  filter(Ano_inicio_afastamento == "2021")

#User Interface 
ui <- dashboardPage(
  ##Barra de cima 
  dashboardHeader(title = "Cooperação Brasileira Para o Desenvolvimento Internacional (COBRADI)", 
                  titleWidth = 700,
                  tags$li(class = "dropdown",
                          tags$a(href = "https://www.ipea.gov.br/portal/index.php?option=com_content&view=article&id=39285&Itemid=343",
                                 icon("globe", lib = "glyphicon"),
                                 "Site COBRADI", 
                                 target = "_blank"))
  ),
  ##Barra Lateral
  dashboardSidebar(
    ##Menu da barra lateral 
    sidebarMenu(
      id = "sidebar",
      # 1 menu item
      menuItem("Dataset",
               tabName = "data",
               icon = icon("database")),
      # 2 menu item 
      menuItem("Visualização",
               tabName = "viz",
               icon = icon("chart-line")),
      # 3 menu item 
      menuItem("Informações",
               tabName = "info",
               icon = icon("info-circle"))
    )
  ),
  
  ##Corpo do DashBoard
  dashboardBody(
    tabItems(
      # 1 tab item
      tabItem(tabName = "data",
              #tab box
              tabBox(id = "t1", width = 12,
                     tabPanel("Sobre",
                              icon = icon("address-card"),
                              fluidPage( 
                              fluidRow(
                                column(width = 12, align = "center", 
                                       tags$img(src = "https://www.ipea.gov.br/portal/images/topo_site_cobradi.jpg",
                                                width = 1100,
                                                height = 242
                                                 ),
                                       tags$br(),
                                       ), 
                                       
                                column(width = 12,
                                       tags$br(),
                                       br(),
                                       span("Utilizando técnicas de ciência de dados, este painel estima o Valor da Hora Técnica para a pesquisa Cooperação Brasileira para o Desenvolvimento Internacional 2021.", style = "font-size:16px"),
                                       br(),
                                       span("Todos os dados utilizados neste painel constam em bases públicas do Governo Federal Brasileiro e podem ser encontradas em ", style = "font-size:16px"),
                                       a(href = "https://dados.gov.br/dataset/afastamento-remunerado", "https://dados.gov.br/dataset/afastamento-remunerado",  style = "font-size:16px"),
                                       span("."),
                                       br(),
                                       p("Críticas e sugestões das instituições participantes da pesquisa são muito bem-vindas para o aprimoramento das estimativas.", style = "font-size:16px"),
                                       br(),
                                       tags$p("Equipe COBRADI.", style = "font-size:16px")
                                              )
                              ))
                     ),
                     
                     tabPanel(title = "Base de Dados",
                              icon = icon("table"),
                              fluidPage(
                                titlePanel("Base de Dados Afastamentos (2021)"),
                                
                                # Create a new Row in the UI for selectInputs
                                fluidRow(
                                  column(6, 
                                         selectInput("UF",
                                                     "Estado:",
                                                     c("TODOS",
                                                       unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)))
                                  ),
                                  column(6,
                                         selectInput("instituicao",
                                                     "Instituição:",
                                                     c("TODOS",
                                                       unique(afastamentos_2021$Nome_orgao_de_origem)))
                                  ),
                                  #column(4,
                                  #       dateRangeInput(inputId = "data_afastamento",
                                  #                      label =  "Data Afastamento:", format = "yyyy",
                                  #                      start = min(as.character(afastamentos_2$`Ano Início Afastamento`)),
                                  #                      end   = max(as.character(afastamentos_2$`Ano Início Afastamento`))
                                  #)),
                                  
                                  column(width = 12,
                                         box(title = "Base Afastamentos",
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             width = "100%",
                                             DT::dataTableOutput("dataT", height = "100px")))
                                  )
                                )
                              ),
                    
                     tabPanel(title = "Estrutura dos Dados",
                              icon = icon("cubes"),
                              fluidPage(
                                titlePanel(""),
                                
                                # Create a new Row in the UI for selectInputs
                                fluidRow(
                                  column(width = 12,
                                         box(title = "Estrutura da Base de Dados de Afastamentos (2021)",
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             width = "100%",
                                             gt_output("str")))
                       )
                      )
                     ),
                     
                     tabPanel(title = "Sumário Estatístico",
                              icon = icon("list-alt"),
                              verbatimTextOutput("summary")))
      ),
      
      # 2 tab item 
      tabItem(tabName = "viz",
              #tab box
              tabBox(id = "t2", width = 12,
                     
                     #I - tab panel 
                     tabPanel(title = "Valor da Hora Técnica",
                              icon = icon("dollar"),
                              value = "trends",
                              fluidPage(
                                fluidRow(
                                  column(width = 12,
                                         box(title = "Filtros", width = "100%",
                                             column(width = 6,
                                                    box(width = "100%",
                                                        selectizeInput(inputId = "select_UF",
                                                                       label =  "Estados:",
                                                                       choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                       multiple = T, 
                                                                       selected = "TODOS"))
                                             ),
                                             
                                             column(width = 6,
                                                    box(width = "100%",
                                                        selectizeInput(inputId = "descricao_2",
                                                                       label = "Descrição do Afastamento:",
                                                                       choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                       multiple = T, options = list(maxItems = 5),
                                                                       selected = "TODOS")))
                                         )
                                  )     
                                ),
                                fluidRow(
                                  column(width = 12,
                                         box(title = "Tabela 1",
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             width = "100%",
                                             gt_output(outputId = "tabela"))))
                                )
                              ),
                     
                     #II - tab panel
                     tabPanel(title = "Histograma",
                              icon = icon("chart-bar"),
                              value = "trends",
                              #fluid row 1
                              fluidRow(
                                column(width = 12,
                                       box(title = "Filtros", width = "100%",
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "uf_2",
                                                                     label =  "Estados:",
                                                                     choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                     multiple = T, 
                                                                     selected = "TODOS"))
                                           ),
                                           
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "descricao_4",
                                                                     label = "Descrição do Afastamento:",
                                                                     choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                     multiple = T, options = list(maxItems = 5),
                                                                     selected = "TODOS")))
                                       )
                                )     
                              ),
                              
                              #fluidrow 2  
                              fluidRow(
                                column(width = 12,
                                       box(title = "Histograma - Valor da Hora Técnica (2021)",
                                           status = "primary", 
                                           solidHeader = TRUE, 
                                           collapsible = TRUE,
                                           width = "100%",
                                           plotlyOutput("histplot"))
                      )
                     )
                    ),
                     
                     #III - tab panel
                     tabPanel(title = "BoxPlot",
                              icon = icon("bar-chart-o", lib = "font-awesome"),
                              value = "trends",
                              
                              fluidRow(
                                column(width = 12,
                                       box(title = "Filtros", width = "100%",
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "uf_1",
                                                                     label =  "Estados:",
                                                                     choices = c("TODOS", unique(afastamentos_2021$UF_da_UPAG_de_vinculacao)),
                                                                     multiple = T, 
                                                                     selected = "TODOS"))
                                           ),
                                           
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "descricao_3",
                                                                     label = "Descrição do Afastamento:",
                                                                     choices = c("TODOS", unique(afastamentos_2021$Descricao_do_afastamento)),
                                                                     multiple = T, options = list(maxItems = 5),
                                                                     selected = "TODOS"))),
                                       )
                                )     
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       box(width = "100%",
                                         title = "BoxPlot - Valor da Hora Técnica (2021)",
                                           status = "primary", 
                                           solidHeader = TRUE, 
                                           collapsible = TRUE, 
                                           plotlyOutput("boxplot"))
                                ))
           )
          )
         ),                   
      
      # 3 tab item 
      tabItem(tabName = "info",
              h1("Informações"),
              infoBox(title = "Contato",
                      icon = icon("envelope-square"),
                      subtitle = "Para mais informações e/ou feedback
                                         entre em contato: cobradi@ipea.gov.br
                                         [Respondemos em até 24h]."))
    )
  )
)

#-------------------------------------------------------------------------------#

#SERVER
server <- function(input, output, session){

#PRIMEIRO TAB ITEM  
  meus_dados_1 <-  reactive({
  ## filtro UF
  print(input)
  b <- afastamentos_2021
  if (! "TODOS" %in% input$UF){
    b <- b |> 
      filter(UF_da_UPAG_de_vinculacao %in% input$UF)
  }

  #Filtro Descricao 
  if(! "TODOS" %in% input$instituicao){
    b <- b |>
      filter(Nome_orgao_de_origem %in% input$instituicao)
  }
  return(b)
  }) 

  #DATATABLE 
  output$dataT <- DT::renderDataTable({
    DT::datatable(meus_dados_1(),
    options = list(autoWidth = TRUE,
                   pagelength = 5,
                   scrollX = TRUE,
                   scrollY = "100%",
                   columnDefs = list(list(
                     width = "110px", targets = "_all"))
    ))})
  
  
  #Estrutura dos Dados
  output$str <- render_gt({
    afastamentos_2021 |> 
      skimr::skim() |> 
      gt::gt()}) 

  #Sumario Estatistico 
  output$summary <- renderPrint({
    summary(afastamentos_2021)
  })

  
#------------------------------------------------------------------------------#  
#SEGUNDO TAB ITEM 
#filtro tabela   
  meus_dados <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$select_UF){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$select_UF)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_2){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_2)
      
    }
    return(a)
  })
  
  #OUTPUT TABELA 
  output$tabela <- render_gt({
    tabela <- meus_dados() |> 
      dplyr::summarize(instituicao = "Afastamentos Realizados por Órgãos Federais",
                       valor_medio_hora = ((sum(Rendimento_Liquido_Hora))/n())
      ) |> 
      gt(rowname_col = "Nome") |> 
      tab_header(
        title = md("**Valor da Hora Técnica para todos os Órgãos**"),
        subtitle = md("**Ano: 2021**")
      ) |> 
      cols_label(
        instituicao = md(""), 
        valor_medio_hora = md("**Valor Médio da Hora Técnica (R$)**")
      ) |> 
      opt_align_table_header(align = "left") |> 
      fmt_number(columns = 2) |> 
      cols_width(
        instituicao~px(200),
        valor_medio_hora~px(200)
      ) |> 
      tab_source_note(source_note = md("*Fonte: Elaboração Própria.*")
      ) |>
      data_color(
        columns = valor_medio_hora,
        colors = "blue"
      ) |> 
      tab_stubhead(
        label = md ("**Nome da Instituição**")
      ) |> 
      opt_table_font(
        font = google_font("Times New Roman"), 
        weight = 600 
      ) |> 
      cols_align(
        align = "center",
        columns = everything()
      )
  })

  #filtro boxplot 
  meus_dados_box <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$uf_1){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$uf_1)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_3){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_3)
      
    }
    return(a)
  })
  
  #OUTPUTBOXPLOT
  
  output$boxplot <- renderPlotly({
    boxplot <- meus_dados_box()|>
      plot_ly() |>
      add_boxplot(~Rendimento_Liquido_Hora) |> 
      layout(xaxis = list(title = "BoxPlot - Valor da Hora Técnica"))
  })

  #filtro histograma 
  meus_dados_hist <-  reactive({
    ## filtro UF
    print(input)
    a <- afastamentos_2021
    if (! "TODOS" %in% input$uf_2){
      a <- a |> 
        filter(UF_da_UPAG_de_vinculacao %in% input$uf_2)
    }
    #Filtro Descricao 
    if(! "TODOS" %in% input$descricao_4){
      a <- a |>
        filter(Descricao_do_afastamento %in% input$descricao_4)
      
    }
    return(a)
  })
  
  #OUTPUT HISTOGRAMA 
  output$histplot <- renderPlotly({
    hist <- meus_dados_hist() |> 
      plot_ly() |> 
      add_histogram(~Rendimento_Liquido_Hora) |> 
      layout(xaxis = list(title = "Histograma - Valor da Hora Técnica"))})
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)