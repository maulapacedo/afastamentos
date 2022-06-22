#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




#Carregar Pacotes 
pacman::p_load(shiny, data.table, dplyr, ggplot2, plotly, shinyWidgets, shinydashboard,
               readr, lubridate, DT, gt)

#Setar diretório
setwd("~/COBRADI/cobradi_app")

#Carregar base 
afastamentos <- readr::read_csv("base_afastamentos.csv", locale = locale(encoding = "latin1"))
colnames(afastamentos) <- c(
  "Descrição do Cargo", "Nome do Órgão de Origem", "UF", "Cidade da Residência",
  "Nível da Escolaridade", "Início do Afastamento", "Ano/Mês Referência",
  "Valor do Rendimento Líquido", "Descrição do Afastamento", "Ano Início Afastamento",
  "Mês Início Afastamento", "Rendimento Líquido Hora")

afastamentos$`Início do Afastamento` <-  as.Date(afastamentos$`Início do Afastamento`,
                                               format = "%Y-%m-%d")


#User Interface 
ui <- dashboardPage(
  ##Barra de cima 
  dashboardHeader(title = "COBRADI", 
                  titleWidth = 500,
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
                                column(width = 8, 
                                       tags$img(src = "logo_cobradi_colorido.jpg",
                                                width = 600,
                                                height = 300),
                                       tags$br(),
                                       tags$a("Logo por Rafael"), 
                                       align = "left"),
                                column(width = 4,
                                       tags$br(),
                                       tags$p("Essa base de dados contém..."))
                              ))
                     ),
                     
                     tabPanel(title = "Base de Dados",
                              icon = icon("table"),
                              fluidPage(
                                titlePanel("Base de Dados Afastamentos"),
                                
                                # Create a new Row in the UI for selectInputs
                                fluidRow(
                                  column(4, 
                                         selectInput("UF",
                                                     "Estado:",
                                                     c("All",
                                                       unique(afastamentos$`UF`)))
                                  ),
                                  column(4,
                                         selectInput("instituicao",
                                                     "Instituição:",
                                                     c("All",
                                                       unique(afastamentos$`Nome do Órgão de Origem`)))
                                  ),
                                  #column(4,
                                  #       dateRangeInput(inputId = "data_afastamento",
                                  #                      label =  "Data Afastamento:", format = "yyyy",
                                  #                      start = min(as.character(afastamentos$`Ano Início Afastamento`)),
                                  #                      end   = max(as.character(afastamentos$`Ano Início Afastamento`))
                                  #)),
                                  
                                  column(12,
                                         height = "500px",
                                         DT::dataTableOutput("dataT", height="100px")))
                                 )
                              ),
                    
                     tabPanel(title = "Estrutura dos Dados",
                              icon = icon("cube"),
                              verbatimTextOutput("str")),
                     
                     tabPanel(title = "Sumário Estatístico",
                              icon = icon("cubes"),
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
                                  column(width = 4,
                                         box(title = "Tabela 1",
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE, 
                                             width = "100%",
                                             gt_output(outputId = "tabela")))))
                              ),
                     
                     #II - tab panel
                     tabPanel(title = "Afastamentos por UF",
                              value = "trends",
                              #fluid row 1
                              fluidRow(
                                column(width = 12,
                                       box(title = "Filtros", width = "100%",
                                           column(width = 12,
                                                  box(width = "100%",
                                                      selectInput(inputId = "UF_2",
                                                                  label =  "Estados:",
                                                                  choices = c("TODOS", unique(afastamentos$UF)),
                                                                  selected = "TODOS")
                                                  ),
                                                  
                                                  box(title = "Tabela Base de Dados",
                                                      status = "danger", 
                                                      solidHeader = TRUE, 
                                                      collapsible = TRUE, 
                                                      width = "100%",
                                                      tableOutput("dt"))
                                           ),
                                           
                                           column(width = 6,
                                                  box(width = "100%",
                                                      dateRangeInput(inputId = "data_afastamento_2",
                                                                     label =  "Data Afastamento:",format = "yyyy-mm-dd",
                                                                     start = min(as.character(afastamentos$`Início do Afastamento`)),
                                                                     end   = max(as.character(afastamentos$`Início do Afastamento`)))
                                                  )
                                           ),
                                           
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "afastamento_2",
                                                                     label = "Descrição do Afastamento:",
                                                                     choices = c("TODOS", unique(afastamentos$`Descrição do Afastamento`)),
                                                                     multiple = T, options = list(maxItems = 5),
                                                                     selected = "TODOS"))))
                                )     
                              ),
                              
                              #fluidrow 2  
                              fluidRow(
                                column(width = 12, 
                                       box(width = "100%",
                                           plotlyOutput(outputId = "descricao", width = "100%")))
                                
                              )
                     ),
                     
                     #III - tab panel
                     tabPanel(title = "Distribuição Amostral",
                              icon = icon("fas fa-chart-area"),
                              value = "trends",
                              
                              fluidRow(
                                column(width = 12,
                                       box(title = "Filtros", width = "100%",
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "select_UF",
                                                                     label =  "Estados:",
                                                                     choices = c("TODOS", unique(afastamentos$UF)),
                                                                     multiple = T, 
                                                                     selected = "TODOS"))
                                           ),
                                           
                                           column(width = 6,
                                                  box(width = "100%",
                                                      selectizeInput(inputId = "descricao_2",
                                                                     label = "Descrição do Afastamento:",
                                                                     choices = c("TODOS", unique(afastamentos$`Descrição do Afastamento`)),
                                                                     multiple = T, options = list(maxItems = 5),
                                                                     selected = "TODOS"))),
                                       )
                                )     
                              ),
                              fluidRow(
                                column(width = 12,
                                       box(title = "BoxPlot - Valor do Rendimento Bruto Mensal",
                                           status = "primary", 
                                           solidHeader = TRUE, 
                                           collapsible = TRUE, 
                                           width = "100%",
                                           plotlyOutput("boxplot"))
                                ),
                              
                              
                              
                                column(width = 12,
                                       box(title = "Histograma - Valor do Rendimento Bruto Mensal",
                                           status = "primary", 
                                           solidHeader = TRUE, 
                                           collapsible = TRUE,
                                           width = "100%",
                                           plotlyOutput("histplot"))
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
  # Filter data based on selections
  output$dataT <- DT::renderDataTable(
    DT::datatable ({
    afast_filt <- afastamentos
    if (input$UF != "All") {
      afast_filt <- afast_filt[afast_filt$`UF` == input$UF, ]
    }
    if (input$instituicao != "All") {
      afast_filt <- afast_filt[afast_filt$`Nome do Órgão de Origem` == input$instituicao, ]
    }
    
    #if (input$data_afastamento != "All") {
    #  afast_filt <- afast_filt[(as.character(afastamentos$`Ano Início Afastamento`)) == input$data_afastamento, ]
    #}
    
    afast_filt
  },
  class = "compact" ,
  options = list(autoWidth = TRUE,
                 pagelength = 15,
                 scrollX = TRUE,
                 scrollY = "100%",
                 columnDefs = list(list(
                   width = "110px", targets = "_all"))
                 ))
  )
  #Estrutura dos Dados
  output$str <- renderPrint({
    str(afastamentos)
  })
  #Sumario Estatistico 
  output$summary <- renderPrint({
    summary(afastamentos)
  })
  
#SEGUNDO TAB ITEM 
  
  #OUTPUTBOXPLOT
  output$boxplot <- renderPlotly({
    boxplot <- afastamentos|>
      plot_ly() |>
      add_boxplot(~`Valor do Rendimento Líquido`) |> 
      layout(xaxis = list(title = "Valor do Rendimento Bruto"))
    
  })
  
  #OUTPUT HISTOGRAMA 
  output$histplot <- renderPlotly({
    hist <- afastamentos |> 
    plot_ly() |> 
    add_histogram(~`Valor do Rendimento Líquido`) |> 
    layout(xaxis = list(title = "Valor do Rendimento Bruto"))})
  
  #OUTPUT TABELA 
  output$tabela <- render_gt({
    tabela <- afastamentos |> 
    dplyr::summarize(instituicao = "Afastamentos Realizados por Órgãos Federais",
                     valor_medio_hora = ((sum(`Rendimento Líquido Hora`))/n())
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
}



# Run the application 
shinyApp(ui = ui, server = server)