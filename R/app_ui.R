#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @depends shinyBS
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    
    shinydashboardPlus::dashboardPagePlus(
      
      # header ------------------------------------------------------------------
      
      header = shinydashboardPlus::dashboardHeaderPlus(
        title = "GEOBOX",
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
      ),
      
      # Menu lateral ------------------------------------------------------------
      
      
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          
          shinydashboard::menuItem(
            text = "Base de Dados",
            #tabName = "studio",
            icon = shiny::icon("server"),
            
            shinydashboard::menuSubItem(
              text = "Inserir Dados",
              icon = shiny::icon("database"),
              tabName = "inicial_data"
            ),
            
            shinydashboard::menuSubItem(
              text = "Engenharia de Dados",
              icon = shiny::icon("layer-group"),
              tabName = "data_eng"
            ),
            
            shinydashboard::menuSubItem(
              text = "Modelagem Urbana",
              icon = shiny::icon("map-marked-alt"),
              tabName = "city_modelling"
            )
          ),
          
          
          
          shinydashboard::menuItem(
            text = "Explorar Dados",
            #tabName = "studio",
            icon = shiny::icon("search"),
            
            shinydashboard::menuSubItem(
              text = "Tabela Explorat\u00F3ria",
              icon = shiny::icon("table"),
              tabName = "table_analysis"
            ),
            
            shinydashboard::menuSubItem(
              text = "An\u00E1lise Explorat\u00F3ria",
              icon = shiny::icon("chart-bar"),
              tabName = "explo_analysis"
            )
          ),
          
          
          shinydashboard::menuItem(
            text = "Est\u00FAdio de Modelagem",
            #tabName = "studio",
            icon = shiny::icon("cog"),
            
            shinydashboard::menuSubItem(
              text = "Regress\u00E3o Linear",
              icon = shiny::icon("dice"),
              tabName = "modelling"
            )
          ),
          
          
          shinydashboard::menuItem(
            text = "Calcular",
            #tabName = "studio",
            icon = shiny::icon("money-bill-alt"),
            
            shinydashboard::menuSubItem(
              text = "Unico Im\u00F3vel",
              icon = shiny::icon("calculator"),
              tabName = "estimative_panel"
            ),
            
            shinydashboard::menuSubItem(
              text = "V\u00e1rios Im\u00F3veis",
              icon = shiny::icon("calculator"),
              tabName = "estimative_panel_mult"
            )
            
          ), #fim do menu estudio modelagem
          
          
          shinydashboard::menuItem(
            text = "Salvar/Exportar",
            icon = shiny::icon("rocket"),
            tabName = "export_data"
          )#,
          
          # 
          # shinydashboard::menuItem(
          #   text = "Laudos",
          #   #tabName = "studio",
          #   icon = shiny::icon("file-invoice-dollar"),
          #   # menu laudo
          #   
          #   shinydashboard::menuItem(
          #     text = "Laudo de Avalia\u00E7\u00E3o",
          #     tabName = "dashboard",
          #     icon = shiny::icon("file-signature")
          #   ),
          #   
          #   # menu analise
          #   shinydashboard::menuItem(
          #     text = "Laudo de An\u00E1lise (LAE)",
          #     tabName = "lae_report",
          #     icon = shiny::icon("dashboard")
          #   )
          # ),
          
          # shiny::actionButton("brow", "Acionar Browser", width = "100%")
          #checkboxInput("brow2", "Acionar Browser 2")
          
        )
      ),
      
      
      
      # BD - BODY ---------------------------------------------------------------
      
      body = shinydashboard::dashboardBody(
        
        shinyjs::useShinyjs(),
        shinyFeedback::useShinyFeedback(),
        shinyEffects::setShadow(class = "box"),
        waiter::use_waiter(),
        
        
        ## CSS cor de fundo
        shiny::tags$head(shiny::tags$style(shiny::HTML('
/* cor cinza na tabela de analise quando o dado e desabilitado */
.excluded { color: rgb(211,211,211); font-style: italic; }

         /* fundo */
         .content-wrapper, .right-side {
                              background-color: 	#c4c4c4;
                              }

         /* logo */
        .skin-blue .main-header .logo {
                              background-color: 	#182024;
                              color: #FFFFFF
                              }

        /* logo quando hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: 	#182024;
        }

        /* botao de side bar quando hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ffdfba;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: 	#182024;
                              }



        /* sidebar principal a esquerda
        .skin-blue .main-sidebar {
                              background-color: #9c9c9c;

        } */

        /* Links no menu lateral cor da letra e do fundo
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{

                              color: #ffdfba;
        } */



        /* active selected tab in the sidebarmenu
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #baffc9;
                              }*/


        /* links do sidebar quando hovered
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ffdfba;
         } */

         /* notificao shiny
          .shiny-notification {
          position:fixed;
             top: calc(50%);
             left: calc(40%);
          }*/

             body {overflow-y: scroll;} 
                                                   
           /* .sidebar { height: 50vh; overflow-y: auto; }*/
              
   '))),
        
        ## fim do CSS
        
        
        
        
        ## fim dos efeitos
        
        # ID - INSERCAO DE DADOS --------------------------------------------------
        
        
        shinydashboard::tabItems(
          
          shinydashboard::tabItem(
            tabName = "inicial_data",
            
            
            
            shinydashboardPlus::boxPlus(
              title = "Insira um Arquivo V\u00E1lido",
              closable = FALSE,
              width = 12,
              status = "primary",
              solidHeader = FALSE,
              collapsible = FALSE,
              
              
              # ID - File Path ----------------------------------------------------------
              
              
              
              shiny::fluidRow(
                shiny::fileInput(
                  inputId = "file_path",
                  label = "Local do Arquivo",
                  placeholder = "Arquivos .rds, .xls, .xlsx, .csv",
                  accept = NULL,
                  buttonLabel = "Buscar",
                  width = "50%"),
                align = 'center'),
              
              
              # ID - Configuracoes de Leitura -------------------------------------------
              
              
              shiny::fluidRow(
                shinyWidgets::dropdownButton(
                  right = FALSE,
                  up = FALSE,
                  inline = TRUE,
                  status = "primary",
                  circle = FALSE,
                  icon = shiny::icon("wrench"),
                  width = "400px",
                  #size = "sm",
                  label = "Configura\u00E7\u00F5es de Leitura",
                  tooltip = TRUE,
                  
                  shiny::tabsetPanel(
                    
                    shiny::tabPanel(
                      title = ".csv",
                      shiny::br(),
                      
                      shiny::selectInput(
                        inputId = "csv_config_encoding",
                        label =  "Codifica\u00E7\u00E3o dos caracteres:", 
                        choices = c("UTF-8", "latin1", "ASCII"), 
                        selected = "UTF-8"),
                      
                      shiny::numericInput(
                        inputId = "csv_config_skip_lines",
                        label = "Ignorar as linhas iniciais",
                        value = 0,
                        min = 0,
                        max = 20,
                        step = 1
                      ),
                      
                      shiny::textInput(
                        inputId = "csv_config_delim", 
                        label = "Delimitador de Colunas", 
                        value = ";"),
                      
                      shinyWidgets::radioGroupButtons(
                        inputId = "csv_config_decimal",
                        label = "Marcador Decimal",
                        choices = c("V\u00EDrgula" = ",", "Ponto" = "."),
                        justified = TRUE
                      )
                    ),
                    
                    shiny::tabPanel(
                      
                      title = "Excel",
                      shiny::br(),
                      
                      shiny::numericInput(
                        inputId = "excel_config_sheet",
                        label = "Aba da planilha a ser lida",
                        value = 1,
                        min = 1,
                        max = 20,
                        step = 1
                      ),
                      
                      shiny::numericInput(
                        inputId = "excel_config_skip_lines",
                        label = "Ignorar as linhas iniciais",
                        value = 0,
                        min = 0,
                        max = 20,
                        step = 1
                      )
                      
                      # ,
                      # shinyWidgets::radioGroupButtons(
                      #   inputId = "excel_config_decimal",
                      #   label = "Marcador Decimal",
                      #   choices = c("V\u00EDrgula" = ",", "Ponto" = "."),
                      #   justified = TRUE
                      # )
                    ),
                    
                    shiny::tabPanel(
                      title = "Geo",
                      
                      shiny::br(),
                      shiny::tags$p("Considerar o mesmo EPSG que o Google Maps (4326) na aus\u00EAncia de identifica\u00E7\u00E3o EPSG na planilha:"),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "geo_config_epsg_default",
                        label = "EPSG: 4326", 
                        status = "warning",
                        right = TRUE
                      ),
                      
                      
                      
                      
                      shiny::hr(),
                      
                      shiny::tags$p("Remover da base de dados dados que n\u00E3o possuam:"),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "geo_config_filter_lat",
                        label = "Latitude", 
                        status = "warning",
                        right = TRUE
                      ),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "geo_config_filter_lng",
                        label = "Longitude", 
                        status = "warning",
                        right = TRUE
                      ),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "geo_config_filter_epsg",
                        label = "ESPG", 
                        status = "warning",
                        right = TRUE
                      )
                    )
                  )
                  
                ),
                align = 'center'),
              
              shiny::br(),
              
              
              
              # ID - Iniciar Programa ---------------------------------------------------
              
              
              
              shiny::fluidRow(
                
                # shinyWidgets::actionBttn(
                #   inputId = "start",
                #   label = "Iniciar Programa",
                #   style = "stretch",
                #   color = "primary",
                #   block = FALSE,
                #   no_outline = FALSE,
                #   size = "md"),
                
                shiny::actionButton(
                  inputId = "start",
                  label = "Iniciar Programa",
                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                
                align = 'center') %>% shinyjs::disabled()
              
            ), #fim da box
            
            
            
            
            # ID - Instrucoes ---------------------------------------------------------
            
            shiny::fluidRow(
              
              
              shinydashboardPlus::boxPlus(
                title = "Informa\u00E7\u00F5es do Modelo",
                closable = FALSE,
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = FALSE,
                
                
                
                shiny::dateInput(
                  "data_criacao", 
                  "Data de Cria\u00E7\u00E3o:", 
                  language = "pt-BR", 
                  format = "dd/mm/yyyy", 
                  value = NA, width = "150px"),
                
                shiny::textAreaInput(
                  "descricao_modelo", 
                  "Descri\u00E7\u00E3o do Modelo", 
                  height = "200px"),
                
                shiny::actionButton(
                  "save_model_information",
                  "Salvar Informa\u00E7\u00F5es", 
                  width = "100%")
                
              )
              
              
            )
          ),
          
          
          
          # ED - ENGENHARIA DE DADOS ------------------------------------------------
          
          
          shinydashboard::tabItem(
            tabName = "data_eng",
            
            
            
            # ED - Estatistica Descritiva ---------------------------------------------
            
            
            
            shiny::fluidRow(
              
               #shinydashboardPlus::boxPlus(
                # title = "Estat\u00EDstica Descritiva",
                # status = "primary",
                # width = 12,
                # closable = FALSE,
                # collapsible = TRUE,
                # solidHeader = FALSE,
                # collapsed = TRUE,
                # enable_sidebar = TRUE,
                # sidebar_start_open = FALSE,
                # sidebar_width = 35,
                # #sidebar_background = "#c4c4c4",
                # sidebar_content = shiny::tagList(
                #   
                #   
                #   shinyWidgets::pickerInput(
                #     "DE_descriptive_table_show",
                #     "Visualizar nesse painel:",
                #     
                #     choices = c(
                #       "Vari\u00E1vel",
                #       "Tipo", 
                #       "Valores Faltantes", 
                #       "Taxa de Completos", 
                #       "Vari\u00E1veis Texto", 
                #       "Vari\u00E1veis Data", 
                #       "Vari\u00E1veis Data/Hora", 
                #       "Vari\u00E1veis Fator", 
                #       "Vari\u00E1veis L\u00F3gicas", 
                #       "Vari\u00E1veis Num\u00E9ricas"),
                #     
                #     selected = c(
                #       "Vari\u00E1vel",
                #       "Tipo", 
                #       "Valores Faltantes", 
                #       "Taxa de Completos"),
                #     
                #     multiple = TRUE,
                #     width = "100%",
                #     
                #     options = shinyWidgets::pickerOptions(
                #       actionsBox = TRUE,
                #       deselectAllText = "Nenhuma",
                #       header = "Informa\u00E7\u00F5es a Visualizar:",
                #       noneSelectedText = "Nada Selecionado",
                #       selectAllText = "Todas",
                #       selectedTextFormat = "count",
                #       maxOptions = NULL,
                #       maxOptionsText = NULL,
                #       multipleSeparator = ", "
                #     )
                #     # ,
                #     # 
                #     # choicesOpt = list(
                #     #   
                #     #   content = c(
                #     #     "pppppppppppooooooooooooooo") %>% 
                #     #     format_choices(3)
                #     # )
                #   )
                # ),
                # 
                # shiny::tags$div(
                #   style = 'overflow-x: auto; overflow-y: auto; min-height: 300px',
                #   
                #   DT::dataTableOutput("DE_descriptive_table") 
                # )
              #)
            ),
            
            
            # ED - Informacoes Adicionais ---------------------------------------------
            
            
            
            shiny::fluidRow(
              
              shinydashboardPlus::boxPlus(
                title = "Var\u00E1veis: NBR 14.653",
                status = "primary",
                width = 12,
                closable = FALSE,
                collapsible = TRUE,
                solidHeader = FALSE,
                collapsed = TRUE,
                enable_sidebar = FALSE,
                sidebar_start_open = FALSE,
                sidebar_width = 35,
                #sidebar_background = "#c4c4c4",
                
                # shinyWidgets::pickerInput(
                #   "DE_addtional_info_select_var",
                #   "Editar as informa\u00E7\u00F5es da vari\u00E1vel:",
                #   choices = NULL,
                #   selected = NULL,
                #   multiple = TRUE,
                #   
                #   options = shinyWidgets::pickerOptions(
                #     actionsBox = FALSE,
                #     deselectAllText = "Nenhuma",
                #     dropupAuto = FALSE,
                #     header = "Selecione uma \u00FAnica vari\u00E1vel",
                #     noneSelectedText = "Nada Selecionado",
                #     selectAllText = "Todas",
                #     liveSearch  = TRUE,
                #     liveSearchNormalize = TRUE,
                #     liveSearchPlaceholder = "Pesquisar",
                #     showContent = FALSE,	
                #     maxOptions = 1,
                #     maxOptionsText = NULL,
                #     multipleSeparator = ", ",
                #     width = "100%"
                #   ),
                #   
                #   choicesOpt = list(
                #     content = NULL
                #   )
                # ),
                
                
                shiny::selectizeInput(
                  'DE_addtional_info_select_var', 
                  label = "", 
                  choices = NULL,
                  selected = NULL,
                  #multiple = FALSE,
                  options = list(
                    highlight = TRUE,
                    maxItems = 1,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                  )
                ),
                
                
                
                shiny::fluidRow(
                  
                  shiny::column(
                    width = 4, 
                    shiny::textAreaInput(
                      "DE_var_description",
                      "Descri\u00E7\u00E3o da Vari\u00E1vel:", 
                      height = "250px") 
                  ),
                  shiny::column(
                    width = 4, 
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "DE_var_type",
                      label = "Defina o tipo da vari\u00E1vel, conforme a NBR 14.653:", 
                      choices = choices_nbr_var_type(),
                      selected = "",
                      icon = shiny::icon("check"), 
                      bigger = TRUE,
                      status = "info",
                      animation = "jelly"
                    ),
                    
                    shiny::h6(
                      shiny::textOutput("check_naaa")
                    ),
                    
                    shiny::div(style = "overflow-y: auto; max-height: 100px",  
                               shiny::h5(
                                 shiny::textOutput("check_micro")
                               )
                    )
                  ),
                  
                  shiny::column(
                    width = 4, 
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "DE_var_behavior",
                      label = "Defina o comportamento esperado da vari\u00E1vel selecionada em rela\u00E7\u00E3o \u00E0 vari\u00E1vel dependente:",
                      choices = choices_var_behavior(),
                      selected = "",
                      icon = shiny::icon("check"),
                      bigger = TRUE,
                      status = "info",
                      animation = "jelly"
                    )
                  )
                ),
                
                shiny::actionButton(
                  "DE_save_var_additional_info", 
                  "Salvar Informa\u00E7\u00F5es", 
                  width = "100%")
              )# fim da box
            ), #fim da fluidrow
            
            
            # shiny::actionButton("botao_add_1", "adicionar 1"),
            # shiny::actionButton("botao_add_3", "adicionar 3"),
            # shiny::actionButton("botao_remove_1", "remover 1"),
            # shiny::actionButton("botao_remove_3", "remover 3"),
            # shiny::actionButton("botao_modificar_1", "modificar 1"),
            # shiny::actionButton("botao_modificar_3", "modificar 3"),
            
            
            
            # ED - Tratamento e Manipulacao -------------------------------------------
            
            
            shiny::fluidRow(
              shinydashboardPlus::boxPlus(
                title = "Tratamento e Manipula\u00E7\u00E3o",
                collapsible = TRUE,
                collapsed = TRUE,
                closable = FALSE,
                width = 12,
                status = "primary",
                
                
                # ED - Escolha das variaveis ----------------------------------------------
                
                shiny::h4("1. Escolha a(s) vari\u00E1vel(eis) para aplicar uma a\u00E7\u00E3o:"),
                
                # shinyWidgets::pickerInput(
                #   "DE_vars_manipulate",
                #   "",
                #   choices = NULL,
                #   selected = NULL,
                #   multiple = TRUE,
                #   
                #   options = shinyWidgets::pickerOptions(
                #     actionsBox = FALSE,
                #     deselectAllText = "Nenhuma",
                #     dropupAuto = FALSE,
                #     header = "Selecione uma ou mais vari\u00E1veis",
                #     noneSelectedText = "Nada Selecionado",
                #     selectAllText = "Todas",
                #     showContent = FALSE,	
                #     liveSearch  = TRUE,
                #     liveSearchNormalize = TRUE,
                #     liveSearchPlaceholder = "Pesquisar",
                #     selectedTextFormat = "count",
                #     
                #     maxOptions = NULL,
                #     maxOptionsText = NULL,
                #     multipleSeparator = ", ",
                #     
                #     width = "100%"
                #   ),
                #   
                #   choicesOpt = list(
                #     content = NULL
                #   )
                # ),
                
                shiny::selectizeInput(
                  'DE_vars_manipulate', 
                  label = "", 
                  choices = NULL,
                  selected = NULL,
                  #multiple = TRUE,
                  options = list(
                    highlight = TRUE,
                    maxItems = NULL,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                    
                  )
                ),
                
                
                
                # ED - Definicao da Acao --------------------------------------------------
                
                
                shiny::column(
                  width = 4,
                  
                  shiny::h4("2. Escolha a a\u00E7\u00E3o:"),
                  
                  # shinyWidgets::pickerInput(
                  #   inputId = "choose_action_modify",
                  #   label =  "",
                  #   multiple = FALSE,
                  #   width = "100%",
                  # 
                  #   choices = c(
                  #     "Opera\u00E7\u00F5es Matem\u00E1ticas entre Vari\u00E1veis" = "oper_mat_var",
                  #     "Opera\u00E7\u00F5es Matem\u00E1ticas com Constante" = "opera_mat_cte",
                  #     "Filtragem de Dados" = "filter_data",
                  #     "Trabalhar Vari\u00E1veis" = "transmute_var",
                  #     "Convers\u00E3o Computacional" = "convert_var",
                  #     "Opera\u00E7\u00F5es Vari\u00E1veis Data" = "oper_date",
                  #     "Remover Vari\u00E1veis" = "remove_var",
                  #     "Renomear Vari\u00E1veis" = "rename_var"
                  #   )
                  # 
                  # ),
                  
                  shiny::selectizeInput(
                    'choose_action_modify', 
                    label = "", 
                    choices = c(
                      "Opera\u00E7\u00F5es Matem\u00E1ticas entre Vari\u00E1veis" = "oper_mat_var",
                      "Opera\u00E7\u00F5es Matem\u00E1ticas com Constante" = "oper_mat_cte",
                      "Filtragem de Dados" = "filter_data",
                      "Trabalhar Vari\u00E1veis" = "transmute_var",
                      "Estrutura Computacional" = "convert_class",
                      "Opera\u00E7\u00F5es Vari\u00E1veis Data" = "oper_date",
                      "Remover Vari\u00E1veis da Base de Dados" = "remove_var",
                      "Alterar o nome de uma Vari\u00E1vel" = "rename_var"
                    ),
                    selected = NULL,
                    #multiple = FALSE,
                    options = list(
                      highlight = TRUE,
                      maxItems = 1,
                      placeholder = "Nada Selecionado",
                      hideSelected = FALSE
                    )
                  ),
                  shiny::hr(),
                  
                  
                  # ED - Subopcoes Oper Mat Var ---------------------------------------------
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'oper_mat_var'",
                    
                    shiny::helpText("Realiza opera\u00E7\u00F5es matem\u00E1ticas, elemento a elemento, entre as vari\u00E1veis selecionadas"),
                    shiny::helpText("Os c\u00E1lculos s\u00E3o realizados da esquerda para a direita, logo a ordem de sele\u00E7\u00E3o das vari\u00E1veis impacta o resultado"),
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "oper_mat_var_operation",
                      label = "Especifique a opera\u00E7\u00E3o:",
                      choices = c(
                        "Soma" = "soma_var",
                        "Multiplica\u00E7\u00E3o" = "multiply_var",
                        "Divis\u00E3o" = "divide_var",
                        "Subtra\u00E7\u00E3o" = "subtract_var",
                        "M\u00E1ximo entre" = "max_between",
                        "M\u00EDnimo entre" = "min_between"
                      ),
                      icon = shiny::icon("check"),
                      bigger = FALSE,
                      status = "info",
                      animation = "jelly"
                    ),
                    shiny::textInput(
                      inputId = "oper_mat_var_new_name",
                      label = "Especifique um nome para a nova vari\u00E1vel:"
                    )
                  ),
                  
                  
                  # ED - Subopcoes Filtrar Dados --------------------------------------------
                  
                  
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'filter_data'",
                    
                    shiny::column(
                      width = 6, 
                      shiny::actionButton(
                        "ED_enable_all", 
                        "Habilitar Todos", 
                        width = "100%")
                    ),
                    
                    shiny::column(
                      width = 6, 
                      shiny::actionButton(
                        "ED_disable_all", 
                        "Desabilitar Todos", 
                        width = "100%")
                    ),
                    
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "con_filter_data_do",
                      label = "O que deseja fazer com os dados que atenderem as condi\u00E7\u00F5es especificadas:",
                      choices = c(
                        "Habilitar"                     = "enable_obs",
                        "Habilitar Somente Filtrados"   = "enable_obs_only",
                        "Desabilitar"                   = "disable_obs",
                        "Desabilitar Somente Filtrados" = "disable_obs_only",
                        "Exluir Filtrados"              = "exclude_data_filtered",
                        "Exluir n\u00E3o filtrados"          = "exclude_data_non_filtered",
                        "Converter"                     = "convert_to"
                      ),
                      icon = shiny::icon("check"),
                      inline = FALSE,
                      bigger = FALSE,
                      status = "info",
                      animation = "jelly"
                    ),
                    
                    shiny::conditionalPanel(
                      "input.con_filter_data_do == 'convert_to'",
                      
                      shinyWidgets::textInputIcon(
                        inputId = "con_convert_to",
                        label = "Converter para",
                        size ="sm",
                        icon = shiny::icon("sync-alt")
                        
                      )),
                    
                    
                    
                    shiny::h6("Quando mais de uma vari\u00E1vel selecionada, as condi\u00E7\u00F5es, para serem consideradas v\u00E1lidas, devem ser atingidas simultaneamente (E) ou n\u00E3o (OU)"),
                    shinyWidgets::switchInput(
                      inputId = "con_between_var",
                      onLabel  = "E" , 
                      offLabel  = "OU",
                      onStatus  = "success",
                      offStatus  = "success"
                    ),
                    
                    
                    shinyWidgets::dropdown(
                      # style = "unite", 
                      icon = icon("sliders"),
                      status = "success", 
                      width = "300px",
                      label = "Condi\u00E7\u00F5es em cada vari\u00E1vel",
                      up = TRUE,
                      animate = shinyWidgets::animateOptions(
                        enter = shinyWidgets::animations$sliding_entrances$slideInDown,
                        exit = shinyWidgets::animations$sliding_exits$slideOutUp, 
                        duration = .5),
                      
                      shiny::h6("Dentro de cada vari\u00E1vel, as condi\u00E7\u00F5es devem ser atingidas simultaneamente (E) ou n\u00E3o (OU)"),
                      shinyWidgets::switchInput(
                        inputId = "con_inside_var",
                        onLabel  = "E" , 
                        offLabel  = "OU",
                        value = FALSE,
                        onStatus  = "danger",
                        offStatus  = "danger"
                      ),
                      
                      
                      shiny::helpText("Preencha somente os filtros que deseja habilitar. Aqueles que n\u00E3o deseja utilizar, deixe em branco."),
                      
                      shinyWidgets::prettySwitch(
                        inputId = "con_remove_na",
                        label = "Remover NA", 
                        status = "warning",
                        slim = TRUE
                      ),
                      
                      shinyWidgets::textInputIcon(
                        inputId = "con_igual_a",
                        label = "Igual a:",
                        size ="sm",
                        icon = shiny::icon("equals")
                      ),
                      shinyWidgets::textInputIcon(
                        inputId = "con_diferente_de",
                        label = "Diferente de:",
                        size ="sm",
                        icon = shiny::icon("not-equal")
                      ),
                      shinyWidgets::textInputIcon(
                        inputId = "con_maior_que",
                        label = "Maior que:",
                        size ="sm",
                        icon = shiny::icon("greater-than")
                      ),
                      shinyWidgets::textInputIcon(
                        inputId = "con_maior_igual_a",
                        label = "Maior/Igual a:",
                        size ="sm",
                        icon = shiny::icon("greater-than-equal")
                      ),
                      shinyWidgets::textInputIcon(
                        inputId = "con_menor_que",
                        label = "Menor que:",
                        size ="sm",
                        icon = shiny::icon("less-than")
                      ),
                      shinyWidgets::textInputIcon(
                        inputId = "con_menor_igual_a",
                        label = "Menor/Igual a:",
                        size ="sm",
                        icon = shiny::icon("less-than-equal")
                      )
                    )
                  ),
                  
                  
                  
                  
                  # ED - Subopcoes Oper Mar Cte ---------------------------------------------
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'oper_mat_cte'",
                    
                    shiny::helpText("Realiza opera\u00E7\u00F5es matem\u00E1ticas entre um valor constante, definido pelo usu\u00E1rio, com cada uma das vari\u00E1veis escolhidas, individualmente"),
                    
                    shiny::helpText("Utilize ou ponto ou v\u00EDrgula, mas n\u00E3o os dois simultaneamente"),
                    
                    shiny::numericInput(
                      inputId = "oper_mat_cte_definition", 
                      label = "Defina o valor da constante", 
                      value = 0,
                      width = "100%"),
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "oper_mat_cte_operation",
                      label = "Especifique a opera\u00E7\u00E3o:",
                      choices = c(
                        "Soma" = "soma_cte",
                        "Multiplica\u00E7\u00E3o" = "multiply_cte",
                        "Divis\u00E3o" = "divide_cte",
                        "Subtra\u00E7\u00E3o" = "subtract_cte",
                        "M\u00E1ximo entre" = "max_between_cte",
                        "M\u00EDnimo entre" = "min_between_cte",
                        "Exponencia\u00E7\u00E3o" = "exponeciar_cte",
                        "Radicia\u00E7\u00E3o" = "raiz_cte"
                      ),
                      icon = shiny::icon("check"),
                      bigger = FALSE,
                      status = "info",
                      animation = "jelly"
                    ),
                    
                    shiny::textInput(
                      inputId = "oper_mat_cte_suffix",
                      label = "Especifique um sufixo para ser adicionado a cada vari\u00E1vel",
                      width = "100%",
                      value = "_novo_valor"
                    )
                    
                  ),
                  
                  
                  
                  # ED - Subopcoes Remover Variaveis ----------------------------------------
                  
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'remove_var'",
                    
                    shiny::helpText("Excluir todas as vari\u00E1veis que forem selecionadas acima."),
                    shiny::helpText("Essa a\u00E7\u00E3o n\u00E3o poder\u00E1 ser desfeita")
                    
                    
                    
                  ),    
                  
                  
                  
                  # ED -  Subopcoes Renomear Variaveis --------------------------------------
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'rename_var'",
                    
                    shiny::helpText("Renomeia uma vari\u00E1vel. Apenas uma vari\u00E1vel por vez"),
                    
                    shinyWidgets::textInputIcon(
                      "new_name_var", 
                      "Especifique o novo nome da vari\u00E1vel", 
                      size = "lg", 
                      placeholder = "Novo nome",
                      width = "100%")
                    
                  ),    
                  
                  
                  # ED - Subopcoes Estrutura Computacional ----------------------------------
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'convert_class'",
                    
                    shiny::helpText("Altera a classe de dados sob a qual a vari\u00E1vel (ou vari\u00E1veis) selecionada est\u00E1 armazenada"),
                    shiny::helpText("Essa convers\u00E3o altera a maneira como a vari\u00E1vel \u00E9 processada internamente. A convers\u00E3o sem crit\u00E9rios pode ocasionar perda de informa\u00E7\u00E3o"),
                    
                    
                    shinyWidgets::prettyRadioButtons(
                      inputId = "new_class",
                      label = "Converter para a classe:",
                      choices = c(
                        "Integer (a parte decimal ser\u00E1 truncada)" = "integer",
                        "Double (n\u00FAmeros reais)" = "double",
                        "Character(Texto)"          = "character",
                        "Factor (Fatores)"          = "factor",
                        "Logical (vari\u00E1vel booleana)"    = "logical"
                      ),
                      icon = shiny::icon("check"),
                      bigger = FALSE,
                      status = "info",
                      animation = "jelly"
                    ),
                    
                    shiny::textInput(
                      inputId = "new_class_suffix",
                      label = "Especifique um sufixo para ser adicionado a cada vari\u00E1vel",
                      width = "100%",
                      value = "_convertida"
                    )
                    
                  ),              
                  
                  
                  
                  
                  
                  
                  # ED - Subopcoes Transf var -----------------------------------------------
                  
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'transmute_var'",
                    
                    
                    selectInput(
                      "transmute_var_sub_options", 
                      "A\u00E7\u00E3o espec\u00EDfica", 
                      choices = c(
                        "Dicotomizar" = "dic", 
                        "Padronizar" = "pad", 
                        "Categorizar Intervalos" = "cat_var")),
                    
                    
                    shiny::conditionalPanel(
                      "input.transmute_var_sub_options == 'dic'",
                      
                      shiny::helpText("Cada n\u00EDvel da vari\u00E1vel selecionada ser\u00E1 transformado em uma nova coluna. O n\u00FAmero 1 na coluna indica se a observa\u00E7\u00E3o se enquadra nesse n\u00EDvel."),
                      shiny::helpText("A verifica\u00E7\u00E3o de micronumerosidade deve ser feita no painel acima")
                      
                    ),
                    
                    shiny::conditionalPanel(
                      "input.transmute_var_sub_options == 'pad'",
                      
                      shinyWidgets::materialSwitch(
                        inputId = "padronizar_med",
                        label = "Centralizar na m\u00E9dia" ,
                        value = TRUE,
                        status = "warning",
                        right = TRUE),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "padronizar_desv_pad",
                        label = "Dividir pelo desvio padrao" ,
                        value = TRUE,
                        status = "warning",
                        right = TRUE),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "padronizar_rem_NA",
                        label = "Desconsiderar NA`s",
                        value = FALSE,
                        status = "warning",
                        right = TRUE),
                      
                      
                      shiny::textInput(
                        "padronizar_suffix", 
                        "Sufixo", 
                        value = "_padronizada")
                      
                    ),
                    
                    
                    shiny::conditionalPanel(
                      "input.transmute_var_sub_options == 'cat_var'",
                      
                      shinyWidgets::prettyRadioButtons(
                        inputId = "cat_suboptions",
                        label = "Subdividir por:",
                        choices = c(
                          "Massa de dados" = "sub_quantile",
                          "Quantidade de grupos" = "sub_n",
                          "Intervalos Definidos" = "sub_interval"
                        ),
                        icon = shiny::icon("check"),
                        bigger = FALSE,
                        status = "info",
                        animation = "jelly",
                        inline = TRUE,
                        thick = TRUE,
                        width = "100%"
                      ),
                      
                      
                      shiny::conditionalPanel(
                        "input.cat_suboptions == 'sub_quantile'",
                        
                        shinyWidgets::materialSwitch(
                          inputId = "cat_quantile_ignore_NA",
                          label = "Ignorar NA`s na subdivis\u00E3o de massa",
                          value = FALSE,
                          status = "warning",
                          right = TRUE),
                        
                        shiny::textInput(
                          "cat_quantile_interval", 
                          "Insira os intervalos")
                        
                        # shiny::helpText("Cria subdivis\u00F5es na vari\u00E1vel com base na porcentagem acumulada de dados. Assim, o usu\u00E1rio especifica, em porcentagem, quais os intervalos de massa de dados acumulada. Por exemplo: "),
                        # shiny::helpText("Por exemplo, se o usu\u00E1rio digitar:"),
                        # shiny::helpText("30; 50"),
                        # shiny::helpText(shiny::HTML("O algoritimo ordenar\u00E1 a vari\u00E1vel do menor para o maior valor, calcular\u00E1 qual o valor da vari\u00E1vel que representa os 30% de dados acumulados e qual valor que est\u00E1 associado a 50% de dados.\nA partir da\u00ED o programa criar\u00E1 3 faixas: a primeira que vai do valor m\u00EDnimo da vari\u00E1vel at\u00E9 o valor correpondente a 30% dos dados, a segunda que compreende valores entre 30% e 50% dos dados e a terceira faixa que compreende o valor associado a 50% at\u00E9 o valor m\u00E1ximo da vari\u00E1vel")),
                        # shiny::helpText("A grafia de como os valores s\u00E3o apresentados \u00E9 importante: a porcentagem deve ser fornecida em valores variando de 0 a 100. O programa sempre divide o valor fornecido por 100 antes de proceder as opera\u00E7\u00F5es. As diferentes faixas devem ser separadas por ponto e v\u00EDrgula (;). A v\u00EDrgula \u00E9 entendida como separador decimal."),
                        # shiny::helpText("Outro exemplo. Se o usu\u00E1rio digitar no campo abaixo 10; 25,7; 78,9 o programa criar\u00E1 faixas para os primeiros 10% de 10% a 25,7%, de 25,7% a 78,9% e de 78,9% at\u00E9 100%. Nota-se que n\u00E3o \u00E9 necess\u00E1rio especificar os limites superiores e inferiores. Se o usu\u00E1rio digitar 25; 50; 75 o programa cria as subdivis\u00F5es para os quartis"),
                        # shiny::helpText("Nessa op\u00E7\u00E3o, somente vari\u00E1veis num\u00E9ricas s\u00E3o aceitas. A nota\u00E7\u00E3o [ significa intervalo fechado no n\u00FAmero. A nota\u00E7\u00E3o ( significa intervalo aberto no n\u00FAmero")
                      ),
                      
                      shiny::conditionalPanel(
                        "input.cat_suboptions == 'sub_n'",
                        
                        shiny::numericInput(
                          "cat_sub_n", 
                          "Quantos grupos", 
                          value = 1, 
                          min = 1, 
                          step = 1)
                        
                      ),
                      
                      
                      shiny::conditionalPanel(
                        "input.cat_suboptions == 'sub_interval'",
                        
                        shiny::textInput(
                          "cat_user_interval", 
                          "Insira os intervalos")
                      ),
                      
                      shiny::textInput(
                        "cat_suffix", 
                        "Sufixo das variaveis", 
                        value = "_categorizada"),
                      
                      shinyWidgets::materialSwitch(
                        inputId = "cat_convert_to_cod_alocado",
                        label = "Converter para C\u00F3digo Alocado",
                        value = FALSE,
                        status = "warning",
                        right = TRUE)
                      
                    )
                    
                  ),
                  
                  
                  
                  
                  # ED - Subopcoes Operacoes Data -------------------------------------------
                  
                  
                  
                  shiny::conditionalPanel(
                    "input.choose_action_modify == 'oper_date'", 
                    
                    selectInput(
                      "oper_date_sub_options", 
                      "", 
                      choices = c(
                        "Converter de Texto para Data" = "text_to_date",
                        "Converter Data para Num\u00E9rico" = "date_to_numeric"
                      )),
                    
                    
                    shiny::conditionalPanel(
                      "input.oper_date_sub_options == 'text_to_date'",
                      
                      shinyWidgets::prettyRadioButtons(
                        inputId = "text_to_date_format",
                        label = "Qual o formato de data armazenado na vari\u00E1vel:",
                        choices = c(
                          "M\u00EAs/Ano" = "my",  
                          "Dia/M\u00EAs/Ano" = "dmy",  
                          "Ano/M\u00EAs/Dia" = "ymd", 
                          "M\u00EAs/Dia/Ano" = "mdy", 
                          "Dia/M\u00EAs/Ano HH:MM:SS" = "dmy_hms",  
                          "Ano/M\u00EAs/Dia HH:MM:SS" = "ymd_hms", 
                          "M\u00EAs/Dia/Ano HH:MM:SS" = "mdy_hms"
                        ),
                        icon = shiny::icon("check"),
                        bigger = FALSE,
                        status = "info",
                        animation = "jelly"
                      ),
                      
                      selectInput(
                        "oper_date_sub_fuso", 
                        "Fuso hor\u00E1rio", 
                        choices = c(
                          "Bras\u00EDlia + 1" = "Brazil/DeNoronha",           
                          "Bras\u00EDlia" = "Brazil/East",                     
                          "Bras\u00EDlia - 1"= "Brazil/West",
                          "Bras\u00EDlia - 2"= "Brazil/Acre"                    
                        ),
                        selected = "Brazil/East"
                      )
                      
                      
                    ),
                    
                    shiny::conditionalPanel(
                      "input.oper_date_sub_options == 'date_to_numeric'",
                      
                      shinyWidgets::prettyRadioButtons(
                        inputId = "date_to_numeric",
                        label = "Qual o formato desejado",
                        choices = c(
                          "Ano" = "year",
                          "Ano M\u00EAs" = "yearmonth",
                          "Ano Semestre" = "yearsemester"
                        ),
                        icon = shiny::icon("check"),
                        bigger = FALSE,
                        status = "info",
                        animation = "jelly"
                      )
                      
                    ),
                    
                    textInput(
                      "text_to_date_suffix", 
                      "Defina uma sufixo",
                      value = "_data")
                    
                  ),
                  
                  shiny::actionButton(
                    "pre_processing", 
                    "Pr\u00E9-processamento",
                    width = "100%"),
                  
                  shinyWidgets::prettyRadioButtons(
                    "preview_type",
                    label = "Tipo de Preview",
                    choices = c("Valores", "Estrutura"),
                    selected = "Valores",
                    inline = TRUE,
                    icon = shiny::icon("check"),
                    animation = "jelly",
                    status = "info"
                    
                  ),
                  
                  shiny::sliderInput(
                    inputId = "perc_preview",
                    label =  "Intensidade de Preview",
                    min = 0,
                    max = 100,
                    value = 2,
                    step = 1,
                    post = "i"),
                  
                  
                  shiny::hr(),
                  
                  
                ), #fim da column
                
                shiny::column(
                  width = 8,
                  
                  shiny::h4("3. Compreenda a l\u00F3gica da a\u00E7\u00E3o e confirme a a\u00E7\u00E3o:"),
                  
                  
                  
                  shiny::div(
                    style = "overflow-x: auto; overflow-y: auto; height: 550px",
                    
                    shiny::column(
                      width = 6,
                      shiny::h4("Antes", align = "center"),
                      
                      shiny::div(style = "overflow-x: auto;",
                                 DT::dataTableOutput("preview_before"))
                      
                    ),
                    shiny::column(
                      width = 6,
                      shiny::h4("Depois", align = "center"),
                      shiny::div(style = "overflow-x: auto;",
                                 DT::dataTableOutput("preview_after"))
                    )
                  ),
                  
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "DE_aplicar_na_base_de_dados",
                    label = "Aplicar Opera\u00E7\u00E3o",
                    width = '100%')
                  
                  
                )
              )
            ), # fim da fluidRow
            
            
            # shinyBS::bsModal(
            #   "ED_preview_confirm", 
            #   "3. Compreenda a l\u00F3gica da a\u00E7\u00E3o e confirme a a\u00E7\u00E3o:",
            #   trigger = "pre_processing",
            #   size = "large",
            #   
            #   #div(
            #     #style = "min-width: 600px; min-height: 400px;",
            #     
            #     
            #     shiny::column(
            #       width = 4,
            #       shinyWidgets::prettyRadioButtons(
            #         "preview_type",
            #         label = "Tipo de Preview",
            #         choices = c("Valores", "Estrutura"),
            #         selected = "Valores", 
            #         inline = TRUE,
            #         icon = shiny::icon("check"),
            #         animation = "jelly",
            #         status = "info"
            #         
            #       )
            #     ),
            #     
            #     shiny::column(8,
            # shiny::sliderInput(
            #   inputId = "perc_preview",
            #   label =  "Massa de dados para Preview",
            #   min = 0,
            #   max = 100,
            #   value = 2,
            #   step = 1,
            #   post = "%")),
            #  
            #     
            #     shiny::column(
            #       width = 6, 
            #       shiny::h4("Antes", align = "center"),
            #       
            #       shiny::div(style = "overflow-x: auto;", 
            #           DT::dataTableOutput("preview_before"))
            #       
            #     ),
            #     shiny::column(
            #       width = 6,
            #       shiny::h4("Depois", align = "center"),
            #       shiny::div(style = "overflow-x: auto;", 
            #           DT::dataTableOutput("preview_after"))
            #     ),
            #   
            #   shiny::br(),
            #   shiny::actionButton(
            #     inputId = "DE_aplicar_na_base_de_dados",
            #     label = "Aplicar Opera\u00E7\u00E3o",
            #     width = '100%')
            #   #)
            #                  
            #                  
            # ),
            
            
            # ED - Editar Valores -----------------------------------------------------
            
            
            shiny::fluidRow(
              
              shinydashboardPlus::boxPlus(
                title = "Dados: Edi\u00E7\u00E3o de Valores",
                status = "primary",
                width = 12,
                closable = FALSE,
                collapsible = TRUE,
                solidHeader = FALSE,
                collapsed = TRUE,
                enable_sidebar = FALSE,
                sidebar_start_open = FALSE,
                sidebar_width = 35,
                
                shiny::selectizeInput(
                  'DE_data_obs_edit', 
                  label = "Selecione as var\u00E1veis", 
                  choices = NULL,
                  selected = NULL,
                  #multiple = FALSE,
                  options = list(
                    highlight = TRUE,
                    maxItems = NULL,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                  )
                ),
                
                
               
                shiny::actionButton("data_edit_init", 
                                    "Iniciar Editor", 
                                    width = "100%"),
                
                shiny::br(),
                
                shinyBS::bsModal(
                  id = "modal_Edit_Data",
                  title = "Editor de Valores",
                  trigger = "data_edit_init",
                  size = "large",
                  
                  rhandsontable::rHandsontableOutput("DE_data_edit"),
                  
                  shiny::br(),

                  shiny::actionButton("save_data_edit",
                                      "Salvar Altera\u00E7\u00F5es",
                                      width = "100%")
                ),
               
                
                #rHandsontableOutput("DE_data_edit"),
                
                shiny::br()
                
                
                
              )# fim da BOX
            ) #fim da fluidRow
            
          ),
          
          
          # TA - TABLE ANALYSIS -----------------------------------------------------
          
          shinydashboard::tabItem(
            
            tabName = "table_analysis",
            
            shiny::fluidRow(
              shinydashboardPlus::boxPlus(
                title = "Configura\u00E7\u00F5es",
                status = "primary",
                width = 12,
                closable = FALSE,
                collapsible = TRUE,
                solidHeader = FALSE,
                collapsed = FALSE,
                enable_sidebar = FALSE,
                sidebar_start_open = FALSE,
                sidebar_width = 35,
                
                
                shiny::selectizeInput(
                  'table_analysis_var', 
                  label = "Vari\u00E1veis a analisar", 
                  choices = NULL,
                  selected = NULL,
                  #multiple = FALSE,
                  options = list(
                    highlight = TRUE,
                    maxItems = NULL,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                  )
                ),
                
                shiny::selectizeInput(
                  'table_analysis_cats', 
                  label = "Vari\u00E1veis de agrupamento", 
                  choices = NULL,
                  selected = NULL,
                  #multiple = FALSE,
                  options = list(
                    highlight = TRUE,
                    maxItems = NULL,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                  )
                ),
                
                shinyWidgets::prettySwitch("TA_remove_na", "Remover NA`s", slim = TRUE,  value = FALSE),
                
                shinyWidgets::checkboxGroupButtons(
                  inputId = "table_analysis_options",
                  label = "Fun\u00E7\u00F5es a serem aplicadas",
                  choices = c(
                    "Quantidade de Elementos" = "N",
                    "Qtde Valores Distintos" = "N_dist",
                    "Qtde de NA`s" = "qtde_na",
                    "M\u00E9dia" = "M\u00E9dia", 
                    "Mediana" = "Mediana", 
                    "Moda" = "Moda", 
                    "Desvio Padr\u00E3o" = "DP",
                    "Valor M\u00EDnimo" = "Min",
                    "1 Quartil" = "1Q",
                    "3 Quartil" = "3Q",
                    "Valor M\u00E1ximo" = "Max"
                  ),
                  individual = TRUE,
                  checkIcon = list(
                    yes = shiny::tags$i(
                      class = "fa fa-circle", 
                      style = "color: steelblue"),
                    no = shiny::tags$i(
                      class = "fa fa-circle-o", 
                      style = "color: steelblue"))
                )
              )#fim box 
            ), #fim fluidrow
            
            shiny::fluidRow(
              shinydashboardPlus::boxPlus(
                title = "Tabela de Valores",
                status = "primary",
                width = 12,
                closable = FALSE,
                collapsible = TRUE,
                solidHeader = FALSE,
                collapsed = FALSE,
                enable_sidebar = FALSE,
                sidebar_start_open = FALSE,
                sidebar_width = 35,
                
                DT::dataTableOutput("table_analysis_DT") %>% 
                  shiny::div(style = "height: 400px; overflow-y: auto")
                
                
                
              )  #fim box
            ), #fim fluidRow
            
            data_panel_UI("TA_data_panel", "Modelagem")
            
          ), # fim tab item      
          
          
          
          # CM - CITY MODELLING -----------------------------------------------------
          
          
          shinydashboard::tabItem(
            
            tabName = "city_modelling",
            
            
            # CM - MAPA ---------------------------------------------------------------
            
            
            shiny::div(
              class="outer",
              
              # shiny::tags$head(
              #   # Include our custom CSS
              #   shiny::includeCSS("www/styles.css")
              # ),
              
              
              leaflet::leafletOutput(
                outputId = "city_modelling",
                width = "100%",
                height = "100%"),
              
              
              # CM - CONTROLES ----------------------------------------------------------
              
              
              
              absolutePanel(
                id = "controls",
                class = "panel panel-default",
                fixed = FALSE,
                draggable = TRUE,
                top = 60,
                left = "auto",
                right = 20,
                bottom = "auto",
                width = 450,
                height = "auto",
                
                shiny::br(),
                
                
                shiny::selectizeInput(
                  'aba_polo', 
                  label = shiny::tags$b("A\u00E7\u00E3o:"), 
                  choices = c(
                    "Desenhar Polos Influenciantes" = "geo_polos",
                    "Importar Arquivos Vetoriais" = "geo_shpp",
                    "Vincular Regi\u00E3o ao Modelo de Regress\u00E3o" = "geo_modell",
                    "Converter Elemento Vetorial em Polo" = "convert_vector_to_pole",
                    "Importar/Exportar Modelagem Urbana" = "inout_maps",
                    "Voar para o dado" = "fly_to",
                    "Editar Coordenadas de um dado" = "alt_coo",
                    "Filtrar Dados" = "spatial_filter"
                  ),
                  selected = NULL,
                  #multiple = FALSE,
                  width = "100%",
                  options = list(
                    highlight = TRUE,
                    maxItems = 1,
                    placeholder = "Nada Selecionado",
                    hideSelected = FALSE
                  )
                ),
                
                
                
                
                # shinyWidgets::pickerInput(
                #   inputId = "aba_polo", 
                #   label = shiny::tags$b("A\u00E7\u00E3o:"), 
                #   
                #   width = "100%",
                #   choices = list(
                #     
                #     Mapas = 
                #       c(
                #         "geo_polos", 
                #         "geo_shpp", 
                #         "geo_modell",
                #         "inout_maps"
                #       ),
                #     
                #     Editar = 
                #       c(
                #         "fly_to",
                #         "alt_coo"
                #         # "crop_region",
                #         # "append_data",
                #         # "ploygon_into_geo_influence",
                #         # "info"
                #       )
                #     # ,
                #     # 
                #     # Analises = c(
                #     #   "analysis_explo", 
                #     #   "analysis_explo2"
                #     #   
                #     # )
                #     
                #   ),
                #   
                #   choicesOpt = list(
                #     content = c(
                #       "<div style='color: black;'>Desenhar Polos Influenciantes</div>",
                #       "<div style='color: black;'>Importar Shape Files</div>",
                #       "<div style='color: black;'>Vincular Regi\u00E3o ao Modelo de Regress\u00E3o</div>",
                #       "<div style='color: black;'>Importar/Exportar Modelagem Urbana</div>",
                #       "<div style='color: black;'>Voar para o dado</div>",
                #       "<div style='color: black;'>Editar Coordenadas de um dado</div>"
                #       # "<div style='color: black;'>Recorte Territorial (Em constru\u00E7\u00E3o)</div>",
                #       # "<div style='color: black;'>Vincular Base de Dados a Mapa j\u00E1 inserido (Em constru\u00E7\u00E3o)</div>",
                #       # "<div style='color: black;'>Transformar em Polo Inlfuenciante (Em constru\u00E7\u00E3o)</div>",
                #       # "<div style='color: black;'>Informa\u00E7\u00E3o</div>",
                #       # "<div style='color: black;'>An\u00E1lise Explorat\u00F3ria I (Em constru\u00E7\u00E3o)</div>",
                #       # "<div style='color: black;'>An\u00E1lise Explorat\u00F3ria II (Em constru\u00E7\u00E3o)</div>"
                #       
                #     )
                #   )
                # ),
                
                
                
                
                conditionalPanel(
                  "input.aba_polo == 'geo_polos'",
                  
                  # CM - GEO POLO INSERIR ---------------------------------------------------
                  
                  
                  
                  shinyWidgets::radioGroupButtons(
                    "geo_polos_action",
                    label = "",
                    choices = c(
                      "Inserir" = "add",
                      "Remover" = "rem"),
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon")
                    )
                  ),
                  
                  conditionalPanel(
                    "input.geo_polos_action == 'add'",
                    
                    
                    shiny::h3("Para Inserir", align = "center"),
                    
                    shiny::helpText("1. Defina uma geometria utilizando as ferramentas de edi\u00E7\u00E3o (obrigat\u00F3rio). Preview da geometria:"),
                    shiny::plotOutput("preview_geo_insert", height = 150),
                    
                    shiny::helpText("2. Defina um nome para a geometria (obrigat\u00F3rio)"),
                    
                    
                    shiny::textInput(
                      inputId = "geo_polo_nome_var",
                      width = "100%",
                      label = "Especifique um nome para o polo:"),
                    
                    
                    shiny::br(),
                    
                    shiny::helpText("3. Descreva o que essa geometria representa (opicional)"),
                    
                    shiny::textAreaInput(
                      inputId = "geo_polo_descr_var",
                      label = "Descri\u00E7\u00E3o do Polo:",
                      width = "100%",
                      rows = 2),
                    
                    shiny::br(),
                    
                    shiny::helpText("4. Para adicionar, clique em Alicar Geo Polo"),
                    
                    shiny::actionButton(
                      inputId = "apply_geo_polo",
                      label = "Aplicar Geo Polo",
                      width = '100%'),
                    shiny::hr(),
                    
                    
                    shiny::helpText("5. O Aplicativo calcular\u00E1 a menor dist\u00e2ncia de cada ponto at\u00E9 a geometria, em metros, e a adicionar\u00E1 como vari\u00E1vel da planilha"),
                    
                  ),
                  
                  # CM - GEO POLO REMOVER ---------------------------------------------------
                  
                  conditionalPanel(
                    "input.geo_polos_action == 'rem'",
                    
                    shiny::h3("Remover", align = "center"),
                    
                    shiny::helpText("Dentre os polos influenciantes existentes, selecione aquele que deseja remover."),
                    
                    shiny::selectInput(
                      inputId = "polo_a_remover",
                      label =  "Polo a remover:", 
                      choices = NULL, 
                      width = "100%"),
                    
                    shiny::plotOutput("preview_geo_polo_remover", height = 150),
                    
                    #shiny::br(),
                    shiny::helpText("A vari\u00E1vel que representa sua dist\u00e2ncia aos pontos ser\u00E1 removida da planilha, bem ser\u00E1 removida sua geometria do mapa"),
                    shiny::br(),
                    
                    shiny::actionButton(
                      inputId = "rem_geo_polo",
                      label = "Remover Geo Polo",
                      width = '100%')
                  )),
                
                
                # CM - SHAPE FILES --------------------------------------------------------
                
                
                
                
                conditionalPanel(
                  "input.aba_polo == 'geo_shpp'",
                  
                  shinyWidgets::radioGroupButtons(
                    "geo_polos_action3",
                    label = "",
                    choices = c(
                      "Inserir" = "add",
                      "Remover" = "rem"),
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon")
                    )
                  ),
                  
                  
                  conditionalPanel(
                    "input.geo_polos_action3 == 'add'",
                    
                    # CM - SHP INSERIR --------------------------------------------------------
                    
                    
                    
                    shiny::h3("Para Inserir", align = "center"),
                    
                    
                    shiny::helpText("1. Defina um nome para o mapa a ser inserido:"),
                    
                    shiny::textInput(
                      inputId = "geo_shp_insert_name", 
                      label = "Defina um nome para o mapa a ser inserido.",
                      width = "100%"),
                    
                    shiny::helpText("2. Selecione os Shape Files. Eles s\u00E3o de extens\u00E3o .shp, .shx, .dbf e .prj e devem possuir o mesmo nome"),
                    
                    shiny::fileInput(
                      inputId = "geo_shp_insert",
                      label = "Selecione os 4 arquivos", 
                      multiple = TRUE, 
                      width = "100%"
                    ),
                    
                    shiny::selectInput(
                      "encoding_shp", 
                      "Escolha um encoding", 
                      choices = c("UTF-8", "latin1"),
                      selected = "UTF-8", 
                      width = "100%"
                    ),
                    
                    shiny::actionButton(
                      inputId = "geo_shp_insert_button",
                      label = "Aplicar Shape File", 
                      width = "100%"
                    )
                    
                    
                  ),
                  
                  conditionalPanel(
                    "input.geo_polos_action3 == 'rem'",
                    
                    # CM - SHP REMOVER --------------------------------------------------------
                    
                    
                    shiny::h3("Remover", align = "center"),
                    
                    
                    shiny::selectInput(
                      inputId = "shp_a_remover",
                      label =  "Mapa a remover:", 
                      width = "100%",
                      choices = NULL),
                    
                    #shiny::br(),
                    shiny::helpText("O mapa ser\u00E1 exclu\u00EDdo, bem como as vari\u00E1veis associadas"),
                    shiny::br(),
                    
                    shiny::actionButton(
                      inputId = "rem_shp",
                      label = "Remover Mapa",
                      width = '100%')
                  )
                  
                ), #fim do conditional panel geo_shpp
                
                
                
                # CM - REGIAO MODELO ------------------------------------------------------
                
                
                
                conditionalPanel(
                  "input.aba_polo == 'geo_modell'",
                  
                  
                  shinyWidgets::radioGroupButtons(
                    "model_region",
                    label = "",
                    choices = c(
                      "Inserir" = "add",
                      "Remover" = "rem"),
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon")
                    )
                  ),
                  
                  
                  # CM - VINCULAR REGIAO MODELO ---------------------------------------------
                  
                  
                  conditionalPanel(
                    "input.model_region == 'add'",
                    
                    shiny::h3("Vincular", align = "center"),
                    
                    shiny::textInput(
                      inputId = "geo_model_nome",
                      label = "Especifique um nome para a regi\u00E3o:",
                      width = "100%"),
                    
                    shiny::plotOutput("regiao_preview_add", height = 150),
                    
                    shiny::textAreaInput(
                      inputId = "geo_model_descr",
                      label = "Descri\u00E7\u00E3o do Regi\u00E3o:",
                      width = "100%",
                      rows = 2),
                    
                    shiny::actionButton(
                      inputId = "apply_geo_model",
                      label = "Vincular Regi\u00E3o ao Modelo",
                      width = '100%')),
                  
                  
                  
                  # CM - DESVINCULAR REGIAO MODELO ------------------------------------------
                  
                  
                  
                  conditionalPanel(
                    "input.model_region == 'rem'",
                    
                    shiny::h3("Desvincular", align = "center"),
                    
                    
                    shiny::selectInput(
                      inputId = "geo_model_rem",
                      label = "Regi\u00E3o:",
                      width = "100%", 
                      choices = NULL),
                    
                    
                    shiny::plotOutput("regiao_preview_rem", height = 150),
                    
                    
                    
                    shiny::actionButton(
                      inputId = "geo_model_rem_button",
                      label = "Desvincular Regi\u00E3o ao Modelo",
                      width = '100%')
                  )
                  
                ), # fim do painel Regiao modelo
                
                
                # CM - Converter Vector para Geopolo --------------------------------------
                
                
                
                conditionalPanel(
                  "input.aba_polo == 'convert_vector_to_pole'",
                  
                  shiny::helpText("1. Clique sobre uma shape"),
                  
                  shiny::helpText("Grupo selecionado:"),
                  shiny::textOutput("vector_to_pole_group"),
                  
                  shiny::helpText("Preview do Elemento:"),
                  
                  shiny::plotOutput("vector_to_pole_preview", height = 150),
                  
                  shiny::textInput("vector_to_pole_new_name", "Nome para o Polo Influenciante (obrigat\u00F3rio)"),
                  shiny::textAreaInput("vector_to_pole_desc", "Descri\u00E7\u00E3o (opicional)"),
                  
                  
                  shiny::actionButton("convert_vector_to_pole_apply", "Converter", width = "100%")
                  
                  
                ),
                
                
                
                
                
                # CM - IN OUT MODELAGEM URBANA --------------------------------------------
                
                
                conditionalPanel(
                  "input.aba_polo == 'inout_maps'",
                  
                  shinyWidgets::radioGroupButtons(
                    "inout_modelagem_urbana",
                    label = "",
                    choices = c(
                      "Importar" = "inn",
                      "Exportar" = "outt"),
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon")
                    )
                  ),
                  
                  shiny::conditionalPanel(
                    "input.inout_modelagem_urbana == 'inn'",
                    
                    
                    # CM - IN MODELAGEM URBANA  -----------------------------------------------
                    
                    
                    
                    shiny::h3("Importar Modelagem Urbana", align = "center"),
                    
                    shiny::br(),
                    
                    shiny::helpText("Insira um arquivo que tenha sido criado previamente nessa plataforma de modelagem urbana. Seu formato deve ser do tipo .rds e ter sido salvo por meio da Exporta\u00E7\u00E3o da Modelagem Urbana (Arquivo Espacial Geobox)"),
                    
                    shiny::fileInput(
                      "insert_geo_influence_rds",
                      "Inserir .rds",
                      width = "100%")
                    
                    
                  ),
                  
                  
                  
                  shiny::conditionalPanel(
                    "input.inout_modelagem_urbana == 'outt'",
                    
                    # CM - OUT MODELAGEM URBANA  ----------------------------------------------
                    
                    
                    shiny::h3("Exportar Modelagem Urbana", align = "center"),
                    shiny::hr(),
                    
                    shinyWidgets::awesomeCheckboxGroup(
                      inputId = "mapas_exportar_incluir",
                      label = "Incluir as segunites camadas:",
                      
                      choices = c(
                        "Polos Influenciantes" = "geo_influence", 
                        "Mapas" = "geo_shp", 
                        "Regi\u00F5es de V\u00EDnculo com Modelo" = "geo_model",
                        "Banco de Dados" = "geo_obs"
                      ),
                      
                      
                      selected = c("geo_influence", "geo_shp")
                      
                    ),
                    
                    
                    shiny::h4("Exportar como:", align = "center"),
                    
                    shiny::hr(),
                    shiny::h4("Arquivo Espacial Geobox"),
                    
                    shiny::downloadButton(
                      "download_geo_influence_rds",
                      "Baixar como .rds",
                      style = "width:100%;"),
                    
                    shiny::hr(),
                    
                    
                    shiny::h4("Arquivos Shape Files"),
                    
                    shiny::downloadButton(
                      "download_geo_shape_files",
                      "Baixar como .shp .prj .dbf .shx",
                      style = "width:100%;"),
                    
                    
                    shiny::hr(),
                    
                    
                    shiny::h4("Arquivo Google"),
                    
                    shiny::downloadButton(
                      "download_geo_influence_kml",
                      "Baixar como .kml",
                      style = "width:100%;")
                    
                    
                  )
                  
                ),
                
                
                
                # CM - INFO PONTO ---------------------------------------------------------
                
                
                conditionalPanel(
                  "input.aba_polo == 'info'",
                  
                  shiny::helpText("O algoritmo para c\u00E1lculo das dist\u00e2ncias \u00E9 o Great Distance")
                ),
                
                
                # CM - EDITAR COORDENADAS PONTO -------------------------------------------
                
                
                conditionalPanel(
                  "input.aba_polo == 'alt_coo'",
                  
                  shiny::helpText("1. Clique sobre o dado do mapa cuja coordenada se deseja alterar"),
                  
                  shiny::column(
                    width = 12,
                    align="center" ,
                    
                    shiny::tableOutput("confirm_obs_selection")
                    
                  ),
                  
                  
                  shiny::helpText("3. Clique sobre o mapa na nova localiza\u00E7\u00E3o desejada para o dado previamente selecionado"),
                  
                  shiny::column(
                    width = 12,
                    align="center" ,
                    
                    shiny::tableOutput("obs_new_coordinates")
                    
                  ),
                  shiny::div(shiny::textOutput("alt_coo_dist2"), align = "center"),
                  
                  shiny::helpText("4. Clique sobre o bot\u00E3o abaixo para alterar a localiza\u00E7\u00E3o"),
                  shiny::actionButton("change_marker_coo", "Alterar para as novas coordenadas", width = "100%")
                  
                  
                ),
                
                
                
                
                # CM - FLY TO -------------------------------------------------------------
                
                
                conditionalPanel(
                  "input.aba_polo == 'fly_to'",
                  
                  shiny::hr(),
                  shiny::h5("Por coordenada"),
                  
                  shiny::helpText("Digite em graus decimais. Separador num\u00E9rico: v\u00EDrgula"),
                  
                  shiny::numericInput("fly_lat", "Latitude:", value = "0", width = "100%"),
                  shiny::numericInput("fly_lng", "Longitude:", value = "0", width = "100%"),
                  #shiny::sliderInput("fly_zoom", "Zoom", min = 1, max = 20, value = 5),
                  shiny::actionButton("fly_button", "Voar para Coordenadas", width = "100%"),
                  
                  
                  shiny::hr(),
                  shiny::h5("Por Sele\u00E7\u00E3o de Dados"),
                  
                  shiny::selectInput("fly_to_obs", "Selecione o dado:", choices = NULL),
                  
                  shiny::actionButton("fly_button2", "Focar no Dado", width = "100%"),
                  
                  
                ), #fim do flyt too
                
                
                # CM - Filtrar Espacial ---------------------------------------------------
                
                conditionalPanel(
                  "input.aba_polo == 'spatial_filter'",
                  
                  plotOutput("preview_spatial_filter_region"),
                  
                  shiny::column(
                    width = 6, 
                    shiny::actionButton(
                      "CM_enable_all", 
                      "Habilitar Todos", 
                      width = "100%")
                  ),
                  
                  shiny::column(
                    width = 6, 
                    shiny::actionButton(
                      "CM_disable_all", 
                      "Desabilitar Todos", 
                      width = "100%")
                  ),
                  
                  
                  shinyWidgets::prettyRadioButtons(
                    inputId = "spatial_filter_data_do",
                    label = "O que deseja fazer com os dados que estiverem internos ao pol\u00EDgono:",
                    choices = c(
                      "Habilitar"                     = "enable_obs",
                      "Habilitar Somente Internos"    = "enable_obs_only",
                      "Desabilitar"                   = "disable_obs",
                      "Desabilitar Somente Internos"  = "disable_obs_only",
                      "Exluir Internos"               = "exclude_data_filtered",
                      "Exluir Externos"               = "exclude_data_non_filtered"
                    ),
                    icon = shiny::icon("check"),
                    inline = FALSE,
                    bigger = FALSE,
                    status = "info",
                    animation = "jelly"
                  ),
                  
                  shiny::actionButton("spatial_filter_go", "Aplicar filtro", width = "100%")
                ) #fim do spatil filter
                
              ) # fim do absolut panel
              
            ), #fim do div
            
            
            
            
            
            
            # esse modal \u00E9 do alterar coordenadas. ele nao pode ser inserido dentro
            # de um conditional panel entao foi inserido aqui mesmo
            shinyBS::bsModal(
              id = "alt_coordinates", 
              title = "Confirma Altera\u00E7\u00E3o de Coordenada?",
              trigger = "change_marker_coo",
              size = "large",
              
              shiny::column(
                width = 6, 
                shiny::h4("Localiza\u00E7\u00E3o Anterior"),
                leaflet::leafletOutput("ponto_antes")
              ),
              
              shiny::column(
                width = 6, 
                shiny::h4("Localiza\u00E7\u00E3o Posterior"),
                leaflet::leafletOutput("ponto_depois")
              ),
              
              shiny::br(),
              
              shiny::div(shiny::textOutput("alt_coo_dist"), align = "center"),
              
              shiny::br(),
              
              shiny::actionButton("confirm_marker_coo", "Confirmar Altera\u00E7\u00E3o", width = "100%")
              
              
            ) #fim BsModal petencente ao alterar coordenadas
            
            
            
            
            
          ), #fim do tab item
          
          
          # AE - ANALISE EXPLORATORIA -----------------------------------------------
          
          
          shinydashboard::tabItem(
            
            tabName = "explo_analysis",
            
            
            # AE - ANALISE GEOMETRICA -------------------------------------------------
            
            
            shiny::fluidRow(
              
              shinydashboardPlus::boxPlus(
                width = 6,
                #height = 450,
                title = "An\u00E1lise Geom\u00E9trica",
                status = "primary",
                closable = FALSE,
                solidHeader = FALSE,
                collapsed = FALSE,
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_start_open = TRUE,
                
                sidebar_width = 50,
                #sidebar_background = "#c4c4c4",
                sidebar_content = shiny::tagList(
                  
                  shinyWidgets::radioGroupButtons(
                    "explo_analy_qtde_eixos",
                    "Escolha o tipo de an\u00E1lise:",
                    choices = c("1 Eixo" = "uni",
                                "2 Eixos" = "bi",
                                "3 Eixos" = "multi"),
                    width = "100%",
                    selected = "uni",
                    justified = TRUE
                  ),
                  
                  shiny::conditionalPanel(
                    "input.explo_analy_qtde_eixos == 'uni'",
                    
                    
                    AE_picker_var("AE_uni_x", "Eixo Horizontal:"),
                    AE_picker_transf("AE_uni_x_trns", "Transformada Horizontal:"),
                    
                    shiny::br(),
                    AE_picker_var("AE_uni_group", "Agrupar por:"),
                    shiny::br(),
                    
                    
                    shinyWidgets::prettySwitch(
                      inputId = "plot_1d_show_mean_median",
                      label = "Exibir Media e Mediana", 
                      slim = TRUE,
                      value = FALSE,
                      status = "primary"
                    )
                    
                    
                  ),
                  shiny::conditionalPanel(
                    "input.explo_analy_qtde_eixos == 'bi'",
                    
                    AE_picker_var("AE_bi_x", "Eixo Horizontal:"),
                    AE_picker_transf("AE_bi_x_trns", "Transformada Horizontal:"),
                    
                    AE_picker_var("AE_bi_y", "Eixo Vertical:"),
                    AE_picker_transf("AE_bi_y_trns", "Transformada Vertical:"),
                    shiny::hr(),
                    
                    AE_picker_var("AE_bi_group", "Agrupar por:"),
                    
                  ),
                  
                  shiny::conditionalPanel(
                    "input.explo_analy_qtde_eixos == 'multi'",
                    
                    
                    AE_picker_var("AE_tri_z", "Eixo Z (vari\u00E1vel dependente):"),
                    AE_picker_transf("AE_tri_z_trns", "Transformada Z:"),
                    
                    AE_picker_var("AE_tri_x", "Eixo X:"),
                    AE_picker_transf("AE_tri_x_trns", "Transformada X:"),
                    
                    AE_picker_var("AE_tri_y", "Eixo Y:"),
                    AE_picker_transf("AE_tri_y_trns", "Transformada Y:"),
                    
                    
                    shiny::br(),
                    
                    AE_picker_var("AE_tri_group", "Agrupar por:"),
                    
                  )
                  
                ), # fim do tagList
                
                shiny::conditionalPanel(
                  "input.explo_analy_qtde_eixos == 'uni'",
                  
                  plotly::plotlyOutput("explo_plot_uni", height = 550)
                  
                ),
                shiny::conditionalPanel(
                  "input.explo_analy_qtde_eixos == 'bi'",
                  
                  plotly::plotlyOutput("explo_plot_bi", height = 550)
                  
                ),
                shiny::conditionalPanel(
                  "input.explo_analy_qtde_eixos == 'multi'",
                  
                  plotly::plotlyOutput("explo_plot_multi", height = 550)
                  
                )
                
              ),
              
              
              # AE- An\u00E1lise Geogr\u00E1fica --------------------------------------------------
              
              
              shinydashboardPlus::boxPlus(
                width = 6,
                #height = 450,
                title = "An\u00E1lise Geogr\u00E1fica",
                status = "primary",
                closable = FALSE,
                solidHeader = FALSE,
                collapsed = FALSE,
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_start_open = FALSE,
                sidebar_width = 50,
                #sidebar_background = "#c4c4c4",
                sidebar_content = shiny::tagList(
                  
                  AE_picker_var("AE_geo_group", "Agrupar por:"),
                  AE_picker_var("AE_geo_focus", "Focar na dado:")
                  
                ),
                
                leaflet::leafletOutput("AE_map", height = 550)
              )
            ), # FIM DO FLUID ROW
            
            
            
            # AE - PAINEL DE DADOS ----------------------------------------------------
            data_panel_UI("AE_data_panel", "Modelagem")
            
            
            
            
            
          ), # Fim analise Exploratoria
          
          
          
          
          
          # MO - MODELAGEM ----------------------------------------------------------
          
          
          shinydashboard::tabItem(
            
            tabName = "modelling",
            
            shiny::fluidRow(
              
              # MO - PAINEL DE MODELAGEM ------------------------------------------------
              
              
              # shinydashboardPlus::boxPlus(
              #   width = 8,
              #   #height = 450,
              #   title = "Painel de Modelagem",
              #   status = "primary",
              #   closable = FALSE,
              #   solidHeader = FALSE,
              #   collapsed = FALSE,
              #   collapsible = TRUE,
              #   enable_sidebar = TRUE,
              #   sidebar_start_open = FALSE,
              #   sidebar_width = 35,
              #   #sidebar_background = "#c4c4c4",
              #   sidebar_content = shiny::tagList(),
              
              
              shinydashboard::tabBox(
                width = 9,
                title = "Painel de Modelagem",
                
                shiny::tabPanel(
                  title = "Geral",
                  shiny::tags$div(
                    style = 'overflow-x: auto; overflow-y: auto; min-height: 500px',
                    
                    
                    # shiny::column(
                    #   width = 2,
                    # 
                    #   shiny::h4("M\u00E9tricas Iniciais", align = "center"),
                    #   shiny::br(),
                    # 
                    #   shinydashboard::valueBoxOutput("dados_utilizados", width = NULL),
                    #   shinydashboard::valueBoxOutput("var_utilizadas", width = NULL),
                    #   shinydashboard::valueBoxOutput("graus_liberdade", width = NULL),
                    #   shinydashboard::infoBoxOutput("f_valor", width = NULL)
                    # ),
                    
                    
                    shiny::uiOutput("some_metrics"),
                    
                    
                    
                    
                    shiny::column(
                      width = 6, 
                      shiny::h4("Coeficientes R", align = "center"),
                      
                      shiny::column(
                        width = 6,
                        shiny::h4("Modelagem", align = "center"), 
                        
                        flexdashboard::gaugeOutput("r_trns", height = "auto"),
                        flexdashboard::gaugeOutput("r2_trns_scale", height = "auto"),
                        flexdashboard::gaugeOutput("r2_adj_trns_scale", height = "auto"),
                      ),
                      shiny::column(
                        
                        width = 6,
                        shiny::h4("Estimativa", align = "center"), 
                        
                        flexdashboard::gaugeOutput("r_natural", height = "auto"),
                        flexdashboard::gaugeOutput("r2_natural_scale", height = "auto"),
                        flexdashboard::gaugeOutput("r2_adj_natural_scale", height = "auto")
                      )
                    ),
                    
                    shiny::column(
                      width = 6,
                      
                      shiny::h4("Indices M\u00E1ximos (0 a 100)", align = "center"),
                      shiny::br(),
                      plotly::plotlyOutput("indices_maximos")
                    )
                    
                    
                    
                    
                  ) # fim do div
                ), # fim do tab box
                
                # PN - Painel de Modelagem ------------------------------------------------
                
                
                shiny::tabPanel(
                  title = "Painel de Norma",
                  
                  shiny::tags$div(
                    style = 'overflow-x: auto; overflow-y: auto; min-height: 500px',
                    
                    
                    shinydashboardPlus::boxPlus(
                      title = "Ader\u00EAncia",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::fluidRow(
                        shiny::column(6, "Ader\u00EAncia - Modelagem", align = "center"),
                        shiny::column(6, "Ader\u00EAncia - Estimativa", align = "center")
                      ),
                      
                      plotly::plotlyOutput(("res_all"), height = '600px')
                      
                    ),
                    
                    shinydashboardPlus::boxPlus(
                      title = "An\u00e1lise dos Res\u00EDduos",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::fluidRow(
                        align = "center",
                        
                        shinyWidgets::dropdownButton(
                          
                          shiny::checkboxInput(
                            "pn_check_res_hist_mod", 
                            "Histograma Res\u00EDduos Padronizados Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_hist_est", 
                            "Histograma Res\u00EDduos Padronizados Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_qqplot_mod", 
                            "QQ-Plot Res\u00EDduos Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_qqplot_est", 
                            "QQ-Plot Res\u00EDduos Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_perc_mod", 
                            "Percentuais Te\u00F3ricos Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_perc_est", 
                            "Percentuais Te\u00F3ricos Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_padro_mod", 
                            "Res\u00EDduos Padronizados por Valor Calculado - Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_padro_est", 
                            "Res\u00EDduos Padronizados por Valor Calculado - Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_padro_var_indep_mod", 
                            "Res\u00EDduos Padronizados por Vari\u00e1vel Independente - Modelagem", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_padro_var_indep_est", 
                            "Res\u00EDduos Padronizados por Vari\u00e1vel Independente - Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_map_mod", 
                            "Mapa Res\u00EDduos Modelagem", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_map_est", 
                            "Mapa Res\u00EDduos Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_cook_dist", 
                            "Dist\u00e2ncia de Cook", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_res_table", 
                            "Tabela dos Res\u00EDduos", 
                            value = TRUE),
                          
                          right = FALSE,
                          up = FALSE,
                          inline = TRUE,
                          status = "info",
                          circle = FALSE,
                          icon = shiny::icon("gear"),
                          width = "500px",
                          size = "xs",
                          label = "Visualizar:",
                          tooltip = TRUE
                          
                        )
                      ),
                      
                      
                      # histogramas
                      shiny::conditionalPanel(
                        "input.pn_check_res_hist_mod == true", 
                        shiny::br(),
                        plotly::plotlyOutput("pn_res_hist_mod")
                        
                      ),
                      
                      
                      # histogramas
                      shiny::conditionalPanel(
                        "input.pn_check_res_hist_est == true", 
                        shiny::br(),
                        plotly::plotlyOutput("pn_res_hist_est")
                        
                      ),
                      
                      #  QQPLOTs
                      shiny::conditionalPanel(
                        "input.pn_check_res_qqplot_mod == true", 
                        shiny::br(),
                        plotly::plotlyOutput("pn_res_qqplot_mod")
                      ),
                      
                      #  QQPLOTs
                      shiny::conditionalPanel(
                        "input.pn_check_res_qqplot_est == true", 
                        shiny::br(),
                        plotly::plotlyOutput("pn_res_qqplot_est")
                        
                      ),
                      
                      
                      #  Percentual
                      shiny::conditionalPanel(
                        "input.pn_check_res_perc_mod == true", 
                        shiny::br(),
                        shiny::h3("Percentuais Te\u00F3ricos Modelagem", align = "center"),
                        DT::dataTableOutput("pn_res_perc_mod")
                        
                      ),
                      
                      #  Percentual
                      shiny::conditionalPanel(
                        "input.pn_check_res_perc_est == true", 
                        shiny::br(),
                        shiny::h3("Percentuais Te\u00F3ricos Estimativa", align = "center"),
                        DT::dataTableOutput("pn_res_perc_est")
                        
                      ),
                      
                      #  Grafico dos Residuos
                      shiny::conditionalPanel(
                        "input.pn_check_res_padro_mod == true", 
                        shiny::br(),
                        shiny::h3("Res\u00EDduos Padronizados x Valores Calculados - Modelagem", align = "center"),
                        shiny::helpText("Aqui s\u00E3o relacionados os valores calculados da vari\u00e1vel dependente na escala da fun\u00E7\u00E3o de modelagem"),
                        shiny::checkboxInput("pn_check_hist_padro_mod", "Exibir Histograma", value = TRUE),
                        
                        plotly::plotlyOutput("pn_res_resP_Vcal_mod")
                        
                      ),
                      
                      #  Grafico dos Residuos
                      shiny::conditionalPanel(
                        "input.pn_check_res_padro_est == true", 
                        shiny::br(),
                        shiny::h3("Res\u00EDduos Padronizados x Valores Calculados - Estimativa", align = "center"),
                        shiny::helpText("Aqui s\u00E3o relacionadosos valores calculados da vari\u00e1vel dependente na escala da fun\u00E7\u00E3o de estimativa"),
                        shiny::checkboxInput("pn_check_hist_padro_est", "Exibir Histograma", value = TRUE),
                        
                        plotly::plotlyOutput("pn_res_resP_Vcal_est")
                        
                      ),
                      
                      #  Grafico dos Residuos contra Variaveis Independentes Modelagem
                      shiny::conditionalPanel(
                        "input.pn_check_res_padro_var_indep_mod == true", 
                        shiny::br(),
                        shiny::h3("Res\u00EDduos Padronizados x Vari\u00e1veis Independentes - Modelagem", align = "center"),
                        shiny::selectInput("pn_check_res_padro_var_indep_select_mod", "Selecione a vari\u00e1vel", choices = NULL),
                        shiny::helpText("Aqui s\u00E3o relacionados os valores observados das vari\u00e1veis independentes na escala da modelagem"),
                        shiny::checkboxInput("pn_check_hist_mod", "Exibir Histograma", value = TRUE),
                        plotly::plotlyOutput("pn_res_resP_var_indep_mod")
                        
                      ),
                      
                      #  Grafico dos Residuos contra Variaveis Independentes Estimativa
                      shiny::conditionalPanel(
                        "input.pn_check_res_padro_var_indep_est == true", 
                        shiny::br(),
                        shiny::h3("Res\u00EDduos Padronizados x Vari\u00e1veis Independentes - Estimativa", align = "center"),
                        shiny::selectInput("pn_check_res_padro_var_indep_select_est", "Selecione a vari\u00e1vel", choices = NULL),
                        shiny::helpText("Aqui s\u00E3o relacionados os valores observados das vari\u00e1veis independentes na escala da fun\u00E7\u00E3o de estimativa"),
                        shiny::checkboxInput("pn_check_hist_est", "Exibir Histograma", value = TRUE),
                        plotly::plotlyOutput("pn_res_resP_var_indep_est")
                        
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_res_map_mod == true", 
                        
                        shiny::br(),
                        shiny::h3("Mapa de Res\u00EDduos da Modelagem", align = "center"),
                        
                        selectInput(
                          "pn_map_residuals_select_mod",
                          "Selecione a grandeza a ser visualizada no mapa",
                          choices = c("", "Var. Dep. Obs. Trns.",
                                      "Var. Dep. Calc. Trns.",
                                      "Res\u00EDduos Modelagem",
                                      "Res\u00EDduos Relativos Modelagem",
                                      "Res\u00EDduos Padronizados Modelagem"
                          ),
                          selected = ""
                        ),
                        
                        leaflet::leafletOutput("pn_res_geo_mod")
                        
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_res_map_est == true", 
                        shiny::br(),
                        shiny::h3("Mapa de Res\u00EDduos da Estimativa", align = "center"),
                        
                        selectInput(
                          "pn_map_residuals_select_est",
                          "Selecione a grandeza a ser visualizada no mapa",
                          
                          choices = c("" , "Var. Dep. Obs. Estimativa", 
                                      "Var. Dep. Calc. Estimativa", 
                                      "Res\u00EDduos Estimativa", 
                                      "Res\u00EDduos Relativos Estimativa", 
                                      "Res\u00EDduos Padronizados Estimativa"),
                          
                          selected = ""
                        ),
                        
                        leaflet::leafletOutput("pn_res_geo_est")
                        
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cook_dist == true", 
                        shiny::br(),
                        shiny::h3("Dist\u00e2ncia de Cook", align = "center"),
                        shiny::numericInput("pn_cook_dist_N", "Exibir os primeiros elementos:", min = 0, value = 20),
                        plotly::plotlyOutput("dist_cook_out")
                      ),
                      
                      
                      shiny::conditionalPanel(
                        "input.pn_check_res_table == true", 
                        shiny::br(),
                        shiny::h3("Tabela de Res\u00EDduos", align = "center"),
                        #DT::dataTableOutput("var_dep_and_residuals_out")
                        DT::dataTableOutput("var_dep_and_residuals_out")
                      )
                    ),
                    
                    
                    
                    
                    shinydashboardPlus::boxPlus(
                      title = "Vari\u00e1veis: Coeficientes, Dispers\u00E3o e Signific\u00e2ncias",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::fluidRow(
                        align = "center",
                        
                        shinyWidgets::dropdownButton(
                          
                          shiny::checkboxInput(
                            "pn_check_coef_plot", 
                            "Gr\u00e1fico dos Coeficientes", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_coef_bar_plot", 
                            "Gr\u00e1fico de Barras dos Coeficientes", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_coef_table", 
                            "Tabela dos Coeficientes", 
                            value = TRUE),
                          
                          right = FALSE,
                          up = FALSE,
                          inline = TRUE,
                          status = "info",
                          circle = FALSE,
                          icon = shiny::icon("gear"),
                          width = "400px",
                          size = "xs",
                          label = "Visualizar:",
                          tooltip = TRUE
                        )
                      ),
                      
                      #grafico dos coeficientes
                      shiny::conditionalPanel(
                        "input.pn_check_coef_plot == true", 
                        shiny::br(),
                        plotly::plotlyOutput("pn_coef_plot")
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_coef_bar_plot == true", 
                        
                        shiny::selectInput(
                          "coef_bar_plot_grandeza", 
                          "Selecione", 
                          choices = c("Coeficiente", 
                                      "Erro-Padr\u00E3o", 
                                      "t-Valor", 
                                      "Signific\u00e2ncia"),
                          selected = "Signific\u00e2ncia"),
                        
                        plotly::plotlyOutput("pn_coef_bar_plot")
                      ),
                      
                      
                      shiny::conditionalPanel(
                        "input.pn_check_coef_table == true", 
                        shiny::br(),
                        DT::dataTableOutput("pn_coef_table")
                      )
                      
                    ),
                    
                    
                    
                    
                    
                    shinydashboardPlus::boxPlus(
                      title = "An\u00e1lise de Colinearidade",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::fluidRow(
                        align = "center",
                        
                        shinyWidgets::dropdownButton(
                          
                          shiny::checkboxInput(
                            "pn_check_cor", 
                            "Correla\u00E7\u00F5es Isoladas - Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_cor_est", 
                            "Correla\u00E7\u00F5es Isoladas - Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_cor_table", 
                            "Tabela de Correlacoes Isoladas - Modelagem", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_cor_table_est", 
                            "Tabela de Correlacoes Isoladas - Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_pcor", 
                            "Correla\u00E7\u00F5es Parciais - Modelagem", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_pcor_est", 
                            "Correla\u00E7\u00F5es Parciais - Estimativa", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_cor_par_table", 
                            "Tabela de Correlacoes Parciais - Modelagem", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_cor_par_table_est", 
                            "Tabela de Correlacoes Parciais - Estimativa", 
                            value = FALSE),
                          
                          
                          right = FALSE,
                          up = FALSE,
                          inline = TRUE,
                          status = "info",
                          circle = FALSE,
                          icon = shiny::icon("gear"),
                          width = "400px",
                          size = "xs",
                          label = "Visualizar:",
                          tooltip = TRUE
                        )
                      ),
                      
                      
                      # PN - Heatmap Correlacoes Isoladas Modelagem -----------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor == true", 
                        shiny::br(),
                        shiny::h3("Correla\u00E7\u00F5es Isoladas - Modelagem", align = "center"),
                        
                        shiny::selectInput(
                          "method_correlation", 
                          "M\u00E9todo de C\u00e1lculo", 
                          choices = c("Pearson" = "pearson", 
                                      "Kendall" ="kendall", 
                                      "Spearman" ="spearman"),
                          selected = "pearson"),
                        
                        
                        shinyWidgets::prettySwitch(
                          "corr_simetric", 
                          "Matriz Sim\u00E9trica", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        shinyWidgets::prettySwitch(
                          "corr_diag", 
                          "Incluir Diagonal", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        plotly::plotlyOutput("corr_heat_map")
                        
                        
                      ),
                      
                      # PN - Tabela Correlacoes Isoladas Modelagem ------------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor_table == true", 
                        
                        shiny::h3("Tabela de Correla\u00E7\u00F5es Isoladas - Modelagem", 
                                  align = "center"),
                        
                        DT::dataTableOutput("table_cor")
                        
                      ),
                      
                      # PN - Heatmap Correlacoes Isoladas Estimativa ----------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor_est == true", 
                        shiny::br(),
                        shiny::h3("Correla\u00E7\u00F5es Isoladas - Estimativa", align = "center"),
                        
                        shiny::selectInput(
                          "method_correlation_est", 
                          "M\u00E9todo de C\u00e1lculo", 
                          choices = c("Pearson" = "pearson", 
                                      "Kendall" ="kendall", 
                                      "Spearman" ="spearman"),
                          selected = "pearson"),
                        
                        
                        shinyWidgets::prettySwitch(
                          "corr_simetric_est", 
                          "Matriz Sim\u00E9trica", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        shinyWidgets::prettySwitch(
                          "corr_diag_est", 
                          "Incluir Diagonal", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        plotly::plotlyOutput("corr_heat_map_est")
                        
                        
                      ),
                      
                      
                      
                      # PN - Tabela Correlacoes Isoladas Estimativa -----------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor_table_est == true", 
                        
                        shiny::h3("Tabela de Correla\u00E7\u00F5es Isoladas - Estimativa", align = "center"),
                        
                        DT::dataTableOutput("table_cor_est")
                        
                      ),
                      
                      
                      # PN - Heatmap Correlacoes Parciais Modelagem -----------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_pcor == true", 
                        shiny::br(),
                        shiny::h3("Correla\u00E7\u00F5es Parciais - Modelagem", align = "center"),
                        
                        shiny::selectInput(
                          "method_partial_correlation", 
                          "M\u00E9todo de C\u00e1lculo", 
                          choices = c("Pearson" = "pearson", 
                                      "Kendall" ="kendall", 
                                      "Spearman" ="spearman"),
                          selected = "pearson"),
                        
                        shinyWidgets::prettySwitch("par_corr_simetric", 
                                                   "Matriz Sim\u00E9trica", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        shinyWidgets::prettySwitch("par_corr_diag", 
                                                   "Incluir Diagonal", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        plotly::plotlyOutput("corr_par_heat_map")
                        
                        
                      ),
                      
                      
                      # PN - Tabela Correlacoes Parciais Modelagem ------------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor_par_table == true", 
                        
                        shiny::h3("Tabela de Correla\u00E7\u00F5es Parciais - Modelagem", align = "center"),
                        
                        DT::dataTableOutput("table_cor_par")
                        
                      ),
                      
                      # PN - Heatmap Correlacoes Parciais Estimativa ----------------------------
                      
                      
                      shiny::conditionalPanel(
                        "input.pn_check_pcor_est == true", 
                        shiny::br(),
                        shiny::h3("Correla\u00E7\u00F5es Parciais - Estimativa", align = "center"),
                        
                        shiny::selectInput(
                          "method_partial_correlation_est", 
                          "M\u00E9todo de C\u00e1lculo", 
                          choices = c("Pearson" = "pearson", 
                                      "Kendall" ="kendall", 
                                      "Spearman" ="spearman"),
                          selected = "pearson"),
                        
                        shinyWidgets::prettySwitch("par_corr_simetric_est", 
                                                   "Matriz Sim\u00E9trica", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        shinyWidgets::prettySwitch("par_corr_diag_est", 
                                                   "Incluir Diagonal", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        plotly::plotlyOutput("corr_par_heat_map_est")
                        
                        
                      ),
                      
                      
                      # PN - Tabela Correlacoes Parciais Estimativa -----------------------------
                      
                      shiny::conditionalPanel(
                        "input.pn_check_cor_par_table_est == true", 
                        
                        shiny::h3("Tabela de Correla\u00E7\u00F5es Parciais - Estimativa", align = "center"),
                        
                        DT::dataTableOutput("table_cor_par_est")
                        
                      )
                      
                    ),
                    
                    
                    # PN - Analise da Equacao -------------------------------------------------
                    
                    
                    shinydashboardPlus::boxPlus(
                      title = "An\u00e1lise da Equa\u00E7\u00E3o",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::fluidRow(
                        align = "center",
                        
                        shinyWidgets::dropdownButton(
                          
                          shiny::checkboxInput(
                            "pn_check_eq_graph_mod", 
                            "Gr\u00e1fico Valores Transformados", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_eq_table_mod", 
                            "Tabela de Valores Transformados", 
                            value = FALSE),
                          
                          shiny::checkboxInput(
                            "pn_check_eq_graph_est", 
                            "Gr\u00e1fico Valores Reescalados", 
                            value = TRUE),
                          
                          shiny::checkboxInput(
                            "pn_check_eq_table_est", 
                            "Tabela de Valores Reescalados", 
                            value = FALSE),
                          
                          right = FALSE,
                          up = FALSE,
                          inline = TRUE,
                          status = "info",
                          circle = FALSE,
                          icon = shiny::icon("gear"),
                          width = "400px",
                          size = "xs",
                          label = "Visualizar:",
                          tooltip = TRUE
                        )
                      ),
                      
                      
                      shiny::conditionalPanel(
                        "input.pn_check_eq_graph_mod == true",
                        
                        shiny::h3("Modelagem", align = "center"),
                        shiny::selectInput(
                          "pn_eq_select_var_x_mod", 
                          "Selecione a Vari\u00e1vel", 
                          choices = NULL),
                        
                        shiny::sliderInput(
                          "pn_eq_analysis_confidence_mod", 
                          "Nivel de Confian\u00E7a",
                          value = 80, 
                          min = 1, 
                          max = 99),
                        
                        shinyWidgets::prettySwitch(
                          "pn_eq_conf_mod", 
                          "Exibir Intervalo de Confian\u00E7a", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary",
                          value = TRUE),
                        
                        shinyWidgets::prettySwitch(
                          "pn_eq_pred_mod", 
                          "Exibir Intervalo de Predi\u00E7\u00E3o", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        shinyWidgets::prettySwitch(
                          "pn_eq_obs_values_mod", 
                          "Exibir Valores Observados", 
                          inline = TRUE, 
                          slim = TRUE, 
                          status = "primary"),
                        
                        # shinyWidgets::prettySwitch("pn_eq_calc_values_mod", 
                        #                            "Exibir Valores Calculados", 
                        #                            inline = TRUE, 
                        #                            slim = TRUE, 
                        #                            status = "primary"),
                        
                        
                        shiny::div(
                          shiny::textOutput("eq_analysis_mod"), 
                          align = "center"),
                        
                        plotly::plotlyOutput("df_eq_analysis_plot_mod")
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_eq_table_mod == true",
                        
                        DT::dataTableOutput("df_eq_analysis_table_mod")
                      ),
                      
                      shiny::hr(),
                      shiny::conditionalPanel(
                        "input.pn_check_eq_graph_est == true",
                        
                        shiny::h3("Estimativa", align = "center"),
                        shiny::selectInput("pn_eq_select_var_x_est", 
                                           "Selecione a Vari\u00e1vel", 
                                           choices = NULL),
                        
                        shiny::selectInput(
                          "pn_eq_select_estimador_log_est", 
                          "Selecione os Estimador do Log", 
                          choices = c(
                            "M\u00E9dia" = "media" ,
                            "Mediana" = "mediana",
                            "Moda" = "moda" ),
                          selected = "mediana"),
                        
                        shiny::sliderInput("pn_eq_analysis_confidence_est", 
                                           "Nivel de Confian\u00E7a",
                                           value = 80, 
                                           min = 1, 
                                           max = 99),
                        
                        shinyWidgets::prettySwitch("pn_eq_conf_est", 
                                                   "Exibir Intervalo de Confian\u00E7a", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary",
                                                   value = TRUE),
                        
                        shinyWidgets::prettySwitch("pn_eq_pred_est", 
                                                   "Exibir Intervalo de Predi\u00E7\u00E3o", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        shinyWidgets::prettySwitch("pn_eq_obs_values_est", 
                                                   "Exibir Valores Observados", 
                                                   inline = TRUE, 
                                                   slim = TRUE, 
                                                   status = "primary"),
                        
                        # shinyWidgets::prettySwitch("pn_eq_calc_values_est", 
                        #                            "Exibir Valores Calculados", 
                        #                            inline = TRUE, 
                        #                            slim = TRUE, 
                        #                            status = "primary"),
                        
                        shiny::div(
                          shiny::textOutput("eq_analysis_est"), 
                          align = "center"),
                        plotly::plotlyOutput("df_eq_analysis_plot_est")
                        
                      ),
                      
                      shiny::conditionalPanel(
                        "input.pn_check_eq_table_est == true",
                        
                        DT::dataTableOutput("df_eq_analysis_table_est")
                        
                      )
                    ),
                    
                    shinydashboardPlus::boxPlus(
                      title = "Micronumerosidade",
                      width = 12,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      enable_sidebar = FALSE,
                      
                      shiny::h3("Micronumerosidade do Modelo"),
                      shiny::textOutput("micro_modelo"),
                      
                      shiny::h3("Micronumerosidade das vari\u00e1veis"),
                      DT::dataTableOutput("tabela_micro")
                      
                    )
                  ) #fim da div
                  
                  
                  
                  
                ), #fim da aba detalhamento
                
                
                # MO - Busca de Variaveis -------------------------------------------------
                
                
                shiny::tabPanel(
                  title = "Busca Autom\u00e1tica",
                  
                  # shinyWidgets::prettyRadioButtons(
                  #   inputId = "MO_trns_search",
                  #   label = "Defina o M\u00E9todo:",
                  #   choices = c(
                  #     "M\u00E9todo Geral" = "MO_trns_general",
                  #     "Box Cox" = "MO_trns_box_cox"
                  #   ),
                  #   icon = shiny::icon("check"),
                  #   inline = FALSE,
                  #   bigger = FALSE,
                  #   status = "info",
                  #   animation = "jelly"
                  # ),
                  
                  shiny::uiOutput("MO_trns_search_vars"),
                  
                  # shiny::textOutput("ncomb"),
                  
                  # shinyWidgets::prettySwitch(
                  #   inputId = "parallel_computing",
                  #   label = "Ativar Computa\u00E7\u00E3o Paralela",
                  #   status = "success",
                  #   fill = TRUE,
                  #   value = FALSE
                  # ),
                  
                  
                  
                  shiny::actionButton("MO_trns_for_search_save", "Salvar", width = "100%"),
                  shiny::div(
                    align = "center", 
                    shiny::textOutput("ncomb")
                  ),
                  shiny::div(
                    align = "center", 
                    shiny::textOutput("tempo_estimado_calc")
                  ),
                  
                  
                  shiny::br(),
                  
                  shiny::actionButton("MO_trns_for_search_go", "Salvar e Pesquisar", width = "100%"),
                  shiny::br(),
                  shiny::br(),
                  DT::dataTableOutput("df_transf_choose"),
                  shiny::br(),
                  shiny::br()
                  
                )
                
              ), #fim da box
              
              
              # MO - PAINEL DE VARIAVEIS ------------------------------------------------
              
              
              # shinydashboardPlus::boxPlus(
              #   width = 4,
              #   #height = 450,
              #   title = "Painel de Vari\u00E1veis",
              #   status = "primary",
              #   closable = FALSE,
              #   solidHeader = FALSE,
              #   collapsed = FALSE,
              #   collapsible = TRUE,
              #   enable_sidebar = TRUE,
              #   sidebar_start_open = FALSE,
              #   sidebar_width = 35,
              #   #sidebar_background = "#c4c4c4",
              #   sidebar_content = shiny::tagList(),
              
              
              shinydashboard::tabBox(
                width = 3,
                title = "Painel de Vari\u00E1veis",
                
                
                shiny::tabPanel(
                  title = "Variaveis",
                  
                  shiny::tags$div(
                    style = 'overflow-x: auto; overflow-y: auto; height: 500px',
                    
                    MO_picker_var("model_hab_calc", 
                                  "Habilitar as Vari\u00E1veis:",
                                  maxOpt = NULL,
                                  actionsBox = TRUE,
                                  deselectAllText = "Nenhuma",
                                  selectAllText = "Todas"),
                    MO_picker_var("model_var_dep", "Vari\u00E1vel Dependente:", 1),
                    
                    shiny::uiOutput("var_transf")
                  )
                ),
                
                shiny::tabPanel(
                  title = "Busca Autom\u00e1tica",
                  
                  DT::dataTableOutput("df_transf_choose2")
                  
                  # shinyWidgets::pickerInput(
                  #   "model_var_study",
                  #   "Estudar a Vari\u00E1vel:",
                  #   choices = NULL,
                  #   width = "100%",
                  #   options = list(
                  #     title = "Selecione",
                  #     `max-options` = 1),
                  #   multiple = TRUE
                  # ),
                  
                  # plotly::plotlyOutput("model_var_study_plot", height = 330), #height = 330
                  
                  # shinyWidgets::pickerInput(
                  #   "model_var_study_trns",
                  #   "Aplicar a Transformada:",
                  #   choices = NULL,
                  #   width = "100%",
                  #   options = list(
                  #     title = "Selecione",
                  #     `max-options` = 1),
                  #   multiple = TRUE
                  # )
                )
              )
              
              #) #fim do div
              #) #fim da box
            ), #fim da fluid row
            
            # MO - PAINEL DE DADOS ----------------------------------------------------
            
            data_panel_UI("MO_data_panel", "An\u00E1lise Explorat\u00F3ria")
            
          ), #FIM DO PAINEL DE MODELAGEM
          
          
          shinydashboard::tabItem(
            
            tabName = "export_data",
            
            shinydashboard::box(
              title = "Exportar como Arquivo R",
              width = 12,
              status = "primary",
              collapsible = FALSE,
              collapsed = FALSE,
              
              shiny::downloadButton(
                outputId = "salvar_em_rds",
                label = "Salvar em .rds",
                style = "width:100%;")
              
            )
            # ,
            # 
            # shinydashboard::box(
            #   title = "Exportar Relat\u00F3rio HTML",
            #   width = 12,
            #   status = "primary",
            #   collapsible = FALSE,
            #   collapsed = FALSE,
            #   
            #   shiny::downloadButton(
            #     outputId = "salvar_em_html",
            #     label = "Salvar em .html",
            #     style = "width:100%;")
            #   
            # )
            
            # ,shinydashboard::box(
            #   title = "Exportar como Arquivo Excel",
            #   width = 12,
            #   status = "primary",
            #   collapsible = FALSE,
            #   collapsed = FALSE,
            #   
            #   shinyjs::disabled(shiny::downloadButton(
            #     outputId = "salvar_em_xls",
            #     label = "Salvar em .xls",
            #     style = "width:100%;")),
            #   
            #   shinyjs::disabled(shiny::downloadButton(
            #     outputId = "salvar_em_xlsx",
            #     label = "Salvar em .xlsx",
            #     style = "width:100%;"))
            # )
            
          ), # fim do painel export data
          
          
          
          # ET - Estimativas --------------------------------------------------------
          
          
          shinydashboard::tabItem(
            
            tabName = "estimative_panel",
            
            shiny::fluidRow(
              
              
              # ET - EQ Pesquisadas -----------------------------------------------------
              
              
              shinydashboard::box(
                title = "Equa\u00E7\u00F5es Testadas",
                width = 12,
                status = "success",
                collapsible = TRUE,
                collapsed = TRUE,
                
                shiny::helpText("Nesse painel s\u00E3o listadas as equa\u00E7\u00F5es simuladas no Painel de Modelagem. Essa equa\u00E7\u00F5es s\u00E3o geradas com base nas trnasformadas selecionadas e nos dados habilitados no momento da pesquisa. "), 
                shiny::helpText("Quando uma linha dessa tabela \u00E9 clicada, automaticamente as trasnformadas associadas s\u00E3o definidas no painel de vari\u00e1veis. \u00c9 poss\u00EDvel testar cada equa\u00E7\u00E3o com os valores definidos abaixo para o computo de uma estimativa de valor."),
                
                DT::dataTableOutput("df_transf_choose3")
              )
            ),
            
            # ET - Painel de Estimativas ----------------------------------------------
            
            
            shiny::fluidRow(
              shinydashboard::box(
                title = "Estimativas",
                width = 12,
                status = "danger",
                collapsible = TRUE,
                collapsed = FALSE,
                
                shiny::column(
                  width = 2,
                  
                  shiny::h3("Insira"),
                  shiny::div(
                    style = "overflow-y: auto; height: 250px",
                    shiny::uiOutput("estimative_variables"),
                    shiny::actionButton("consultar_geo_button", "Consultar GEO", width = "100%")
                  ),
                  
                  shiny::hr(),
                  
                  shiny::sliderInput(
                    inputId = "intervalo_confianca",
                    label = "Confian\u00E7a",
                    min = 1,
                    max = 99,
                    value = 80,
                    step = 1,
                    post = "%"),
                  
                  shiny::selectInput(
                    "estimador_log_nep",
                    label = "Estimador Logar\u00EDtimico:",
                    choices = c(
                      "M\u00E9dia" = "media" ,
                      "Mediana" = "mediana",
                      "Moda" = "moda" ),
                    selected = "mediana" ),
                  
                  shinyWidgets::prettySwitch(
                    inputId = "incluir_ip",
                    label = "Intervalo de Predi\u00E7\u00E3o",
                    status = "info",
                    fill = TRUE
                  ),
                  
                  shiny::actionButton("estimar", "Calcular!", width = "100%")
                  
                  
                  
                ),
                
                shinyBS::bsModal(
                  id = "consultar_geo_panel", 
                  title = "Localize o Im\u00F3vel Avaliando",
                  trigger = "consultar_geo_button",
                  size = "large",
                  
                  shiny::column(
                    width = 4, 
                    #shiny::h4("Latitude"),
                    numericInput("consultar_geo_var_lat", "Latitude", value = 0)
                  ),
                  
                  shiny::column(
                    width = 4, 
                    #shiny::h4("Longitude"),
                    numericInput("consultar_geo_var_lng", "Longitude", value = 0)
                  ),
                  
                  shiny::column(
                    width = 4, 
                    #shiny::h4("EPSG"),
                    numericInput("consultar_geo_var_epsg", "EPSG", value = 4326)
                  ),
                  
                  shiny::br(),
                  
                  shiny::actionButton("consultar_geo_var_lng_lat", "Buscar no Mapa Abaixo", width = "100%"),
                  
                  shiny::br(),
                  
                  shiny::div(
                    leaflet::leafletOutput("consultar_geo_var_mapa"), 
                    
                    align = "center"),
                  
                  shiny::br(),
                  
                  shiny::actionButton("confirm_geo_var", "Confirmar a Localiza\u00E7\u00E3o do Mapa", width = "100%")
                  
                ),
                
                shinydashboard::tabBox(
                  width = 10,
                  
                  shiny::tabPanel(
                    "Estimativas",
                    
                    # shiny::column(
                    #   width = 6,
                    #   
                    #   plotly::plotlyOutput("grafico_previsao_model", height = "200px"),
                    #   DT::dataTableOutput("tb_previsao_model")
                    #   
                    # ),
                    # 
                    # shiny::column(
                    #   width = 6,
                    #   
                    #   plotly::plotlyOutput("grafico_previsao_real", height = "200px"),
                    #   DT::dataTableOutput("tb_previsao_real")
                    #   
                    # ),
                    
                    # ET - Estimativas Modelo -------------------------------------------------
                    
                    
                    shiny::fluidRow(
                      plotly::plotlyOutput("grafico_previsao_model", height = "200px"),
                      # DT::dataTableOutput("tb_previsao_model")
                      shiny::div(
                        align = "center",
                        style = 'overflow-x: auto;',
                        tableOutput("tb_previsao_model")
                      )
                      
                    ),
                    
                    # ET - Estimativas Natural ------------------------------------------------
                    
                    
                    shiny::fluidRow(
                      plotly::plotlyOutput("grafico_previsao_real", height = "200px"),
                      
                      shiny::div(
                        align = "center",
                        style = 'overflow-x: auto;',
                        tableOutput("tb_previsao_estimativa")
                      )
                      
                    ),
                    
                    # ET - IVA ----------------------------------------------------------------
                    
                    
                    shiny::fluidRow(
                      shiny::div(
                        align = "center",
                        style = 'overflow-x: auto;',
                        
                        shiny::column(
                          width = 6,
                          uiOutput("iva_central_esti"),
                        ), # fim do column
                        
                        shiny::column(
                          width = 6,
                          tableOutput("iva_table")
                        ) #fim do column
                      ) # fim do div
                    ) #fim do fluidRow
                    
                    
                  ), # fim do tabPanel
                  
                  shiny::tabPanel(
                    "Gr\u00e1fico de Previs\u00E3o",
                    
                    shiny::selectInput(
                      "et_graph_prev_var_x", 
                      "Vari\u00e1vel no eixo X", 
                      choices = NULL),
                    
                    shinyWidgets::prettySwitch(
                      "et_eq_conf", 
                      "Exibir Intervalo de Confian\u00E7a", 
                      inline = TRUE, 
                      slim = TRUE, 
                      status = "primary", 
                      value = TRUE),
                    
                    shinyWidgets::prettySwitch(
                      "et_eq_pred", 
                      "Exibir Intervalo de Predi\u00E7\u00E3o", 
                      inline = TRUE, 
                      slim = TRUE, 
                      status = "primary"),
                    
                    shinyWidgets::prettySwitch(
                      "et_eq_obs_values", 
                      "Exibir Valores Observados", 
                      inline = TRUE, 
                      slim = TRUE, 
                      status = "primary"),
                    
                    
                    shiny::selectInput(
                      "et_graph_prev_relation",
                      "Relacionar Vari\u00e1vel Dependente a seguinte vari\u00e1vel:", 
                      choices = NULL),
                    
                    shiny::radioButtons(
                      "et_graph_prev_operation", 
                      "Relacionar:", 
                      choices = c("Multiplicando" = "mult", 
                                  "Dividindo" = "div")
                    ),
                    
                    plotly::plotlyOutput("et_graph_prev_plot")
                    
                  ),
                  
                  shiny::tabPanel(
                    "Obter Geo Variaveis",
                    
                    shiny::helpText("Click no mapa para obter a dist\u00e2ncia do ponto clicado aos polos Geo Influenciantes"),
                    leaflet::leafletOutput("geo_var_avaliando")
                    
                  )
                  
                )# fim do tabBox
              )
              
            ), # fim do Setor de Estimativas
            
            shiny::fluidRow(
              shinydashboard::box(
                title = "Avaliar em Multiplos Modelos - Selec\u00E3o das Equa\u00E7\u00F5es",
                width = 12,
                status = "info",
                collapsible = TRUE,
                collapsed = TRUE,
                
                shiny::helpText("Nessa tabela, as equa\u00E7\u00F5es pesquisadas na Busca Autom\u00e1tica do Painel de Modelagem. \u00c9 poss\u00EDvel selecionar mais de uma linha e ao clicar em 'Avaliar nas Equa\u00E7\u00F5es Selecionadas', as informa\u00E7\u00F5es do im\u00F3vel avaliando s\u00E3o inseridas em cada modelo retornando a avalaic\u00E3o do im\u00F3vel em diferentes equa\u00E7\u00F5es simultanetamente."), 
                shiny::helpText("Os resultados s\u00E3o exibidos no painel abaixo"),
                
                
                DT::dataTableOutput("data_choose_model_multiple"),
                
                shiny::actionButton("mult_model_valuation", "Avaliar nas Equa\u00E7\u00F5es Selecionadas", width = "100%")
                
              )
            ),
            
            
            shiny::fluidRow(
              shinydashboard::box(
                title = "Avaliar em Multiplos Modelos - Resultados",
                width = 12,
                status = "info",
                collapsible = FALSE,
                collapsed = FALSE,
                
                DT::dataTableOutput("mult_model_relations")
                
                
              )
            )
            
            
          ), #fim do estimative panel
          
          # ET - Varios Imoveis ----------------------------------------------------
          
          
          shinydashboard::tabItem(
            
            tabName = "estimative_panel_mult",
            shiny::fluidRow(
              shinydashboard::box(
                title = "Avaliar Varios Im\u00F3veis no Modelo Selecionado",
                width = 12,
                status = "primary",
                collapsible = FALSE,
                collapsed = FALSE,
                
                shiny::helpText("Essa etapa permite que sejam avaliados mais de 1 um im\u00F3vel simultaneamente no modelo selecionado no Est\u00FAdio de Modelagem"),
                
                
                shiny::helpText("Faz-se necess\u00e1rio que todas as informa\u00E7\u00F5es de cada dado em cada vari\u00e1vel estejam prenchidas. Os dados que possu\u00EDrem alguma informa\u00E7\u00E3o n\u00E3o preenchida ser\u00E3o automaticamente filtrados"),
                # duas maneiras de inserir os dados: inserir planilha de excel
                ## vantagem: nao \u00E9 necessario definir q quantidade de dados inicialmente
                ## desvantagem: pode entrar mto lixo, variaveis com nome errado, etc
                
                # criar rhandsontable e nele colar os valores
                ## vantagem:  
                ### controle dos nomes das vari\u00e1veis
                ### padronizacao de valores
                ## desvantagem:
                ### tem q informar q quantidade de dados com antecedencia
                shinydashboard::box(
                  title = "Entrada de Dados",
                  width = 12,
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  shiny::numericInput(
                    "n_avaliando", 
                    "Informe uma previs\u00E3o de Im\u00F3veis Avaliandos", 
                    value = 2, 
                    width = "100%",
                    min = 2),
                  
                  shinyWidgets::prettySwitch(
                    inputId = "auto_capture_spatial_var", 
                    "Se houver vari\u00e1veis espaciais, caputur\u00e1-las automaticamente", 
                    status = "primary",
                    value = TRUE),
                  
                  shiny::actionButton(
                    "create_plan_avaliando", 
                    "Gerar Planilha de Avaliandos", 
                    width = "100%"),
                  
                  shiny::br(),
                  
                  rhandsontable::rHandsontableOutput("plan_mult_ava")
                  
                ),
                
                shinydashboard::box(
                  title = "Sa\u00EDda de Dados",
                  width = 12,
                  status = "info",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  sliderInput(
                    "int_confianca_multi_ava",
                    "N\u00EDvel de Confian\u00E7a", 
                    value = 80, 
                    min = 1, 
                    max = 99),
                  
                  
                  shiny::actionButton(
                    "eval_plan_avaliando", 
                    "Avaliar Planilha", 
                    width = "100%"),
                  
                  DT::dataTableOutput("plan_mult_ava_results_DT"),
                  
                  shiny::br())
              )
            )
          ),
          
          
          shinydashboard::tabItem(
            tabName = "lae_report"
            
            #div(style="display:inline-block",textInput(inputId="xlimitsmin", label="x-min", value = 0.0)),
            #div(style="display:inline-block",textInput(inputId="xlimitsmax", label="x-max", value = 0.5)),
            
            
            # shinydashboard::box(
            #   title = "Identifica\u00E7\u00E3o do Empreendimento", 
            #   footer = NULL, 
            #   status = NULL,
            #   solidHeader = FALSE, 
            #   background = NULL, 
            #   width = 12, 
            #   height = NULL,
            #   collapsible = TRUE, 
            #   collapsed = FALSE,
            #   
            #   shiny::fluidRow(
            #     shiny::column(
            #       width = 6,
            #       
            #       textInput(inputId="xlimitsmin", label="x-min", value = 0.0)
            #     ), 
            #     
            #     shiny::column(
            #       width = 6,
            #       
            #       textInput(inputId="xlimitsmax", label="x-max", value = 0.5)
            #     )
            #     
            #     
            #   )
            
            
            
            
            
            
            #)
            
            
            
            
            
            
            
            
          ),
          
          shinydashboard::tabItem(
            tabName = "unknown1"
            
          ),
          
          shinydashboard::tabItem(
            tabName = "unknown2"
            
          ),
          
          shinydashboard::tabItem(
            tabName = "unknown3"
            
          ),
          shinydashboard::tabItem(tabName = "subMenu4")
        )
      ),
      
      
      
      # Menu lateral direita ----------------------------------------------------
      
      
      rightsidebar = shinydashboardPlus::rightSidebar(
        background = "dark",
        width = 300,
        
        
        # Config - Mapas ----------------------------------------------------------
        
        
        shinydashboardPlus::rightSidebarTabContent(
          id = 1,
          title = "Configura\u00E7\u00F5es dos Mapas",
          icon = "globe-americas", #shiny::icon("globe-americas"),
          active = TRUE,
          
          shiny::sliderInput(
            "config_mapa_point_opacity_border", 
            label = "Ponto: Opacidade Borda " ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.6),
          
          shiny::sliderInput(
            "config_mapa_point_opacity_inside", 
            label = "Ponto: Opacidade Interna " ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.6),
          
          shiny::sliderInput(
            "config_mapa_point_radius", 
            label = "Ponto: Tamanho" ,
            min = 5, 
            max = 500, 
            step = 5,
            value = 20),
          
          shiny::sliderInput(
            "config_mapa_point_jitter", 
            label = "Ponto: Jittering" ,
            step = 0.0001,
            min = 0, 
            max = 0.01, 
            value = 0),
          
          shiny::sliderInput(
            "config_mapa_point_color_bins",
            "Intervalos de Colora\u00E7\u00E3o Propostos",
            value = 5,
            min = 2, 
            max = 15),
          
          
          shinyWidgets::materialSwitch(
            inputId = "auto_atualizar",
            label = "Calcular Modelo",
            right = TRUE,
            status = "primary",
            value = TRUE
          )
          
        ), #fim do rightSidebarTabContent
        
        # Config - Histogram ------------------------------------------------------
        
        
        shinydashboardPlus::rightSidebarTabContent(
          id = 2,
          title = "Gr\u00E1ficos 1D",
          icon = "chart-bar", #shiny::icon("chart-bar"),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_1d_show_disabled",
            label = "Exibir Desabilitados", 
            #right = TRUE,
            value = FALSE,
            status = "primary",
            slim = TRUE
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_1d_cumalative",
            label = "Acumular", 
            slim = TRUE,
            value = FALSE,
            status = "primary"
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_1d_show_legend",
            label = "Exibir Legenda", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          shiny::sliderInput(
            "plot_1d_alpha", 
            label = "Transpar\u00EAncia:" ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.8),
          
          
          shinyWidgets::sliderTextInput(
            inputId = "plot_1d_barmode",
            label = "Tipo de barras:", 
            grid = TRUE,
            force_edges = TRUE,
            selected = "Sobrepor",
            choices = c(
              "Empilhar"  , 
              "Agrupar"  , 
              "Sobrepor"  , 
              "Relativo" 
            )
          ),
          
          shinyWidgets::sliderTextInput(
            inputId = "plot_1d_histnorm",
            label = "Eixo Vertical:", 
            grid = TRUE,
            force_edges = TRUE,
            selected = "Dens. Prob.", 
            choices = c(
              "Freq. Absoluta" , 
              "Freq. Relativa", 
              "Freq. Relativa (%)", 
              "Dens. Absoluta",
              "Dens. Prob."
            )
          ),
          
          # shinyWidgets::sliderTextInput(
          #   inputId = "plot_1d_histfunc",
          #   label = "Eixo Vertical:", 
          #   grid = TRUE,
          #   force_edges = TRUE,
          #   selected = "count", 
          #   choices = c(
          #     "Contagem" = "count" , 
          #     "Soma" = "sum" , 
          #     "M\u00E9dia" = "avg" , 
          #     "M\u00EDnimo" = "min",
          #     "M\u00E1ximo" = "max"
          #   )
          # ),
          
          shiny::numericInput(
            inputId = "plot_1d_nbinsx",
            label = "N\u00FAmero M\u00E1ximo de Bins:",
            value = 0
          )
          
        ), #fim do rightSidebarTabContent
        
        # Config - Plot2D ---------------------------------------------------------
        
        
        shinydashboardPlus::rightSidebarTabContent(
          id = 3,
          icon = "chart-bar", #shiny::icon("chart-bar"),
          title = "Gr\u00E1ficos 2D",
          
          shinyWidgets::prettySwitch(
            inputId = "plot_2d_show_disabled",
            label = "Exibir Desabilitados", 
            #right = TRUE,
            value = FALSE,
            status = "primary",
            slim = TRUE
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_2d_show_legend",
            label = "Exibir Legenda", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_2d_lm_all",
            label = "Reta Regress\u00E3o Todos", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_2d_lm_by_group",
            label = "Reta Regress\u00E3o por grupo", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          shiny::sliderInput(
            "plot_2d_marker_size", 
            label = "Tamanho do ponto" ,
            step = 1,
            min = 1, 
            max = 20, 
            value = 10),
          
          shiny::sliderInput(
            "plot_2d_jitter", 
            label = "Jittering" ,
            step = 10,
            min = 0, 
            max = 100, 
            value = 0),
          
          
          shiny::sliderInput(
            "plot_2d_alpha", 
            label = "Transpar\u00EAncia:" ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.8),
          
          shiny::sliderInput(
            "plot_2d_alpha_line", 
            label = "Transpar\u00EAncia da Reta:" ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.8)
          
        ), #fim do rightSidebarTabContent
        
        
        # Config - Plot 3D --------------------------------------------------------
        
        
        shinydashboardPlus::rightSidebarTabContent(
          id = 4,
          icon = "cubes", #shiny::icon("cubes"),
          title = "Graficos 3D",
          
          
          shinyWidgets::prettySwitch(
            inputId = "plot_3d_show_disabled",
            label = "Exibir Desabilitados", 
            #right = TRUE,
            value = FALSE,
            status = "primary",
            slim = TRUE
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_3d_show_legend",
            label = "Exibir Legenda", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          shinyWidgets::prettySwitch(
            inputId = "plot_2d_plan_hab",
            label = "Plano de Regress\u00E3o dos Habilitados", 
            slim = TRUE,
            value = TRUE,
            status = "primary"
          ),
          
          
          shiny::sliderInput(
            "plot_3d_marker_size", 
            label = "Tamanho do ponto" ,
            step = 1,
            min = 1, 
            max = 20, 
            value = 10),
          
          shiny::sliderInput(
            "plot_3d_jitter", 
            label = "Jittering" ,
            step = 1,
            min = 0, 
            max = 10, 
            value = 0),
          
          
          shiny::sliderInput(
            "plot_3d_marker_alpha", 
            label = "Transpar\u00EAncia:" ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.8),
          
          shiny::sliderInput(
            "plot_3d_plan_alpha", 
            label = "Transpar\u00EAncia do Plano:" ,
            step = 0.1,
            min = 0, 
            max = 1, 
            value = 0.8)
          
          
          
          
        ), #fim do rightSidebarTabContent
        
        shinydashboardPlus::rightSidebarTabContent(
          id = 5,
          icon = "table",
          title = "Tabelas",
          
          
          sliderInput("config_decimal_digits", "Casas Decimais", 2, min = 0, max = 30)
        ) #fim do rightSidebarTabContent
        
      ),
      title = " - Reactive Regression Modelling"
    )
    
    
    
    
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @import shinyBS
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'geobox'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

