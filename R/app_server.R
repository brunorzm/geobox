#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr `%>%`
#' @importFrom rlang `:=`
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  . <- media <- pred_inf <- conf_inf <- conf_sup <- pred_sup <- se_fit <- NULL
  mediana <- Latitude <- Longitude <- EPSG <- NULL
  
  
  
  shiny::observeEvent(input$brow, {
    
    browser()
    
  })
  
  
  
  # ID - INSERCAO DADOS -----------------------------------------------------
  
  shiny::observe({
    
    shinyjs::disable("start")
    
    req(input$file_path)
    
    shinyjs::enable("start")
    
  })
  
  
  
  data <- shiny::reactiveValues()
  prop <- shiny::reactiveValues()
  
  shiny::observeEvent(input$start, ignoreInit = TRUE, {
    shiny::validate(need(input$file_path, "Nenhum caminho de arquivo inserido"))
    
    id <- shiny::showNotification(
      ui = "Preparando os dados, aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    base::on.exit(removeNotification(id), add = TRUE)
    
    
    
    is_simil <- stringr::str_detect(input$file_path$name, "Dados_Simil_GIHAB-")
    
    csv_skip <- input$csv_config_skip_lines
    if (is_simil) {csv_skip <- 2 }
    
    
    
    
    file_path <- input$file_path$datapath
    
    data$main <- file_path %>% 
      
      read_file(session, 
                input$csv_config_delim, 
                input$csv_config_encoding, 
                input$csv_config_decimal, 
                csv_skip,
                input$excel_config_sheet, 
                input$excel_config_skip_lines, 
                input$excel_config_decimal) %>% 
      
      check_encoding(session) %>% 
      
      check_prepare_simil(session, 
                          is_simil, 
                          input$simil_variables,
                          input$simil_peca, 
                          input$simil_tipologia,
                          input$simil_intervalo_data[1],
                          input$simil_intervalo_data[2],
                          input$simil_excluir_sem_data_final
      ) %>% 
      
      check_data(session) %>% 
      
      create_key_column() %>% 
      
      set_geo(session, 
              input$geo_config_epsg_default,
              input$geo_config_filter_lat, 
              input$geo_config_filter_lng,
              input$geo_config_filter_epsg) 
    
    
    data$main %>% start_properties(prop)
    
    
    
    # ID - Acoes de Saida -----------------------------------------------------
    
    
    shiny::updateDateInput(
      session, "data_criacao",
      value = prop$model_date_declared)
    
    
    
    shiny::updateTextAreaInput(
      session, 
      "descricao_modelo", 
      value = prop$model_description)
    
    
    
    
    shinyjs::disable("start")
    
    msg <- shiny::HTML(paste("Dados Inseridos com Sucesso"))
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Base de Dados",
      text = msg,
      type = "success",
      html = TRUE
    )
    
  })  
  
  
  
  
  shiny::observeEvent(input$save_model_information, {
    
    shiny::req(data$main)
    
    prop$model_description   <- input$descricao_modelo
    prop$model_date_declared <- input$data_criacao
    
    shiny::showNotification(
      ui = "Informa\u00e7\u00f5es Salvas!",
      type = "message",
      duration = 2,
      closeButton = TRUE)
    
  })
  
  
  
  
  df_non_spatial <- shiny::reactive({ data$main %>% remove_geo() })
  non_struct_names <- shiny::reactive({ get_non_structural_names(data$main) })
  
  
  # ED - ENGENHARIA DE DADOS ------------------------------------------------
  
  
  
  # ED - Estatistica Descritiva ---------------------------------------------
  
  
  # Estatistica Descritiva
  output$DE_descriptive_table <- DT::renderDataTable({
    
    input$start
    input$config_decimal_digits
    prop$obs_disabled
    
    
    shiny::isolate({
      
      df <- data$main
      req(df)
      
      id <- shiny::showNotification(
        ui = "Calculando Estat\u00EDsticas Descritivas",
        type = "message",
        duration = NULL,
        closeButton = TRUE)
      base::on.exit(removeNotification(id), add = TRUE)
     
      
      req(any(!prop$obs_disabled))
      
      df_skim <- df[!prop$obs_disabled, , drop = FALSE] %>% skim_to_table()
      
      num <- vapply(df_skim, is.numeric, logical(1))
      num <- num[num] %>% names()
      num <- setdiff(num, "Taxa de Completos")
      
      
      data$old_skim <- df_skim
      
      tabela <- df_skim %>% 
        
        DT::datatable(
          
          options = list(
            columnDefs = list(
              list(visible = FALSE,  targets = 4:29),
              list(className = 'dt-center', targets = "_all")
            ), # fim columnDefs
            
            # #dom = "liftp",
            scrollX = TRUE,
            scrollY = TRUE,
            paging = TRUE,
            lengthMenu = list(c(5, 10, 15, -1),
                              c("5", "10", "15", "Todos")),
            pageLength = 5,
            autoWidth = FALSE,
            fixedColumns = list(leftColumns = 1, rightColumns = 0)
            
          ), # fim options
          
          class = "display",
          callback = DT::JS("return table;"),
          rownames = FALSE,
          #colnames,
          #container,
          caption = NULL,
          filter = "none",
          escape = TRUE,
          style = "default",
          width = NULL,
          height = NULL,
          elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = "multiple",
          extensions = c('FixedColumns'),
          plugins = NULL,
          editable = FALSE
        ) 
      
      tabela %>% 
        
        DT::formatPercentage(
          columns = c("Taxa de Completos"),
          digits = 1,
          dec.mark = ","
        ) %>% 
        
        DT::formatRound(
          num, 
          digits = input$config_decimal_digits , 
          dec.mark = ", ", 
          mark = ".")
      
    }) #fim do isolate
    
  }, server = TRUE) 
  
  
  
  proxy_DE_descriptive_table <- DT::dataTableProxy("DE_descriptive_table")
  
  # Visibilidade das colunas
  shiny::observeEvent(
    input$DE_descriptive_table_show,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      
      selected <- input$DE_descriptive_table_show
      
      menu <- list(
        "Vari\u00E1vel" = 1,
        "Tipo" = 2,
        "Valores Faltantes" = 3,
        "Taxa de Completos" = 4,
        "Vari\u00E1veis Texto" = 5:9,
        "Vari\u00E1veis Data" = 10:13,
        "Vari\u00E1veis Data/Hora" = 27:30,
        "Vari\u00E1veis Fator" = 14:16,
        "Vari\u00E1veis L\u00F3gicas" = 17:18,
        "Vari\u00E1veis Num\u00E9ricas" = 19:26
      )
      
      
      selected <- menu[selected] %>% unlist() %>% unname()
      
      selected <- selected - 1
      
      proxy_DE_descriptive_table %>%
        DT::hideCols(
          setdiff(0:30, selected),
          reset = FALSE
        ) %>% DT::showCols(
          selected,
          reset = FALSE
        )
    })
  
  
  shiny::observeEvent(
    data$reload,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      req(isolate(data$old_skim))
      req(any(!prop$obs_disabled))
      
      if (data$action == "add" ) {
        
        df <- data$main[!prop$obs_disabled, data$modified_vars, drop = FALSE]
        
        df_skim <- df %>% skim_to_table()
        
        df_skim <- dplyr::bind_rows(data$old_skim, df_skim)
        data$old_skim <- df_skim
        
      } else if (data$action == "update") {
        
        df <- data$main[!prop$obs_disabled, data$modified_vars, drop = FALSE]
        df_skim <- df %>% skim_to_table()
        
        
        i <- match(data$modified_vars, data$old_skim[["Vari\u00E1vel"]])
        data$old_skim[i, ] <- df_skim
        df_skim  <- data$old_skim
        
        
      } else if (data$action == "remove") {
        
        i <- match(data$modified_vars, data$old_skim[["Vari\u00E1vel"]])
        df_skim <- data$old_skim[-i, ]
        data$old_skim <- df_skim
        
      }
      
      proxy_DE_descriptive_table %>%
        
        DT::replaceData(
          df_skim,
          resetPaging = FALSE,
          #clearSelection = c("none"),
          rownames = FALSE
        )
      
      
    })
  
  
  # ED - Informacoes Adicionais ---------------------------------------------
  
  # atualiza a lista de varaiveis do data$main
  shiny::observeEvent(data$main, {
    
    nms <- data$main %>% get_non_structural_names()
    
    # shinyWidgets::updatePickerInput(
    #   session, 
    #   "DE_addtional_info_select_var", 
    #   choices = nms,
    #   selected = character(0),
    #   choicesOpt = list(
    #     content = nms %>% format_choices()
    #   )
    # )
    
    selected <- shiny::isolate(input$DE_addtional_info_select_var)
    
    if (is.null(selected)) { selected <- character(0) }
    
    shiny::updateSelectizeInput(
      session, 
      "DE_addtional_info_select_var", 
      choices = nms,
      selected = selected
    )
  })
  
  
  # habilita e desabilita os botoes se houver var selecionadas ou nao
  shiny::observe({
    
    shinyjs::disable("DE_var_description")
    shinyjs::disable("DE_var_type")
    shinyjs::disable("DE_var_behavior")
    shinyjs::disable("DE_save_var_additional_info")
    
    shiny::req(input$DE_addtional_info_select_var)
    
    shinyjs::enable("DE_var_description")
    shinyjs::enable("DE_var_type")
    shinyjs::enable("DE_var_behavior")
    shinyjs::enable("DE_save_var_additional_info")
    
  })
  
  
  # le as informacoes nas propriedades quando uma variavel \u00E9 selecionada ou nao
  shiny::observeEvent(
    input$DE_addtional_info_select_var, 
    ignoreNULL = FALSE, ignoreInit = TRUE, {
      
      var <- input$DE_addtional_info_select_var 
      
      
      if (is.null(var) || var == "") {
        
        shiny::updateTextAreaInput(
          session,
          "DE_var_description", 
          value = "")
        
        shinyWidgets::updatePrettyRadioButtons(
          session,
          "DE_var_type",
          selected = "")
        
        shinyWidgets::updatePrettyRadioButtons(
          session,
          "DE_var_behavior",
          selected = "")
        
        return()
      }
      
      
      type <- prop$var_nbr_type[[var]]
      description <- prop$var_description[[var]]
      behavior <- prop$var_expect_behavior[[var]]
      
      
      shiny::updateTextAreaInput(
        session, 
        "DE_var_description", 
        value = description)
      
      shinyWidgets::updatePrettyRadioButtons(
        session, 
        "DE_var_type",
        selected = type)
      
      shinyWidgets::updatePrettyRadioButtons(
        session, 
        "DE_var_behavior",
        selected = behavior
      )
      
    })
  
  
  # salva as informacoes digitadas nas propriedades
  shiny::observeEvent(
    input$DE_save_var_additional_info, 
    ignoreNULL = TRUE, ignoreInit = TRUE, {
      
      req(input$DE_addtional_info_select_var)
      
      var <- input$DE_addtional_info_select_var         
      
      
      prop$var_nbr_type[[var]] <- input$DE_var_type
      prop$var_description[[var]] <- input$DE_var_description
      prop$var_expect_behavior[[var]] <- input$DE_var_behavior
      
      
      # shinyWidgets::updatePickerInput(
      #   session, 
      #   "DE_addtional_info_select_var", 
      #   selected = character(0)
      #   )
      
      shiny::showNotification(
        ui = "As informa\u00E7\u00F5es foram salvas!",
        type = "message",
        duration = 1.5,
        closeButton = TRUE)
      
    })
  
  
  
  # checagem de NA na variavel selecionada
  output$check_naaa <- shiny::renderText({
    req(data$main)
    req(input$DE_addtional_info_select_var)
    
    
    var <- input$DE_addtional_info_select_var
    
    test <- check_var_na(data$main[[var]]) 
    
    test
  })
  
  
  # checagem de micronumerosidade na variavel selecionada
  output$check_micro <- shiny::renderText({
    req(data$main)
    req(input$DE_addtional_info_select_var)
    #input$DE_var_type
    #input$DE_save_var_additional_info
    
    var <- input$DE_addtional_info_select_var
    req(var %in% names(data$main))
    
    test <- check_micronumerosidade(data$main[[var]], 
                                    prop$var_nbr_type[[var]], 
                                    prop$obs_disabled) 
    
    test
    
  })
  
  
  
  
  
  
  # ED - Tratamento e Manipulacao -------------------------------------------
  
  # lista de opcoes de variaveis existentes no data$main
  shiny::observeEvent(
    data$main, 
    ignoreNULL = FALSE, ignoreInit = TRUE, {
      
      nms <- data$main %>% get_non_structural_names()
      
      # shinyWidgets::updatePickerInput(
      #   session = session,
      #   inputId = "DE_vars_manipulate",
      #   choices = nms, 
      #   choicesOpt = list(
      #     content = nms %>% format_choices(50)
      #   )
      # )
      
      shiny::updateSelectInput(
        session = session,
        inputId = "DE_vars_manipulate",
        choices = nms
      )
      
      
    })
  
  # massa de dados para preview
  mass_preview <- shiny::reactive({
    shiny::req(data$main)
    shiny::req(input$perc_preview)
    
    prev_intensity(input$perc_preview, data$main) %>% seq_len()
    
  })
  
  # habilita ou desabilita a massa de dados de preview, dependendo da escolha de
  # preview
  shiny::observe({
    
    if (input$preview_type == "Estrutura") {
      
      shinyjs::disable("perc_preview")
      
    } else {
      
      shinyjs::enable("perc_preview")
      
    }
  })
  
  
  # dados para preivew antes
  preview_data_before <- shiny::reactive({
    shiny::req(data$main)
    shiny::req(input$DE_vars_manipulate)
    
    shiny::req(input$DE_vars_manipulate %in% names(data$main))
    
    var <- c("Elemento", input$DE_vars_manipulate)
    
    data$main[, var, drop = FALSE] %>% remove_geo()
    
  })
  
  # Tabela de preview antes
  output$preview_before <- DT::renderDataTable({
    shiny::req(data$main)
    shiny::req(input$DE_vars_manipulate)
    
    
    if (input$preview_type == "Valores") {
      
      out <- preview_data_before()[mass_preview(), , drop = FALSE]
      
    } else if (input$preview_type == "Estrutura") {
      
      out <- preview_data_before()[0, , drop = FALSE] %>%
        dplyr::select(-Elemento) %>% 
        strucure_preview()
    }
    
    
    nms <- out %>% dplyr::select_if(is.numeric) %>% names()
    
    tb <- data_table_preview(out) 
    
    if (shiny::isTruthy(tb)) {
      
      tb <- tb %>% 
        
        DT::formatRound(nms, 
                        dec.mark = ",", 
                        mark = ".", 
                        digits = input$config_decimal_digits)
      
    }
    
    tb
    
  })
  
  # Para sinserir um novo elemento na UI que seja utilizado por argumento por
  # alguma das funcoes: Insere-se o elemento na UI. 2- coloca-se o
  # input$novo_elemento na funcao correspondente abaixo. ele ser\u00E1 passado como
  # argumento para a funcao correta. 3 -Vai no global.R e adciona o argumento de
  # entrada na ordem certa na declaracao de argumentos dos funcao. 4 -
  # Processa-se a funcao normalmente que retornara um dataframe modificado com
  # metadadados. Esses metadados sera interpretados futuramente pela funcao
  # data_update_reload(). Cada formato de metados \u00E9 respecito a uma funcao
  # diferente. Lembrar de incluir o input$novo_elemento no Observer q desabilita 
  # o botao de aplicar a operacao na base de dados
  
  
  
  # dados para preview depois
  preview_data_after <- shiny::eventReactive(
    input$pre_processing, {
      
      shiny::req(data$main)
      shiny::validate(need(input$DE_vars_manipulate, "Selecione uma ou mais vari\u00E1veis"))
      
      id_preview <- shiny::showNotification(
        ui = "Calculando... Aguarde",
        type = "default",
        duration = NULL,
        closeButton = TRUE)
      base::on.exit(removeNotification(id_preview), add = TRUE)
      
      
      
      df <- data$main %>% 
        
        oper_mat_var_group(
          input$choose_action_modify, 
          input$oper_mat_var_operation, 
          input$DE_vars_manipulate, 
          input$oper_mat_var_new_name) %>% 
        
        oper_mat_cte_group(
          input$choose_action_modify,
          input$oper_mat_cte_operation,
          input$DE_vars_manipulate, 
          input$oper_mat_cte_definition,
          input$oper_mat_cte_suffix
          
        ) %>% 
        
        remove_var_group(
          input$choose_action_modify,
          input$DE_vars_manipulate
        ) %>% 
        
        rename_var(
          input$choose_action_modify,
          input$DE_vars_manipulate, 
          input$new_name_var) %>% 
        
        convert_var(
          input$choose_action_modify,
          input$DE_vars_manipulate,
          input$new_class,
          input$new_class_suffix
        )  %>% 
        
        transmute_var_group(
          input$choose_action_modify, 
          input$transmute_var_sub_options, 
          input$DE_vars_manipulate, 
          input$padronizar_med,
          input$padronizar_desv_pad,
          input$padronizar_rem_NA,
          input$padronizar_suffix,
          
          input$cat_suboptions,
          
          input$cat_quantile_ignore_NA,
          input$cat_quantile_interval,
          
          input$cat_sub_n,
          
          input$cat_user_interval,
          
          input$cat_convert_to_cod_alocado,
          input$cat_suffix
          
        ) %>% 
        
        oper_date_group(
          input$choose_action_modify, 
          input$oper_date_sub_options,
          input$DE_vars_manipulate, 
          input$oper_date_sub_fuso,
          input$text_to_date_format,
          input$text_to_date_suffix,
          input$date_to_numeric
        ) %>% 
        
        filter_data_group(
          input$choose_action_modify,
          input$DE_vars_manipulate, 
          input$con_filter_data_do,
          
          input$con_convert_to,
          
          input$con_between_var,
          input$con_inside_var,
          
          input$con_remove_na,
          
          input$con_igual_a,
          input$con_diferente_de,
          input$con_maior_que,
          input$con_maior_igual_a,
          input$con_menor_que,
          input$con_menor_igual_a
        )
      
      shinyjs::enable("DE_aplicar_na_base_de_dados")
      
      df
      
    })
  
  
  # desabilita o botao de aplicar na base de dados quando qq um dos argumentos
  # for alterado. Para habilita ro botao de aplicar na base de dados o usuario
  # devera aperta, antes, o botao de pre processamento
  shiny::observe({ 
    
    
    input$choose_action_modify 
    input$oper_mat_var_operation 
    input$DE_vars_manipulate 
    input$oper_mat_var_new_name
    
    
    input$choose_action_modify
    input$oper_mat_cte_operation
    input$DE_vars_manipulate 
    input$oper_mat_cte_definition
    input$oper_mat_cte_suffix
    input$choose_action_modify
    input$DE_vars_manipulate
    input$choose_action_modify
    input$DE_vars_manipulate 
    input$new_name_var
    input$choose_action_modify
    input$DE_vars_manipulate
    input$new_class
    input$choose_action_modify 
    input$transmute_var_sub_options 
    input$DE_vars_manipulate 
    input$padronizar_med
    input$padronizar_desv_pad
    input$padronizar_rem_NA
    input$padronizar_suffix
    
    input$cat_suboptions
    
    input$cat_quantile_ignore_NA
    input$cat_quantile_interval
    
    input$cat_sub_n
    
    input$cat_user_interval
    
    input$cat_convert_to_cod_alocado
    input$cat_suffix
    input$choose_action_modify 
    input$oper_date_sub_options
    input$DE_vars_manipulate 
    input$oper_date_sub_fuso
    input$text_to_date_format
    input$text_to_date_suffix
    input$date_to_numeric
    input$choose_action_modify
    input$DE_vars_manipulate 
    input$con_filter_data_do
    input$new_class_suffix
    
    input$con_convert_to
    
    input$con_between_var
    input$con_inside_var
    
    input$con_remove_na
    
    input$con_igual_a
    input$con_diferente_de
    input$con_maior_que
    input$con_maior_igual_a
    input$con_menor_que
    input$con_menor_igual_a
    
    shinyjs::disable("DE_aplicar_na_base_de_dados")
    
  })
  
  
  
  
  
  
  # tabela para preview depois
  output$preview_after <- DT::renderDataTable({
    df <-  preview_data_after()
    
    vars <- attr(df, "act_on_var")
    
    
    # Preview do REMOVER VARIAVEL
    if (attr(df, "oper_group") == "remove_var") {
      
      old_var <- attr(df, "act_on_var")
      
      out <- dplyr::tibble(
        "Vari\u00E1veis Exclu\u00EDdas" = old_var
      ) 
      
      return(data_table_preview(out))
      
      # Preview do ALTERAR NOME DA VARIAVEL
    } else if (is.list(vars) && vars$action == "rename") {
      
      
      out <- dplyr::tibble(
        "Nome Original" = vars$old_name,
        "Novo Nome" = vars$new_name
      ) 
      
      return(data_table_preview(out))
      
      # Preview do EXCLUIR DADOS
    } else if (is.list(vars) && 
               vars$action %in% c("exclude_data_filtered", 
                                  "exclude_data_non_filtered")) {
      
      # out <- dplyr::tibble(
      #   "Elementos nessa Condi\u00E7\u00E3o" = sum(vars$indexes)
      # ) 
      # 
      # return(data_table_preview(out))
      
      
      var <- attr(preview_data_after(), "act_on_var")
      var <- c("Elemento", var$vars)
      df <- preview_data_after() %>% remove_geo()
      
      
      if (input$preview_type == "Valores") {
        
        if (NROW(df) == 0) {
          
          i <- 0
          
        } else {
          
          i <- prev_intensity(input$perc_preview, df) %>% seq_len()
          
        }
        
        out <-  df[i, var, drop = FALSE ]
        
      } else if (input$preview_type == "Estrutura") {
        
        out <- df[, var, drop = FALSE] %>%
          dplyr::select(-Elemento) %>% 
          strucure_preview()
        
        
      } 
      
      # Preview do HAB/DESAB DADOS
      
    } else if (is.list(vars) &&
               vars$action %in% c("enable_obs", 
                                  "disable_obs", 
                                  "enable_obs_only", 
                                  "disable_obs_only")) {
      
      # out <- dplyr::tibble(
      #   "Elementos nessa Condi\u00E7\u00E3o" = sum(vars$indexes)
      # ) 
      # 
      # return(data_table_preview(out))
      # browser()
      
      index <- attr(preview_data_after(), "act_on_var")$indexes
      
      
      
      
      var <- attr(preview_data_after(), "act_on_var")
      var <- c("Elemento", var$vars)
      df <- preview_data_after() %>% remove_geo()
      
      
      if (input$preview_type == "Valores") {
        
        #out <-  df[mass_preview(), var, drop = FALSE ]
        
        out <- dplyr::tibble(
          "Qtde nados na Condi\u00E7\u00E3o" = sum(index),
          "Propor\u00E7\u00E3o" = mean(index)
        ) 
        
        
      } else if (input$preview_type == "Estrutura") {
        
        out <- df[, var, drop = FALSE] %>%
          dplyr::select(-Elemento) %>% 
          strucure_preview()
        
      } 
      
      
    } else  {
      # Preview dE TODO O RESTO
      
      var <- attr(preview_data_after(), "act_on_var")
      var <- c("Elemento", var)
      df <- preview_data_after() %>% remove_geo()
      
      
      if (input$preview_type == "Valores") {
        
        out <-  df[mass_preview(), var, drop = FALSE ]
        
      } else if (input$preview_type == "Estrutura") {
        
        out <- df[, var, drop = FALSE] %>%
          dplyr::select(-Elemento) %>% 
          strucure_preview()
        
      }
    }
    
   nms <- out %>% dplyr::select_if(is.numeric) %>% names()
    
   
   
    tb <- data_table_preview(out) 
      
    if (shiny::isTruthy(tb)) {
      
      tb <- tb %>% 
        
        DT::formatRound(nms, 
                        dec.mark = ",", 
                        mark = ".", 
                                   digits = input$config_decimal_digits)
      
    }
    
    tb
      
    
  })
  
  
  # ED - Aplicar na Base de Dados -------------------------------------------
  
  
  # habilitar o botao de aplicar na base de dados conforme a existencia de dados
  # para preview ou nao
  shiny::observe({
    
    shinyjs::disable("DE_aplicar_na_base_de_dados")
    
    shiny::req(preview_data_after())
    
    shinyjs::enable("DE_aplicar_na_base_de_dados")
    
  })
  
  
  
  # botao de aplicar na base de dados
  shiny::observeEvent(
    input$DE_aplicar_na_base_de_dados, {
      req(preview_data_after())
      
      df <-  preview_data_after()
      
      act_on_var <- base::attr(df, "act_on_var")
      
      data_update_reload(df, data, prop, vars = act_on_var)
      
    })
  
  
  # ED - Edicao de Valores de Observacoes -----------------------------------
  
  # lista de opcoes de variaveis no data$main
  shiny::observeEvent(
    data$main, 
    ignoreNULL = FALSE, ignoreInit = TRUE, {
      
      nms <- data$main %>% get_non_structural_names()
      
      shiny::updateSelectInput(
        session = session,
        inputId = "DE_data_obs_edit",
        choices = nms
      )
      
      
    })
  
  
  shiny::observe({
    
    shinyjs::disable("data_edit_init")
    
    shiny::req(input$DE_data_obs_edit)
    
    shinyjs::enable("data_edit_init")
    
  })
  
  
  # geracao da tabela
  output$DE_data_edit <- rhandsontable::renderRHandsontable({
    
    input$data_edit_init
    
    
    shiny::isolate({
      shiny::req(data$main)
      shiny::req(input$DE_data_obs_edit)
      
      nms <- input$DE_data_obs_edit
      nms <- c("Elemento", nms)
      
      df <- data$main[, nms, drop = FALSE] %>% remove_geo() 
      
      rhandsontable::rhandsontable(
        df, 
        rowHeaders = NULL, 
        #width = 550, 
        height = 600, 
        language = "pt-BR", 
        stretchH = "all"
      ) %>%
        
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, 
          allowColEdit = FALSE,
        )%>%
        
        rhandsontable::hot_cols(fixedColumnsLeft = 1) %>% 
        
        rhandsontable::hot_col("Elemento", readOnly = TRUE) #%>%
      
      #hot_table(highlightCol = TRUE, highlightRow = TRUE)
      
    })
    
  })
  
  
  #salvamento das alteracoes
  shiny::observeEvent(
    input$save_data_edit, 
    ignoreNULL = TRUE, 
    ignoreInit = TRUE,{
      #verificacoes iniciais
      shiny::need(input$DE_data_obs_edit, "Nada selecionado") %>% shiny::validate()
      shiny::need(input$DE_data_edit, "A tabela de edi\u00E7\u00E3o n\u00E3o existe") %>% shiny::validate()
      
      
      
      table <- input$DE_data_edit %>% rhandsontable::hot_to_r()
      
      nms <- names(table)
      nms <- nms[!(nms %in% "Elemento")]
      
      df <- data$main
      
      
      for (i in nms) {
        
        df[[i]] <- table[[i]]
        
      }
      
      
      
      data_update_reload(df, data, prop, vars = input$DE_data_obs_edit)
      
      
      # Acoes de Saida
      shiny::updateSelectInput(
        session = session,
        inputId = "DE_data_obs_edit",
        selected = character(0)
      )
      
      shinyBS::toggleModal(session, "modal_Edit_Data", toggle = "toggle")
      
    })
  
  
  # habiltiar/desabilitar todos - Botao da FIltragem de dados
  
  observeEvent(input$ED_enable_all, {
    
    DT::selectRows(DT::dataTableProxy("explo_datatable"), selected = NULL)
    
  })
  
  observeEvent(input$ED_disable_all, {
    
    DT::selectRows(DT::dataTableProxy("explo_datatable"), selected = seq_len(NROW(data$main)))
    
  })
  
  
  
  
  
  
  
  # CM - CITY MODELLING -----------------------------------------------------
  
  spatial_data <- shiny::reactive({
    
    is_spatial <- inherits(data$main, "sf")
    
    shiny::validate(shiny::need(is_spatial, "A base de dados inserida n\u00E3o \u00E9 georreferenciada"))
    
    data$main
    
  })
  
  spatial_data_jit <- shiny::reactive({
    
    shiny::req(spatial_data())
    
    sf::st_jitter(spatial_data(), input$config_mapa_point_jitter)
    
  })
  
  # CM - CRIACAO DO MAPA ----------------------------------------------------
  
  output$city_modelling <- leaflet::renderLeaflet({
    
    input$start
    
    shiny::isolate({
      shiny::req(spatial_data())
      shiny::req(prop)
      
      
      city_map(data$main)  %>% 
        city_map_data(spatial_data(), 
                      prop$obs_disabled,
                      cat = NULL, 
                      opacity_border = input$config_mapa_point_opacity_border, 
                      opacity_fill = input$config_mapa_point_opacity_inside, 
                      size = input$config_mapa_point_radius
        ) %>% 
        city_map_influence(prop$geo_influence, "Set3") %>%
        city_map_influence(prop$geo_model, "Set2") %>%
        city_map_influence(prop$geo_shp, "Set1") %>% 
        city_map_legend(
          prop$obs_disabled, 
          prop$geo_model, 
          prop$geo_influence, 
          prop$geo_shp) %>% 
        
        tool_draw()
      
      
    })
    
  })
  
  #criacao do proxy
  proxy_city_map <- leaflet::leafletProxy("city_modelling")
  
  
  # atualizacao dos dados habilitados ou nao
  shiny::observe({
    
    proxy_city_map %>% 
      city_map_data(
        spatial_data_jit(), 
        prop$obs_disabled,
        cat = NULL, 
        opacity_border = input$config_mapa_point_opacity_border, 
        opacity_fill = input$config_mapa_point_opacity_inside, 
        size = input$config_mapa_point_radius
      ) 
    
  })
  
  
  #atualizacao geo_shp
  shiny::observe({
    is_empty <- !length(prop$geo_shp)
    req(!is_empty)
    
    proxy_city_map %>% city_map_influence(prop$geo_shp, "Set1")
    
    
  })
  
  #atualizacao geo_influence
  shiny::observe({
    is_empty <- !length(prop$geo_influence)
    req(!is_empty)
    
    proxy_city_map %>% city_map_influence(prop$geo_influence, "Set3") 
    
  })
  
  #atualizacao geo_model
  shiny::observe({
    is_empty <- !length(prop$geo_model)
    req(!is_empty)
    
    proxy_city_map %>% city_map_influence(prop$geo_model, "Set3") 
    
  })
  
  
  # atualizacao de legenda
  shiny::observe({
    
    prop$obs_disabled
    prop$geo_model
    prop$geo_influence
    prop$geo_shp
    req(isolate(spatial_data()))
    
    
    proxy_city_map %>% 
      city_map_legend(
        prop$obs_disabled,
        prop$geo_model,
        prop$geo_influence,
        prop$geo_shp)
    
  })
  
  
  # CM - Desenho Novo no Mapa -----------------------------------------------
  
  
  new_feature <- shiny::reactive({
    shiny::req(input$city_modelling_draw_new_feature)
    shiny::req(isolate(spatial_data()))
    
    
    feature <- input$city_modelling_draw_new_feature
    
    get_sf(feature)
    
  })  
  
  
  # CM - BOTAO INSERIR GEO POLO ---------------------------------------------
  
  
  # Condicoes para o botao funcionar
  shiny::observe({
    
    shinyjs::disable("apply_geo_polo")
    
    shiny::validate(need(new_feature(), "Defina um objeto espacial"))
    shiny::validate(need(input$geo_polo_nome_var, "Defina um nome para o objeto"))
    
    shinyjs::enable("apply_geo_polo")
    
  })
  
  # preview da geom
  
  output$preview_geo_insert <- shiny::renderPlot({
    
    new_feature() %>% sf::st_geometry() %>%  graphics::plot()
    
  })
  
  # Acao do Botao
  shiny::observeEvent(input$apply_geo_polo, {
    
    
    id <- shiny::showNotification(
      ui = "Aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    base::on.exit(shiny::removeNotification(id), add = TRUE)
    
    
    df <- spatial_data()
    new_name <- input$geo_polo_nome_var
    geo_descr <- input$geo_polo_descr_var
    
    
    check_geo_name(session, new_name, names(data$main))
    
    
    obj <- new_feature() %>%
      
      sf::st_sf(
        geometry = .,
        "Nome" = new_name,
        "Detalhamento" = geo_descr,
        "\u00C1rea" = paste( round(sf::st_area(.), 2) , "m2")
      )
    
    
    dist <- sf::st_distance(df, obj)
    
    df[[new_name]] <- base::as.vector(dist)
    
    
    data_update_reload(df, data, prop, new_name)
    prop$geo_influence[[new_name]] <- obj
    
    shiny::updateTextAreaInput(session, "geo_polo_descr_var", value = "")
    shiny::updateTextInput(session, "geo_polo_nome_var", value = "")
    
  })
  
  
  # CM - BOTAO REMOVER GEO POLO ---------------------------------------------
  
  
  # lista os polos existentes para escolha e posterior remocao
  shiny::observe({
    
    nms <- names(prop$geo_influence)
    
    shiny::updateSelectInput(
      session, 
      "polo_a_remover", 
      choices = nms, 
      selected = character(0))
    
  })
  
  # Condicoes para habilitar o botao de remocao
  shiny::observe({
    
    shinyjs::disable("rem_geo_polo")
    
    shiny::req(input$polo_a_remover %in% base::names(isolate(spatial_data())))
    
    shinyjs::enable("rem_geo_polo")
    
  })
  
  output$preview_geo_polo_remover <- shiny::renderPlot({
    req(input$polo_a_remover)
    
    name <- input$polo_a_remover
    
    shiny::isolate({
      
      shiny::req(prop$geo_influence[[name]])
      
      prop$geo_influence[[name]] %>%
        sf::st_geometry() %>% plot()
      
    })
  })
  
  # acao do boto de remover geo polo
  shiny::observeEvent(input$rem_geo_polo, {
    
    id <- shiny::showNotification(
      ui = "Aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    base::on.exit(shiny::removeNotification(id), add = TRUE)
    
    df <- spatial_data()
    name <- input$polo_a_remover
    
    df[[name]] <- NULL
    data_update_reload(df, data, prop, name)
    
    prop$geo_influence[[name]] <- NULL
    proxy_city_map %>% leaflet::clearGroup(name)
    
    
  })
  
  
  # CM - BOTAO INSERIR SHAPE FILE -------------------------------------------
  
  # verificacao para habilitar o botao
  shiny::observe({
    
    shinyjs::disable("geo_shp_insert_button")
    
    shiny::req(input$geo_shp_insert_name)
    shiny::req(input$geo_shp_insert)
    shiny::req(isolate(spatial_data()))
    
    shinyjs::enable("geo_shp_insert_button")
    
  })
  
  # acao do botao
  shiny::observeEvent(input$geo_shp_insert_button, {
    
    id <- shiny::showNotification(
      ui = "Aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    base::on.exit(shiny::removeNotification(id), add = TRUE)
    
    
    df <- spatial_data()
    name <- input$geo_shp_insert_name
    infile <- input$geo_shp_insert
    
    check_geo_name(session, name, names(prop$geo_shp))
    
    
    # extrai a extensao do arquivo
    ext <- tools::file_ext(infile$name)
    
    # verifica se todas as extensoes necessarias foram inseridas
    if (!all(c("shp", "dbf", "prj", "shx") %in% ext)) {
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Erro de leitura!",
        text = "Ao menos um arquivo de extensao .shp, .dbf, .prj, .shx n\u00E3o foi selecionado",
        type = "error",
        html = TRUE
      )
      
      shiny::req(FALSE)
      
    }
    
    
    new_dir <- base::tempdir()
    base::file.copy(infile$datapath, file.path(new_dir, infile$name), overwrite = TRUE)
    
    
    layer_name <- stringr::str_subset(infile$name, "\\.(shp|shx|prj|dbf)$") %>%
      stringr::str_extract( "(.+)(?=\\..{3})") %>% unique()
    
    #verifica se o nome dos arquivos eh o mesmo para todas as
    if (base::length(layer_name) > 1) {
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Erro de leitura!",
        text = "Ao menos um arquivo de extensao .shp, .dbf, .prj, .shx n\u00E3o possui nome coincidente com os demais",
        type = "error",
        html = TRUE
      )
      shiny::req(FALSE)
    }
    
    
    opts <- base::paste0("ENCODING=", input$encoding_shp)
    
    shp <- sf::st_read(
      new_dir,
      layer = layer_name,
      options = opts,
      stringsAsFactors = FALSE) %>%
      sf::st_transform(4326) %>% check_encoding(session)
    
    a_alterar <- !(names(shp) %in% "geometry")
    
    base::names(shp)[a_alterar] <-  base::names(shp)[a_alterar] %>%
      base::paste0(input$geo_shp_insert_name,  "_", .)
    
    
    geom_type <- shp %>% sf::st_geometry_type() %>% base::unique() %>% base::as.character()
    
    
    #if (geom_type %in%  c("POLYGON","MULTIPOLYGON") ) {
    
    
    uniao <- sf::st_join(
      x = sf::st_transform(df, 3857),
      y = sf::st_transform(shp, 3857),
      suffix = c("", paste0(input$geo_shp_insert_name,  "_"))) %>%
      
      sf::st_transform(4326) 
    
    
    new_vars <- base::setdiff(names(uniao), names(df))
    names(shp) <- c(new_vars, "geometry")
    
    
    data_update_reload(uniao, data, prop, new_vars)
    prop$geo_shp[[name]] <- shp
    
    shiny::updateTextInput(session, "geo_shp_insert_name", value = "")
    
    #}
    
  })
  
  
  # CM - BOTAO REMOVER SHAPE FILE -------------------------------------------
  
  # lista os polos existentes para escolha e posterior remocao
  shiny::observe({
    
    nms <- names(prop$geo_shp)
    
    shiny::updateSelectInput(
      session = session,
      "shp_a_remover",
      choices = nms,
      selected = character(0))
    
  })
  
  
  # Condicoes para habilitar o botao de remocao
  observe({
    shinyjs::disable("rem_shp")
    
    req(input$shp_a_remover %in% names(prop$geo_shp))
    req(isolate(spatial_data()))
    
    shinyjs::enable("rem_shp")
    
  })
  
  
  # acao do botao de remover mapa
  observeEvent(input$rem_shp, {
    
    id <- shiny::showNotification(
      ui = "Aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    
    req(input$shp_a_remover)
    df <- spatial_data()
    
    vars_rem <- prop$geo_shp[[input$shp_a_remover]] %>% names()
    vars_rem <- setdiff(vars_rem, "geometry")
    
    for (i in vars_rem) {
      
      df[[i]] <- NULL
      
    }
    
    data_update_reload(df, data, prop, vars_rem)
    prop$geo_shp[[input$shp_a_remover]] <- NULL
    proxy_city_map %>% leaflet::clearGroup(input$shp_a_remover)
    
  })
  
  
  # CM - VINCULAR REGIAO MODELO ---------------------------------------------
  
  
  # Condicoes para o botao funcionar
  observe({
    
    shinyjs::disable("apply_geo_model")
    
    req(new_feature())
    validate(need(input$geo_model_nome, "Defina um nome para a regi\u00E3o"))
    req(isolate(spatial_data()))
    
    shinyjs::enable("apply_geo_model")
    
  })
  
  # preview da geom
  
  output$regiao_preview_add <- renderPlot({
    
    new_feature() %>% sf::st_geometry() %>%  plot()
    
  })
  
  
  # Acao do Botao
  observeEvent(input$apply_geo_model, {
    
    id <- shiny::showNotification(
      ui = "Aguarde!",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    name <- input$geo_model_nome
    check_geo_name(session, name, names(prop$geo_model) )
    
    
    obj <- new_feature() %>%
      sf::st_sf(geometry = .,
                "Nome" = input$geo_model_nome,
                "Detalhamento" = input$geo_model_descr,
                "\u00C1rea" = paste( round(sf::st_area(.), 2) , "m2"))
    
    
    prop$geo_model[[name]] <- obj
    proxy_city_map %>% leaflet::clearGroup(name)
    
    shiny::updateTextAreaInput(session, "geo_model_nome", value = "")
    shiny::updateTextInput(session, "geo_model_descr", value = "")
    
    
  })
  
  
  
  # CM - REMOVER REGIAO VINCULADA -------------------------------------------
  
  
  # lista os polos existentes para escolha e posterior remocao
  shiny::observe({
    
    nms <- names(prop$geo_model)
    
    shiny::updateSelectInput(
      session,
      "geo_model_rem",
      choices = nms,
      selected = character(0)
    )
    
  })
  
  # Condicoes para habilitar o bota de remocao
  observe({
    
    shinyjs::disable("geo_model_rem_button")
    
    req(input$geo_model_rem %in% names(prop$geo_model))
    req(isolate(spatial_data()))
    
    shinyjs::enable("geo_model_rem_button")
    
  })
  
  
  # preview da regiao
  output$regiao_preview_rem <- renderPlot({
    req(input$geo_model_rem)
    
    req(prop$geo_model[[input$geo_model_rem]])
    
    prop$geo_model[[input$geo_model_rem]] %>%
      sf::st_geometry() %>% plot()
    
    
  })
  
  
  # acao do boto de remover
  observeEvent(input$geo_model_rem_button,{
    
    prop$geo_model[[input$geo_model_rem]] <- NULL
    
    
  })
  
  
  
  # CM - Converter Vetor em Polo --------------------------------------------
  
  fonte_geo_shp <- reactiveVal()
  observe({
    fonte_geo_shp(input$city_modelling_shape_click )
    
  })
  
  observe({
    fonte_geo_shp(input$city_modelling_marker_click )
    
  })
  
  
  convert_vector_to_pole <- reactive({
    
    req(fonte_geo_shp())
    shape <- fonte_geo_shp()
    
    is_shp <-  shape$group %in% names(prop$geo_shp) 
    validate(need(is_shp, "O elmento selecionado n\u00E3o \u00E9 um objeto importado"))
    
    
    index <- as.numeric(shape$id)
    validate(need(index, "Indice inv\u00E1lido"))
    
    obj <- prop$geo_shp[[shape$group]][index, ] %>% sf::st_geometry() 
    
    
    list(
      obj = obj,
      index = index,
      group = shape$group
      
      
    )
  })
  
  
  
  output$vector_to_pole_group <- renderText({
    
    convert_vector_to_pole()$group
    
  })
  
  
  output$vector_to_pole_preview <- renderPlot({
    
    convert_vector_to_pole()$obj %>% plot()
    
  })
  
  
  
  observeEvent(input$convert_vector_to_pole_apply, {
    
    df <- spatial_data()
    obj <- convert_vector_to_pole()$obj
    new_name <- input$vector_to_pole_new_name
    req(new_name)
    
    
    obj <- obj %>% 
      
      sf::st_sf(
        geometry = .,
        "Nome" = new_name,
        "Detalhamento" = input$vector_to_pole_desc,
        "\u00C1rea" = paste( round(sf::st_area(.), 2) , "m2")
      )
    
    
    dist <- sf::st_distance(df, obj)
    
    df[[new_name]] <- as.vector(dist)
    
    
    data_update_reload(df, data, prop, new_name)
    prop$geo_influence[[new_name]] <- obj
    
  })    
  
  
  
  
  # CM - DOWNLOAD ARQUIVOS ESPACIAIS ----------------------------------------
  
  
  # CM - DOWNLOAD KML -------------------------------------------------------
  
  
  output$download_geo_influence_kml <- downloadHandler(
    filename = "elementos_espaciais.kml",
    content = function(file) {
      
      # verificacoes
      check_download_maps(
        session,
        input$mapas_exportar_incluir,
        prop$geo_influence,
        prop$geo_shp,
        prop$geo_model,
        data$main)
      
      
      plotKML::kml_open(
        file.name = file,
        folder.name = "Elementos Espaciais")
      
      
      
      
      if ("geo_influence" %in% input$mapas_exportar_incluir) {
        
        for (i in names(prop$geo_influence)) {
          
          obj <- prop$geo_influence[[i]]
          
          obj <- obj %>% fix_encoding(.to = "UTF-8")
          obj <- methods::as(obj, "Spatial")
          pop <- create_popup_tb(obj@data) %>% iconv(to = "UTF-8")
          
          plotKML::kml_layer(
            obj,
            html.table = pop,
            subfolder.name =  i,
            colour = "blue",
            fill = "blue",
            size = 1,
            alpha = 0.45,
            shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            width = 5)
          
        }
        
      }
      
      
      
      if ("geo_shp" %in% input$mapas_exportar_incluir) {
        
        # arq <- lapply(prop$geo_shp, as, "Spatial")
        
        # pop <- lapply(arq, function(x) {  create_popup_tb(x@data)  })
        
        for (i in names(prop$geo_shp)) {
          
          # plotKML::kml_layer(
          #   arq[[i]],
          #   html.table = pop[[i]],
          #   subfolder.name =  i,
          #   colour = "blue",
          #   fill = "blue",
          #   size = 1,
          #   alpha = 0.45,
          #   shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
          #   width = 5)
          
          
          obj <- prop$geo_shp[[i]]
          
          obj <- obj %>% fix_encoding(.to = "UTF-8")
          obj <- methods::as(obj, "Spatial")
          pop <- create_popup_tb(obj@data) %>% iconv(to = "UTF-8")
          
          plotKML::kml_layer(
            obj,
            html.table = pop,
            subfolder.name =  i,
            colour = "blue",
            fill = "blue",
            size = 1,
            alpha = 0.45,
            shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            width = 5)
          
        }
        
      }
      
      
      
      
      
      if ("geo_model" %in% input$mapas_exportar_incluir) {
        
        # arq <- lapply(prop$geo_model, as, "Spatial")
        # 
        # pop <- lapply(arq, function(x) {  create_popup_tb(x@data)  })
        
        for (i in names(prop$geo_model)) {
          
          # plotKML::kml_layer(
          #   arq[[i]],
          #   html.table = pop[[i]],
          #   subfolder.name =  i,
          #   colour = "blue",
          #   fill = "blue",
          #   size = 1,
          #   alpha = 0.45,
          #   shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
          #   width = 5)
          
          obj <- prop$geo_model[[i]]
          
          obj <- obj %>% fix_encoding(.to = "UTF-8")
          obj <- methods::as(obj, "Spatial")
          pop <- create_popup_tb(obj@data) %>% iconv(to = "UTF-8")
          
          plotKML::kml_layer(
            obj,
            html.table = pop,
            subfolder.name =  i,
            colour = "blue",
            fill = "blue",
            size = 1,
            alpha = 0.45,
            shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
            width = 5)
        }
        
      }
      
      
      if ("geo_obs" %in% input$mapas_exportar_incluir) {
        
        pop <- data$main %>% remove_geo() %>%  create_popup_tb()
        arq <- sf::as_Spatial(data$main)
        
        plotKML::kml_layer(
          arq,
          html.table = pop,
          subfolder.name =  "Banco de Dados",
          colour = "blue",
          fill = "blue",
          size = .5,
          alpha = 0.45,
          shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
          width = 5)
        
      }
      
      
      
      plotKML::kml_close(file)
      
      
    })
  
  
  
  # CM - DOWNLOAD RDS -------------------------------------------------------
  
  
  
  
  output$download_geo_influence_rds <- downloadHandler(
    filename = "elementos_espaciais.rds",
    content = function(file) {
      
      
      shiny::showNotification(
        ui = "Por padr\u00E3o, as observa\u00E7\u00F5es da planilha n\u00E3o s\u00E3o exportadas nos arquivos .rds",
        type = "message",
        duration = 5,
        closeButton = TRUE)
      
      
      # verificacoes
      check_download_maps(
        session,
        input$mapas_exportar_incluir,
        prop$geo_influence,
        prop$geo_shp,
        prop$geo_model,
        data$main)
      
      
      geo <- list()
      
      
      if ("geo_influence" %in% input$mapas_exportar_incluir) {
        
        geo[["geo_influence"]] <- prop$geo_influence
        
        
      }
      
      if ("geo_shp" %in% input$mapas_exportar_incluir) {
        
        geo[["geo_shp"]] <- prop$geo_shp
        
        
      }
      
      if ("geo_model" %in% input$mapas_exportar_incluir) {
        
        geo[["geo_model"]] <- prop$geo_model
        
        
      }
      
      # if ("geo_obs" %in% input$mapas_exportar_incluir) {
      #
      #   geo[["geo_obs"]] <- central$rzm
      #
      #
      # }
      
      attr(geo, "type") <- "geobox_spatial"
      
      saveRDS(geo, file)
      
      
    })
  
  
  
  # CM - DOWNLOAD SHAPE FILES -----------------------------------------------
  
  output$download_geo_shape_files <- downloadHandler(
    filename = "elementos_espaciais.zip",
    content = function(file) {
      
      
      #  # verificacoes
      #  check_download_maps(
      #    session,
      #    input$mapas_exportar_incluir,
      #    central$prop$geo_influence,
      #    central$prop$geo_shp,
      #    central$prop$geo_model,
      #    central$rzm)
      #
      #
      # fake_dir <- tempdir()
      #
      #
      #  if ("geo_influence" %in% input$mapas_exportar_incluir) {
      #
      #    geo <- do.call(rbind, central$prop$geo_influence)
      #
      #    sf::st_write(geo,
      #             dsn = file.path(fake_dir, "polos_influencitantes.shp"),
      #             driver = "ESRI Shapefile")
      #
      #
      #
      #  }
      #
      #  if ("geo_shp" %in% input$mapas_exportar_incluir) {
      #
      #    geo[["geo_shp"]] <- central$prop$geo_shp
      #
      #
      #  }
      #
      #  if ("geo_model" %in% input$mapas_exportar_incluir) {
      #
      #    geo[["geo_model"]] <- central$prop$geo_model
      #
      #
      #  }
      #
      #  # if ("geo_obs" %in% input$mapas_exportar_incluir) {
      #  #
      #  #   geo[["geo_obs"]] <- central$rzm
      #  #
      #  #
      #  # }
      #
      #
      #
      # zip(file, file.path(fake_dir, "polos_influencitantes.shp"))
      
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Recurso temporariamente n\u00E3o dispon\u00EDvel!",
        text = "A exporta\u00E7\u00E3o em shape files ser\u00E1 elaborada nas pr\u00F3ximas vers\u00F5es do aplicativo",
        type = "info",
        html = TRUE
      )
      
      
      req(FALSE)
      
      
      
    })
  
  
  
  
  # CM - IMPORTAR RDS -------------------------------------------------------
  
  
  
  observeEvent(input$insert_geo_influence_rds, {
    
    req(spatial_data())
    
    
    # if (!check_sf) {
    #   shinyWidgets::sendSweetAlert(
    #     session = session,
    #     title = "Erro de leitura!",
    #     text = "O arquivo inicial n\u00E3o \u00E9 georreferenciado. Inser\u00E7\u00E3o de Geo Polos abortada.",
    #     type = "error",
    #     html = TRUE
    #   )
    # }
    # 
    # validate(need(check_sf, "O arquivo inicial n\u00E3o \u00E9 georreferenciado"))
    
    
    
    
    infile <- input$insert_geo_influence_rds
    
    
    obj <- tryCatch({
      
      readRDS(infile$datapath)
      
    }, error = function(e) {
      
      NULL
      
    })
    
    
    if (
      is.null(obj) ||
      is.null(attr(obj, "type")) ||
      attr(obj, "type") != "geobox_spatial") {
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Erro de leitura!",
        text = "Esse n\u00E3o \u00E9 um arquivo v\u00E1lido para Polos Influenciantes",
        type = "error",
        html = TRUE
      )
      
      req(FALSE)
      
    } 
    
    
    
    df <- data$main
    new_var <- NULL
    
    
    if (!is.null(obj$geo_influence)) {
      
      df <- add_geo_influence_var(df, obj$geo_influence)
      
      new_var <- append(new_var, names(obj$geo_influence) )
      
      prop$geo_influence <- add_geo(old = prop$geo_influence, 
                                    new = obj$geo_influence)
      
    }
    
    
    if (!is.null(obj$geo_shp)) {
      
      df <- add_geo_shp_var(df, obj$geo_shp)
      
      new_vars <- lapply(obj$geo_shp, function(x) { setdiff(names(x), "geometry") }) %>% unlist()
      
      new_var <- append(new_var, new_vars)
      
      prop$geo_shp <- add_geo(old = prop$geo_shp, new = obj$geo_shp)
      
    }
    
    
    if (!is.null(obj$geo_model)) {
      
      prop$geo_mdoel <- add_geo(old = prop$geo_model, new = obj$geo_model)
      
    }
    
    if (!is.null(new_var)) {
      
      data_update_reload(df, data, prop, new_var)
      
    }
    
    
  })
  
  
  
  
  # CM - FLY TO -------------------------------------------------------------
  
  # voar para a coordenada
  observeEvent(input$fly_button, {
    
    req(input$fly_lat)
    req(input$fly_lng)
    
    latitude <- input$fly_lat
    longitude <- input$fly_lng
    
    proxy_city_map %>%
      
      leaflet::flyTo(
        lng = longitude,
        lat = latitude,
        zoom = 12)
    
  })
  
  
  #voar para o dado: Escolha do dado
  observe({
    
    shiny::updateSelectInput(
      session,
      "fly_to_obs",
      choices = spatial_data_jit()[["Elemento"]],
      selected = character(0))
    
  })
  
  #voar para o dado
  observeEvent(input$fly_button2, {
    req(input$fly_to_obs)
    req(spatial_data_jit())
    
    line <- spatial_data_jit()[as.numeric(input$fly_to_obs), ]
    
    coo <- sf::st_coordinates(line)
    
    proxy_city_map %>%
      leaflet::flyTo(coo[1], coo[2], zoom = 18)
    
    shiny::updateSelectInput(
      session = session,
      inputId = "fly_to_obs",
      selected = character(0))
    
  })
  
  
  
  
  # CM - ALTERAR COORDENADAS ------------------------------------------------
  
  # Identificando o ponto a ter suas coordenadas alteradas
  point_old_location <- reactive({
    req(input$aba_polo == "alt_coo")
    req(input$city_modelling_shape_click)
    
    pnt <- input$city_modelling_shape_click
    
    req(pnt$group %in% c("Habilitado", "Desabilitado"))
    req(pnt$id)
    
    pnt$obj <- spatial_data() %>% dplyr::filter(Elemento == pnt$id)
    
    pnt
  })
  
  
  # tabelinha com informacoes do ponto, antiga localizacao
  output$confirm_obs_selection <- renderTable({
    req(point_old_location())
    
    pnt <- point_old_location()
    
    coo <- pnt$obj %>% sf::st_coordinates()
    
    dplyr::tibble(Elemento = pnt$id, Latitude = coo[2], Longitude = coo[1])
    
  }, align  = "c", digits = 6)
  
  
  # mapa antes
  output$ponto_antes <- leaflet::renderLeaflet({
    
    obj <- point_old_location()$obj
    
    obj %>% leaflet::leaflet() %>% leaflet::addMarkers() %>% leaflet::addTiles()
    
  })
  
  
  
  
  # identificando a nova localizacao do ponto
  point_new_location <- reactive({
    
    req(input$aba_polo == "alt_coo")
    req(input$city_modelling_click)
    
    pnt <- input$city_modelling_click
    
    
    df <- data.frame(Longitude = pnt$lng, Latitude = pnt$lat)
    
    pnt$obj <- sf::st_as_sf(df, coords = c("Longitude", "Latitude")) %>%
      sf::st_set_crs(4326)
    
    pnt
  })
  
  
  #tabelinha com informacaoes do ponto, nova localizacao
  output$obs_new_coordinates <- renderTable({
    
    req(point_new_location())
    
    pnt <- point_new_location()
    
    dplyr::tibble(Latitude = pnt$lat, Longitude = pnt$lng)
    
    
  }, align  = "c", digits = 6)
  
  
  # mapa com nova localizacao
  output$ponto_depois <- leaflet::renderLeaflet({
    
    req(point_new_location())
    
    obj <- point_new_location()$obj
    
    obj %>% leaflet::leaflet() %>% leaflet::addMarkers() %>% leaflet::addTiles()
    
  })
  
  alt_coo_distance <- reactive({
    req(point_old_location())
    req(point_new_location())
    
    old <- point_old_location()$obj
    new <- point_new_location()$obj
    
    sf::st_distance(old, new) %>% 
      round(2) %>% 
      paste0("Dist\u00e2ncia de " ,. ," metros")
    
  })
  
  
  output$alt_coo_dist <- renderText({ alt_coo_distance() })
  
  output$alt_coo_dist2 <- renderText({ alt_coo_distance() })
  
  
  observeEvent(input$confirm_marker_coo, {
    req(point_old_location())
    req(point_new_location())
    
    
    
    pnt_destiny <- point_new_location()
    pnt_origin <- point_old_location()
    
    df <- spatial_data()
    
    index <- df$Elemento == pnt_origin$id
    
    elemento <- df[index, ]
    
    #altera as coordenadas na base de dados principal
    sf::st_geometry(elemento) <- sf::st_sfc(sf::st_point(c(pnt_destiny$lng, pnt_destiny$lat)), crs = 4326)
    
    
    new_var <- NULL
    
    if (!is.null(prop$geo_influence)) {
      
      elemento <- add_geo_influence_var(elemento, prop$geo_influence)
      new_var <- append(new_var, names(prop$geo_influence) )
      
    }
    
    
    if (!is.null(prop$geo_shp)) {
      
      elemento <- add_geo_shp_var(elemento, prop$geo_shp)
      
      new_vars <- lapply(prop$geo_shp, function(x) { setdiff(names(x), "geometry") }) %>% unlist()
      new_var <- append(new_var, new_vars)
      
    }
    
    # compatibilidade das classes
    for (i in names(df)) {
      if (i == "geometry") next
      
      dest_class <- class(df[[i]])
      from_class <- class(elemento[[i]])
      
      #browser()
      if (dest_class != from_class) {
        
        elemento[[i]] <- convert_var_fit(elemento[[i]], dest_class)
        
      }
    }
    
    
    for (i in names(df)) {
      
      df[which(index),  ][[i]] <- elemento[1, ][[i]]
      
    }
    
    
    #df[which(index), "nucleos_sp_id" ] <- elemento[, "nucleos_sp_id"]
    
    
    
    
    shinyBS::toggleModal(session, "alt_coordinates", toggle = "close")
    
    data_update_reload(df, data, prop, vars = new_var)
    
    
  })
  
  
  
  
  # CM - Filtrar Espacial ---------------------------------------------------
  
  observeEvent(input$spatial_filter_go, {
    
    
    df <- spatial_data()
    region <- new_feature()
    action <- input$spatial_filter_data_do
    
    common <- sf::st_intersection(
      df %>% sf::st_transform(3857), 
      region%>% sf::st_transform(3857))
    
    n_elements <- NROW(common)
    
    # if (n_elements < 1) {
    #   
    #   id <- shiny::showNotification(
    #     ui = "Nenhum elemento encontrado na regi\u00E3o especificada",
    #     type = "error",
    #     duration = 2,
    #     closeButton = TRUE)
    #   
    #   validate(need(n_elements > 0, "Nenhum elemento encontrado. Opera\u00E7\u00E3o Cancelada!"))
    #   
    # }
    
    
    if (n_elements > 0) {
      
      index <- df$Elemento %in% common$Elemento 
      
    } else {
      
      index <-  df$Elemento %in%  seq_len(NROW(df))
      
    }
    
    df <- filter_prepare(df, index, action, vars = NULL, oper_group = "filter_data")
    
    
    data$confirm <- df 
    
    shinyWidgets::confirmSweetAlert(
      session = session, 
      inputId = "myconfirmation", 
      type = "warning",
      title = "Confirma a Opera\u00E7\u00E3o?", 
      danger_mode = TRUE,
      html = TRUE
    )
    
    
    
  })
  
  observeEvent(input$myconfirmation, { 
    
    req(input$myconfirmation)
    
    
    var <- attr(data$confirm, "act_on_var")
    
    data_update_reload(data$confirm, data, prop, var)
    
    data$confirm <- NULL
  })
  
  
  # habiltiar/desabilitar todos - Botao da Filtragem Espacial
  observeEvent(input$CM_enable_all, {
    
    DT::selectRows(DT::dataTableProxy("explo_datatable"), selected = NULL)
    
  })
  
  observeEvent(input$CMD_disable_all, {
    
    DT::selectRows(DT::dataTableProxy("explo_datatable"), selected = seq_len(NROW(data$main)))
    
  })
  
  
  
  
  
  
  # TA - Table Analysis -----------------------------------------------------
  
  
  observeEvent(data$main, {
    
    nms <- data$main %>% get_non_structural_names()
    
    
    # seletor de variaveis
    selected <- isolate(input$table_analysis_var)
    
    if (is.null(selected)) { selected <- character(0) }
    
    shiny::updateSelectizeInput(
      session, 
      "table_analysis_var", 
      choices = nms,
      selected = selected
    )
    
    # seletor de categorias
    selected <- isolate(input$table_analysis_cats)
    
    if (is.null(selected)) { selected <- character(0) }
    
    shiny::updateSelectizeInput(
      session, 
      "table_analysis_cats", 
      choices = nms,
      selected = selected
    )
    
    
    
  })
  
  # Criacao da tabela
  
  output$table_analysis_DT <- DT::renderDataTable({
    req(data$main)
    req(input$table_analysis_var)
    
    
    groups <- input$table_analysis_cats
    vars <- input$table_analysis_var
    
    subset <- union(vars, groups)
    
    validate(need(any(!prop$obs_disabled_ae), "Nenhum dado habilitado"))
    
    df <- data$main[!prop$obs_disabled_ae, subset, drop = FALSE] %>% 
      remove_geo() %>% 
      remove_key_column() %>% 
      dplyr::group_by_at(groups)
    
    na_rm <- input$TA_remove_na
    
    list_functions <- list(
      "N" = ~dplyr::n(),
      "N_dist" = dplyr::n_distinct,
      "qtde_na" = ~sum(is.na(.)),
      "M\u00E9dia" = ~mean(., na.rm = na_rm), 
      "Mediana" = ~median(., na.rm = na_rm),#, 
      "Moda" = ~moda(.), 
      "DP" = ~sd(., na.rm = na_rm),
      "Min" = ~min(., na.rm = na_rm),
      "1Q" = ~quantile(., probs = 0.25, na.rm = na_rm),
      "3Q" = ~quantile(., probs = 0.75, na.rm = na_rm),
      "Max" = ~max(., na.rm = na_rm)
    )
    
    list_functions <- list_functions[input$table_analysis_options]
    
    
    if (any(names(list_functions) %in% 
            c("M\u00E9dia", "Mediana","Moda","DP" ,"Min", "1Q", "3Q", "Max"))) {
      
      check_numeric_and_na(df[, vars, drop = FALSE], input$TA_remove_na)
      
    }
    
    validate(need(!(!length(list_functions)), "Nenhuma fun\u00E7\u00E3o selecionada"))
    
    
    df2 <- df %>% dplyr::summarise_all(list_functions) 
    
    
    df2 %>% 
      
      data_table_preview() %>% 
      
      DT::formatRound(
        base::names(df2),
        digits = input$config_decimal_digits,
        dec.mark = ",",
        mark = ".")
    
  })
  
  
  # TA - Data Panel ---------------------------------------------------------
  
  # ARGUMENTOS PARA O MODULO
  
  TA_obs_disabled_principal <- reactive({ prop$obs_disabled_ae })
  TA_obs_disabled_secundary <- reactive({ prop$obs_disabled })
  
  # EXECUCAO DO MODULO
  TA_enabling <- data_panel_SERVER(
    "TA_data_panel", 
    non_spatial_data = df_non_spatial, 
    obs_disabled_principal = TA_obs_disabled_principal, 
    obs_disabled_secundary = TA_obs_disabled_secundary)
  
  # SALVAMENTO DAS INFORMACOES DO MODULO
  # base principal
  observeEvent(TA_enabling$principal(), ignoreInit = TRUE, {
    
    prop$obs_disabled_ae <- TA_enabling$principal()
    
  })
  
  # base secundaria
  observeEvent(TA_enabling$secundary(), ignoreInit = TRUE, {
    
    prop$obs_disabled <- TA_enabling$secundary()
    
  })
  
  
  # AE - ANALISE EXPLORATORIA --------------------------------------------------
  
  
  
  # AE - PAINEL DE DADOS ----------------------------------------------------
  
  # ARGUMENTOS PARA O MODULO
  
  AE_obs_disabled_principal <- reactive({ prop$obs_disabled_ae })
  AE_obs_disabled_secundary <- reactive({ prop$obs_disabled })
  
  # EXECUCAO DO MODULO
  AE_enabling <- data_panel_SERVER(
    "AE_data_panel", 
    non_spatial_data = df_non_spatial, 
    obs_disabled_principal = AE_obs_disabled_principal, 
    obs_disabled_secundary = AE_obs_disabled_secundary)
  
  # SALVAMENTO DAS INFORMACOES DO MODULO
  # base principal
  observeEvent(AE_enabling$principal(), ignoreInit = TRUE, {
    # print(AE_enabling$principal())
    prop$obs_disabled_ae <- AE_enabling$principal()
    
  })
  
  # base secundaria
  observeEvent(AE_enabling$secundary(), ignoreInit = TRUE, {
    
    prop$obs_disabled <- AE_enabling$secundary()
    
  })
  
  
  # AE - GEOMETRICA E GEOGRAFICA --------------------------------------------
  
  
  
  # MENUS DE OPCAO DA ANALISE GEOMETRICA E GEOGRAFICA
  observeEvent(non_struct_names(), {
    shiny::req(non_struct_names())
    
    opts <- c("Elemento", non_struct_names())
    opts_formated <- opts %>% format_choices(30)
    
    
    # 1d
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_uni_x",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_uni_group",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    
    # 2d
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_bi_x",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_bi_y",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_bi_group",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    
    # 3d
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_tri_x",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_tri_y",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_tri_z",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_tri_group",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    
    
    # geo
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_geo_group",
      choices = opts,
      selected = character(0),
      choicesOpt = list(
        content = opts_formated
      )
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "AE_geo_focus",
      
      choices = data$main$Elemento
    )
    
  })
  
  
  # AE - Atualizacao Transformadas Possiveis -----------------------------------
  
  # atualiza as transformadas possiveis para cada variavel com base na
  # variavel escolhida pelo usuario nos respectivos pickers
  
  
  ## analise unidimensional
  ### eixo horizontal
  observeEvent(input$AE_uni_x, ignoreInit = TRUE, {
    
    var <- input$AE_uni_x
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_uni_x_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  ## analise bidimensional
  ### eixo horizontal
  observeEvent(input$AE_bi_x, ignoreInit = TRUE, {
    
    var <- input$AE_bi_x
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_bi_x_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  ## analise bidimensional
  #### eixo vertical
  observeEvent(input$AE_bi_y, ignoreInit = TRUE, {
    
    var <- input$AE_bi_y
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_bi_y_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  ## analise multi
  ### eixo x
  observeEvent(input$AE_tri_x, ignoreInit = TRUE, {
    
    var <- input$AE_tri_x
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_tri_x_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  ## analise multi
  ### eixo x
  observeEvent(input$AE_tri_y, ignoreInit = TRUE, {
    
    var <- input$AE_tri_y
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_tri_y_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  ## analise multi
  #### eixo z
  observeEvent(input$AE_tri_z, ignoreInit = TRUE, {
    
    var <- input$AE_tri_z
    var_trns_possible <- prop$var_trns_possible[[var]]
    
    selected <- prop$var_trns_selected[[var]]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "AE_tri_z_trns",
      selected = selected,
      choices = var_trns_possible)
    
  })
  
  
  
  
  
  
  # AE - Base Compartilhada Geometrica e Geografica -------------------------
  
  # indice dos elementos a exibir. Esse valor e modificado pelo grafico do
  # plotly e futuramente sera pelo grafico geografico
  selected_elments <- reactiveVal(value = TRUE)
  
  # com base no selected_elements(), filtra-se a base de dados
  AE_shared_data <- reactive({
    
    is_geo <- inherits(data$main, "sf")
    validate(need(is_geo, "A Base de dados n\u00E3o \u00E9 georreferenciada"))
    
    
    i <- selected_elments()
    
    data$main[i, ]
    
  })
  
  
  AE_shared_data_jit <- reactive({
    #shiny::req()
    AE_shared_data()
    
  })
  
  
  
  
  # AE - MAPA - Criacao -----------------------------------------------------
  
  
  output$AE_map <- leaflet::renderLeaflet({
    
    input$start
    
    isolate({
      shiny::req(AE_shared_data())
      shiny::req(prop)
      
      #browser()
      city_map(AE_shared_data())  %>%
        
        city_map_data(AE_shared_data(),
                      prop$obs_disabled_ae,
                      cat = NULL,
                      opacity_border = input$config_mapa_point_opacity_border,
                      opacity_fill = input$config_mapa_point_opacity_inside,
                      size = input$config_mapa_point_radius
        ) %>%
        
        city_map_influence(prop$geo_influence, "Set3") %>%
        city_map_influence(prop$geo_model, "Set2") %>%
        city_map_influence(prop$geo_shp, "Set1") %>%
        city_map_legend(
          prop$obs_disabled,
          prop$geo_model,
          prop$geo_influence,
          prop$geo_shp)
      
    })    
    
  })
  
  
  #criacao do proxy
  proxy_AE_map <- leaflet::leafletProxy("AE_map")
  
  
  #atualizacao dos dados habilitados ou nao
  observe({
    
    
    i <- selected_elments()
    
    proxy_AE_map %>%
      
      city_map_data(
        spatial_data_jit()[i, ],
        prop$obs_disabled_ae[i],
        cat = input$AE_geo_group,
        opacity_border = input$config_mapa_point_opacity_border,
        opacity_fill = input$config_mapa_point_opacity_inside,
        size = input$config_mapa_point_radius
      )
    
  })
  
  #atualizacao geo_shp
  observe({
    is_empty <- !length(prop$geo_shp)
    shiny::req(!is_empty)
    
    proxy_AE_map %>% city_map_influence(prop$geo_shp, "Set1")
    
    
  })
  
  #atualizacao geo_influence
  observe({
    is_empty <- !length(prop$geo_influence)
    shiny::req(!is_empty)
    
    proxy_AE_map %>% city_map_influence(prop$geo_influence, "Set3")
    
  })
  
  #atualizacao geo_model
  observe({
    is_empty <- !length(prop$geo_model)
    shiny::req(!is_empty)
    
    proxy_AE_map %>% city_map_influence(prop$geo_model, "Set3")
    
  })
  
  
  # atualizacao de legenda
  observe({
    
    prop$obs_disabled_ae
    prop$geo_model
    prop$geo_influence
    prop$geo_shp
    shiny::req(isolate(AE_shared_data_jit()))
    
    
    proxy_AE_map %>%
      city_map_legend(
        prop$obs_disabled_ae,
        prop$geo_model,
        prop$geo_influence,
        prop$geo_shp)
    
  })
  
  
  
  
  
  # AE - MAPA - Focar no Dado --------------------------------------------------
  
  
  observeEvent(
    input$AE_geo_focus,
    ignoreNULL = TRUE,
    ignoreInit = TRUE, {
      
      shiny::req(spatial_data())
      
      line <- spatial_data()[as.numeric(input$AE_geo_focus), ]
      
      coo <- sf::st_coordinates(line)
      
      leaflet::leafletProxy("AE_map") %>%
        leaflet::flyTo(coo[1], coo[2], zoom = 18)
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "AE_geo_focus",
        selected = character(0))
      
    })
  
  
  
  
  # AE - PLOT 1D - Cria Grafico ------------------------------------------------
  
  plot_1d_barmode <- reactive({
    
    
    switch(input$plot_1d_barmode,
           "Empilhar" = "stack" , 
           "Agrupar" = "group" , 
           "Sobrepor" = "overlay" , 
           "Relativo" = "relative"
    )
    
  })
  
  plot_1d_histnorm <- reactive({
    
    input$plot_1d_histnorm
    
    switch(input$plot_1d_histnorm,
           "Freq. Absoluta" = "", 
           "Freq. Relativa" = "probability", 
           "Freq. Relativa (%)" = "percent", 
           "Dens. Absoluta" = "density",
           "Dens. Prob." = "probability density"
    )
    
  })
  
  # https://stackoverflow.com/questions/53614645/how-to-use-plotlyproxy-in-shiny-app-with-ggplotly-to-make-plots-render-faste
  
  output$explo_plot_uni <- plotly::renderPlotly({
    
    shiny::req(data$main)
    shiny::req(isolate(input$explo_analy_qtde_eixos == "uni"))
    validate(need(input$AE_uni_x, "Especifique uma vari\u00E1vel para o eixo horizontal"))
    validate(need(input$AE_uni_x_trns, "Especifique uma transformada para a vari\u00E1vel"))
    
    
    
    plot1d(data$main,
           var_x = input$AE_uni_x, 
           var_x_trs = input$AE_uni_x_trns, 
           disabled = prop$obs_disabled_ae,
           show_disabled = input$plot_1d_show_disabled,
           cat = input$AE_uni_group, 
           show_legend = input$plot_1d_show_legend,
           alpha = input$plot_1d_alpha,
           barmode = plot_1d_barmode(),
           nbinsx = input$plot_1d_nbinsx,
           histnorm  = plot_1d_histnorm(),
           #histfunc = input$plot_1d_histfunc,
           show_mean_median = input$plot_1d_show_mean_median,
           cumula = input$plot_1d_cumalative,
           source = "plot_1d")
    
  })
  
  
  selected_from_1d <- reactive({
    shiny::req(input$explo_analy_qtde_eixos == "uni")
    
    plotly::event_data("plotly_brushing", source = "plot_1d")
  })
  
  # Plot Click 1D
  observeEvent(
    selected_from_1d(),
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      
      shiny::req(data$main)
      
      df <- data$main
      
      var_x <- input$AE_uni_x
      selected <- selected_from_1d()
      
      
      if (!is.null(selected)) {
        
        i <- df[[var_x]] >= selected$x[1] & df[[var_x]] <= selected$x[2]
        
        #i <- df$Elemento[i] %>%  as.numeric()
        
      } else {
        
        i <- TRUE
        
      }
      
      selected_elments(i)
      
    })
  
  
  
  
  # AE - PLOT 2D - Cria Grafico ------------------------------------------------
  
  output$explo_plot_bi <- plotly::renderPlotly({
    
    shiny::req(data$main)
    shiny::req(isolate(input$explo_analy_qtde_eixos == "bi"))
    
    
    validate(need(input$AE_bi_x, "Especifique uma vari\u00E1vel para o eixo horizontal"))
    validate(need(input$AE_bi_y, "Especifique uma vari\u00E1vel para o eixo vertical"))
    validate(need(input$AE_bi_x_trns, "Especifique uma transformada para o eixo horizontal"))
    validate(need(input$AE_bi_y_trns, "Especifique uma transformada para o eixo vertical"))
    
    
    plot2d(
      data$main,
      var_x = input$AE_bi_x, 
      var_y = input$AE_bi_y, 
      
      var_x_trs = input$AE_bi_x_trns, 
      var_y_trs = input$AE_bi_y_trns, 
      
      disabled = prop$obs_disabled_ae,
      
      cat = input$AE_bi_group, 
      
      show_disabled = input$plot_2d_show_disabled,
      show_legend = input$plot_2d_show_legend,
      
      alpha = input$plot_2d_alpha,
      alpha_line = input$plot_2d_alpha_line,
      
      lm_all = input$plot_2d_lm_all,
      lm_by_group  = input$plot_2d_lm_by_group,
      marker_size = input$plot_2d_marker_size,
      
      jit = input$plot_2d_jitter,
      
      source = "plot_2d")
    
  })    
  
  selected_from_2d <- reactive({
    shiny::req(input$explo_analy_qtde_eixos == "bi")
    
    plotly::event_data("plotly_selected", source = "plot_2d")
  })
  
  # Plot Click 2d
  observeEvent(
    selected_from_2d(),
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      shiny::req(data$main)
      
      df <- data$main
      
      selected <- selected_from_2d()
      
      #browser()
      if (!is.null(selected)) {
        
        i <- (df$Elemento %in% selected$customdata)
        
      } else {
        
        i <- TRUE
        
      }
      
      selected_elments(i)
      
      
    })
  
  # AE - PLOT 3D - Cria Grafico ------------------------------------------------
  output$explo_plot_multi <- plotly::renderPlotly({
    
    shiny::req(data$main)
    shiny::req(isolate(input$explo_analy_qtde_eixos == "multi"))
    
    
    validate(need(input$AE_tri_x, "Especifique uma vari\u00E1vel para o eixo X"))
    validate(need(input$AE_tri_y, "Especifique uma vari\u00E1vel para o eixo Y"))
    validate(need(input$AE_tri_z, "Especifique uma vari\u00E1vel para o eixo Z"))
    validate(need(input$AE_tri_x_trns, "Especifique uma transformada para o eixo X"))
    validate(need(input$AE_tri_y_trns, "Especifique uma transformada para o eixo Y"))
    validate(need(input$AE_tri_z_trns, "Especifique uma transformada para o eixo Z"))
    
    
    plot3d(
      data$main,
      var_x = input$AE_tri_x, 
      var_y = input$AE_tri_y, 
      var_z = input$AE_tri_z, 
      
      var_x_trs = input$AE_tri_x_trns, 
      var_y_trs = input$AE_tri_y_trns, 
      var_z_trs = input$AE_tri_z_trns, 
      
      disabled = prop$obs_disabled_ae,
      show_disabled = input$plot_3d_show_disabled,
      
      cat = input$AE_tri_group,
      
      plan_hab = input$plot_2d_plan_hab,
      show_legend = input$plot_3d_show_legend,
      marker_size = input$plot_3d_marker_size,
      alpha = input$plot_3d_marker_alpha,
      alpha_plane = input$plot_3d_plan_alpha,
      jit = input$plot_3d_jitter,
      source = "plot_3d"
      
    )
    
  })
  
  
  selected_from_3d <- reactive({
    shiny::req(input$explo_analy_qtde_eixos == "bi")
    
    plotly::event_data("plotly_click", source = "plot_2d")
  })
  
  # Plot Click 2d
  observeEvent(
    selected_from_2d(),
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      shiny::req(data$main)
      
      df <- data$main
      
      selected <- selected_from_3d()
      
      
      if (!is.null(selected)) {
        
        i <- (df$Elemento %in% selected$customdata)
        
      } else {
        
        i <- TRUE
        
      }
      
      selected_elments(i)
      
    })
  
  
  
  
  # MO - PAINEL DE DADOS ----------------------------------------------------
  
  # ARGUMENTOS PARA O MODULO
  
  MO_obs_disabled_principal <- reactive({ prop$obs_disabled })
  MO_obs_disabled_secundary <- reactive({ prop$obs_disabled_ae })
  
  
  
  # EXECUCAO DO MODULO
  MO_enabling <- data_panel_SERVER(
    "MO_data_panel",
    non_spatial_data = df_non_spatial,
    obs_disabled_principal = MO_obs_disabled_principal,
    obs_disabled_secundary = MO_obs_disabled_secundary)
  
  # SALVAMENTO DAS INFORMACOES DO MODULO
  # base principal
  observeEvent(MO_enabling$principal(), ignoreInit = TRUE, {
    
    prop$obs_disabled <- MO_enabling$principal()
    
  })
  
  
  
  # base secundaria
  observeEvent(MO_enabling$secundary(), ignoreInit = TRUE, {
    
    #prop$obs_disabled_AE[] <- FALSE
    
    prop$obs_disabled_ae[] <- MO_enabling$secundary()
    
  })
  
  # Habilitacao proveninete da Engeharia de Dados ( Filtragem de Dados)
  
  observeEvent(input$ED_enable_all, {
    
    prop$obs_disabled[] <- FALSE
    
    
    shiny::showNotification(
      ui = "Todos os dados da Modelagem foram Habilitados",
      type = "message",
      duration = 2,
      closeButton = TRUE)
    
  })
  
  
  observeEvent(input$ED_disable_all, {
    
    prop$obs_disabled[] <- TRUE
    
    shiny::showNotification(
      ui = "Todos os dados da Modelagem foram Desabilitados",
      type = "message",
      duration = 2,
      closeButton = TRUE)
    
  })
  
  
  # Habilitacao proveninete da City Modelling (Filter sptail data)
  
  shiny::observeEvent(input$CM_enable_all, {
    
    prop$obs_disabled[] <- FALSE
    
    shiny::showNotification(
      ui = "Todos os dados da Modelagem foram Habilitados",
      type = "message",
      duration = 2,
      closeButton = TRUE)
    
  })
  
  
  shiny::observeEvent(input$CM_disable_all, {
    
    prop$obs_disabled[] <- TRUE
    
    shiny::showNotification(
      ui = "Todos os dados da Modelagem foram Desabilitados",
      type = "message",
      duration = 2,
      closeButton = TRUE)
    
  })
  
  
  
  
  
  
  # MO - PAINEL DE VARIAVEIS ------------------------------------------------
  # Inicia os menus de selecao de variaveis ao inserir ou trabalhar uma base
  # de dados
  shiny::observeEvent(
    data$main,
    ignoreNULL = TRUE,
    ignoreInit = FALSE, {
      
      df <- data$main %>% remove_geo()
      
      nms <- df %>% dplyr::select_if(is.numeric) %>% names()
      
      previous_selected <- prop$var_enabled[prop$var_enabled] %>% names()
      
      # atualizando variaveis habilitadas
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "model_hab_calc",
        choices = nms,
        selected = previous_selected)
      
      # atualizando variavel dependente
      if (is.na(prop$var_dependent)) {
        
        vd <- character(0)
        
      } else {
        
        vd <- prop$var_dependent
        
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "model_var_dep",
        choices = previous_selected,
        selected = vd)
      
    })
  
  # Quando a lista de habilitadas se altera, as opcoes para variavel
  # dependente se altera. Mantendo  selecionada a var previamente escolhida.
  # Alem disso, salva as var habilitadas na lista de propriedades
  shiny::observeEvent(
    input$model_hab_calc,
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      
      
      # atualizando a var dep
      sel <- input$model_var_dep
      
      if (is.null(input$model_hab_calc)) {
        
        choi <- character(0)
        
      } else {
        
        choi <- input$model_hab_calc
        
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "model_var_dep",
        choices = choi,
        selected = sel)
      
      # salvando as var habilitadas na lista de propriedades
      prop$var_enabled[] <- FALSE
      prop$var_enabled[input$model_hab_calc] <- TRUE
      
      
    })
  
  # Quando a var dep \u00e9 alterada em seu menu, salva nas propriedades a var
  # selecionada
  shiny::observeEvent(
    input$model_var_dep,
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      
      shiny::req(data$main)
      
      if(shiny::isTruthy(input$model_var_dep)) {
        
        prop$var_dependent <- input$model_var_dep
        
      } else {
        
        prop$var_dependent <- NA_character_
        
      }
      
    })
  
  # Cria os menus iterativos de selecao das transformadas
  output$var_transf <- shiny::renderUI({
    
    if (is.null(input$model_hab_calc)) { return() }
    
    vars <- input$model_hab_calc
    
    list(
      shiny::hr(),
      shiny::hr(),
      #shiny::fluidRow(class = "text-center",shiny::h4("Transformadas")),
      
      lapply(vars, function(x){
        
        name_id <- x %>% make_shiny_id() %>% paste0("_trns")
        
        MO_picker_var(
          input_id = name_id, 
          paste0(x, ":"),
          maxOpt = 1,
          choices = prop$var_trns_possible[[x]],
          selected = prop$var_trns_selected[[x]])
        
      }))
  })
  
  # Cria os observers de resposta aos menus de selecao de transformadas que
  # salva nas propriedades
  shiny::observeEvent(
    input$model_hab_calc,
    ignoreNULL = FALSE,
    ignoreInit = TRUE, {
      
      vars <- input$model_hab_calc
      
      lapply(vars, function(x) {
        
        name_id <- x %>% make_shiny_id() %>% paste0("_trns")
        
        if (!is.null(input[[name_id]])) return()
        
        observeEvent(
          input[[name_id]],
          ignoreInit = TRUE,
          ignoreNULL = TRUE, {
            
            prop$var_trns_selected[[x]] <- input[[name_id]]
            
          }) #fim do Observe Event
        
      }) # Fim do lapply
      
    })
  
  
  # MO - Busca de Vari\u00e1veis -------------------------------------------------
  
  output$MO_trns_search_vars <- shiny::renderUI({
    
    if (is.null(input$model_hab_calc)) { return() }
    
    vars <- input$model_hab_calc
    
    list(
      shiny::hr(),
      shiny::hr(),
      #shiny::fluidRow(class = "text-center",shiny::h4("Transformadas")),
      
      lapply(vars, function(x) {
        
        
        previous_selected <- prop$var_trns_for_search[[x]]
        # previous_selected <-  if (is.null(previous_selected)) "none"
        
        MO_picker_var(
          input_id = paste0(x, "_trns_for_search"), 
          label = paste0(x, ":"),
          maxOpt = NULL,
          choices = prop$var_trns_possible[[x]],
          selected = previous_selected,
          actionsBox = TRUE,
          deselectAllText = "Nenhuma",
          selectAllText = "Todas"
        ) %>% shiny::column(width = 6, .)
        
      }))
  })
  
  
  shiny::observeEvent(input$MO_trns_for_search_save, {
    
    var_enabled <- input$model_hab_calc
    var_dep <- input$model_var_dep
    
    # salva os valores na lista de prop for search
    for (i in var_enabled) {
      
      prop$var_trns_for_search[[i]] <- input[[paste0(i, "_trns_for_search")]]
      
    } # Fim do lapply
    
    
  })
  
  
  
  
  
  #Obtem os valores nas lista de input
  df_prepared_many_models <- shiny::eventReactive(
    input$MO_trns_for_search_go, {
      
      # observeEvent(input$MO_trns_for_search_go, {
      
      var_enabled <- input$model_hab_calc
      var_dep <- input$model_var_dep
      
      # salva os valores na lista de prop for search
      for (i in var_enabled) {
        
        prop$var_trns_for_search[[i]] <- input[[paste0(i, "_trns_for_search")]]
        
      } # Fim do lapply
      
      #pega os valores salvos
      transf_for_test <- prop$var_trns_for_search[var_enabled]
      
      # mensagens ao usuario
      id <- shiny::showNotification(
        ui = "Preparando dados, Aguarde!",
        type = "message",
        duration = 2,
        closeButton = TRUE)
      
      shinyjs::disable("MO_trns_for_search_go")
      
      on.exit(removeNotification(id), add = TRUE)
      on.exit(shinyjs::enable("MO_trns_for_search_go"), add = TRUE)
      
      #calculos de fato
      data$main %>%
        
        check_data_conditions(session = session,
                              obs_disabled = prop$obs_disabled,
                              var_enabled = var_enabled,
                              var_dep = var_dep,
                              transf_for_test) %>%
        
        filter_data_model(prop$obs_disabled,
                          var_enabled = var_enabled) %>%
        
        check_data_na(session) %>% as.matrix()
      
    })
  
  
  #numero de combinacoes possiveis
  n_comb <- shiny::reactive({
    
    var_enabled <- input$model_hab_calc
    
    transf_for_test <- prop$var_trns_for_search[var_enabled]
    shiny::req(transf_for_test)
    
    
    lapply(transf_for_test, length) %>% 
      unlist() %>% 
      prod() %>% 
      prettyNum(big.mark = ".", decimal.mark = ",")
    
  })
  
  output$ncomb <- shiny::renderText({
    
    paste(n_comb(), "combina\u00e7\u00f5es a serem executadas")
    
  })
  
  output$tempo_estimado_calc <-  shiny::renderText({
    input$MO_trns_for_search_save
    
    
    
    var_enabled <- input$model_hab_calc
    transf_for_test <- prop$var_trns_for_search[var_enabled]
    shiny::req(transf_for_test)
    df_select() %>% shiny::req()
    isolate({
      p <- bench::mark(a ={
        faster_reg(df_select() %>% as.matrix(), 
                   var_dep = prop$var_dependent)
        
      }, iterations = 100)
      
      tx <- p$`itr/sec`
      m <- p$median
      
      n_comb <- lapply(transf_for_test, length) %>% 
        unlist() %>% 
        prod()
      
      time <- (n_comb / tx) %>% formatC2()
      
      paste("Tempo estimado de c\u00e1lculo:", time, "segundos")
      
    })
  })
  
  
  
  
  
  many_models <- shiny::eventReactive(df_prepared_many_models(), {
    shiny::req(df_prepared_many_models())
    #browser()
    
    
    # 1. Buscar os valores de referencia definidos pelo usu\u00e1rio
    var_enabled <- input$model_hab_calc
    var_dep <- input$model_var_dep
    transf_for_test <- prop$var_trns_for_search[var_enabled]
    shiny::req( !is.null(unlist(transf_for_test)) )
    
    
    # 1.1 Informacoes ao Usuario
    # numero de combinacoes possiveis
    n_comb <- n_comb()
    
    #mensagens aos usuarios
    id <- shiny::showNotification(
      ui = paste0("Calculando ",n_comb ," modelos, Aguarde!"),
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    shinyjs::disable("MO_trns_for_search_go")
    
    on.exit(removeNotification(id), add = TRUE)
    on.exit(shinyjs::enable("MO_trns_for_search_go"), add = TRUE)
    
    #barra tela para calculo
    waiter <- waiter::Waiter$new(
      color = grDevices::rgb(0, 0, 0, .3),
      html = waiter::spin_wave())
    waiter$show()
    on.exit(waiter$hide(), add = TRUE)
    
    
    # 2. Preparar a matriz de dados para o calculo
    
    ## converter em matrix
    mtz <- df_prepared_many_models() 
    
    # 3. Todas as combina\u00e7\u00f5es poss\u00edveis de vari\u00e1veis
    #all_combinations <- expand.grid(transf_for_test, stringsAsFactors = FALSE) %>% as.matrix()
    all_combinations <- create_possibilities(transf_for_test, n_comb(), session)
    
    # 4. Criar o loop no qual: para cada linha da matriz all_combinations,
    # far-se-\u00e1 a transformacao dos dadose depois a regressao desses dados
    # regredidos
    
    seq_loop <- (seq_len(NROW(all_combinations)))
    n_X_m <- dim(mtz) #numero de linhas e numero de colunas
    n_obs <- n_X_m[1] #numero de linhas
    n_var <- n_X_m[2] #numero de colunas
    nms <- colnames(mtz) #nome das colunas
    
    #numero de colunas da matriz + 6 resultados obtidos
    saida <- NCOL(mtz) + 6
    
    
    #if (!parallel_computing) {
    
    
    # system.time({
    #   metricas <- vapply(X = seq_loop,
    #                      #MARGIN = 1,
    #                      FUN =  regression_loop,
    #                      FUN.VALUE = character(saida),
    #                      mtz,
    #                      var_dep,
    #                      n_obs,
    #                      n_var,
    #                      nms,
    #                      all_combinations) %>% t()
    # 
    # 
    # 
    # })
    
    
    metricas <-  vapply(X = seq_loop, 
                        FUN =  regression_loop, 
                        FUN.VALUE = character(saida),
                        mtz, 
                        var_dep, 
                        n_obs, 
                        n_var, 
                        nms, 
                        all_combinations) %>% t() 
    
    
    
    
    
    # } else {
    #   
    # system.time({
    # # computacao paralela
    # fn <- function(x) {
    # 
    #   combinations <- all_combinations[x, , drop = FALSE]
    #   seq_loop <- seq_len(NROW(combinations))
    # 
    #   n_X_m <- dim(mtz) #numero de linhas e numero de colunas
    #   n_obs <- n_X_m[1] #numero de linhas
    #   n_var <- n_X_m[2] #numero de colunas
    #   nms <- colnames(mtz) #nome das colunas
    # 
    # 
    #   r <- vapply(X = seq_loop,
    #          # MARGIN = 1,
    #          FUN =  regression_loop,
    #          FUN.VALUE = character(saida),
    #          mtz,
    #          var_dep,
    #          n_obs,
    #          n_var,
    #          nms,
    #          combinations)  
    #   
    #   t(r)
    # 
    # }
    # 
    # 
    # 
    # ## detectar a quantidade de nucleos:
    # n_core <- parallel::detectCores(logical = TRUE)
    # 
    # ## criar cluster em cada nucleo
    # clusters <- parallel::makeCluster(n_core, type = "PSOCK", outfile="")
    # 
    # ## envia os objetos a serem utilizadas em cada cluster
    # 
    # 
    # # variaveis do ambiente global
    # parallel::clusterExport(clusters, c("regression_loop",
    #                                     "lista_transf",
    #                                     "anti_transf",
    #                                     "r2",
    #                                     "adj_r2"
    # 
    # ) )
    # # vari
    # parallel::clusterExport(clusters, c("n_var",
    #                                     "n_obs",
    #                                     "nms",
    #                                     "var_dep",
    #                                     "all_combinations",
    #                                     "saida",
    #                                     "fn"), envir = environment())
    # 
    # 
    # 
    # parallel::clusterEvalQ(clusters, { library(dplyr) })
    # 
    # indices <- parallel::splitIndices(NROW(all_combinations), n_core)
    # 
    # resultados <- clusterApply(clusters, x =  indices, fun =  fn)
    # 
    # metricas <- resultados %>% do.call(rbind, .)
    # 
    # stopCluster(clusters)
    # # fim computacao paralela
    # 
    # })
    #   
    # }
    
    metricas <- cbind(Modelo = seq_len(dim(all_combinations)[1]), metricas)
    metricas 
    
  })
  
  
  many_models_for_dt <- shiny::reactive( { 
    shiny::req(many_models())
    
    
    var_enabled <- input$model_hab_calc
    mtz <- many_models()
    
    shiny::req( all(var_enabled %in% colnames(many_models()))  )
    
    format_result_matrix(mtz, var_enabled, rename_prediction = FALSE)
    
    
  })
  
  
  data_choose_model <- shiny::eventReactive(many_models_for_dt(), {
    
    mtz <- many_models_for_dt()
    
    DT::datatable(mtz,
                  options = list(
                    columnDefs = list(
                      list(className = 'dt-center', targets = "_all")
                    ),
                    lengthMenu = list(c( 5, 10, 25, 50, 100, -1),
                                      c( "5", "10", "25" , "50", "100", "Todos")),
                    searching = T,
                    dom = "liftp", #dom = "liftp",
                    scrollX = TRUE,
                    scrollY = TRUE,
                    paging = TRUE,
                    lengthMenu = FALSE,
                    #pageLength = 5,
                    autoWidth = FALSE
                  ),
                  class = "display",
                  callback = DT::JS("return table;"),
                  rownames = FALSE,
                  filter = "top",
                  selection = "single",
                  caption = "Acaso alguma vari\u00e1vel ou dado seja habilitado/desabilitado, refa\u00e7a a pesquisa"
                  
    ) %>%
      
      DT::formatSignif(
        c("R\u00B2 Mod",
          "R\u00B2 Adj Mod",
          "Correla\u00e7\u00e3o Mod",
          "R\u00B2 Est",
          "R\u00B2 Adj Est",
          "Correla\u00e7\u00e3o Est"),
        digits = 4,
        dec.mark = ",")
    
  })
  
  
  
  # data table visualizacao no painel de modelagem
  output$df_transf_choose <- DT::renderDataTable({
    # shiny::req(many_models())
    
    data_choose_model()
    
  }, server = TRUE)
  
  # data table de visualizcao no Painel de Variaveis
  output$df_transf_choose2 <- DT::renderDataTable({
    shiny::req(data_choose_model())
    
    data_choose_model()
    
  }, server = TRUE)
  
  
  # data table de visualizcao no paienl de Estimativas
  output$df_transf_choose3 <- DT::renderDataTable({
    shiny::req(data_choose_model())
    
    data_choose_model()
    
  }, server = TRUE)
  
  #indice compartilhado pelos dois data table acima
  shared_index <- shiny::reactiveVal()
  
  # atualiza o indice vindo do data table do paienl de modelagem
  shiny::observe({
    
    input$df_transf_choose_rows_selected %>% shared_index()
    
  })
  
  
  # atualiza o indice vindo do data table do painel de variaveis
  shiny::observe({
    
    input$df_transf_choose2_rows_selected %>% shared_index()
    
  })
  
  
  # atualiza o indice vindo do data.table do Painel de Estimativas
  shiny::observe({
    
    input$df_transf_choose3_rows_selected %>% shared_index()
    
  })
  
  
  shiny::observeEvent(shared_index(), {
    
    i <- shared_index()
    var_enabled <- input$model_hab_calc
    
    final_col <- (NCOL(many_models()) - 6) %>% seq_len() + 1
    
    transformadas <- many_models()[i, var_enabled ]
    
    lapply(names(transformadas), function(i) {
      
      name_id <- i %>% make_shiny_id() %>% paste0("_trns")
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = name_id,
        selected = transformadas[[i]][[1]]
        
      )
    })
    
  })
  
  # MO - CRIAR MODELO -------------------------------------------------------
  
  # primeiro prepara o DF que sera regredido
  
  df_select <- shiny::reactive({
    shiny::req(prop$var_enabled)
    shiny::req(prop$var_dependent)
    
    
    obs_disabled <- prop$obs_disabled
    var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
    var_dep <- prop$var_dependent
    transf_for_test <- prop$var_trns_selected
    
    # calculos de fato
    data$main %>% 
      
      check_data_conditions(session = session, 
                            obs_disabled = obs_disabled, 
                            var_enabled = var_enabled, 
                            var_dep = var_dep,
                            transf_for_test = NULL) %>% 
      
      filter_data_model(obs_disabled = obs_disabled,
                        var_enabled = var_enabled) %>%  
      
      check_data_na(session = session) 
    
    
  })
  
  
  df_prepared <- shiny::reactive({
    shiny::req(prop$var_enabled)
    shiny::req(prop$var_dependent)
    
    df_select() %>% transform_data_model(prop$var_trns_selected, .) 
    
  })
  
  model <- shiny::eventReactive(df_prepared(), {
    
    var_dep <- prop$var_dependent
    
    df_prepared() %>% create_model(var_dep)
    
  })
  
  
  # MO - METRICAS MODELO ----------------------------------------------------
  
  
  var_dep_and_residuals <- shiny::reactive({
    shiny::req(model())
    
    get_residuals(modelo = model(), 
                  df_ready = df_prepared(),
                  df_raw =  data$main,
                  prop =  prop)
    
  })
  
  
  
  
  
  model_summary <- shiny::reactive({
    shiny::req(model())
    
    stats::summary.lm(model())
    
  })
  
  
  coefs_r <- shiny::reactive({
    
    mat <- var_dep_and_residuals()
    
    r2_natural_scale <- r2(obs = mat[ ,"Var. Dep. Obs. Estimativa", drop = TRUE], 
                           res = mat[ ,"Res\u00edduos Estimativa", drop = TRUE])
    
    r2_adj_natural_scale <- adj_r2(r2 = r2_natural_scale, 
                                   n_obs = NROW(df_prepared()), 
                                   n_var = NCOL(df_prepared()))
    
    
    model_summary <- model_summary()
    
    c(
      r_trns_scale = ((model_summary$r.squared)^2)^(1/4), 
      r2_trns_scale = model_summary$r.squared, 
      r2_adj_trns_scale = model_summary$adj.r.squared,
      
      r_natural_scale = sign(r2_natural_scale) * (r2_natural_scale^2)^(1/4),
      r2_natural_scale = r2_natural_scale,
      r2_adj_natural_scale = r2_adj_natural_scale
    )
    
    
  })
  
  
  
  correlation <- shiny::reactive({
    shiny::req(df_prepared())
    
    get_correlation(df_prepared(), prop$var_dependent, "only_indep")
    
    # matrix_cor_max <- matrix_cor2[lower.tri(matrix_cor2)] %>%
    #   abs() %>%
    #   max %>%
    #   round(digits = 4)
    
  })
  
  
  
  
  
  n_dados_considerados <- shiny::reactive({
    shiny::req(model())
    
    sum(!prop$obs_disabled)
    
  })
  
  n_var_consideradas <- shiny::reactive({
    shiny::req(model())
    
    sum(prop$var_enabled)
    
  })
  
  n_var_total <- shiny::reactive({
    shiny::req(data$main)
    
    data$main %>% 
      remove_geo() %>% 
      remove_key_column() %>% 
      length()
    
  })
  
  
  n_graus_liberdade <- shiny::reactive({
    shiny::req(model())
    
    model()$df.residual
    
  })
  
  
  f_calc <- shiny::reactive({
    shiny::req(model())
    
    model_summary()$fstatistic[1]
    
    
  })
  
  f_p_value <- shiny::reactive({
    shiny::req(model())
    
    f <- model_summary()$fstatistic
    
    p <- stats::pf(f[1], f[2], f[3], lower.tail = F)
    
    unname(p)
    
  })
  
  residuals_sd_modelagem <- shiny::reactive({
    shiny::req(model())
    
    model_summary()$sigma
    
  })
  
  residuals_sd_estimativa <- shiny::reactive({
    shiny::req(model())
    
    var_dep_and_residuals()[ ,"Res\u00edduos Estimativa"] %>% stats::sd()
    
  })
  
  dist_cook <- shiny::reactive({
    shiny::req(model())
    
    matrix(
      c(var_dep_and_residuals()[, "Elemento"], 
        stats::cooks.distance(model())), ncol = 2) %>% `colnames<-`(c("Elemento", "cook"))
    
  })
  
  
  # MO - GRAFICO METRICAS GERAIS --------------------------------------------
  
  output$some_metrics <- shiny::renderUI({
    
    tagList(
      
      column(
        width = 3,
        
        shinydashboardPlus::descriptionBlock(
          number = "Var\u00e1veis Consideradas", 
          number_color = "blue", 
          #number_icon = "fa fa-caret-down",
          header = n_var_consideradas(), 
          text =  paste("/", n_var_total()), 
          right_border = TRUE,
          margin_bottom = TRUE
        )
      ),
      
      column(
        width = 3,
        
        shinydashboardPlus::descriptionBlock(
          number = "Dados Considerados", 
          number_color = "blue", 
          #number_icon = "fa fa-caret-down",
          header = n_dados_considerados(), 
          text = paste("/", NROW(data$main)), 
          right_border = TRUE,
          margin_bottom = TRUE
        )
      ),
      
      column(
        width = 3,
        
        shinydashboardPlus::descriptionBlock(
          number = "F-Snedecor" , 
          number_color = "blue", 
          #number_icon = "fa fa-caret-down",
          header = paste0("Sig: ", f_p_value() %>% round(3), "%"), 
          text =  paste0("F: ", f_calc() %>% round(3)), 
          right_border = TRUE,
          margin_bottom = TRUE
        )
      ),
      
      column(
        width = 3,
        
        shinydashboardPlus::descriptionBlock(
          number = "Desvio padr\u00e3o", 
          number_color = "blue", 
          #number_icon = "fa fa-caret-down",
          header = paste("Mod.", residuals_sd_modelagem() %>% round(3)), 
          text = paste("Estim.", residuals_sd_estimativa() %>% round(3)), 
          right_border = FALSE,
          margin_bottom = TRUE
        )
        
      )
    )
  })
  
  
  
  
  output$dados_utilizados <- shinydashboard::renderValueBox({
    shiny::req(model())
    
    n <- n_dados_considerados()
    
    shinydashboard::valueBox(
      value = n,
      subtitle = paste("Dados Utilizados de", NROW(data$main)),
      icon = shiny::icon("database"),
      #color = "yellow",
      width = NULL
    )
    
  })
  
  output$var_utilizadas <- shinydashboard::renderValueBox({
    shiny::req(model())
    
    shinydashboard::valueBox(
      value = n_var_consideradas(),
      subtitle = "Vari\u00E1veis Utilizadas",
      icon = shiny::icon("list"),
      #color = "purple",
      width = NULL
    )
    
  })
  
  output$graus_liberdade <- shinydashboard::renderValueBox({
    shiny::req(model())
    
    shinydashboard::valueBox(
      value = model()$df.residual,
      subtitle = "Graus de Liberdade",
      icon = shiny::icon("info"),
      #color = "yellow",
      width = NULL
    )
    
  })
  
  output$f_valor <- shinydashboard::renderInfoBox({
    shiny::req(model())
    
    p_valor_f <- f_p_value() %>% signif(4)
    
    shinydashboard::infoBox(
      "F-Snedecor",
      value = p_valor_f,
      icon = shiny::icon("tachometer-alt") )
    
  })
  
  
  
  
  output$r_trns <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r_trns_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Correla\u00E7\u00E3o (R)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
  })
  output$r_natural <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r_natural_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Correla\u00E7\u00E3o (R)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
  })
  
  
  output$r2_trns_scale <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r2_trns_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Determina\u00E7\u00E3o (R\u00B2)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
  })
  
  output$r2_natural_scale <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r2_natural_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Determina\u00E7\u00E3o (R\u00B2)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
  })
  
  
  
  
  output$r2_adj_trns_scale  <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r2_adj_trns_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Det. Ajustado (R\u00B2 adj)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
    
  })
  
  output$r2_adj_natural_scale <- flexdashboard::renderGauge({
    shiny::req(model())
    
    flexdashboard::gauge(
      value = round(coefs_r()["r2_adj_natural_scale"] %>% unname(), 4),
      min = 0,
      max = 1,
      label = "Det. Ajustado (R\u00B2 adj)",
      sectors = flexdashboard::gaugeSectors(
        success = c(0.6, 1),
        warning = c(0.4, 0.6),
        danger = c(0, 0.4)
      )
    )
  })
  
  
  
  
  output$indices_maximos <- plotly::renderPlotly({
    shiny::req(model())
    
    matrix_cor_max <- correlation()[lower.tri(correlation())] %>%
      abs() %>%
      max() %>%
      round(digits = 4)
    
    polar_metrics(
      cor_max =  matrix_cor_max * 100,
      sig_max = max(model_summary()$coefficients[, 4, drop = TRUE], na.rm = TRUE) * 100,
      cook_max = dist_cook()[, 2, drop = TRUE] %>% max() * 100, 
      res_max_modelo = var_dep_and_residuals()[ ,"Res\u00edduos Relativos Modelagem", drop = TRUE] %>% abs() %>%  max() * 100,
      res_max_escala_invertida = var_dep_and_residuals()[ ,"Res\u00edduos Relativos Estimativa", drop = TRUE] %>% abs() %>% max() * 100
    )
    
    
  })
  
  
  # PN - Painel de Norma ----------------------------------------------------
  
  # PN - Residuos -----------------------------------------------------------
  
  # PN - Histograma de Res\u00edduos ---------------------------------------------
  
  # Histograma Residuos Padronizados
  output$pn_res_hist_mod <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_hist_mod)
    
    plot_residuals_hist(var_dep_and_residuals(), 
                        "modelling", 
                        plot_1d_histnorm(), 
                        input$plot_1d_cumalative,
                        input$plot_1d_nbinsx, 
                        input$plot_1d_alpha,
                        input$plot_1d_show_legend)
    
  })
  
  
  output$pn_res_hist_est <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_hist_est)
    
    plot_residuals_hist(var_dep_and_residuals(),
                        "estimate",
                        plot_1d_histnorm(),
                        input$plot_1d_cumalative,
                        input$plot_1d_nbinsx,
                        input$plot_1d_alpha,
                        input$plot_1d_show_legend)
    
  })
  
  
  
  # PN - QQPLOT -------------------------------------------------------------
  
  
  # QQ Plot
  output$pn_res_qqplot_mod <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_qqplot_mod)
    
    plot_residuals_qqplot(var_dep_and_residuals(), 
                          "modelling", 
                          point_alpha = input$plot_2d_alpha, 
                          point_size = input$plot_2d_marker_size,
                          alpha_line = input$plot_2d_alpha_line,
                          jit = input$plot_2d_jitter)
    
  })
  output$pn_res_qqplot_est <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_qqplot_est)
    
    plot_residuals_qqplot(var_dep_and_residuals(),
                          "estimate", 
                          point_alpha = input$plot_2d_alpha, 
                          point_size = input$plot_2d_marker_size,
                          alpha_line = input$plot_2d_alpha_line,
                          jit = input$plot_2d_jitter)
    
  })
  
  
  
  
  # PN - Percentuais Teoricos -----------------------------------------------
  
  # Percentuais Teoricos
  output$pn_res_perc_mod <- DT::renderDataTable({
    shiny::req(model())
    shiny::req(input$pn_check_res_perc_mod)
    
    residuals_theoretical(var_dep_and_residuals(), "modelling") %>% 
      
      
      data_table_preview() %>%
      
      DT::formatPercentage(
        c("Observado", 
          "Te\u00f3rico"),
        dec.mark = ",",
        mark = ".",
        digits = input$config_decimal_digits)
    
  })
  output$pn_res_perc_est <- DT::renderDataTable({
    shiny::req(model())
    shiny::req(input$pn_check_res_perc_est)
    
    residuals_theoretical(var_dep_and_residuals(), "estimate") %>% 
      
      data_table_preview() %>%
      
      DT::formatPercentage(
        c("Observado", 
          "Te\u00f3rico"),
        dec.mark = ",",
        mark = ".",
        digits = input$config_decimal_digits)
    
  })
  
  
  
  # PN - Residuos Padronizados X Valores Calculados -------------------------
  
  # Residuos Padronizados por Valor Calculado
  output$pn_res_resP_Vcal_mod <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_padro_mod)
    
    plot_residuals_graph(
      var_dep_and_residuals(), 
      "modelling", 
      prop,
      point_alpha = input$plot_2d_alpha,
      point_size = input$plot_2d_marker_size,
      show_hist = input$pn_check_hist_padro_mod,
      nbinsx = input$plot_1d_nbinsx,
      histnorm  = plot_1d_histnorm(),
      alpha = input$plot_1d_alpha,
      cumula = input$plot_1d_cumalative
    )
    
    
  })
  output$pn_res_resP_Vcal_est <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_padro_est)
    
    plot_residuals_graph(
      var_dep_and_residuals(), 
      "estimate", 
      prop,
      point_alpha = input$plot_2d_alpha,
      point_size = input$plot_2d_marker_size,
      show_hist = input$pn_check_hist_padro_est,
      nbinsx = input$plot_1d_nbinsx,
      histnorm  = plot_1d_histnorm(),
      alpha = input$plot_1d_alpha,
      cumula = input$plot_1d_cumalative)
    
  })
  
  
  # Residuos Padronizados por Variavel Idependente
  
  observeEvent(model(), {
    shiny::req(model())
    
    var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
    var_dep <- prop$var_dependent
    
    var_indep <- setdiff(var_enabled, var_dep)
    
    shiny::updateSelectInput(
      session, 
      "pn_check_res_padro_var_indep_select_mod", 
      choices = var_indep)
    
    
    shiny::updateSelectInput(
      session, 
      "pn_check_res_padro_var_indep_select_est", 
      choices = var_indep)
    
  })  
  
  # Residuos Padronizados por Valor Calculado
  output$pn_res_resP_var_indep_mod <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_padro_var_indep_mod)
    
    plot_residuals_graph_indep(
      model(),
      var_dep_and_residuals(), 
      "modelling", 
      prop, 
      input$pn_check_res_padro_var_indep_select_mod, 
      df_select(),
      point_alpha = input$plot_2d_alpha,
      point_size = input$plot_2d_marker_size,
      show_hist = input$pn_check_hist_mod,
      nbinsx = input$plot_1d_nbinsx,
      histnorm  = plot_1d_histnorm(),
      alpha = input$plot_1d_alpha,
      cumula = input$plot_1d_cumalative
    )
    
    
  })
  output$pn_res_resP_var_indep_est <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_res_padro_var_indep_est)
    
    plot_residuals_graph_indep(
      model(),
      var_dep_and_residuals(), 
      "estimate", 
      prop,
      input$pn_check_res_padro_var_indep_select_est, 
      df_select(),
      point_alpha = input$plot_2d_alpha,
      point_size = input$plot_2d_marker_size,
      show_hist = input$pn_check_hist_est,
      nbinsx = input$plot_1d_nbinsx,
      histnorm  = plot_1d_histnorm(),
      alpha = input$plot_1d_alpha,
      cumula = input$plot_1d_cumalative)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  # PN - Mapa de Res\u00edduos  --------------------------------------------------
  
  # Esses elementos sao criados para alimentar o Mapa dos Residuos da Modelagem
  # e da Estimativa
  
  # formatacao dos residuos para exibicao no popup
  var_dep_and_residuals_ftmd <- shiny::reactive({
    shiny::req(var_dep_and_residuals())
    
    
    data_res <- var_dep_and_residuals()
    
    
    Elemento <- data_res[, 1, drop = FALSE] %>% as.character()
    
    data_res <- data_res[, -1, drop = FALSE] %>% 
      
      apply(c(1, 2), formatC2) %>% 
      
      cbind(Elemento, .) #%>% 
    
    # apply(
    #   MARGIN = 2, 
    #   FUN = parse_number, 
    #   locale = locale(decimal_mark = ",", 
    #                   grouping_mark = ".")
    # )
    
    data_res
    
  })
  
  
  # Vinculacao dos Residuos aos seus elementos Espaciais. Se a base nao for
  # espacializada, isso aqui nao \u00e9 criado. 
  
  # \u00c9 apenas um data.frame que vincula o elemento espacial aos seus respectivos
  # residuos da regressao
  
  spatial_residuals <- shiny::reactive({
    shiny::req(spatial_data_jit())
    shiny::req(var_dep_and_residuals())
    
    var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
    var_dep <- prop$var_dependent
    var_indep <- setdiff(var_enabled, var_dep)
    
    data_geo <- spatial_data_jit()[, c("Elemento", var_indep), drop = FALSE]
    
    dplyr::inner_join(
      data_geo, 
      
      var_dep_and_residuals() %>% 
        dplyr::as_tibble() %>% 
        dplyr::mutate_at("Elemento", as.character), 
      
      by = "Elemento")
    
    
  })
  
  
  
  # PN - Mapa de Residuos Modelagem -----------------------------------------
  
  # Criacao do Mapa de Res\u00edduos da Modelagem Espacializados
  output$pn_res_geo_mod <- leaflet::renderLeaflet({
    
    #chave_map_res_mod() 
    shiny::req(input$pn_check_res_map_mod)
    
    
    isolate({
      
      shiny::req(model())
      shiny::req(spatial_residuals())
      
      spatial_residuals() %>% 
        
        city_map() %>%
        
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", "Toner Lite", "Satelite", "Escuro"),
          #overlayGroups = legenda,
          options = leaflet::layersControlOptions(
            collapsed = TRUE,
            position = "bottomright")
        ) 
    })
    
  })
  
  # Criacao da Proxy dos Res\u00edduos Espacializados da Modelagem
  proxy_pn_res_geo_mod <- leaflet::leafletProxy("pn_res_geo_mod")
  
  
  observe({
    shiny::req(input$pn_check_res_map_mod)
    shiny::req(model())
    
    proxy_pn_res_geo_mod %>% 
      
      city_map_residuals(
        prop = prop, 
        analysis_type = "modelling", #input
        spatial_residuals = spatial_residuals(),
        residuos_formatados = var_dep_and_residuals_ftmd(), 
        grandeza = input$pn_map_residuals_select_mod,
        opacity_border = input$config_mapa_point_opacity_border,
        opacity_fill = input$config_mapa_point_opacity_inside,
        size = input$config_mapa_point_radius,
        bins = input$config_mapa_point_color_bins 
      )
  })
  
  
  
  
  
  # PN - Mapa de Residuos Estimativa ----------------------------------------
  
  
  
  # Criacao do Mapa de Res\u00edduos da Modelagem Espacializados
  output$pn_res_geo_est <- leaflet::renderLeaflet({
    
    shiny::req(input$pn_check_res_map_est)
    
    isolate({
      
      shiny::req(model())
      shiny::req(spatial_residuals())
      
      
      spatial_residuals() %>% 
        
        city_map() %>%
        
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", "Toner Lite", "Satelite", "Escuro"),
          #overlayGroups = legenda,
          options = leaflet::layersControlOptions(
            collapsed = TRUE,
            position = "bottomright")
        ) 
    })
    
  })
  
  
  
  # Criacao da Proxy dos Res\u00edduos Espacializados da Modelagem
  proxy_pn_res_geo_est <- leaflet::leafletProxy("pn_res_geo_est")
  
  
  observe({
    shiny::req(input$pn_check_res_map_est)
    
    proxy_pn_res_geo_est %>% 
      
      city_map_residuals(
        prop = prop, 
        analysis_type = "estimate", #input
        spatial_residuals = spatial_residuals(),
        residuos_formatados = var_dep_and_residuals_ftmd(), 
        grandeza = input$pn_map_residuals_select_est,
        opacity_border = input$config_mapa_point_opacity_border,
        opacity_fill = input$config_mapa_point_opacity_inside,
        size = input$config_mapa_point_radius,
        bins = input$config_mapa_point_color_bins 
      )
  })
  
  
  
  # PN - Distancia de Cook --------------------------------------------------
  
  output$dist_cook_out  <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_cook_dist)
    
    cook_graph(dist_cook(), input$pn_cook_dist_N)
    
  })
  
  
  # PN - Tabela de Res\u00edduos Padronizados ------------------------------------
  
  output$var_dep_and_residuals_out <- DT::renderDataTable({
    shiny::req(var_dep_and_residuals())
    shiny::req(input$pn_check_res_table)
    
    
    var_dep_and_residuals() %>% 
      
      dplyr::as_tibble() %>% 
      
      dplyr::mutate_at("Elemento", as.factor) %>% 
      
      DT::datatable(
        .,
        extensions = 'Buttons',
        options = list(
          
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
            
          ),
          lengthMenu = list(c( 5, 10, 25, 50, 100, -1),
                            c( "5", "10", "25" , "50", "100", "Todos")),
          searching = T,
          dom = "liftBp", #dom = "liftp",
          scrollX = TRUE,
          scrollY = TRUE,
          paging = TRUE,
          lengthMenu = FALSE,
          # pageLength = 5,
          autoWidth = FALSE,
          
          buttons = list(
            'copy', 
            list(
              extend = 'collection',
              buttons = c('csv', 'excel'),
              text = 'Download'
            )
          )
          
        ),
        class = "display",
        callback = DT::JS("return table;"),
        rownames = FALSE,
        filter = "top",
        selection = "multiple"
        
      ) %>%
      
      DT::formatPercentage(
        c("Res\u00edduos Relativos Modelagem", 
          "Res\u00edduos Relativos Estimativa"),
        dec.mark = ",",
        mark = ".",
        digits = input$config_decimal_digits) %>%
      
      DT::formatRound(
        c( 
          "Var. Dep. Obs. Estimativa", 
          "Var. Dep. Obs. Trns.", 
          "Var. Dep. Calc. Trns.",
          "Var. Dep. Calc. Estimativa", 
          "Res\u00edduos Modelagem", 
          "Res\u00edduos Estimativa", 
          "Res\u00edduos Padronizados Modelagem", 
          "Res\u00edduos Padronizados Estimativa"
        ),
        dec.mark = ",",
        mark = ".",
        digits = input$config_decimal_digits)
    
    
    # DT::formatSignif(
    #   c("R\u00B2 Mod",
    #     "R\u00B2 Adj Mod",
    #     "Correla\u00e7\u00e3o Mod",
    #     "R\u00B2 Est",
    #     "R\u00B2 Adj Est",
    #     "Correla\u00e7\u00e3o Est"),
    #   digits = 4,
    #   dec.mark = ",")
    
  })
  
  output$pn_var_dep_and_residuals_out <- shiny::renderUI({
    
    shiny::req(input$pn_check_res_table)
    
    
    shiny::tagList(
      
      DT::dataTableOutput("var_dep_and_residuals_out")
      
    )
    
  })
  
  
  
  
  
  # PN - Variaveis: Coeficientes, Dispersao e Significancias ----------------
  
  # Coeficients Plot
  output$pn_coef_plot <- plotly::renderPlotly({
    shiny::req(model())
    shiny::req(input$pn_check_coef_plot)
    
    coef_graph(model(), model_summary(), prop)
    
  })
  
  
  
  # coeficients bar plot
  output$pn_coef_bar_plot <- plotly::renderPlotly({  
    shiny::req(model())
    shiny::req(input$pn_check_coef_bar_plot)
    
    coef_bar_graph(
      model_summary(), 
      prop, 
      input$coef_bar_plot_grandeza)
    
  })  
  
  
  tabela_coeficientes <- shiny::reactive({
    shiny::req(model())
    
    coef_tab(model_summary(), prop, input$config_decimal_digits)
    
  })
  
  output$pn_coef_table <- DT::renderDataTable({  
    shiny::req(model())
    shiny::req(input$pn_check_coef_table)
    
    coef_tab(model_summary(), prop, input$config_decimal_digits)
    
  })  
  
  
  
  # PN - Aderencia ----------------------------------------------------------
  #RESIDUOS ADERENCIA E GRAFICO DOS RESIDUOS
  output$res_all  <- plotly::renderPlotly({
    shiny::req(model())
    
    resi <- var_dep_and_residuals() %>%  dplyr::as_tibble() %>%  plotly::highlight_key()
    
    p1 <- plot_residuals_ade(resi, 
                             prop,
                             "modelling",
                             point_alpha = input$plot_2d_alpha,
                             point_size = input$plot_2d_marker_size) %>%
      plotly::highlight(on = 'plotly_selected', off = 'plotly_deselect')
    
    p2 <- plot_residuals_graph(resi, 
                               "modelling",
                               prop,
                               point_alpha = input$plot_2d_alpha,
                               point_size = input$plot_2d_marker_size,
                               show_hist = FALSE,
                               nbinsx = input$plot_1d_nbinsx,
                               histnorm  = plot_1d_histnorm(),
                               alpha = input$plot_1d_alpha,
                               cumula = input$plot_1d_cumalative) %>%
      plotly::highlight(on = 'plotly_selected', off = 'plotly_deselect')
    
    p4 <- plot_residuals_ade(resi, 
                             prop, 
                             "estimate",
                             point_alpha = input$plot_2d_alpha,
                             point_size = input$plot_2d_marker_size) %>%
      plotly::highlight(on = 'plotly_selected', off = 'plotly_deselect')
    
    p5 <- plot_residuals_graph(resi, 
                               "estimate",
                               prop,
                               point_alpha = input$plot_2d_alpha,
                               point_size = input$plot_2d_marker_size,
                               show_hist = FALSE,
                               nbinsx = input$plot_1d_nbinsx,
                               histnorm  = plot_1d_histnorm(),
                               alpha = input$plot_1d_alpha,
                               cumula = input$plot_1d_cumalative) %>%
      plotly::highlight(on = 'plotly_selected', off = 'plotly_deselect')
    
    plotly::subplot(p1, p4, p2, p5, nrows = 2, titleX = TRUE, titleY = TRUE)
    #crosstalk::bscols(list(p1, p2, p3), list(p4, p5, p6))
    
  })
  
  
  # PN - Correlacoes --------------------------------------------------------
  
  # PN - Matriz de Correlacoes Isoladas Modelagem ---------------------------
  
  cor_isoladas <- shiny::reactive({
    shiny::req(df_prepared())
    
    stats::cor(df_prepared(), method = input$method_correlation)
    
  })
  
  
  # PN - Heatmap Correlacoes Isoladas Modelagem -----------------------------
  
  
  output$corr_heat_map <- plotly::renderPlotly({
    shiny::req(cor_isoladas())
    shiny::req(input$pn_check_cor)
    
    plot_cor_heatmap(cor_isoladas(), 
                     input$corr_simetric, 
                     input$corr_diag, 
                     "modelling", 
                     prop)
    
  }) 
  
  
  # PN - Tabela Correlacoes Isoladas Modelagem ------------------------------
  
  
  output$table_cor <- DT::renderDataTable({
    shiny::req(cor_isoladas())
    shiny::req(input$pn_check_cor_table)
    
    data_table_cor(cor_isoladas(), 
                   input$config_decimal_digits, 
                   "modelling", 
                   prop)
    
    
  })
  
  
  # PN - Matriz Correlacoes Isoladas Estimativa -----------------------------
  
  
  cor_isoladas_est <- shiny::reactive({
    shiny::req(df_select())
    
    stats::cor(df_select(), method = input$method_correlation_est)
    
  })
  
  
  # PN - Heatmap Correlacoes Isoladas Estimativa ----------------------------
  
  
  output$corr_heat_map_est <- plotly::renderPlotly({
    shiny::req(cor_isoladas_est())
    shiny::req(input$pn_check_cor_est)
    
    plot_cor_heatmap(cor_isoladas_est(), 
                     input$corr_simetric_est, 
                     input$corr_diag_est,
                     "estimate", 
                     prop)
    
  }) 
  
  
  # PN - Tabela Correlacoes Isoladas Estimativa -----------------------------
  
  output$table_cor_est <- DT::renderDataTable({
    shiny::req(cor_isoladas_est())
    shiny::req(input$pn_check_cor_table_est)
    
    data_table_cor(cor_isoladas_est(), 
                   input$config_decimal_digits, 
                   "estimate", 
                   prop)
    
  })
  
  
  
  
  # PN - Matriz Correlacoes Parciais Modelagem ------------------------------
  
  
  cor_parciais <- shiny::reactive({
    shiny::req(df_prepared())
    
    m <- base::tryCatch({ 
      
      ppcor::pcor(df_prepared(), method = input$method_partial_correlation)
      
    }, error = function(e) {
      
      FALSE
      
    }, warning = function(e) {
      
      FALSE
      
    })
    
    validate(need(m, "O inverso da matriz de vari\u00e2ncia-covari\u00e2ncia \u00e9 calculado por meio do m\u00e9todo de Moore-Penrose Inverse, resultando em determinante igual ou muito pr\u00f3ximo de zero. Isso inviabiliza a continuidade do c\u00e1lculo. Opte por outro m\u00e9todo de c\u00e1lculo"))
    
    m
    
  })
  
  
  # PN - Heatmap Correlacoes Parciais Modelagem -----------------------------
  
  output$corr_par_heat_map <- plotly::renderPlotly({
    shiny::req(cor_parciais())
    shiny::req(input$pn_check_pcor)
    
    plot_cor_heatmap(cor_parciais()$estimate,
                     input$par_corr_simetric,
                     input$par_corr_diag, 
                     "modelling", 
                     prop)
    
  })
  
  
  # PN - Tabela Correlacoes Parciais Modelagem ------------------------------
  
  
  output$table_cor_par <- DT::renderDataTable({
    shiny::req(cor_parciais())
    shiny::req(input$pn_check_cor_par_table)
    
    data_table_cor(cor_parciais()$estimate, 
                   input$config_decimal_digits, 
                   "modelling", 
                   prop)
    
    
  })
  
  
  
  # PN - Matriz de Correlacoes Parciais Estimativa --------------------------
  
  
  cor_parciais_est <- shiny::reactive({
    shiny::req(df_select())
    
    m <- base::tryCatch({ 
      
      ppcor::pcor(df_select(), method = input$method_partial_correlation_est)
      
    }, error = function(e) {
      
      FALSE
      
    }, warning = function(e) {
      
      FALSE
      
    })
    
    validate(need(m, "O inverso da matriz de vari\u00e2ncia-covari\u00e2ncia \u00e9 calculado por meio do m\u00e9todo de Moore-Penrose Inverse, resultando em determinante igual ou muito pr\u00f3ximo de zero. Isso inviabiliza a continuidade do c\u00e1lculo. Opte por outro m\u00e9todo de c\u00e1lculo"))
    
    m
    
  })
  
  
  # PN - Heatmap Correlacoes Parciais Estimativa ----------------------------
  
  
  output$corr_par_heat_map_est <- plotly::renderPlotly({
    shiny::req(input$pn_check_pcor_est)
    shiny::req(cor_parciais_est())
    
    
    plot_cor_heatmap(cor_parciais_est()$estimate,
                     input$par_corr_simetric_est,
                     input$par_corr_diag_est, 
                     "estimate",
                     prop)
    
  })
  
  
  # PN - Tabela Correlacoes Parciais Estimativa -----------------------------
  
  output$table_cor_par_est <- DT::renderDataTable({
    shiny::req(cor_parciais_est())
    shiny::req(input$pn_check_cor_par_table_est)
    
    data_table_cor(cor_parciais_est()$estimate, 
                   input$config_decimal_digits,
                   "estimate", 
                   prop)
    
  })
  
  
  
  # PN - Analise da Equacao -------------------------------------------------
  
  # PN - Singular Equacao ---------------------------------------------------
  
  # Equacao Selecinoada
  eq_selected <- shiny::reactive({ 
    shiny::req(model_summary())
    
    write_eq(model_summary(), prop)
    
    
  })
  
  
  # Equacao Selecinoada
  eq_selected_mod <- shiny::reactive({ 
    shiny::req(model_summary())
    
    write_eq(model_summary(), prop, FALSE)
    
    
  })
  
  
  
  # Plotagem em forma de Texto da Equacao Selecionada
  output$eq_analysis_mod <- shiny::renderText({
    shiny::req(model_summary())
    
    eq_selected_mod()
    
  })
  
  output$eq_analysis_est <- shiny::renderText({
    shiny::req(model_summary())
    
    eq_selected()
    
  })
  
  
  
  shiny::observe({
    shiny::req(model())
    
    shiny::updateSelectInput(
      session, 
      "pn_eq_select_var_x_mod", 
      choices = get_indep(prop),
      selected = pn_eq_var_sel_mod())
    
  })
  pn_eq_var_sel_mod <- reactiveVal()
  
  observe({
    shiny::req(input$pn_eq_select_var_x_mod)
    pn_eq_var_sel_mod(input$pn_eq_select_var_x_mod)
    
  })
  
  observe({
    shiny::req(model())
    
    shiny::updateSelectInput(
      session, 
      "pn_eq_select_var_x_est", 
      choices = get_indep(prop),
      selected = pn_eq_var_sel_est())
    
  })
  pn_eq_var_sel_est <- reactiveVal()
  
  shiny::observe({
    shiny::req(input$pn_eq_select_var_x_est)
    pn_eq_var_sel_est(input$pn_eq_select_var_x_est)
    
  })
  
  
  
  df_eq_analysis_mod <- shiny::reactive({
    
    var_x <- pn_eq_var_sel_mod()
    # grid elaborado em valores nao transformados
    df_grid <- df_select() %>% 
      modelr::data_grid(!!rlang::sym(var_x), .model = model())
    
    #transformam-se esse valores para entrarem no modelo
    df_grid_trns <- df_grid %>% as.matrix() %>%  transform_data2(prop) 
    
    #calculam-se os valores no modelo e retransfroma-os
    tab <- calc_new_data(df_grid_trns %>% dplyr::as_tibble(), 
                         modelo = model(), 
                         confianca = input$pn_eq_analysis_confidence_mod) %>% 
      
      cbind(df_grid_trns, .) %>% dplyr::as_tibble()
    
    tab %>% dplyr::mutate("Tx Varia\u00e7\u00e3o (%)" := (diff(c( 0, media ))/ dplyr::lag(media)  )) 
    
  })
  
  
  df_eq_analysis_est <- shiny::reactive({
    
    var_x <- pn_eq_var_sel_est()
    # grid elaborado em valores nao transformados
    df_grid <- df_select() %>% 
      modelr::data_grid(!!rlang::sym(var_x), .model = model())
    
    #transformam-se esse valores para entrarem no modelo
    df_grid_trns <- df_grid %>% as.matrix() %>% transform_data2(prop) 
    
    #calculam-se os valores no modelo e retransfroma-os
    tab <- calc_new_data(df_grid_trns %>% dplyr::as_tibble(), 
                         modelo = model(), 
                         confianca = input$pn_eq_analysis_confidence_est) %>% 
      
      calc_back_scale_new_data( 
        input$pn_eq_select_estimador_log_est, 
        prop = prop) %>% 
      
      cbind(df_grid, .) %>% 
      
      dplyr::mutate("Tx Varia\u00e7\u00e3o (%)" := (diff(c( 0, !!rlang::sym(input$pn_eq_select_estimador_log_est) ))/
                                            dplyr::lag(!!rlang::sym(input$pn_eq_select_estimador_log_est))) )
    tab
  })
  
  
  #tabela modelagem
  output$df_eq_analysis_table_mod <- DT::renderDataTable({
    
    tb <- df_eq_analysis_mod() %>% 
      
      dplyr::rename(
        "Predi\u00e7\u00e3o Inferior" = pred_inf,
        "Confian\u00e7a Inferior" = conf_inf,
        "M\u00e9dia" = media,
        "Confian\u00e7a Superior" = conf_sup,
        "Predi\u00e7\u00e3o Superior" = pred_sup
      ) %>%  dplyr::select( -se_fit, -df)
    
    tb %>%
      
      data_table_preview2() %>% 
      
      DT::formatRound(setdiff(names(tb), "Tx Varia\u00e7\u00e3o (%)"),
                      digits = input$config_decimal_digits,
                      dec.mark = ",",
                      mark = ".") %>% 
      
      DT::formatPercentage("Tx Varia\u00e7\u00e3o (%)",
                           digits = input$config_decimal_digits,
                           dec.mark = ",",
                           mark = ".")
    
  })
  
  
  
  
  
  # Grafico da Modelagem
  output$df_eq_analysis_plot_mod <- plotly::renderPlotly({
    
    
    var_dep <- prop$var_dependent
    var_x <- pn_eq_var_sel_mod()
    df_grid <- df_eq_analysis_mod()
    ic_show <- input$pn_eq_conf_mod
    ip_show <- input$pn_eq_pred_mod
    show_obs <- input$pn_eq_obs_values_mod
    
    elemento <- data$main[!prop$obs_disabled, "Elemento" , drop = TRUE] 
    
    df_obs <- df_select() %>% dplyr::as_tibble()  
    df_obs_trns <- df_prepared() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(Elemento = elemento)
    
    
    plot_data_grid(var_dep, 
                   var_x, 
                   df_grid, 
                   ic_show, 
                   ip_show, 
                   show_obs, 
                   df_obs, 
                   df_obs_trns, 
                   point_size = input$plot_2d_marker_size,
                   point_jit = input$plot_2d_jitter,
                   point_opacity = input$plot_2d_alpha)
    
  })
  
  
  
  #tabela modelagem
  output$df_eq_analysis_table_est <- DT::renderDataTable({
    
    tb <- df_eq_analysis_est() %>% 
      
      dplyr::rename(
        "Predi\u00e7\u00e3o Inferior" = pred_inf,
        "Confian\u00e7a Inferior" = conf_inf,
        "Moda" = moda,
        "Mediana" = mediana,
        "M\u00e9dia" = media,
        "Confian\u00e7a Superior" = conf_sup,
        "Predi\u00e7\u00e3o Superior" = pred_sup
      ) %>%  dplyr::select( -se_fit) 
    
    tb %>%
      
      data_table_preview2() %>% 
      
      DT::formatRound(setdiff(names(tb), "Tx Varia\u00e7\u00e3o (%)"),
                      digits = input$config_decimal_digits,
                      dec.mark = ",",
                      mark = ".") %>% 
      
      DT::formatPercentage("Tx Varia\u00e7\u00e3o (%)",
                           digits = input$config_decimal_digits,
                           dec.mark = ",",
                           mark = ".")
    
  })
  
  
  
  
  
  # Grafico da Modelagem
  output$df_eq_analysis_plot_est <- plotly::renderPlotly({
    
    
    var_dep <- prop$var_dependent
    var_x <- pn_eq_var_sel_est()
    df_grid <- df_eq_analysis_est()
    ic_show <- input$pn_eq_conf_est
    ip_show <- input$pn_eq_pred_est
    show_obs <- input$pn_eq_obs_values_est
    
    elemento <- data$main[!prop$obs_disabled, "Elemento" , drop = TRUE] 
    
    df_obs <- df_select()  %>% dplyr::as_tibble()  %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(Elemento = elemento)
    
    
    plot_data_grid(var_dep, 
                   var_x, 
                   df_grid, 
                   ic_show, 
                   ip_show, 
                   show_obs, 
                   df_obs, 
                   df_obs, 
                   point_size = input$plot_2d_marker_size,
                   point_jit = input$plot_2d_jitter,
                   point_opacity = input$plot_2d_alpha)
    
  })
  
  

# PN - Teste de Micronumerosidade -----------------------------------------

  output$micro_modelo <- shiny::renderText({
    
    k <- get_indep(prop) %>% length()
    
    n <- sum(!prop$obs_disabled)
    
    validate(need(n > 0, "Nenhum dado habilitado"))
    validate(need(k > 0, "Nenhuma vari\u00e1vel habilitada"))
    
    
    msg <- if (n >= 3*(k+1)) { 
      
      "a micronumerosidade est\u00e1 ok!"
      
    } else { 
        
      "a micronumerosidade N\u00e3O est\u00e1 ok!"
      
      }
    paste0("Existem ", k, " vari\u00e1veis independentes habilitadas (k), logo s\u00e3o necess\u00e1rios ao menos (3*(k+1)) ", 3*(k+1), " dados. A quantidade de dados habilitados \u00e9: ", n, ". Portanto, ", msg)
    
  })
  
output$tabela_micro <- DT::renderDataTable({
  req(df_select())
  
  
  check_micronumerosidade_all(df_select(), prop) %>% 
    
    dplyr::mutate_all(as.factor) %>% 
    
    data_table_preview2()
  
  
  
})  
  
  
  
  # ET - ESTIMATIVAS --------------------------------------------------------
  
  
  # ET - Singular Insercao Avaliando ----------------------------------------
  
  
  
  # cria os campos para insercao dos valores do avaliando
  output$estimative_variables <- shiny::renderUI({
    shiny::validate(shiny::need(model(), "Modelo de Regress\u00E3o n\u00E3o definido"))
    
    model <- model()
    
    var_dep <- prop$var_dependent
    var_indep <- setdiff(names(model$model), var_dep)
    
    lapply(var_indep, function(x) {
      
      name_id <- x %>% make_shiny_id() %>% paste0("_input_calc")
      
      numericInput(inputId = name_id, 
                   label = x, 
                   value = prop$predict_data[[x]], #isso \u00e9 alimentado quando o botao estimar \u00e9 acionado
                   width = "100%")
      
    })
    
  })
  
  # Se dentre as variaveis habilitadas existirem variaveis proveninentes das
  # espaciais, habilita o botao de consulta georreferenciamento
  
  alguma_geo_var_hab <- shiny::reactive({
    
    
    # Obtendo Todas as Variaveis habilitadas
    var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
    
    # Obtendo a Variavel Dependente
    var_dep <- prop$var_dependent
    
    # Obtendo Todas as Variaveis Independentes, apenas
    var_indep <- setdiff(var_enabled, var_dep)
    
    all_geo_var <- get_spatial_names(prop, unlist = TRUE)
    
    i <- (var_indep %in% get_spatial_names(prop, unlist = TRUE))
    
    var_indep[i]
    
    # se houver alguma habilitada, retorna a lista das variaveis habilitdas em um
    # vetor, se nao hover, retorna um vetor do tipo character(0)
    
    
  })
  # verificacao se existe alguma variavel habilitada do tipo geoespacial
  
  
  shiny::observe({
    
    shinyjs::disable("consultar_geo_button")
    shinyjs::disable("auto_capture_spatial_var")
    
    shiny::req(alguma_geo_var_hab())
    
    shinyjs::enable("consultar_geo_button")
    shinyjs::enable("auto_capture_spatial_var")
  })
  
  # criacao do Mapa de COleta de Variaveis espaciais dfo avaliando
  output$consultar_geo_var_mapa <- leaflet::renderLeaflet({
    shiny::req(spatial_data())
    shiny::req(prop)
    
    city_map(data$main) %>%
      
      city_map_data(spatial_data(),
                    prop$obs_disabled,
                    cat = NULL,
                    opacity_border = input$config_mapa_point_opacity_border,
                    opacity_fill = input$config_mapa_point_opacity_inside,
                    size = input$config_mapa_point_radius
      ) %>%
      city_map_influence(prop$geo_influence, "Set3") %>%
      city_map_influence(prop$geo_model, "Set2") %>%
      city_map_influence(prop$geo_shp, "Set1") %>%
      city_map_legend(
        prop$obs_disabled,
        prop$geo_model,
        prop$geo_influence,
        prop$geo_shp)
    
  })
  
  # Para coletar as informacoes do Avaliando, \u00e9 necessario definir sua
  # localizacao no MAPA. Essa localizacao pode vir de duas fontes: A primeira
  # clicando sobre o Mapa, a Segunda digitando a latitude e a longitude. Entao
  # criou-se esse ReativeVAl que sera alimentado por duas fontes diferentes. E
  # dele, somente entao, ser\u00e1 dada continuidade a informacao coletada Central
  # com informacoes do ponto onde se localiza o Avaliando
  lat_avaliando <- reactiveVal()
  lng_avaliando <- reactiveVal()
  epsg_avaliando <- reactiveVal()
  
  
  
  # Fonte 1 q modifica a localizacao do Avaliando, proveninente do clique sobre
  # o Mapa
  shiny::observeEvent(input$consultar_geo_var_mapa_click, {
    
    pnt <- input$consultar_geo_var_mapa_click 
    shiny::req(pnt)
    
    lat_avaliando(pnt$lat)
    lng_avaliando(pnt$lng)
    epsg_avaliando(4326)
    
  })
  
  # Fonte 2 que modifica a localizacao do Avaliando com base no digitacao do
  # Avaliando
  observeEvent(input$consultar_geo_var_lng_lat, {
    
    longitude <- input$consultar_geo_var_lng
    latitude <- input$consultar_geo_var_lat
    epsg <- input$consultar_geo_var_epsg
    
    
    lat_avaliando(latitude)
    lng_avaliando(longitude)
    epsg_avaliando(epsg)
    
  })
  
  # Uma vez alimentado o ReactiveVal(), a partir dele \u00e9 criado o ponto de
  # referencia do Avalaiando
  
  ponto_avaliando <- reactive({
    req(lat_avaliando())
    req(lng_avaliando())
    req(epsg_avaliando())
    
    criar_ponto_espacial(lat_avaliando(), lng_avaliando(), epsg_avaliando())
    
    
  })
  
  # Uma vez o ponto definido, plata-o sobre o grafico
  observeEvent(ponto_avaliando(), {
    
    leaflet::leafletProxy("consultar_geo_var_mapa") %>% 
      
      leaflet::clearGroup("avaliando") %>% 
      
      leaflet::addMarkers(data = ponto_avaliando(), group = "avaliando")
    
  })
  
  
  # Ponto deifnido, vamos buscar as informacoes relativas ao ponto frente aos
  # arquivos espaciais 
  observeEvent(input$confirm_geo_var, {
    #browser()
    
    pontos <- ponto_avaliando()
    
    # Obtendo Todas as Variaveis habilitadas
    var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
    
    # Obtendo a Variavel Dependente
    var_dep <- prop$var_dependent
    
    # Obtendo Todas as Variaveis Independentes, apenas
    var_indep <- setdiff(var_enabled, var_dep)
    
    # Para Cada Variavel Independente, criando nome id que acessa os dados
    # inseridos pelo usuario. Temos todas as vari\u00e1veis indep habilitadas aqui
    var <- var_indep %>% make_shiny_id() %>% paste0("_input_calc")
    names(var) <- var_indep
    
    spatial_values <- coletar_spatial_avaliandos(ponto_avaliando(), 
                                                 var_indep, 
                                                 prop )
    
    req(NCOL(spatial_values) > 0)
    
    for (i in names(spatial_values)) {
      
      shiny::updateNumericInput(
        session = session, 
        inputId = var[i] %>% unname(), 
        value = spatial_values[1, i, drop = TRUE] %>% unname() %>% round(2)
      )
    }
    
    shinyBS::toggleModal(session, "consultar_geo_panel", toggle = "toggle")
    
    
  })
  
  
  
  
  
  
  # habilita o seltor de estimador em funcao da transformada LN da variavel
  # dependente
  observe({
    
    shinyjs::disable("estimador_log_nep")
    
    # primeiro tem q existir variavel depentende definida
    var_dep <- prop$var_dependent
    req(var_dep)
    
    # depois a trnasformada dessa variavel dependente deve ser do tipo log_nep
    trans <- prop$var_trns_selected[[var_dep]]
    
    req(model())
    req(trans == "log_nep")
    
    shinyjs::enable("estimador_log_nep")
    
  })
  
  
  
  
  # ET - Singular Coleta Avaliando ------------------------------------------
  
  # Uma vez que os dados foram inseridos em seus respectivos lugares, procede-se
  # a sua coleta desses lugares, com posterior verificacao de consistencia
  # Essa coleta \u00e9 desencadeada ou pelo bota "Estimar" que avalia o avalaidno no
  # modelo selecionado, ou pelo botao "Avaliar em Multiplos Modelos"que avalia o
  # avaliando em varios modelos previamente selecionados
  gatilho <- reactive({
    
    list(input$estimar , input$mult_model_valuation)
    
  })
  
  new_data <- eventReactive(gatilho(), ignoreInit = T, {
    req(model())
    
    var_enabled <- get_enabled(prop)
    var_dep <- prop$var_dependent
    var_indep <- get_indep(prop)
    
    # var <- var_indep %>% make_shiny_id() %>% paste0("_input_calc")
    # names(var) <- var_indep
    
    var <- input %>% names() %>% stringr::str_subset("_input_calc")
    names(var) <- var %>% stringr::str_replace("_input_calc", "") %>% remove_shiny_id()
    
    # names(new_data) <- names(new_data) %>%
    #   stringr::str_replace_all("_input_calc", "") %>%
    #   stringr::str_replace_all("`", "")
    
    prop$predict_data <- lapply(var, function(x) { input[[x]] })
    # essa lista alimenta o output$estimative_variables
    
    # garantindo que restem apenas para as variaveis habilitadas. Uma variavel
    # habilitada utilziada na estimativa que depois \u00e9 desabilitada fica com o
    # _input_calc salvo na memoria com seu ultimo valor. Quando o loop lapply
    # \u00e9 executado ele capta isso da memoria o que 'e interessante manter no
    # memoria. Mas para os calculos adiante, vamos retirar as variaveis que
    # nao estao habilitadas
    var <- var[var_indep]
    
    new_data <-  prop$predict_data[names(var)] %>% 
      
      do.call(cbind, .) %>% 
      
      `rownames<-`("natural") %>% 
      
      transform_data(prop)
    
    
    # uma vez coltas as informacoes do avalaiando, essa informacao tem sua
    # consistencia verifricada, bem como as medidas de extrapolacao
    new_data %>% 
      
      check_consistencia_inseridos() %>% 
      
      check_extrapolacao(df_select(), 
                         prop$var_nbr_type, 
                         session) 
    
  })
  
  
  
  
  
  
  # ET - Singular Unico Modelo ----------------------------------------------
  
  
  
  # calculo de um unico imovel na escala do modelo
  estimativas_unico <- eventReactive(new_data(), {
    req(model())
    
    new_data()[2, , drop = FALSE] %>% 
      
      dplyr::as_tibble() %>% 
      
      calc_new_data(., model(), input$intervalo_confianca)
    
  })
  
  # reaclculo da estimativa na escala retransformada
  estimativas_unico_back_scale <- reactive({
    req(estimativas_unico())
    
    calc_back_scale_new_data(estimativas_unico(), 
                             input$estimador_log_nep, 
                             prop)
    
  })
  
  
  
  # ET - Singular IVA -------------------------------------------------------
  
  # ET - Predicao dos Valores a 80% de Confian\u00e7a 
  # isso guia os valores possiveis de serem arbitrados e o IVA
  estimativa_conf_80 <- eventReactive(new_data(), {
    req(new_data())
    
    new_data()[2, , drop = F] %>% 
      
      dplyr::as_tibble() %>% 
      
      calc_new_data(., model(), 80) %>% 
      
      calc_back_scale_new_data( 
        input$estimador_log_nep, 
        prop)
    
    
  })
  
  
  # cria o campo a ser preenchido pelo valor arbitrado. A partir dele sao
  # calculados os valores do IVA
  output$iva_central_esti <- renderUI({
    req(new_data())
    
    var_dep <- prop$var_dependent
    var_dep_trns <- prop$var_trns_selected[[var_dep]]
    
    if (var_dep_trns == "log_nep") { 
      
      tc <- input$estimador_log_nep
      
    } else {
      
      tc <- "media"
      
    }
    
    central <- estimativa_conf_80()[1, tc, drop = T]
    
    numericInput(
      inputId = "valor_arbitrado", 
      label = "Valor Arbitrado", 
      value = central %>% round(2),
      min = central * 0.85, 
      max = central * 1.15
    )
    
  })
  
  
  # Consolida os Valores Admiss\u00edveis
  intervalo_valores_adm <- reactive({
    req(model())
    req(new_data())
    req(input$valor_arbitrado)
    
    
    calc_iva(estimativa_conf_80(), 
             input$estimador_log_nep, 
             input$valor_arbitrado, 
             prop)
    
    
  })
  
  # Cria a Tabela dos Valores Admiss\u00edveis
  output$iva_table <- renderTable(align = "c", spacing = "xs", {
    req(model())
    req(new_data())
    
    intervalo_valores_adm() %>% 
      
      `colnames<-`(c("M\u00ednimo do IVA",
                     "Valor Arbitrado", 
                     "M\u00e1ximo do IVA"))
    
  })
  
  
  
  # ET - Singular Graficos --------------------------------------------------
  
  
  output$grafico_previsao_model <- plotly::renderPlotly({
    req(input$estimar)
    
    isolate({
      req(estimativas_unico())
      
      prediction_graph_modelling(estimativas_unico(), 
                                 input$intervalo_confianca, 
                                 input$incluir_ip)
      
    })
  })
  
  
  output$grafico_previsao_real <- plotly::renderPlotly({
    req(input$estimar)
    
    isolate({
      req(estimativas_unico())
      
      prediction_graph_natural(estimativas_unico_back_scale(), 
                               input$intervalo_confianca, 
                               prop, 
                               input$estimador_log_nep, 
                               input$incluir_ip)
      
    })
  })
  
  
  
  # ET - Singular Tabelas ---------------------------------------------------
  
  output$tb_previsao_model <- renderTable(align = "c", spacing = "xs",  {
    req(input$estimar)
    isolate({
      
      estimativas_unico() %>% format_pred_table()
      
    })
  })
  
  
  
  output$tb_previsao_estimativa <- renderTable(align = "c", spacing = "xs", {
    req(input$estimar)
    
    isolate({
      
      
      var_dep_trns <- prop$var_trns_selected[[prop$var_dependent]]
      
      format_pred_table(estimativas_unico_back_scale(), 
                        var_dep_trns, 
                        input$estimador_log_nep)
      
    })
  })
  
  
  
  
  
  # ET - Singular em Varios Modelos -----------------------------------------
  
  # Nessa tabela sao apresentado os Modelos Pesquisados na Busca por
  # transformadas. A tabela permite a selecao de multiplas linhas, cada uma
  # representando um modelo. Cada modelo \u00e9 recriado e os dados do Avaliando sao
  # avaliados em cada um deles
  data_choose_model_multiple <- eventReactive(many_models_for_dt(), {
    req(many_models_for_dt())
    
    mtz <- many_models_for_dt()
    
    DT::datatable(
      mtz,
      options = list(
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        ),
        lengthMenu = list(c( 5, 10, 25, 50, 100, -1),
                          c( "5", "10", "25" , "50", "100", "Todos")),
        searching = T,
        dom = "liftp", #dom = "liftp",
        scrollX = TRUE,
        scrollY = TRUE,
        paging = TRUE,
        lengthMenu = FALSE,
        #pageLength = 5,
        autoWidth = FALSE
      ),
      class = "display",
      callback = DT::JS("return table;"),
      rownames = FALSE,
      filter = "top",
      selection = "multiple",
      caption = "Acaso alguma vari\u00e1vel ou dado seja habilitado/desabilitado, refa\u00e7a a pesquisa"
      
    ) %>%
      
      DT::formatSignif(
        c("R\u00B2 Mod",
          "R\u00B2 Adj Mod",
          "Correla\u00e7\u00e3o Mod",
          "R\u00B2 Est",
          "R\u00B2 Adj Est",
          "Correla\u00e7\u00e3o Est"),
        digits = 4,
        dec.mark = ",")
    
  })
  
  # Renderiza o Data Table criado acima
  output$data_choose_model_multiple <- DT::renderDataTable({
    req(data_choose_model_multiple())
    
    data_choose_model_multiple()
    
  })
  
  
  
  
  
  # Quando o botao de "Avaliar nas Equa\u00e7\u00f5es Selecionadas" \u00e9 acionado, os
  # calculos abaixo sao executados, ou seja, os modelos sao recriados e o imovel
  # \u00e9 avaliado
  mult_model_values_df  <- eventReactive(input$mult_model_valuation, {
    
    #mensagens aos usuarios
    id <- shiny::showNotification(
      ui = paste0("Recriando Modelos e Calculando Estimativas. Aguarde!"),
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    # iniciando
    var_enabled <- input$model_hab_calc
    var_dep <- input$model_var_dep
    mtz <- df_prepared_many_models()
    
    line <- input$data_choose_model_multiple_rows_selected
    
    req(line)
    req(mtz)
    req(new_data())
    
    new_data <- new_data()[2, , drop = F] %>% dplyr::as_tibble()
    
    
    results <- many_models()[line, ] %>% 
      apply(1, 
            regression_loop_lm2,
            matriz = mtz,
            matriz_new = new_data,
            conf = input$intervalo_confianca,
            var_dep = var_dep,
            nms = var_enabled,
            n_obs = NROW(mtz),
            n_var = NCOL(mtz),
            estimador_log_nep = input$estimador_log_nep,
            prop = prop 
      ) %>% t()
    
    results
    # many_models()[line, ] %>% 
    #   
    #   dplyr::as_tibble() %>% 
    #   
    #   nest(transformadas = all_of(var_enabled)) %>% 
    #   
    #   mutate(
    #     estimativas = map(transformadas, 
    #                       regression_loop_lm, 
    #                       matriz = mtz,
    #                       matriz_new = new_data,
    #                       conf = input$intervalo_confianca,
    #                       var_dep = var_dep,
    #                       nms = var_enabled,
    #                       n_obs = NROW(mtz),
    #                       n_var = NCOL(mtz),
    #                       estimador_log_nep = input$estimador_log_nep,
    #                       prop = prop 
    #     )
    #   ) %>% 
    #   
    #   unnest(c("transformadas", "estimativas")) 
    
    
    
    
    
  })
  
  output$mult_model_relations <- DT::renderDataTable({
    req(mult_model_values_df())
    
    
    var_enabled <- input$model_hab_calc
    mtz <- mult_model_values_df()
    
    format_result_matrix(mtz, var_enabled, rename_prediction = TRUE) %>% 
      
      dplyr::select(-"Erro-Padr\u00e3o") %>% 
      
      DT::datatable(
        .,
        extensions = 'Buttons',
        options = list(
          
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
            
          ),
          lengthMenu = list(c( 5, 10, 25, 50, 100, -1),
                            c( "5", "10", "25" , "50", "100", "Todos")),
          searching = T,
          dom = "liftBp", #dom = "liftp",
          scrollX = TRUE,
          scrollY = TRUE,
          paging = TRUE,
          lengthMenu = FALSE,
          # pageLength = 5,
          autoWidth = FALSE,
          
          buttons = list(
            'copy', 
            
            list(
              extend = 'collection',
              buttons = c('csv', 'excel'),
              text = 'Download'
            )
          )
          
        ),
        class = "display",
        callback = DT::JS("return table;"),
        rownames = FALSE,
        filter = "top",
        selection = "multiple",
        caption = "Acaso alguma vari\u00e1vel ou dado seja habilitado/desabilitado, refa\u00e7a a pesquisa"
        
      )  %>%
      
      DT::formatSignif(
        c("R\u00B2 Mod",
          "R\u00B2 Adj Mod",
          "Correla\u00e7\u00e3o Mod",
          "R\u00B2 Est",
          "R\u00B2 Adj Est",
          "Correla\u00e7\u00e3o Est"),
        digits = 4,
        dec.mark = ",") %>% 
      
      DT::formatRound(
        c("Predi\u00e7\u00e3o Inferior", 
          "Confian\u00e7a Inferior" , 
          "Moda" ,
          "Mediana" ,
          "M\u00e9dia",
          "Confian\u00e7a Superior", 
          "Predi\u00e7\u00e3o Superior"),
        digits = input$config_decimal_digits,
        dec.mark = ",",
        mark = "."
        
      )
    
  })
  
  
  
  # ET - Varios Avaliandos --------------------------------------------------
  
  # geracao da tabela para colar os avaliandos
  output$plan_mult_ava <- rhandsontable::renderRHandsontable({
    
    input$create_plan_avaliando
    
    isolate({
      req(model())
      req(input$create_plan_avaliando)
      validate(need(input$n_avaliando > 0, "Ao menos uma linha deve ser criada"))
      # Vamos criar a planilha de coleta de variaveis dos avaliandos
      
      # Pegamos aqui o nome das variaveis independentes que estao habilitadas
      var_indep <- get_indep(prop)
      
      names(var_indep) <- var_indep
      
      # 1 -  Possui var geo ou nao?
      possui_geo_var_hab <- alguma_geo_var_hab() %>% shiny::isTruthy()
      
      # 2 - Quem sao as geo var habilitadas?
      geo_var_hab <- alguma_geo_var_hab()
      
      # 3 - O Usuario quer coleta automatica sim ou nao?
      
      
      if (!possui_geo_var_hab | 
          (possui_geo_var_hab && !input$auto_capture_spatial_var)) {
        
        df <- array(NA_real_, 
                    c(input$n_avaliando, length(var_indep)),
                    dimnames = list(NULL, var_indep)
        ) %>% 
          
          dplyr::as_tibble() 
        
        
        
      } else if (possui_geo_var_hab && input$auto_capture_spatial_var) {
        
        i <- !(var_indep %in% geo_var_hab)
        
        col_names <- c("Latitude", "Longitude", "EPSG", var_indep[i])
        
        df <- array(NA_real_, 
                    c(input$n_avaliando, length(col_names)),
                    dimnames = list(NULL, col_names)
        ) %>% dplyr::as_tibble() 
        
        
        
      }
      
      
      # Se nao houver geo_var_hab, ja faz o df somente com as variaveis indep
      
      # se houver geo_hab e o usuario quiser coleta automatica, cria o df com latitude long e EPSG mas sem as var indep
      
      # se houver geo_hab e o usuario nao quiser coleta automatica, cria o DF normalmente
      
      
      # criamos uma matriz de valores NA que sera utilizada para a coleta. Se
      # houver variavel espacial habitlitada, a disposicao dessa variavel ocrre
      # de maneira normal. Se o usuario quiser q a coleta de valores espaciais
      # seja automatica, as variaveis espacial sao excluidas e sao inseridas as
      # variaveis Latitude Longitude e EPSG. 
      
      # Precismos identificar se ha variaveis espaciais ha bilitadas e quais sao elas
      
      
      
      rhandsontable::rhandsontable(
        df, 
        rowHeaders = NULL, 
        #width = 550, 
        #height = 600, 
        language = "pt-BR", 
        stretchH = "all"
      ) %>%
        
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, 
          allowColEdit = FALSE
          
        )%>%
        
        rhandsontable::hot_cols(fixedColumnsLeft = 1) # %>% 
      
      #hot_col("Obs", readOnly = TRUE) #%>%
      
      #hot_table(highlightCol = TRUE, highlightRow = TRUE)
      
    })
    
  })
  
  
  # coleta dos dados inseridos
  
  plan_mult_ava_results <- eventReactive(input$eval_plan_avaliando, {
    
    # mensagens aos usuarios
    id <- shiny::showNotification(
      ui = paste0("Calculando... Aguarde!"),
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    on.exit(removeNotification(id), add = TRUE)
    
    # variaveis independentes
    var_indep <- get_indep(prop)
    
    names(var_indep) <- var_indep
    
    
    # transformadas das variaveis independentes
    var_dep_trns <- prop$var_trns_selected[[prop$var_dependent]]
    
    
    # tabela com Avaliandos
    table <- input$plan_mult_ava %>% rhandsontable::hot_to_r() %>% dplyr::as_tibble()
    req(table)
    # dados que possuem informacoes completas
    dados_completos <- stats::complete.cases(table)
    
    # tabela somente com dados completos
    table <- table[dados_completos, , drop = FALSE]
    
    validate(need(NROW(table) > 0, "N\u00e3o foram fornecidos im\u00f3veis ou nem todas as informa\u00e7\u00f5es do im\u00f3veis inseridos est\u00e3o completas"))
    
    ## Condicoes: 
    
    # se possuir Latitude, Longitude e EPSG, \u00e9 pq existem variaveis espaciais
    # habilitadas e o usuario quer que elas sejam coletadas automaticamente
    
    if (all(c("Latitude", "Longitude", "EPSG") %in% names(table))) {
      # Entao cria-se um ponto espacial para a coleta das variaveis espaciais
      # verifica o EPSG unico
      epsg <- unique(table$EPSG)
      validate(need(length(epsg) == 1, "A coleta das vari\u00e1veis espaicias de todos os dados devem ser proveninetes do mesmo EPSG" ))
      
      ponto <- criar_ponto_espacial(table$Latitude, table$Longitude, epsg)
      
      spatial_info <- coletar_spatial_avaliandos(ponto, var_indep, prop)
      
      table <- cbind(table, spatial_info)
      
      
      nome_tabela <- (table %>% dplyr::select(-Latitude, -Longitude,-EPSG) %>% names())
      validate(need(all(var_indep %in% nome_tabela) & all(nome_tabela %in% var_indep) , "Recrie a tabela de Avaliandos"))
      
      apply(table %>% dplyr::select(-Latitude, -Longitude,-EPSG), 1, function(x) {
        #browser()
        x <- x %>% rbind()
        
        check_consistencia_inseridos(x) %>%
          
          check_extrapolacao(df_select(),
                             prop$var_nbr_type,
                             session)
      })
      
      
      
    } else {
      nome_tabela <- (table %>% names())
      validate(need(all(var_indep %in% nome_tabela) & all(nome_tabela %in% var_indep) , "Recrie a tabela de Avaliandos"))
      
      apply(table, 1, function(x) {
        #browser()
        x <- x %>% rbind()
        
        check_consistencia_inseridos(x) %>%
          
          check_extrapolacao(df_select(),
                             prop$var_nbr_type,
                             session)
      })
      
    }
    
    table <- table %>% transform_data2(prop) %>% dplyr::as_tibble()
    # extrair valores avaliando
    re <- calc_new_data(
      table,
      modelo = model(),
      input$int_confianca_multi_ava) %>%
      
      calc_back_scale_new_data(
        input$estimador_log_nep,
        prop,
        var_dep_trns) %>%
      
      format_result_matrix(names(.), TRUE)
    
    dplyr::bind_cols(table, re)
    
  })
  
  
  output$plan_mult_ava_results_DT <- DT::renderDataTable({
    req(plan_mult_ava_results())
    
    df <-  plan_mult_ava_results() %>% 
      
      dplyr::select(-"Erro-Padr\u00e3o") 
    
    df %>% 
      
      DT::datatable(
        .,
        extensions = 'Buttons',
        options = list(
          
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
            
          ),
          lengthMenu = list(c( 5, 10, 25, 50, 100, -1),
                            c( "5", "10", "25" , "50", "100", "Todos")),
          searching = T,
          dom = "liftBp", #dom = "liftp",
          scrollX = TRUE,
          scrollY = TRUE,
          paging = TRUE,
          lengthMenu = FALSE,
          # pageLength = 5,
          autoWidth = FALSE,
          
          buttons = list(
            'copy', 
            
            list(
              extend = 'collection',
              buttons = c('csv', 'excel'),
              text = 'Download'
            )
          )
          
        ),
        class = "display",
        callback = DT::JS("return table;"),
        rownames = FALSE,
        filter = "top",
        selection = "multiple",
        caption = paste0("Acaso alguma vari\u00e1vel ou dado seja habilitado/desabilitado, refa\u00e7a a pesquisa \n ", eq_selected())
        
      )   %>% 
      
      DT::formatRound(
        base::names(df),
        digits = input$config_decimal_digits,
        dec.mark = ",",
        mark = ".")
    
    
    
  })
  
  
  
  
  
  
  # ET - Grafico de Previsao ------------------------------------------------
  
  
  # atualizar lista de variaveis do eixo X
  et_eq_var_sel <- shiny::reactiveVal()
  
  shiny::observe({
    shiny::req(model())
    
    shiny::updateSelectInput(
      session, 
      "et_graph_prev_var_x", 
      choices = get_indep(prop),
      selected = et_eq_var_sel())
  })
  
  shiny::observe({
    shiny::req(input$et_graph_prev_var_x)
    
    et_eq_var_sel(input$et_graph_prev_var_x)
    
  })
  
  # atualizar lista de variaveis para multiplicar ou dividir
  et_eq_var_relate <- shiny::reactiveVal()
  
  shiny::observe({
    shiny::req(model())
    
    shiny::updateSelectInput(
      session, 
      "et_graph_prev_relation", 
      choices = c("Nenhuma", get_indep(prop)),
      selected = et_eq_var_relate())
  })
  
  shiny::observe({
    shiny::req(input$et_graph_prev_relation)
    
    et_eq_var_relate(input$et_graph_prev_relation)
    
  })
  
  
  # cria o data frame
  et_df_eq_analysis <- shiny::reactive({
    var_x <- et_eq_var_sel()
    shiny::req(var_x)
    # grid elaborado em valores nao transformados
    ab <- new_data()[1, , drop = F] %>% dplyr::as_tibble()
    nms <- setdiff(names(ab), var_x)
    df_grid <- df_select() %>% modelr::data_grid(!!rlang::sym(var_x), ab[nms]) 
    
    #transformam-se esse valores para entrarem no modelo
    df_grid_trns <- df_grid %>% base::as.matrix() %>% transform_data2(prop) 
    
    #calculam-se os valores no modelo e retransfroma-os
    tab <- calc_new_data(df_grid_trns %>% dplyr::as_tibble(), 
                         modelo = model(), 
                         confianca = input$intervalo_confianca) %>% 
      
      calc_back_scale_new_data( 
        input$estimador_log_nep, 
        prop = prop) %>% 
      
      cbind(df_grid, .) 
    
    
    if (input$et_graph_prev_relation != "Nenhuma") {
      
      var_relation <- input$et_graph_prev_relation
      
      fun <- base::switch(input$et_graph_prev_operation, div = `/`, mult = `*`)
      
      tab <- tab %>% 
        dplyr::mutate_at(
          c("pred_inf", 
            "conf_inf", 
            "moda", 
            "mediana", 
            "media", 
            "conf_sup", 
            "pred_sup"),
          
          ~fun(., !!rlang::sym(var_relation)))
      
    }
    
    tab %>%  
      dplyr::mutate(
        "Tx Varia\u00e7\u00e3o (%)" := (diff(c( 0, !!rlang::sym(input$pn_eq_select_estimador_log_est) ))/
                                dplyr::lag(!!rlang::sym(input$pn_eq_select_estimador_log_est))) )
    
  })
  
  # prepara a tabela
  output$et_graph_prev_table <- DT::renderDataTable({
    req(FALSE)
    tb <- et_df_eq_analysis() %>% 
      
      dplyr::rename(
        "Predi\u00e7\u00e3o Inferior" = pred_inf,
        "Confian\u00e7a Inferior" = conf_inf,
        "M\u00e9dia" = media,
        "Confian\u00e7a Superior" = conf_sup,
        "Predi\u00e7\u00e3o Superior" = pred_sup
      ) %>%  dplyr::select( -se_fit)
    
    tb %>%
      
      data_table_preview2() %>% 
      
      DT::formatRound(setdiff(names(tb), "Tx Varia\u00e7\u00e3o (%)"),
                      digits = input$config_decimal_digits,
                      dec.mark = ",",
                      mark = ".") %>% 
      
      DT::formatPercentage("Tx Varia\u00e7\u00e3o (%)",
                           digits = input$config_decimal_digits,
                           dec.mark = ",",
                           mark = ".")
    
  })
  
  
  
  # plota o grafico
  output$et_graph_prev_plot <- plotly::renderPlotly({
    
    var_x <- et_eq_var_sel()
    var_dep <- prop$var_dependent
    df_grid <- et_df_eq_analysis()
    estimador <- input$estimador_log_nep
    
    ic_show <- input$et_eq_conf
    ip_show <- input$et_eq_pred
    show_obs <- input$et_eq_obs_values
    
    elemento <- data$main[!prop$obs_disabled, "Elemento" , drop = TRUE] 
    
    df_obs <- df_select() %>% 
      dplyr::as_tibble()  %>% 
      dplyr::mutate(Elemento = elemento)
    
    if (input$et_graph_prev_relation != "Nenhuma") {
      
      var_relation <- input$et_graph_prev_relation
      
      fun <- base::switch(input$et_graph_prev_operation, div = `/`, mult = `*`)
      
      df_obs <- df_obs %>% 
        dplyr::mutate_at(
          var_dep, ~fun(., !!rlang::sym(var_relation)))
      
    }
    
    
    
    
    
    plot_data_grid(var_dep, 
                   var_x, 
                   df_grid, 
                   ic_show, 
                   ip_show, 
                   show_obs, 
                   df_obs, 
                   df_obs, 
                   point_size = input$plot_2d_marker_size,
                   point_jit = input$plot_2d_jitter,
                   point_opacity = input$plot_2d_alpha, 
                   estimador = estimador)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  # EX - EXPORTAR ARQUIVO ---------------------------------------------------
  
  output$salvar_em_rds <- downloadHandler(
    filename = "modelo.rds",
    content = function(file) {
      
      
      
      # model_check <- check_model(prop, input$auto_atualizar)
      # 
      # if (model_check) {
      # 
      #   prop$model_defined <- 1
      # 
      # }
      
      
      properties <- list()
      
      for (i in names(prop)) {
        
        properties[[i]] <- prop[[i]]
        
        properties
        
      }
      
      
      
      save <- data$main
      attr(save, "properties") <- properties
      attr(save, "saved_file") <- "saved_file"
      
      saveRDS(save, file)
      
      
    })
  
  
  
  
  
  # output$salvar_em_html <- downloadHandler(
  #   
  #   filename = "relatorio.html",
  #   content = function(file) {
  #     
  #     
  #     
  #     tempReport <- file.path(tempdir(), "relatorio.Rmd")
  #     padrao <- "www/relatorio.Rmd"
  #     file.copy(padrao, tempReport, overwrite = TRUE)
  #     
  #     
  #     model_check <- check_model(central$prop, input$auto_atualizar)
  #     
  #     
  #     params <- list(rzm = central$rzm,
  #                    prop = central$prop,
  #                    model = if(model_check) {  model() } else {NA} ,
  #                    metrics = if(model_check) {  metrics() } else {NA})
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  
  
  
  
  

}
