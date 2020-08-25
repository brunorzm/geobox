# incluir na .onLoad()
#shiny::addResourcePath('www', system.file('www', package = 'rcaixa'))

options(shiny.maxRequestSize = 100000*1024^2,
        shiny.reactlog = T
        #useFancyQuotes = FALSE,
        #shiny.trace = FALSE,
        # para debugging
        #shiny.error = recover#, # para debugging
        ,shiny.launch.browser = T
        #,browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
)




locale_br <- readr::locale(
  date_names = readr::date_names_lang("pt"),
  decimal_mark = ",", 
  grouping_mark = ".",
  encoding = "UTF-8",
  tz = "Brazil/East"
)


# Inserir Dados -----------------------------------------------------------



read_file <- function(file_path, 
                      session, 
                      csv_delim, 
                      csv_encoding, 
                      csv_decimal, 
                      csv_skip,
                      excel_sheet, 
                      excel_skip,
                      excel_decimal) {
  in_file <- NULL
  # waiter <- waiter::Waiter$new(
  #   color = rgb(0, 0, 0, .3),  
  #   html = spin_wave())
  # waiter$show()
  # on.exit(waiter$hide())
  
  ext <- tools::file_ext(file_path)
  
  
  
  shiny::showNotification(
    ui = paste("Formato do arquivo inserido:", ext),
    type = "message",
    duration = 2,
    closeButton = TRUE)
  
  showNotification(
    ui = "Analisando os Dados...",
    duration = 2,
    type = "message",
    closeButton = FALSE)
  
  
  df <- tryCatch({
    
    switch(ext,
           
           csv = readr::read_delim(
             file = file_path,
             delim = csv_delim,
             skip = csv_skip,
             locale = readr::locale(
               encoding = csv_encoding, 
               decimal_mark = csv_decimal)),
           
           xlsx = readxl::read_xlsx(
             path = file_path,
             sheet = excel_sheet,
             skip = excel_skip),
           
           xls = readxl::read_xls(
             path = file_path,
             sheet = excel_sheet,
             skip = excel_skip),
           
           kml = sf::st_read(file_path),
           
           kmz = sf::st_read(file_path),
           
           rds = readRDS(file_path),
           
           shp = {
             
             path <-  stringr::str_extract(in_file$datapath, "^.+\\\\")
             #st_read_segura(path)
             
           },
           
           ext
    )
    
  },
  
  error = function(e) { ext })
  
  
  
  if(is.character(df) & length(df) == 1) {
    
    msg <- shiny::HTML(paste("Erro na compreens\u00E3o do arquivo.<br/><br/> Extens\u00E3o identificada:", df))
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Erro de leitura!",
      text = msg,
      type = "error",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  
  if (ext == "rds") {
    
    df <- readRDS(file_path)
    is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
    
    if (!is_saved) {
      
      msg <- shiny::HTML(paste("O arquivo rds inserido n\u00E3o \u00E9 v\u00E1lido"))
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Erro de leitura!",
        text = msg,
        type = "error",
        html = TRUE
      )
      
      req(FALSE)
      
    } else {
      
      return(df)
      
    }
    
  }
  
  df
  
}



check_data <- function(df, session) {
  
  is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
  if (is_saved) return(df)
  
  
  if (!inherits(df, "data.frame")) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Erro de leitura!",
      text = "O objeto lido n\u00E3o \u00E9 da classe data.frame",
      type = "error",
      html = TRUE
    )
    req(FALSE)
    
  } 
  
  
  
  if (NCOL(df) < 2) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Erro de leitura!",
      text = "O arquivo inserido possui menos duas colunas",
      type = "error",
      html = TRUE
    )
    req(FALSE)
    
  } 
  
  if (NROW(df) < 2) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Erro de leitura!",
      text = "O arquivo inserido deve possuir mais de dois dados",
      type = "error",
      html = TRUE
    )
    req(FALSE)
    
  } 
  
  df
}

fix_encoding <- function(df, .from = "", .to = "latin1") {
  
  names(df) <- iconv(names(df), from = .from, to = .to)
  
  df <- df %>% 
    
    dplyr::mutate_if(is.character, function(x) {
      
      iconv(x,  to = .to, from = .from)
      
    }) 
  
  df
}


check_encoding <- function(df, session) {
  
  is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
  if (is_saved) return(df)
  
  
  tryCatch({
    
    gsub("</", "",  names(df), fixed = TRUE) %>% invisible
    
    df %>% dplyr::mutate_if(is.character, function(x) {
      
      gsub("</", "", x, fixed = TRUE) %>% invisible
      
    })
    
  }, 
  
  error = function(e) {
    
    
    msg <- shiny::HTML(paste("O programa encontrou inconsist\u00EAncia entre o encoding dos caracteres definido no ato de cria\u00E7\u00E3o do arquivo com o encoding definido para sua leitura.<br/><br/> Dessa forma, a leitura foi cancelada.<br/><br/> Tente ler o arquivo com o enconding 'latin1'. Se o erro persistir, procure o autor do arquivo ."))
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Encoding!",
      text = msg,
      type = "info",
      html = TRUE
    )
    req(FALSE)
    #df %>% fix_encoding()
    
  }
  
  )
}


nomes_simil <- c("C\u00F3digo da GIHAB vinculada ao munic\u00EDpio do im\u00F3vel", 
                 "Grupo de Im\u00F3veis", 
                 "Tipo de Pe\u00E7a T\u00E9cnica", 
                 "C\u00F3digo da GIHAB vinculada \u00E0 Unidade Demandante", 
                 "C\u00F3digo da Unidade Demandante", 
                 "Ordenador", 
                 "Ano", 
                 "Complemento", 
                 "Sequencial", 
                 "Objetivo", 
                 "Finalidade",
                 "Pressupostos, Ressalvas e Fatores Limitantes", 
                 "Atividade", 
                 "Produto", 
                 "Linha", 
                 "Fonte", 
                 "Tipo de Interessado", 
                 "CPF ou CNPJ do interessado", 
                 "Grau de Sigilo",
                 "Categoria do Im\u00F3vel (tipo de im\u00F3vel)", 
                 "Cadastro Municipal",
                 "CEP",
                 "UF",
                 "Munic\u00EDpio",
                 "Distrito / Localidade / Cidade", 
                 "Bairro", 
                 "Tipo de Logradouro",
                 "Logradouro",
                 "N\u00ba", 
                 "Setor", 
                 "Quadra",
                 "Bloco", 
                 "Lote", 
                 "Conjunto", 
                 "N\u00ba da Unidade", 
                 "T\u00EDtulo do Empreendimento", 
                 "Nome do Empreendimento",
                 "Complementos", 
                 "Coordenadas Grau Decimal", 
                 "Latitude", 
                 "Hemisf\u00E9rio", 
                 "Longitude",
                 "Residencial Unifamiliar - Usos na Regi\u00E3o", 
                 "Residencial Multifamiliar - Usos na Regi\u00E3o", 
                 "Comercial  - Usos na Regi\u00E3o", 
                 "Industrial - Usos na Regi\u00E3o", 
                 "Institucional - Usos na Regi\u00E3o", 
                 "Outros Usos na Regi\u00E3o", 
                 "\u00C1gua Pot\u00E1vel no Endere\u00E7o", 
                 "Esgoto Sanit\u00E1rio   no Endere\u00E7o", 
                 "Energia El\u00E9trica   no Endere\u00E7o", 
                 "Telefone no Endere\u00E7o", 
                 "Pavimenta\u00E7\u00E3o no Endere\u00E7o", 
                 "Esgoto Pluvial no Endere\u00E7o", 
                 "G\u00E1s Canalizado no Endere\u00E7o", 
                 "Ilumina\u00E7\u00E3o P\u00FAblica no Endere\u00E7o", 
                 "Coleta de Lixo no Bairro", 
                 "Transporte Coletivo no Bairro", 
                 "Com\u00E9rcio no Bairro", 
                 "Rede Banc\u00E1ria no Munic\u00EDpio", 
                 "Educa\u00E7\u00E3o no Bairro", 
                 "Sa\u00FAde no Bairro", 
                 "Seguran\u00E7a no Bairro", 
                 "Outros Servi\u00E7os no Bairro", 
                 "Padr\u00E3o das Edifica\u00E7\u00F5es na Regi\u00E3o", 
                 "Influ\u00EAncias Valorizantes Sobre o Im\u00F3vel", 
                 "Influ\u00EAncias Desvalorizantes sobre o Im\u00F3vel", 
                 "Classifica\u00E7\u00E3o da Via de Acesso ao Im\u00F3vel", 
                 "Condi\u00E7\u00E3o da Regi\u00E3o no Contexto Urbano", 
                 "Inser\u00E7\u00E3o (tipo de implanta\u00E7\u00E3o)", 
                 "Tipo de Vistoria", "Data da Vistoria", 
                 "Ocupa\u00E7\u00E3o da Unidade", 
                 "Orienta\u00E7\u00E3o Solar da Unidade", 
                 "Vista Panor\u00e2mica a partir da unidade", 
                 "Aquecimento Solar", "Arm\u00E1rios Embutidos",
                 "Ar Condicionado", 
                 "Campos de Futebol/Golfe", 
                 "Equipamentos de Seguran\u00E7a",
                 "Espa\u00E7o com Churrasqueira", 
                 "Espa\u00E7o c/ Churrasqueira Privativo", 
                 "Estacionamento para visitantes", 
                 "Gerador", 
                 "Lareira", 
                 "Rede Interna de G\u00E1s", 
                 "Mini-Quadra Esportiva", 
                 "Piscina", 
                 "Piscina Privativa",
                 "Play Ground",
                 "Po\u00E7o Artesiano", 
                 "Portaria e/ou Guarita", 
                 "Sal\u00E3o de Festas",
                 "Outros Sal\u00F5es de Lazer", 
                 "Sauna / Ofur\u00F4 / Hidromassagem",
                 "Sauna / Ofur\u00F4 / Hidromassagem Privativa", 
                 "Quadra de T\u00EAnis", "Quadra Poliesportiva",
                 "Central Aquecimento de \u00C1gua", 
                 "Pilotis", 
                 "Outros Equipamentos", 
                 "N\u00E3o Possui Equipamentos", 
                 "Quantidade de compartimentos com arm\u00E1rios embutidos", 
                 "Quantidade de Sal\u00F5es de Lazer", 
                 "Nome do Campo Customiz\u00E1vel 1", 
                 "Atributo do Campo Customiz\u00E1vel  1", 
                 "Nome do Campo Customiz\u00E1vel 2", 
                 "Atributo do Campo Customiz\u00E1vel  2", 
                 "Nome do Campo Customiz\u00E1vel 3", 
                 "Atributo do Campo Customiz\u00E1vel  3", 
                 "Nome do Campo Customiz\u00E1vel 4", 
                 "Atributo do Campo Customiz\u00E1vel  4", 
                 "Nome do Campo Customiz\u00E1vel 5", 
                 "Atributo do Campo Customiz\u00E1vel  5", 
                 "Nome do Campo Customiz\u00E1vel 6", 
                 "Atributo do Campo Customiz\u00E1vel  6", 
                 "Uso da Unidade", 
                 "\u00C1rea Privativa da Unidade  Averbada (m2)", 
                 "\u00C1rea Privativa Descont\u00EDnua Averbada  (m2)", 
                 "\u00C1rea Comum Averbada  (m2)", 
                 "\u00C1rea Averbada Total (m2)", 
                 "\u00C1rea Privativa da Unidade  N\u00E3o Averbada  (m2)", 
                 "\u00C1rea Privativa Descont\u00EDnua N\u00E3o Averbada (m2)", 
                 "\u00C1rea Comum N\u00E3o Averbada (m2)", 
                 "\u00C1rea N\u00E3o Averbada Total (m2)", 
                 "\u00C1rea Privativa Total da Unidade (m2)", 
                 "\u00C1rea Privativa Descont\u00EDnua Total (m2)", 
                 "\u00C1rea Comum Total (m2)", 
                 "Total Constru\u00EDda (m2)", 
                 "\u00C1rea Privativa Descoberta (m2)", 
                 "Vagas  livres/ Independentes Cobertas", 
                 "Vagas  Bloqueadas/ Bloqueantes Cobertas", 
                 "Total de Vagas Cobertas", 
                 "Vagas  livres / Independentes Descobertas", 
                 "Vagas  Bloqueadas/ Bloqueantes Descobertas", 
                 "Total de Vagas Descobertas", 
                 "Total Geral de Vagas", 
                 "\u00C1rea(s) de Servi\u00E7o", 
                 "\u00C1rea(s) de Servi\u00E7o Descoberta(s)",
                 "Banheiro(s) Privativo (s)", 
                 "Banheiro(s) Privativo(s) de Empregada", 
                 "Banheiro(s) de Servi\u00E7o", 
                 "Banheiro(s) Social(ais)", 
                 "Closet(s)", "Copa(s)",
                 "Copa(s) Cozinha(s) Conjugada(s)", 
                 "Cozinha(s)", 
                 "Cozinha(s) \u00C1rea(s) de Servi\u00E7o(s) Conjugada(s)", 
                 "Cozinha(s) Americana (s)", 
                 "Despensa(s) Dep\u00F3sito(s)", 
                 "Escrit\u00F3rio (s) / Gabinete(s)", 
                 "Hall(s) de Circula\u00E7\u00E3o", 
                 "Lavabo", 
                 "Mezanino(s)",
                 "Quarto(s) (dormit\u00F3rios)", 
                 "Quarto(s) de Empregada", 
                 "Sacada(s) Varanda(s)", 
                 "Sala(s)", 
                 "Terra\u00E7o(s)", 
                 "Outros Compartimentos", 
                 "Paredes (internas para UP e externas para UIC", 
                 "Pisos Partes Molhadas", 
                 "Paredes Partes Molhadas", 
                 "Pisos Partes Secas", 
                 "Teto da UIC",
                 "Cobertura da UIC",
                 "Acabamento Fachada Principal da Edifica\u00E7\u00E3o", 
                 "Acabamento Demais Fachadas da Edifica\u00E7\u00E3o",
                 "Esquadrias Fachada Principal da Edifica\u00E7\u00E3o", 
                 "Esquadrias Demais Fachadas da Edifica\u00E7\u00E3o", 
                 "Padr\u00E3o de acabamento da Unidade", 
                 "Estado de Conserva\u00E7\u00E3o da Unidade",
                 "Idade Estimada da edifica\u00E7\u00E3o como um todo", 
                 "Vida \u00datil Remanescente da edifica\u00E7\u00E3o como um todo",
                 "Posi\u00E7\u00E3o F\u00EDsica da UP", 
                 "Implanta\u00E7\u00E3o da UIC no terreno", 
                 "Andar da UP", 
                 "N\u00ba de Unidades no Andar da UP", 
                 "A UP \u00E9 de cobertura ?", 
                 "Identifica\u00E7\u00E3o dos Pavimentos da UP", 
                 "N\u00ba de Subsolos  da UIC", "N\u00ba de T\u00E9rreos  da UIC", 
                 "N\u00ba de Mezanimos  da UIC", 
                 "N\u00ba de Sobrelojas  da UIC", 
                 "N\u00ba Pavimentos outros  da UIC",
                 "N\u00ba de Coberturas  da UIC", 
                 "N\u00ba Total de Pavimentos da UIC ou UP", 
                 "N\u00ba de pavimentos do Pr\u00E9dio (UP)", 
                 "N\u00ba de unidades do pr\u00E9dio (UP)", 
                 "N\u00ba de subsolos do pr\u00E9dio (UP)", 
                 "N\u00ba de elevadores do pr\u00E9dio (UP) ou da UIC",
                 "Caracteriza\u00E7\u00E3o da cobertura do pr\u00E9dio (UP)", 
                 "Estado de Conserva\u00E7\u00E3o do pr\u00E9dio (UP)", 
                 "Destina\u00E7\u00E3o do pr\u00E9dio (UP)", 
                 "Exist\u00EAncia de Lojas no T\u00E9rreo do Pr\u00E9dio (UP)",
                 "Exist\u00EAncia de Salas no T\u00E9rreo do Pr\u00E9dio (UP)", 
                 "Exist\u00EAncia de Pilotis no T\u00E9rreo do Pr\u00E9dio (UP)",
                 "Exist\u00EAncia de Lazer no T\u00E9rreo do Pr\u00E9dio (UP)", 
                 "Exist\u00EAncia de Unidades Residenciais no T\u00E9rreo do Pr\u00E9dio (UP)", 
                 "Exist\u00EAncia de Garagens no T\u00E9rreo do Pr\u00E9dio (UP)", 
                 "Exist\u00EAncia de Outros Elementos no T\u00E9rreo do Pr\u00E9dio  (UP)", 
                 "Pisos \u00C1reas de Circula\u00E7\u00E3o do Pr\u00E9dio (UP)", 
                 "Paredes \u00C1reas de Circula\u00E7\u00E3o do Pr\u00E9dio (UP)", 
                 "Padr\u00E3o de Acabamento do Pr\u00E9dio (UP)", 
                 "V\u00EDcios de Constru\u00E7\u00E3o Graves na Edifica\u00E7\u00E3o", 
                 "Outros Danos Graves na Edifica\u00E7\u00E3o", 
                 "Estabilidade e Solidez da Edifica\u00E7\u00E3o", 
                 "Condi\u00E7\u00F5es de Habitabilidade da Edifica\u00E7\u00E3o", 
                 "\u00C1rea do Terreno (m2)", 
                 "Testada do Terreno (m)", 
                 "Profundidade Equivalente do Terreno (m)", 
                 "Lados do Terreno", 
                 "Posi\u00E7\u00E3o do Terreno", 
                 "Cota/Greide", 
                 "Formato Aproximado do  Terreno", 
                 "Inclina\u00E7\u00E3o do Terreno", 
                 "Exist\u00EAncia de  Cercamento no Terreno", 
                 "Tipo de Cercamento na(s) Frente(s)", 
                 "Tipo de Cercamento nas laterais e fundos", 
                 "Voca\u00E7\u00E3o Principal do Terreno", 
                 "Destina\u00E7\u00F5es do terreno", 
                 "Nivel de Restri\u00E7\u00E3o de Uso do terreno", 
                 "Superf\u00EDcie do terreno", 
                 "Exist\u00EAncia Conten\u00E7\u00E3o no terreno", 
                 "Exist\u00EAncia de Terraplanagem no terreno", 
                 "Exist\u00EAncia de Pavimenta\u00E7\u00E3o no terreno", 
                 "Exist\u00EAncia de Esgoto Sanit\u00E1rio no terreno", 
                 "Exist\u00EAncia Esgoto Pluvial no terreno", 
                 "Exist\u00EAncia de \u00C1gua Pot\u00E1vel no terreno", 
                 "Exist\u00EAncia de  Energia El\u00E9trica no Terreno",
                 "Exist\u00EAncia de  Ilumina\u00E7\u00E3o no Terreno", 
                 "Exist\u00EAncia de Projeto Aprovado para o Terreno",
                 "Exist\u00EAncia de Outras benfeitorias no terreno", 
                 "Restri\u00E7\u00F5es F\u00EDsicas para aproveitamento do terreno (%)",
                 "CAB (%) - Coeficiente de Aproveitamento B\u00E1sico", 
                 "CAM (%) - Coeficiente de Aproveitamento M\u00E1ximo", 
                 "Gabarito M\u00E1ximo (m)", 
                 "Gabarito M\u00E1ximo (N\u00ba de Pavimentos)", 
                 "Taxa de Ocupa\u00E7\u00E3o M\u00E1xima (%)", 
                 "N\u00ba de Frentes do Terreno", 
                 "Valor de Benfeitorias Residuais (R$)", 
                 "Fra\u00E7\u00E3o Ideal da Unidade", 
                 "Taxa de Condom\u00EDnio da Unidade", 
                 "N\u00ba de Pr\u00E9dios no Empreendimento", 
                 "N\u00ba Total de Unidades no Empreendimento", 
                 "Exist\u00EAncia de Pr\u00E9dios no Empreendimento", 
                 "Exist\u00EAncia de Unidades Isoladas", 
                 "Exist\u00EAncia de Equipamentos de Lazer no Empreendimento", 
                 "Exist\u00EAncia Garagens/Estacionamentos na \u00C1rea do Empreendimento", 
                 "Exist\u00EAncia de  Arruamento do Empreendimento", 
                 "Exist\u00EAncia de Com\u00E9rcio no Empreendimento", 
                 "Exist\u00EAncia de Lotes Vagos no Empreendimento", 
                 "Exist\u00EAncia de Outros Elementos no Empreendimento", 
                 "Exist\u00EAncia de \u00C1gua pot\u00E1vel no Empreendimento", 
                 "Exist\u00EAncia de Esgoto Sanit\u00E1rio no Empreendimento", 
                 "Exist\u00EAncia de Energia El\u00E9trica no Empreendimento", 
                 "Exist\u00EAncia de Telefone no Empreendimento", 
                 "Exist\u00EAncia de Pavimenta\u00E7\u00E3o no Empreendimento", 
                 "Exist\u00EAncia de Esgoto Pluvial no Empreendimento", 
                 "Exist\u00EAncia de G\u00E1s Canalizado no Empreendimento", 
                 "Exist\u00EAncia de Ilumina\u00E7\u00E3o Publica no Empreendimento", 
                 "Padr\u00E3o do Empreendimento", 
                 "\u00C1rea (m2) do Terreno do Empreendimento", 
                 "Testada (m) do Terreno do Emprendimento", 
                 "Profundidade Equivalente (m) do Terreno do Empreendimento", 
                 "Posi\u00E7\u00E3o do Empreendimento", 
                 "Cota / Greide no Empreendimento", 
                 "Exist\u00EAncia de Cercamento no Empreendimento", 
                 "Tipo de Cercamento na Frente  Empreendimento", 
                 "Tipo de Cercamento nas Laterais/Fundos no Empreendimento",
                 "Controle de Acesso ao Empreendimento", 
                 "Im\u00F3vel Real ou Parad\u00EDgma Aparenta Conformidade Com a Documenta\u00E7\u00E3o", 
                 "Manifesta\u00E7\u00E3o Sobre a Possibilidade de Aceita\u00E7\u00E3o como Garantia", 
                 "Atendimento aos Requisitos Minimos",
                 "M\u00E9todo Utilizado na Avalia\u00E7\u00E3o", 
                 "Precis\u00E3o Obtida", 
                 "Fundamenta\u00E7\u00E3o Atingida", 
                 "Performance do Mercado Percebida", 
                 "N\u00EDvel de Ofertas Verificado no Segmento de Mercado",
                 "Liquidez Inferida Para o Im\u00F3vel", 
                 "Situa\u00E7\u00E3o Considerada para Efeito de Avalia\u00E7\u00E3o",
                 "Tipo de Valor Definido", 
                 "Data de Ref\u00EAncia da Avalia\u00E7\u00E3o", 
                 "\u00C1rea considerada para avalia\u00E7\u00E3o (m2)", 
                 "R$/m2", 
                 "Produto \u00C1rea X  R$/m2", 
                 "\u00ectem 1 da Avalia\u00E7\u00E3o Itemizada", 
                 "Valor item 1",
                 "\u00ectem 2 da Avalia\u00E7\u00E3o Itemizada",
                 "Valor item 2", 
                 "\u00ectem 3 da Avalia\u00E7\u00E3o Itemizada", 
                 "Valor item 3", 
                 "\u00ectem 4 da Avalia\u00E7\u00E3o Itemizada", 
                 "Valor item 4", 
                 "\u00ectem 5 da Avalia\u00E7\u00E3o Itemizada", 
                 "Valor item 5", 
                 "Somat\u00F3rio dos Itens (Avalia\u00E7\u00E3o Itemizada)",
                 "Valor de Avalia\u00E7\u00E3o (R$)", 
                 "Valor M\u00EDnimo Admiss\u00EDvel (conforme NBR 14.653-2) R$", 
                 "Valor M\u00E1ximo Admiss\u00EDvel (conforme NBR 14.653-2) R$", 
                 "N\u00FAmero da Matr\u00EDcula da Unidade", 
                 "Of\u00EDcio/Zona da Matr\u00EDcula da Unidade", 
                 "UF da Matr\u00EDcula da Unidade", 
                 "Comarca da Matr\u00EDcula da Unidade", 
                 "Data de Refer\u00EAncia da Certid\u00E3o de Matr\u00EDcula da Unidade", 
                 "N\u00FAmero da Matr\u00EDcula M\u00E3e do Empreendimento", 
                 "Of\u00EDcio/Zona da Matr\u00EDcula M\u00E3e do Empreendimento", 
                 "UF da Matr\u00EDcula M\u00E3e do Empreendimento", 
                 "Comarca da Matr\u00EDcula M\u00E3e do Empreendimento", 
                 "Data de Refer\u00EAncia da Certid\u00E3o da Matr\u00EDcula M\u00E3e do Empreendimento", 
                 "Informa\u00E7\u00F5es Complementares Oriundas das Op\u00E7\u00F5es 'Outros' Assinaladas no Formul\u00E1rio", 
                 "Demais Informa\u00E7\u00F5es Complementares", 
                 "Nome do Respons\u00E1vel T\u00E9cnico (RT)", 
                 "Forma\u00E7\u00E3o/cargo do RT", "CREA / CAU / Mar\u00EDcula do RT",
                 "CPF do RT", 
                 "Empresa", 
                 "CNPJ", 
                 "Nome do Representante Legal (RL)", 
                 "CPF do RL", 
                 "Refer\u00EAncia-Descri\u00E7\u00E3o das Fotos Anexadas", 
                 "Nome da Fonte da Informa\u00E7\u00E3o", 
                 "Telefone da Fonte da Informa\u00E7\u00E3o",
                 "E-mail / Site da Fonte da Informa\u00E7\u00E3o", 
                 "Classifica\u00E7\u00E3o da Fonte da Informa\u00E7\u00E3o", 
                 "Tipo de Informa\u00E7\u00E3o", 
                 "Tipo de Neg\u00F3cio", 
                 "Valor R$", 
                 "Data do Evento", 
                 "Situa\u00E7\u00E3o da Pe\u00E7a T\u00E9cnica", 
                 "Estado Conserva\u00E7\u00E3o Empreendimento",
                 "SIAPF", 
                 "Tipologia", 
                 "Qtde Unidades Tipologia", 
                 "Versao", 
                 "Data Finaliza\u00E7\u00E3o", 
                 "Data da Atualiza\u00E7\u00E3o")

simil_selected <- c("CEP",
                    "UF",
                    "Munic\u00EDpio","Logradouro",
                    "N\u00ba", "N\u00ba da Unidade", 
                    "T\u00EDtulo do Empreendimento", 
                    "Nome do Empreendimento",
                    "Complementos", 
                    
                    "Coordenadas Grau Decimal", 
                    "Latitude", 
                    "Hemisf\u00E9rio", 
                    "Longitude",
                    
                    "\u00C1rea Privativa da Unidade  Averbada (m2)", 
                    "\u00C1rea Privativa Descont\u00EDnua Averbada  (m2)", 
                    "\u00C1rea Comum Averbada  (m2)", 
                    "\u00C1rea Averbada Total (m2)", 
                    "\u00C1rea Privativa da Unidade  N\u00E3o Averbada  (m2)", 
                    "\u00C1rea Privativa Descont\u00EDnua N\u00E3o Averbada (m2)", 
                    "\u00C1rea Comum N\u00E3o Averbada (m2)", 
                    "\u00C1rea N\u00E3o Averbada Total (m2)", 
                    "\u00C1rea Privativa Total da Unidade (m2)", 
                    "\u00C1rea Privativa Descont\u00EDnua Total (m2)", 
                    "\u00C1rea Comum Total (m2)", 
                    "Total Constru\u00EDda (m2)", 
                    "\u00C1rea Privativa Descoberta (m2)", 
                    "Vagas  livres/ Independentes Cobertas", 
                    "Vagas  Bloqueadas/ Bloqueantes Cobertas", 
                    "Total de Vagas Cobertas", 
                    "Vagas  livres / Independentes Descobertas", 
                    "Vagas  Bloqueadas/ Bloqueantes Descobertas", 
                    "Total de Vagas Descobertas", 
                    "Total Geral de Vagas", 
                    "\u00C1rea(s) de Servi\u00E7o", 
                    "\u00C1rea(s) de Servi\u00E7o Descoberta(s)",
                    "Banheiro(s) Privativo (s)", 
                    "Banheiro(s) Privativo(s) de Empregada", 
                    "Banheiro(s) de Servi\u00E7o", 
                    "Banheiro(s) Social(ais)",
                    "Quarto(s) (dormit\u00F3rios)", 
                    "Quarto(s) de Empregada", 
                    "Sacada(s) Varanda(s)", 
                    "Sala(s)", 
                    "\u00C1rea considerada para avalia\u00E7\u00E3o (m2)", 
                    "R$/m2", 
                    "Produto \u00C1rea X  R$/m2", 
                    "Valor de Avalia\u00E7\u00E3o (R$)")



check_prepare_simil <- function(df, 
                                session, 
                                is_simil, 
                                simil_variables, 
                                simil_peca, 
                                simil_tipologia, 
                                data_inicio = NA,
                                data_fim = NA,
                                simil_excluir_sem_data_final) {
  . <- `Longitude Oeste` <- Latitude <- `Coordenadas Grau Decimal` <-  NULL
  
  
  is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
  if (is_saved) return(df)
  
  if (!is_simil) { return(df) }
  
  
  
  msg <- shiny::HTML(paste("Foi detectada base de dados proveniente do Banco de dados SIMIL"))
  
  id <- shiny::showNotification(
    ui = "Base do SIMIL detectada!",
    type = "message",
    duration = 2,
    closeButton = TRUE)
  
  
  
  
  
  df <- df %>% 
    
    dplyr::rename(
      Latitude_GMS = Latitude,
      Longitude_GMS = `Longitude Oeste`
    ) %>%
    
    tidyr::separate(
      col = `Coordenadas Grau Decimal`,
      into =  c("Latitude", "Longitude"),
      sep = ",",
      remove = FALSE,
      convert = TRUE)
  
  
  
  # Filtro de Pe\u00E7a tecnica
  
  if (shiny::isTruthy(simil_peca)) {
    
    df <- df %>% dplyr::filter_at("Tipo de Pe\u00E7a T\u00E9cnica",  dplyr::any_vars(. %in% simil_peca))
    
    
    
  }
  
  # FIltro de Tipologia
  if (shiny::isTruthy(simil_tipologia)) {
    
    df <- df %>% dplyr::filter_at("Grupo de Im\u00F3veis",  dplyr::any_vars(. %in% simil_tipologia))
    
    
  }
  
  
  
  # FIltrod e Data
  data_finalizacao <- df[["Data Finaliza\u00E7\u00E3o"]] %>% lubridate::dmy_hms()
  
  date_test <- data_finalizacao >= data_inicio & data_finalizacao <= data_fim
  
  if (simil_excluir_sem_data_final) {
    
    date_test[is.na(date_test)] <- FALSE
    
  } else {
    
    date_test[is.na(date_test)] <- TRUE
  }
  
  df[["Data Finaliza\u00E7\u00E3o"]] <- data_finalizacao
  
  df <- df[date_test, ]
  
  
  
  # Filtro de coluna
  if (shiny::isTruthy(simil_variables)) {
    
    i <- simil_variables == "Longitude Oeste"
    simil_variables[i] <- "Longitude"
    
    df <- df[, c(simil_variables), drop = FALSE]
    
  }
  
  df
  
}


set_geo <- function(df, 
                    session, 
                    standart_epsg,
                    filter_lat, 
                    filter_lng, 
                    filter_epsg) {
  
  is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
  if (is_saved) return(df)
  
  
  
  if (standart_epsg) {  
    
    df[["EPSG"]] <- 4326 
    
  }
  
  
  
  
  nms <- names(df)
  
  lat_pattern <- "^(?i)latitude(?-i)$"
  lng_pattern <- "^(?i)longitude(?-i)$"
  epsg_pattern <- "^(?i)epsg(?-i)$"
  
  lat_column  <- stringr::str_detect(nms, lat_pattern)
  lng_column  <- stringr::str_detect(nms, lng_pattern)
  epsg_column <- stringr::str_detect(nms, epsg_pattern)
  
  n_lat  <- sum(lat_column) 
  n_lng  <- sum(lng_column) 
  n_epsg <- sum(epsg_column) 
  
  if (n_lat == 0) { return(df) }
  if (n_lng == 0) { return(df) }
  if (n_epsg == 0) { return(df) }
  
  
  if (n_lat > 1) {
    
    shiny::showNotification(
      ui = "O arquivo possui mais de uma coluna relacionada \u00E0 Latitude. Para que o arquivo seja tratado como georreferenciado, essa especifica\u00E7\u00E3o n\u00E3o deve estar duplicada.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  if (n_lng > 1) {
    
    shiny::showNotification(
      ui = "O arquivo possui mais de uma coluna relacionada \u00E0 Longitude. Para que o arquivo seja tratado como georreferenciado, essa especifica\u00E7\u00E3o n\u00E3o deve estar duplicada.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  if (n_epsg > 1) {
    
    shiny::showNotification(
      ui = "O arquivo possui mais de uma coluna relacionada ao EPSG. Para que o arquivo seja tratado como georreferenciado, essa especifica\u00E7\u00E3o n\u00E3o deve estar duplicada.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  
  nm_lat <- stringr::str_subset(nms,  lat_pattern)
  nm_lng <- stringr::str_subset(nms,  lng_pattern)
  nm_epsg <- stringr::str_subset(nms, epsg_pattern) # extrai o nome da coluna epsg
  
  if (!identical(nm_epsg, character(0))) {
    
    epsg_dif <- dplyr::n_distinct(df[[nm_epsg]]) # quanitdade de epsg diferentes
    
  } else {
    
    epsg_dif <- NA
    
  }
  
  if (epsg_dif > 1) {
    
    shiny::showNotification(
      ui = "A coluna EPSG apresenta mais de um valor distinto. Para que o arquivo seja tratado como georreferenciado, s\u00F3 pode haver um \u00FAnico valor nessa coluna.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  
  lat_NA_check <- df[[nm_lat]] %>% is.na()
  lng_NA_check <- df[[nm_lng]] %>% is.na()
  epsg_NA_check <- df[[nm_epsg]] %>% is.na()
  
  if (  all(lat_NA_check | lng_NA_check | epsg_NA_check) ) {
    
    shiny::showNotification(
      ui = "N\u00E3o restar\u00E3o dados ap\u00F3s a filtragem por Latitude/Longitude/EPSG vazios. A base n\u00E3o ser\u00E1 tratada como georreferenciada",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
  }
  
  
  
  lat_NA_check <- df[[nm_lat]] %>% is.na()
  
  if (filter_lat) {
    
    df <- df[!lat_NA_check, ]
    
  }
  
  lng_NA_check <- df[[nm_lng]] %>% is.na()
  
  if (filter_lng) {
    
    df <- df[!lng_NA_check, ]
    
  }
  
  epsg_NA_check <- df[[nm_epsg]] %>% is.na()
  
  if (filter_epsg) {
    
    df <- df[!epsg_NA_check, ]
    
  }
  
  
  
  
  
  
  
  
  lat_NA_check <- df[[nm_lat]] %>% is.na()
  lng_NA_check <- df[[nm_lng]] %>% is.na()
  epsg_NA_check <- df[[nm_epsg]] %>% is.na()
  
  if (any(lat_NA_check)) {
    
    shiny::showNotification(
      ui = "Existem valores ausentes na coluna Latitude. Para que o arquivo seja tratado como georreferenciado, todos os pontos devem possuir informa\u00E7\u00F5es de Latitude.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  if (any(lng_NA_check)) {
    
    shiny::showNotification(
      ui = "Existem valores ausentes na coluna Latitude. Para que o arquivo seja tratado como georreferenciado, todos os pontos devem possuir informa\u00E7\u00F5es de Latitude.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  if (any(epsg_NA_check)) {
    
    shiny::showNotification(
      ui = "Existem valores ausentes na coluna Latitude. Para que o arquivo seja tratado como georreferenciado, todos os pontos devem possuir informa\u00E7\u00F5es de Latitude.",
      type = "message",
      duration = NULL,
      closeButton = TRUE)
    
    return(df)
    
  }
  
  
  # SEGUNDO IF: PLANILHA COM INFO ESPACIAIS VALIDAS -------------------------
  
  ## COLUNA EXISTENTE E UNICA DE latitude. VERIFICAR GRAFIA
  ## COLUNA EXISTENTE E UNICA DE longitude. VERIFICAR GRAFIA
  ## COLUNA EXISTENTE E UNICA DE epsg. VERIFICAR GRAFIA
  ## COLUNA epsg COM UM UNICO VALOR
  ## INEXISTENCIA DE VALORES nulos E na NA latitude E longitude
  
  epsg <- as.numeric(unique(df[[nm_epsg]])) # pega o valor de epsg e trata como numerico
  
  # cria-se o objeto sf
  df <- sf::st_as_sf(df, coords = c(nm_lng, nm_lat), remove = FALSE)
  
  # atribui-se o CRS (sistema de coordenadas de referencia)
  df <- df %>% sf::st_set_crs(epsg) %>% sf::st_transform(4326)
  
  df
  
}


remove_geo <- function(df) {
  
  df$geometry <- NULL
  
  df
  
}

create_key_column <- function(df) {
 
  
 
  
  df[["Elemento"]] <- seq_len(NROW(df)) %>% as.character()
  
  col_idx <- grep("Elemento", names(df))
  
  df[, c(col_idx, (1:ncol(df))[-col_idx])] 
  
}


remove_key_column <- function(df) {
  
  
  df[["Elemento"]] <- NULL
  
  df
  
}

get_non_structural_names <- function(df) {
  
  nms <- names(df)
  
  i <- nms %in% c("Elemento", "geometry")
  
  nms[!i]
  
}





get_numeric_names <- function(df) {
  
  nms <- df %>% dplyr::select_if(is.numeric) %>% names()
  
  i <- nms %in% "geometry"
  
  nms[!i]
  
}


# Propriedades ------------------------------------------------------------



start_properties <- function(df, prop) {
 
  is_saved <- attr(df, "saved_file") %>% is.null() %>% `!`
  
  if (is_saved) {
    
    recover <- attr(df, "properties")
    
    
    # prop$obs_disabled        <- recover$obs_disabled
    # prop$obs_disabled_ae     <- recover$obs_disabled_ae
    # 
    # prop$var_dependent       <- recover$var_dependent
    # prop$var_enabled         <- recover$var_enabled
    # prop$var_trns_possible   <- recover$var_trns_possible
    # prop$var_trns_selected   <- recover$var_trns_selected
    # 
    # prop$var_nbr_type        <- recover$var_nbr_type
    # prop$var_description     <- recover$var_description
    # prop$var_expect_behavior <- recover$var_expect_behavior
    # 
    # prop$var_trns_for_search <- recover$var_trns_for_search
    # prop$var_trns_searched   <- recover$var_trns_searched
    # 
    # prop$geo_model           <- recover$geo_model
    # prop$geo_influence       <- recover$geo_influence
    # prop$geo_shp             <- recover$geo_shp
    # 
    # prop$predict_data        <- prop$predict_data
    # 
    # prop$model_description   <- prop$model_description
    # prop$model_date_declared <- prop$model_date_declared
    # prop$model_defined       <- recover$model_defined
    # prop$file_source         <- recover$file_source
    # prop$date_init           <- recover$date_init
    # prop$author              <- recover$author
    
    
    for (i in names(recover)) {
      
      prop[[i]] <- recover[[i]]
      
      
    }
    
   
  } else {
    
    df <- df %>% remove_geo()
    . <- NULL
    nms <- names(df)
    n_var <- NCOL(df)
    n_obs <- NROW(df)

    prop$obs_disabled        <- vector("logical", n_obs)
    prop$obs_disabled_ae     <- vector("logical", n_obs)

    prop$var_dependent       <- NA_character_

    prop$var_enabled         <- structure(vector("logical", n_var), names = nms)
    prop$var_trns_possible   <- lapply(df, trns_possible)
    prop$var_trns_selected   <- structure(rep("none", n_var), names = nms) %>% c(.,"(Intercept)" = "none")

    prop$var_nbr_type        <- structure(vector("character", n_var), names = nms)
    prop$var_description     <- structure(vector("character", n_var), names = nms)
    prop$var_expect_behavior <- structure(vector("character", n_var), names = nms)


    prop$var_trns_for_search <- list()
    prop$var_trns_searched   <- NA

    prop$geo_model           <- list()
    prop$geo_influence       <- list()
    prop$geo_shp             <- list()

    prop$predict_data        <- list()

    prop$model_description   <- ""
    prop$model_date_declared <- NA
    prop$model_defined       <- 0
    prop$file_source         <- NA_character_
    prop$date_init           <- as.character(Sys.time())
    prop$author              <- Sys.getenv()

  }
 
  prop
} 

prop_add_new_var <- function(df, prop, var_name) {
  
  for (i in var_name) {
    
    x <- df[[i]]
    
    prop$var_enabled[[i]] <- FALSE
    prop$var_trns_possible[[i]] <- trns_possible(x)
    prop$var_trns_selected[[i]] <- "none" 
    
    prop$var_nbr_type[[i]] <- ""
    prop$var_description[[i]] <- ""
    prop$var_expect_behavior[[i]] <- ""
    
  }
  
  prop
  
}


prop_update_var <- function(df, prop, var_name) {
  
  for (i in var_name) {
    
    x <- df[[i]]
    
    # atualizacao das transformadas possiveis
    prop$var_trns_possible[[i]] <- trns_possible(x)
    
    # atualizacao da transformada selecionadas em funcao das novas opcoes de
    # transofmradas possiveis. Se a selecionada vigente nao estiver entre as
    # novas possiveis, atribui "none", se estiver, mantem
    
    selected <- prop$var_trns_selected[[i]] 
    if ( !(selected %in% prop$var_trns_possible[[i]]) ) { 
      
      prop$var_trns_selected[[i]] <- "none" 
      
    } 
  }
  
  prop
  
}


prop_rem_var <- function(df, prop, var_name) {
  
  prev <- prop$var_enabled
  
  for (i in var_name) {
    
    index <- match(i, names(prop$var_enabled))
    
    prop$var_enabled <- prop$var_enabled[-index]
    prop$var_trns_possible <- prop$var_trns_possible[-index]
    prop$var_trns_selected <- prop$var_trns_selected[-index]
    
    prop$var_nbr_type <- prop$var_nbr_type[-index]
    prop$var_description <- prop$var_description[-index]
    prop$var_expect_behavior <- prop$var_expect_behavior[-index]
    
  }
  
  prop
  
}


data_update_reload <- function(new_data, data, prop, vars ) {
  
  
  ncol_new <- length(new_data)
  ncol_old <- length(data$main) 
  
  
  if (!is.list(vars) & (ncol_new > ncol_old)) {
    #Adicao de variaveis
    # enquadram-se aqui:
    # -  a criacao de uma ou mais variaveis
    
    action <- "add"
    
    prop_add_new_var(new_data, prop, vars)
    
    
  } else if (!is.list(vars) & (ncol_new < ncol_old)) {
    # exclusao de variaveis
    # enquadram-se aqui:
    # - a exclusao de uma ou mais variaveis
    
    action <- "remove"
    
    prop_rem_var(data, prop, vars)
    
    
  } else if (!is.list(vars) & (ncol_new == ncol_old)) {
    # atualizacao de variaveis
    # enquadram-se aqui:
    # - alteracao do valor d eum dado em especifico em uma ou mais variaveis
    # - alteracao de uma variavel
    
    action <- "update"
    
    prop_update_var(new_data, prop, vars)
    
    
  } else if (is.list(vars) & vars$action == "exclude_data_filtered") {
    # refazer as transformadas das numericas
    # atualizar a lista de Elementos
    # Atualizar as prop habilitadas
    # enquadram-se aqui:
    # - Adicao de dados
    # - Exclusao de dados
    
    
    # atualizar as transformadas das numericas
    nms <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, nms)
    
    #atualizar a lista Elementos com os novos nomes
    
    new_data[["Elemento"]] <- seq_len(NROW(new_data))
    #atualizar a lista de obs_enables
    prop$obs_disabled <- prop$obs_disabled[vars$indexes]
    prop$obs_disabled_ae <- prop$obs_disabled_ae[vars$indexes]
    
  } else if (is.list(vars) & vars$action == "exclude_data_non_filtered") {
    # refazer as transformadas das numericas
    # atualizar a lista de Elementos
    # Atualizar as prop habilitadas
    # enquadram-se aqui:
    # - Adicao de dados
    # - Exclusao de dados
    
    
    # atualizar as transformadas das numericas
    nms <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, nms)
    
    #atualizar a lista Elementos com os novos nomes
    
    new_data[["Elemento"]] <- seq_len(NROW(new_data))
    #atualizar a lista de obs_enables
    prop$obs_disabled <- prop$obs_disabled[vars$indexes]
    prop$obs_disabled_ae <- prop$obs_disabled_ae[vars$indexes]
    
  }  else if (is.list(vars) & vars$action == "enable_obs") { 
    # refazer as transformadas das numericas 
    # atualizar a prop$obs_enable
    # enquadram-se aqui:
    # - Desabilitacao de dados
    
    # atualizar a prop$obs_disable
    
    prop$obs_disabled[vars$indexes] <- FALSE
    # CRIAR O HABILITAR
    
    # atualizar as transformadas das numericas
    vars <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, vars)
    
    
  } else if (is.list(vars) & vars$action == "disable_obs") { 
    # refazer as transformadas das numericas 
    # atualizar a prop$obs_enable
    # enquadram-se aqui:
    # - Desabilitacao de dados
    
    # atualizar a prop$obs_disable
    
    prop$obs_disabled[vars$indexes] <- TRUE
    # CRIAR O HABILITAR
    
    # atualizar as transformadas das numericas
    vars <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, vars)
    
    
  } else if (is.list(vars) & vars$action == "enable_obs_only") { 
    # refazer as transformadas das numericas 
    # atualizar a prop$obs_enable
    # enquadram-se aqui:
    # - Desabilitacao de dados
    
    # atualizar a prop$obs_disable
    prop$obs_disabled[vars$indexes] <- FALSE
    prop$obs_disabled[!vars$indexes] <- TRUE
    
    # CRIAR O HABILITAR
    
    # atualizar as transformadas das numericas
    vars <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, vars)
    
    
  } else if (is.list(vars) & vars$action == "disable_obs_only") { 
    # refazer as transformadas das numericas 
    # atualizar a prop$obs_enable
    # enquadram-se aqui:
    # - Desabilitacao de dados
    
    # atualizar a prop$obs_disable
    prop$obs_disabled[vars$indexes] <- TRUE
    prop$obs_disabled[!vars$indexes] <- FALSE
    
    # CRIAR O HABILITAR
    
    # atualizar as transformadas das numericas
    vars <- get_numeric_names(new_data)
    
    action <- "update"
    
    prop_update_var(new_data, prop, vars)
    
    
  } else if (is.list(vars) & vars$action == "rename") {
    # Altera o nome da var nas listas de propriedades
    # enquadram-se aqui:
    # - renomear variavel
    
    prop_to_change <- c(
      "var_enabled",
      "var_trns_possible",
      "var_trns_selected",
      "var_nbr_type",
      "var_description",
      "var_expect_behavior",
      "var_trns_for_search",
      "var_trns_searched"
      
    )
    old_name <- vars$old_name
    new_name <- vars$new_name
    
    for (prop_name in prop_to_change) {
      
      nms <- names(prop[[prop_name]])
      i <- which(nms %in% old_name) 
      
      names(prop[[prop_name]])[i] <- new_name
      
    }
    
    action <- vars$action
    data$modified_vars <- new_name
    
  } 
  
  
  data$modified_vars <- vars
  data$action <- action
  data$main <- new_data
  data$reload <- stats::rnorm(1)
  
}


# FUNCOES DE TRANSFORMACAO ------------------------------------------------

lista_transf <- list(
  none = function(x) {x},
  inverse = function(x) {1/x},
  log_nep = function(x) {log(x)},
  quadratic = function(x) {x^2},
  quadra_inverse = function(x) {1/(x^2)},
  sqrt = function(x){(x^(1/2))},
  sqrt_inverse = function(x) {1/(x^(1/2))},
  dic_grupo = function(x) {as.character(x)}
)


anti_transf <- list(
  none = function(x) {x},
  inverse = function(x) {1/x},
  log_nep = function(x) {exp(x)},
  quadratic = function(x) {x^(1/2)},
  quadra_inverse = function(x) {1/(x^(1/2))},
  sqrt = function(x){(x^(2))},
  sqrt_inverse = function(x) {1/(x^(2))},
  dic_grupo = function(x) {as.character(x)}
)



all_trns <- function() {
  
  c(  "x"         = 'none',
      "1/x"       = 'inverse',
      "ln(x)"     = 'log_nep',
      "x\u00B2"   = 'quadratic',
      "1/x\u00B2" = 'quadra_inverse',
      "x\u00BD"   = 'sqrt',
      "1/x\u00BD" = 'sqrt_inverse'
      # "Dicotomica de grupo" = 'dic_grupo'
  )
  
}

all_trns2 <-  function() {
  c("none"= "x"  , 
    "inverse" = "1/x" , 
    "log_nep" = "ln(x)" , 
    "quadratic"= "x\u00B2"  , 
    "quadra_inverse"  = "1/x\u00B2" , 
    "sqrt"= "x\u00BD"  , 
    "sqrt_inverse" = "1/x\u00BD" 
  )
}


trns_possible <- function(x) {
  
  # se nao for numerica
  if (!is.numeric(x)) { return( c("x" = 'none') )}
  
  # se for dicotomica
  if (all(x %in% c(0,1))) { return( c("x" = 'none') ) }
  
  
  permitted <- all_trns()
  
  # se o vetor contiver 0, retiram-se as seguintes transformadas
  if (any(x == 0, na.rm = TRUE)) {
    
    not_permitted <- c(
      "inverse",
      "log_nep",
      "quadra_inverse",
      "sqrt_inverse"
    )
    
    permitted <- permitted[!(permitted %in% not_permitted)]
    
  }
  
  
  # se o vetor contiver valores negativos, retiram-se as seguintes transformadas
  if (any(sign(x) == -1, na.rm = TRUE)) {
    
    not_permitted <- c(
      
      "log_nep",
      "sqrt",
      "sqrt_inverse"
    )
    
    permitted <- permitted[!(permitted %in% not_permitted)]
  }
  
  permitted
  
}


nome_transf <-  list(
  none = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    x
  },
  inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/", x)
    paste(x, "na 1/x")
    
  },
  log_nep = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("log(", x, ")")
    paste(x, "na ln(x)")
  },
  quadratic = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "\u00B2")
    paste(x, "na x\u00B2")
  },
  quadra_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")\u00B2")
    paste(x, "na 1/x\u00B2")
  },
  sqrt = function(x){
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "^1/2")
    paste(x, "na \u221Ax")
  },
  sqrt_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")^1/2")
    paste(x, "na 1/\u221Ax")
  },
  
  "(Intercept)" = function(x) {
    "Intercepto"
  },
  dic_grupo = function(x) {""}
)


nome_transf2 <-  list(
  none = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    x
  },
  inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/", x)
    paste0("1/(", x, ")")
    
  },
  log_nep = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("log(", x, ")")
    paste0( "ln(", x, ")")
  },
  quadratic = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "\u00B2")
    paste0("(", x, ")\u00B2")
  },
  quadra_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")\u00B2")
    paste0("1/(",x, ")\u00B2")
  },
  sqrt = function(x){
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "^1/2")
    paste0("(", x, ")^1/2")
  },
  sqrt_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")^1/2")
    paste0("1/\u221A(", x, ")")
  },
  
  "(Intercept)" = function(x) {
    "Intercepto"
  },
  dic_grupo = function(x) {""}
)


anti_nome_transf <-  list(
  none = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    x
  },
  inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/", x)
    paste0("1/(", x, ")")
    
  },
  log_nep = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("log(", x, ")")
    paste0( "exp^(", x, ")")
  },
  quadratic = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "\u00B2")
    paste0("(", x, ")^1/2")
  },
  quadra_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")\u00B2")
    paste0("1/(",x, ")^1/2")
  },
  sqrt = function(x){
    x <- stringr::str_replace_all(x, "`", "")
    #paste0(x, "^1/2")
    paste0("(", x, ")^2")
  },
  sqrt_inverse = function(x) {
    x <- stringr::str_replace_all(x, "`", "")
    #paste0("1/(", x, ")^1/2")
    paste0("1/(", x, ")^1/2")
  },
  
  "(Intercept)" = function(x) {
    "Intercepto"
  },
  dic_grupo = function(x) {""}
)




apply_name_trns <- function(name, prop) {
  
  if (name == "(Intercept)") { return("Intercepto") }
  name <- name %>% stringr::str_replace_all("`", "")
  
  trns <- prop$var_trns_selected[[name]]
  func <- nome_transf2[[trns]]
  func(name)
  
}


write_eq <- function(model_summary, prop, trns_back = TRUE) {
  . <- NULL
  
  esti <- model_summary$coefficients[, "Estimate", drop = TRUE] %>% 
    lapply(., formatC2) %>% unlist()
  
  
  
  names(esti) <- vapply(names(esti), function(x) {
    
    if (x == "(Intercept)") { return("1") }
    
    tr <- prop$var_trns_selected[x]
    
    fun <- nome_transf2[[tr]]
    fun(x)
    
  }, character(1)) 
  
  
  eq <- paste(names(esti), esti, collapse = " + ", sep = " * ")
  
  
  tr <- prop$var_trns_selected[prop$var_dependent]
  
  fun <- anti_nome_transf[[tr]]
  
  
  if (trns_back) {
    
    equals_to <- fun(eq)
    
    paste(prop$var_dependent, "=", equals_to)
    
  } else {
    
    var_dep_trns <- apply_name_trns(prop$var_dependent, prop)
    
    paste(var_dep_trns, "=", eq)
    
    
  }
  
  
}




# ED - Engenharia de Dados ------------------------------------------------




skim_new_names <- function() {
  
  c(
    "Tipo" = "skim_type", 
    "Vari\u00E1vel" = "skim_variable", 
    "Valores Faltantes" = "n_missing", 
    "Taxa de Completos" = "complete_rate", 
    "Texto: Qtde min caracteres" = "character.min", 
    "Texto: Qtde max caracteres" = "character.max", 
    "Texto: Qtde Vazios" = "character.empty", 
    "Texto: Qtde Distintos" = "character.n_unique", 
    "Texto: Qtde Inicia Vazio" = "character.whitespace", 
    "Data: M\u00EDnimo" = "Date.min", 
    "Data: M\u00E1ximo" = "Date.max", 
    "Data: Mediana" = "Date.median", 
    "Data: Distintos" = "Date.n_unique", 
    "Fator: Ordenados?" = "factor.ordered", 
    "Fator: Distintos" = "factor.n_unique", 
    "Fator: Maiores Contagens" = "factor.top_counts", 
    "L\u00F3gica: Media" = "logical.mean", 
    "L\u00F3gica: Contagem" = "logical.count", 
    "Num\u00E9rica: Media" = "numeric.mean", 
    "Num\u00E9rica: Desv Pad" = "numeric.sd", 
    "Num\u00E9rica: M\u00EDnimo" = "numeric.p0", 
    "Num\u00E9rica: 1Q" = "numeric.p25", 
    "Num\u00E9rica: Mediana" = "numeric.p50", 
    "Num\u00E9rica: 3Q" = "numeric.p75", 
    "Num\u00E9rica: M\u00E1ximo" = "numeric.p100", 
    "Num\u00E9rica: Histograma" = "numeric.hist",
    "Data/Hora: M\u00EDnimo" = "POSIXct.min", 
    "Data/Hora: M\u00E1ximo" = "POSIXct.max", 
    "Data/Hora: Mediana" = "POSIXct.median", 
    "Data/Hora: Distintos" = "POSIXct.n_unique")
  
}


skim_standart <- function() {
  
  dplyr::tibble(
    "skim_type" = logical(0), 
    "skim_variable"= logical(0), 
    "n_missing" = logical(0), 
    "complete_rate" = logical(0), 
    "character.min" = logical(0), 
    "character.max" = logical(0), 
    "character.empty" = logical(0), 
    "character.n_unique" = logical(0), 
    "character.whitespace" = logical(0), 
    "Date.min" = logical(0), 
    "Date.max" = logical(0), 
    "Date.median" = logical(0), 
    "Date.n_unique" = logical(0), 
    "factor.ordered" = logical(0), 
    "factor.n_unique" = logical(0), 
    "factor.top_counts" = logical(0), 
    "logical.mean" = logical(0), 
    "logical.count" = logical(0), 
    "numeric.mean" = logical(0), 
    "numeric.sd" = logical(0), 
    "numeric.p0" = logical(0), 
    "numeric.p25" = logical(0), 
    "numeric.p50" = logical(0), 
    "numeric.p75" = logical(0), 
    "numeric.p100" = logical(0), 
    "numeric.hist" = logical(0), 
    "POSIXct.min" = logical(0), 
    "POSIXct.max" = logical(0), 
    "POSIXct.median" = logical(0), 
    "POSIXct.n_unique" = logical(0)
  )
  
}




skim_to_table <- function(df) {
  bind_rows <- NULL
  df <- df %>% remove_geo() %>% remove_key_column() 
  
  prev_names <- names(df) #prev_names[1:20]  df_skim[["skim_variable"]][1:20]
  
  df_skim <- df %>% skimr::skim() %>% dplyr::as_tibble()
  
  i <- match(names(df) , df_skim[["skim_variable"]])
  df_skim <- df_skim[i, ]
  
  
  
  df_skim <- dplyr::bind_rows(skim_standart(), df_skim) 
  
  names(df_skim) <- names(skim_new_names())
  
  df_skim <- df_skim %>% dplyr::select("Vari\u00E1vel", dplyr::everything())
  
  # df[[1]] <- as.factor(df[[1]])
  # df[[2]] <- as.factor(df[[2]])
  
  df_skim
}



format_choices <- function(x, width = 50) {
  
  x %>% 
    stringr::str_wrap(width = width) %>% 
    stringr::str_replace_all("\\n", "<br>")
  
}


# Variaveis: Definicoes de Norma ------------------------------------------


choices_nbr_var_type <- function() {
  
  c(
    "N\u00E3o declarado" = "",
    "Dicot\u00F4mica" = "dicotomic", 
    "Quantitativa" = "quant", 
    "C\u00F3digo Ajustado" = "cod_ajus",
    "C\u00F3digo Alocado" = "cod_aloc",
    "Proxy" = "proxy")
}

choices_var_behavior <- function() {
  
  c(
    "N\u00E3o declarado" = "",
    "Crescente" = "cresc",
    "Decrescente" = "decresc")
  
}

check_micronumerosidade <- function(x, nbr_type, obs_disabled) {
  #browser()
  n_obs <- sum(!obs_disabled)
  
  validate(need(n_obs > 0, "N\u00e3o h\u00e1 dados habilitados"))
  
  
  if (n_obs <= 30) {
    
    lim <- 3
    
  } else if (n_obs > 30 & n_obs <= 100) {
    
    lim <-  .1 * n_obs
    
  } else if (n_obs > 100) {
    
    lim <- 10
    
  }
  
  
  var_ty <- choices_nbr_var_type()
  type <- var_ty[var_ty == nbr_type] %>% names()
  
  validate(
    need(nbr_type %in% c("cod_aloc", "dicotomic", "cod_ajus"), 
         paste("O teste de micronumerosidade n\u00E3o se aplica a vari\u00E1veis do tipo", type)))
  
  
  cont <- table(x, useNA = "no")
  
  
  i <- which(cont < lim)
  
  cont <- cont[i] #%>% as.vector()
  
  validate(need(cont, "A vari\u00E1vel n\u00E3o apresenta micronumerosidade"))
  
  
  niveis <- paste0(names(cont), " com ", cont, " dado(s)") %>% 
    paste(collapse = ", ")
  
  # msg <- paste("A vari\u00E1vel apresenta micronumerosidade nos n\u00EDveis:\n\n", niveis)
  
  
  msg <- paste("Essa vari\u00E1vel apresenta micronumerosidade")
  
  msg 
  
  
}

check_micronumerosidade_all <- function(df, prop) {
  
  n_obs <- sum(!prop$obs_disabled)
  
  validate(need(n_obs > 0, "N\u00e3o h\u00e1 dados habilitados"))
  
  
  if (n_obs <= 30) {
    
    lim <- 3
    
  } else if (n_obs > 30 & n_obs <= 100) {
    
    lim <-  .1 * n_obs
    
  } else if (n_obs > 100) {
    
    lim <- 10
    
  }
  df <- df %>% remove_geo() %>% remove_key_column()
 
  
  
  tb <- lapply(names(df), function(var) {
   
    nbr_type <- prop$var_nbr_type[[var]]
    x <- choices_nbr_var_type()
    nbr_type_name <- x[x == nbr_type] %>% names()
     
    
    if (nbr_type %in% c("cod_aloc", "dicotomic", "cod_ajus")) {
      
       
    contagem <- table(df[[var]], useNA = "no")
    
    i <- which(contagem < lim)
    
    contagem <- contagem[i]
    
    dplyr::tibble(
      Var = var,
      Tipo = nbr_type_name,
      Micronumerosidade = "Sim",
      Valor = names(contagem),
      Qtde = contagem, 
      "Necess\u00e1rio" = lim
    )
      
      
    } else {
      
    dplyr::tibble(
      Var = var,
      Tipo = nbr_type_name,
      Micronumerosidade = "N\u00e3o",
      Valor = NA,
      Qtde = NA,
      "Necess\u00e1rio" = NA
    )
      
   }
    
  }) %>% dplyr::bind_rows()
  
  
  
 tb
  
  
  
  
}



check_var_na <- function(x) {
  
  qtde_na <- is.na(x) %>% sum()
  
  validate(need(qtde_na > 0, "" ))
  
  paste0("A vari\u00E1vel possui ", qtde_na, " valor(es) n\u00E3o atribu\u00EDdo(s) [NA`s]")
  
}

check_numeric_and_na <- function(df, remove_na) {
  
  nms <- names(df)
  
  for (i in nms) {
    
    x <- df[[i]]
    
    #implies
    has_na <- any(is.na(x)) 
    dont_stop_na <- !has_na | remove_na
    
    validate(need(dont_stop_na, paste0("A vari\u00E1vel ",  i," possui NA. Remova-a ou selecione a op\u00E7\u00E3o para desconsiderar NA`s")   ))
    
    test <- is.numeric(x)
    
    validate(need(test, paste0("A vari\u00E1vel ",  i," n\u00E3o \u00E9 do tipo num\u00E9rico. Remova-a ou remova as fun\u00E7\u00F5es que necessitam de valores num\u00E9rico para serem aplicadas")   ))
    
  }
}





# check_var_na(c(2, 3))
# check_micronumerosidade(mtcars[[8]], "dicotomic") 





# Tratamento e Manipulacao ------------------------------------------------


# Oper Mat Entre Variaveis ------------------------------------------------






oper_mat_var_group <- function(df, 
                               oper_group, 
                               oper_specific, 
                               primary_vars, 
                               new_var_name) {
  
  if (oper_group != "oper_mat_var") { return(df) }
  
  
  validate(need(new_var_name, "Defina o nome para a nova vari\u00E1vel"))
  validate(need( length(primary_vars) > 1  ,"Selecione pelo menos duas vari\u00E1veis" ))
  
  df2 <- df[,primary_vars , drop = FALSE] %>% remove_geo()
  
  for (i in primary_vars) {
    
    validate(need(is.numeric(df[[i]]), paste0("Para essa opera\u00E7\u00E3o, todas as vari\u00E1veis devem ser do tipo num\u00E9rico. A vari\u00E1vel ", i, " n\u00E3o possui essa estrutura. Remova-a antes de continuar") ))
    
  }
  
  
  
  
  
  if (oper_specific == "soma_var") {
    
    func <- function(x) Reduce("+", x)
    
    
  } else if (oper_specific == "multiply_var") {
    
    func <- function(x) Reduce("*", x)
    
    
  } else if (oper_specific == "divide_var" ) {
    
    func <- function(x) Reduce("/", x)
    
    
  } else if (oper_specific == "subtract_var") {
    
    func <- function(x) Reduce("-", x)
    
    
  } else if (oper_specific == "max_between") {
    
    func <- function(x) Reduce("pmax", x)
    
    
  } else if (oper_specific == "min_between") {
    
    func <- function(x) Reduce("pmin", x)
    
  }
  
  
  df[[new_var_name]] <- func(df2)
  
  attr(df, "oper_group") <- oper_specific
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- new_var_name
  
  df
  
}



# Oper Mat Variaveis CTE --------------------------------------------------



oper_mat_cte_group <- function(df, 
                               oper_group, 
                               oper_specific, 
                               primary_vars, 
                               cte,
                               var_suffix) {
  
  if (oper_group != "oper_mat_cte") { return(df) }
  
  
  validate(need(cte, paste0("O valor informado para a constante n\u00E3o foi interpretado corretamente. Verifique o valor informado:", cte)  ))
  #validate(need(var_suffix, shiny::HTML("Especifique um sufixo informativo. Uma nova vari\u00E1vel ser\u00E1 criada mesclando o nome da antiga com o sufixo.\n\nSugerem-se sufixos come\u00E7ando com underline (_). Exemplos: \n_x10 \n _divi2 \n __elevado3")))
  
  
  
  if (oper_specific == "soma_cte") {
    
    func <- `+`
    
    
  } else if (oper_specific == "multiply_cte") {
    
    func <- `*`
    
    
  } else if (oper_specific == "divide_cte" ) {
    
    func <- `/`
    
    
  } else if (oper_specific == "subtract_cte") {
    
    func <- `-`
    
    
  } else if (oper_specific == "max_between_cte") {
    
    func <- `pmax`
    
  } else if (oper_specific == "min_between_cte") {
    
    func <- `pmin`
    
  } else if (oper_specific == "exponeciar_cte") {
    
    func <- `^`
    
  } else if (oper_specific == "raiz_cte") {
    
    func <- function(x, r) { x^(1/r) }
    
  }
  
  
  for (i in primary_vars) {
    
    validate(need(is.numeric(df[[i]]), paste0("Para essa opera\u00E7\u00E3o, todas as vari\u00E1veis devem ser do tipo num\u00E9rico. A vari\u00E1vel ", i, " n\u00E3o apresenta esse comportamento. Remova-a antes de continuar") ))
    
  }
  
  
  act_on_var <- character(0)
  for (i in primary_vars) {
    
    new_name <- paste0(i, var_suffix)
    act_on_var <- append(act_on_var, new_name)
    
    df[, new_name] <- df[[i]] %>% func(cte)
    
  }
  
  attr(df, "oper_group") <- oper_specific
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- act_on_var
  
  
  df
  
}



# Filtragem de Dados ------------------------------------------------------




# filter_data_group(
#   df = df, 
#   oper_group = "filter_data", 
#   primary_vars = c("var_double","erros_numericos", "var_double_NA"), 
#   con_filter_data_do = NA, 
#   con_between_var = T,
#   con_inside_var = F, # FALSE \u00E9 OU. TRUE \u00E9 E
#   con_remove_na = F, 
#   
#   con_igual_a = "",
#   con_diferente_de =  "           ", 
#   con_maior_que = "  ",
#   con_maior_igual_a = " 3,8  ", 
#   con_menor_que = "", 
#   con_menor_igual_a = "9,88")

filter_data_group <- function(df, 
                              oper_group, 
                              primary_vars, 
                              con_filter_data_do,
                              
                              con_convert_to,
                              
                              con_between_var,
                              con_inside_var,
                              
                              con_remove_na,
                              
                              con_igual_a = "",
                              con_diferente_de = "",
                              con_maior_que = "",
                              con_maior_igual_a = "",
                              con_menor_que = "",
                              con_menor_igual_a = "") {
  
  if (oper_group != "filter_data") { return(df) }
  
  # Para revisao posterior: Essa funcao possui o seguinte funcionamento: Uma
  # funcao para realizar as validacoes das condicoes  retornar o indice para
  # cada variavel. Em cada variavel, \u00E9 feita a combinacao E/OU ocnforme
  # especificado. Essa funcao eh executada em loop para cada variavel. Ai entra
  # outra funcao para realizar a combinacao dos indices da primeira varavel com
  # as variaveis subsequentes em loop por meio de E/OU. Por fim, uma funcao que
  # trabalha os indices obtidos e o data frame conforme os indices obtidos e se
  # \u00E9 para habilitar, desabilitar, excluir, etc.
  
  # Em resumo:
  # 1. Checar condicoes numa vaiavel
  # 2. Checar em loop varias variaveis
  # 3. combinar indices de varias varaiveis
  # 4. Trabalhar a base conforme os indices e a necessidade: hab, desab, excluir
  
  
  
  #if (is.null(con_remove_na)) { con_remove_na <- FALSE}
  
  #composicao do vetor de valores de referecnia
  values <- c(
    "Igual a" = con_igual_a,
    "Diferente de" = con_diferente_de,
    "Maior que" = con_maior_que,
    "Maior/Igual a" = con_maior_igual_a,
    "Menor que" = con_menor_que,
    "Menor igual a" = con_menor_igual_a
  ) %>% # reune os valores no vetor
    stringr::str_trim() %>% # retira os espacoes em bracno das laterais
    dplyr::na_if("") %>%  # troca os valores "" por NA
    stringr::str_replace(",", "\\.") %>% #substitui virgula por ponto, se necessario
    as.numeric() # transforma me numerico
  
  # se algum nao for NA, entao havera comparacoes matematicas, para esse tipo de
  # comparacoes \u00E9 necessario q todas as variaveis sejam numericas.se nao forem
  # encontradas somente NA, entao o unico testes q sera feitos \u00E9 o de remover NA
  has_na <- any(!is.na(values))
  #checagem numericas
  if (has_na) {
    
    for (i in primary_vars) {
      
      validate(need(is.numeric(df[[i]]), paste0("Quando um filtro num\u00E9rico for inserido, todas as vari\u00E1veis devem ser do tipo num\u00E9rico. A vari\u00E1vel ", i, " n\u00E3o possui essa estrutura. Remova-a antes de continuar") ))
      
    }
  }
  
  
  # separando as variaveis nas quasi serao aplicados os filtros
  df2 <- df[, primary_vars, drop = FALSE] %>% remove_geo()
  
  # aplicando os checks em loop nas variaveis selecionadas (Etapa 1 e 2)
  check_all <- vapply(
    X = df2,
    FUN =  filtering, 
    FUN.VALUE = logical(NROW(df2)), 
    con_inside_var,
    con_remove_na,
    
    con_igual_a  = values[1],
    con_diferente_de = values[2],
    con_maior_que = values[3],
    con_maior_igual_a = values[4],
    con_menor_que = values[5],
    con_menor_igual_a = values[6]
  )
  
  
  # reducao dos vetores das varias variaveis em um s\u00F3 (Etapa 3)
  
  if (con_between_var == TRUE) {
    
    check_reduce <- turbo_E(as.data.frame(check_all))
    
  } else {
    
    check_reduce <- turbo_OU(as.data.frame(check_all))
    
  }
  
  
  # acoes a tomar com base no vetor de indices (Etapa 4):
  # essa funcao prepara o df para ser lido pela data_update_reload()
  df <- filter_prepare(df, 
                       check_reduce, 
                       con_filter_data_do, 
                       primary_vars, 
                       con_convert_to, 
                       oper_group)
  
  
  df
  
}
not_null <- function(x) !is.null(x)
turbo_E <- function(x)  Reduce(`&`, x)
turbo_OU <- function(x)  Reduce(`|`, x)

# filtering(df$var_double_NA, F, T, 1.3, NA, NA, NA, NA, NA)
# filtering(
#   df$var_double_NA, 
#   con_inside_var = F, # FALSE \u00E9 OU. TRUE \u00E9 E
#   con_remove_na = F,
#   con_igual_a = NA,
#   con_diferente_de = NA,
#   con_maior_que = NA,
#   con_maior_igual_a = NA,
#   con_menor_que = NA,
#   con_menor_igual_a = NA)




filtering <- function(x, 
                      con_inside_var, # FALSE \u00E9 OU. TRUE \u00E9 E
                      con_remove_na,
                      
                      con_igual_a,
                      con_diferente_de,
                      con_maior_que,
                      con_maior_igual_a,
                      con_menor_que,
                      con_menor_igual_a) {
  
  
  checks <- list(
    
    
    igual_a = if (!is.na(con_igual_a)) { 
      
      x == con_igual_a 
      
    } else NULL,
    
    diferente_de = if (!is.na(con_diferente_de)) { 
      
      x != con_diferente_de 
      
    } else NULL,
    
    maior_que = if (!is.na(con_maior_que)) {
      
      x > con_maior_que 
      
    } else NULL,
    
    maior_igual_a = if (!is.na(con_maior_igual_a)) { 
      
      x >= con_maior_igual_a 
      
    } else NULL,
    
    menor_que = if (!is.na(con_menor_que)) {
      
      x < con_menor_que 
      
    } else NULL,
    
    menor_igual_a = if (!is.na(con_menor_igual_a)) {
      
      x <= con_menor_igual_a 
      
    } else NULL
    
  )
  
  
  checks <- Filter(not_null, checks)
  
  if (!rlang::is_empty(checks)) {
    
    if (con_inside_var == TRUE) { 
      checks <- turbo_E(checks)
      
    } else { 
      checks <- turbo_OU(checks)
      
    }
    
    
    
    indexes_na <- is.na(x)
    
    if (con_remove_na == FALSE) {
      # se nao for pra remover o NA, mantem como indice TRUE
      checks[indexes_na] <- TRUE
      
    } else {
      # se for pra remover o NA, faz o resultado dos filtros matem\u00E1ticos E
      # indices_na
      checks[indexes_na] <- FALSE
    }
    
  } else { 
    # se o resultado do teste matematico der vazio, faz s\u00F3 o filtro de NA. Se
    # nao estiver habiltiado para filtro de NA, todos sao exibidos
    
    if (con_remove_na == TRUE) {
      
      checks <- is.na(x)
      
    } else {
      
      checks <- !logical(length(x))
      
    }
  }
  
  
  checks
  
}



filter_prepare <- function (df, 
                            index, 
                            action, 
                            vars = NULL, 
                            convert_to = NULL, 
                            oper_group) {
  # O vetor index possui TRUE para os elementos que atendem as condicoes de
  # filtro e possui FALSE para os elementos que nao atendem
  #browser()
  if (action == "exclude_data_filtered") {
    # exclui os elementos que atendem, mantem os que nao atendem
    
    #validate(need(any(!index), "Todos os dados seriam exclu\u00EDdos, opera\u00E7\u00E3o cancelada"))
    
    df <- df[!index, ] #exclui os TRUE do index original, mantem os FALSE
    
    act_on_var <- list(
      
      action = action,
      indexes = !index,
      vars = vars
      
    )
    
  } else if (action == "exclude_data_non_filtered") {
    # exclui os elementos que nao atendem, mantem os que atendem
    
    #validate(need(any(index), "Todos os dados seriam exclu\u00EDdos, opera\u00E7\u00E3o cancelada"))
    
    df <- df[index, ] #exclui os FALSE do index original, mantem os TRUE
    
    act_on_var <- list(
      
      action = action,
      indexes = index,
      vars = vars
      
    )
    
  } else if (action == "enable_obs") {
    
    # tem que agir nas propriedades desabilitadnro os dados em obs_disable
    # refazer as transformadas. Nao modifica o df
    
    act_on_var <- list(
      
      action = action, 
      indexes = index,
      vars = vars
      
    )
    
  } else if (action == "enable_obs_only") {
    
    # tem que agir nas propriedades desabilitadnro os dados em obs_disable
    # refazer as transformadas. Nao modifica o df
    
    act_on_var <- list(
      
      action = action, 
      indexes = index,
      vars = vars
      
    )
    
  } else if (action == "disable_obs") {
    
    # tem que agir nas propriedades desabilitadnro os dados em obs_disable
    # refazer as transformadas
    
    act_on_var <- list(
      
      action = action, 
      indexes = index,
      vars = vars
      
    )
    
  } else if (action == "disable_obs_only") {
    # tem que agir nas propriedades desabilitadnro os dados em obs_disable
    # refazer as transformadas
    
    act_on_var <- list(
      
      action = action, 
      indexes = index,
      vars = vars
      
    )
    
  } else if(action == "convert_to") {
    #no caso do filtro espacial, esse caso nao chegara a ser atingido
    validate(need(any(index), "Nenhum dado satisfez as condi\u00E7\u00F5es especificadas"))
    
    i_row <- which(index)
    i_col <- which(names(df) %in% vars)
    
    
    # fazer compatibilizacao do formato do novo valor com o formato do antigo.
    # Por exemplo, quando o vetor no qual convertido o valor \u00E9 da classe
    # character e tenta-se inserir um do tipo numeric, ele da erro. Ou seja, o
    # numeric precisa antes ser convertido para o tipo character para nao dar
    # erro
    df[i_row, i_col] <- convert_to %>% as.numeric()
    
    act_on_var <- vars
    
  }
  
  
  attr(df, "oper_group") <- oper_group
  # attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- act_on_var
  
  df
  
}










# Trabalhar Variaveis -----------------------------------------------------



transmute_var_group <- function(df, 
                                oper_group, 
                                oper_specific, 
                                primary_vars, 
                                pad_center_mean,
                                pad_divide_desv_pad,
                                pad_na_rm,
                                pad_suffix,
                                
                                cat_suboptions,
                                cat_quantile_ignore_NA,
                                cat_quantile_interval,
                                
                                cat_sub_n,
                                
                                cat_user_interval,
                                
                                cat_convert_to_cod_alocado,
                                cat_suffix
) {
  varName <- NULL
  if (oper_group != "transmute_var") { return(df) }
  
  preveious_names <- names(df)
  
  if (oper_specific == "pad") {
    
    
    for (i in primary_vars) {
      
      validate(need(is.numeric(df[[i]]), paste0("Para essa opera\u00E7\u00E3o, todas as vari\u00E1veis devem ser do tipo num\u00E9rico. A vari\u00E1vel ", i, " n\u00E3o paresenta esse comportamento. Remova-a antes de continuar") ))
      
      if (pad_divide_desv_pad) {
        
        check_desv_pad <- stats::sd(df[[i]], na.rm = pad_na_rm) 
        validate(need(!is.na(check_desv_pad ), paste0("A vari\u00E1vel ", i, " n\u00E3o pode ser dividida pelo desvio-padr\u00E3o. Verifique a exist\u00EAncia de NA`s")))
        validate(need(check_desv_pad != 0, paste0("A vari\u00E1vel ", i, " n\u00E3o pode ser dividida pelo desvio-padr\u00E3o. Possivelmente todos os valores da vari\u00E1vel s\u00E3o iguais. Essa condi\u00E7\u00E3o resulta em desvio-padr\u00E3o igual a zero")))
        
      }
      
      if (pad_center_mean) {
        
        check_mean <- base::mean(df[[i]], na.rm = pad_na_rm) 
        validate(need(!is.na(check_mean), paste0("A vari\u00E1vel ", i, " n\u00E3o pode centralizar a vari\u00E1vel na m\u00E9dia. Verifique a exist\u00EAncia de NA`s")))
        
      }
      
    }
    
    
    
    act_on_var <- character(0)
    for (i in primary_vars) {
      
      new_name <- paste0(i, pad_suffix)
      act_on_var <- append(act_on_var, new_name)
      
      df[[new_name]] <- scale2(df[[i]], 
                               center = pad_center_mean, 
                               scale = pad_divide_desv_pad, 
                               na.rm = pad_na_rm)
      
    }
    
  } else if (oper_specific == "dic") {
    
    
    df2 <- df[, primary_vars, drop = FALSE] %>% remove_geo()
    
    n_levels_all <-  vapply(df2, dplyr::n_distinct, FUN.VALUE = numeric(1))
    check <- vapply(n_levels_all, `>`, FUN.VALUE = logical(1), 1) %>% all()
    validate(need(check, "Cada uma das vari\u00E1veis selecionadas deve possuir ao menos dois n\u00EDveis diferentes"))
    
    
    df2 <- df2 %>% 
      dplyr::mutate_at(primary_vars, as.factor)
    
    treat <- df2 %>% 
      vtreat::designTreatmentsZ(primary_vars, verbose = FALSE)
    
    var_treated <- treat$scoreFrame %>%
      dplyr::filter(code %in% c("lev")) %>%
      dplyr::pull(varName)
    
    df_treated <- vtreat::prepare(treat, df2, varRestriction =  var_treated)
    
    
    
    act_on_var <- names(df_treated)
    
    df <- dplyr::bind_cols(df, df_treated)
    
    
  } else if (oper_specific == "cat_var") {
    
    
    for (i in primary_vars) {
      
      validate(need(is.numeric(df[[i]]), paste0("Para essa opera\u00E7\u00E3o, todas as vari\u00E1veis devem ser do tipo num\u00E9rico. A vari\u00E1vel ", i, " n\u00E3o paresenta esse comportamento. Remova-a antes de continuar") ))
      
    }
    
    
    validate(need(cat_suffix, "Defina um sufixo"))
    
    
    if (cat_suboptions == "sub_quantile") {
      
      
      interval <- cat_quantile_interval %>% 
        stringr::str_split("/") %>%  
        unlist() %>%  
        readr::parse_number(locale = readr::locale(decimal_mark = ",")) %>% sort()
      
      validate(need(interval, "Especifique um intervalo v\u00E1lido"))
      
      validate(need(!any(is.na(interval)), "Especifique um intervalo v\u00E1lido"))
      validate(need(all(interval <= 100), "Ao menos um dos valores fornecidos \u00E9 maior que 100%"))
      validate(need(all(interval >= 0), "Ao menos um dos valores fornecidos \u00E9 menor que 0%"))
      
      
      for (i in primary_vars) {
        
        has_na <- any(is.na(df[[i]])) 
        dont_stop_na <- !has_na | cat_quantile_ignore_NA
        validate(need(dont_stop_na, paste0("A vari\u00E1vel ", i, " possui NA e o tratamento de NA`s n\u00E3o est\u00E1 habilitado. Remova a vari\u00E1vel ou habilite a desconsidera\u00E7\u00E3o de NA`s")))
      }
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <-  paste0(i, cat_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        quan <- stats::quantile(stats::df[[i]], 
                                interval/100, 
                                na.rm = cat_quantile_ignore_NA) %>% 
          unique()
        
        df[[new_name]] <-  Hmisc::cut2(df[[i]], cuts = quan, m = 3)
        
        if(cat_convert_to_cod_alocado) {
          
          df[[new_name]] <- as.numeric(df[[new_name]])
          
        }
        
      }
      
      
      
    } else if (cat_suboptions == "sub_n") {
      
      cat_sub_n <- cat_sub_n %>% as.integer()
      
      validate(need(cat_sub_n > 0, "A quantidade de grupos deve ser maior que 0"))
      validate(need(!is.na(cat_sub_n), "Especifique uma quantidade de grupos v\u00E1lida"))
      
      for (i in primary_vars) {
        
        disti <- dplyr::n_distinct(df[[i]])
        validate(need(cat_sub_n <= disti, paste0("A quantidade de grupos definida deve ser menor que a quantidade de valores distintos na vari\u00E1vel ", i)))
        
      }
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <-  paste0(i, cat_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        df[[new_name]] <-  Hmisc::cut2(df[[i]], g = cat_sub_n, m = 3)
        
        
        if(cat_convert_to_cod_alocado) {
          
          df[[new_name]] <- as.numeric(df[[new_name]])
          
        }
        
      }
      
      
      
    } else if (cat_suboptions == "sub_interval") {
      
      
      interval <- cat_user_interval %>% 
        stringr::str_split("/") %>%  
        unlist() %>%  
        readr::parse_number(locale = readr::locale(decimal_mark = ",")) %>% 
        sort() %>% 
        unique()
      
      validate(need(interval, "Especifique um intervalo v\u00E1lido"))
      
      validate(need(!any(is.na(interval)), "Especifique um intervalo v\u00E1lido"))
      
      
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <-  paste0(i, cat_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        df[[new_name]] <- Hmisc::cut2(df[[i]], cuts = interval, m = 3)
        
        
        if(cat_convert_to_cod_alocado) {
          
          df[[new_name]] <- as.numeric(df[[new_name]])
          
        }
        
      }
      
    }
    
  }
  
  attr(df, "oper_group") <- oper_group
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- act_on_var
  
  df
  
}



scale2 <- function(x, center = TRUE, scale = TRUE, na.rm = FALSE) {
  
  if (center) {
    
    x <- (x - base::mean(x, na.rm = na.rm))
    
  }
  
  if (scale) {
    
    x <- x / stats::sd(x, na.rm = na.rm)
    
  }
  
  x
  
}



# Conversao Variaveis -----------------------------------------------------

convert_var_fit <- function (x, new_class) {
  
  if (inherits(x, "sfc")) { return(x) }
  if (is.numeric(x)) { dec <- "."} else { dec <- ","}
  x <- as.character(x)
  
  
  if (new_class == "integer") {
    
    func <- function(x){
      
      readr::parse_double(x, locale = readr::locale(decimal_mark = dec)) %>% as.integer()
      
    }
    
  } else if (new_class == "double") {
    
    func <- function(x){
      
      readr::parse_double(x, locale = readr::locale(decimal_mark = dec))
      
    }
    
  } else if (new_class == "character") {
    
    func <- as.character
    
  } else if (new_class == "factor") {
    
    func <- as.factor
    
  } else if (new_class == "logical") {
    
    func <- as.logical
    
  }
  
  func(x)
  
  
}






convert_var <- function(df, 
                        oper_group, 
                        primary_vars, 
                        new_class, 
                        new_class_suffix) {
  
  if (oper_group != "convert_class") { return(df) }
  
  
  
  act_on_var <- character(0)
  for (i in primary_vars) {
    
    new_name <- paste0(i, new_class_suffix)
    act_on_var <- append(act_on_var, new_name)
    
    
    df[[new_name]] <- convert_var_fit(df[[i]], new_class)
    
  }
  
  attr(df, "oper_group") <- oper_group
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- act_on_var
  
  df
  
  
}


# Remocao de Variaveis ----------------------------------------------------


remove_var_group <- function(df, oper_group, primary_vars) {
  
  if (oper_group != "remove_var") { return(df) }
  
  validate(need(primary_vars, "Faz-se necess\u00E1rio selecionar uma ou mais vari\u00E1veis a serem exclu\u00EDdas"))
  
  n_exclude <- length(primary_vars)
  n_col <- length(df %>% remove_geo() %>% remove_key_column())
  
  validate(need( (n_col - n_exclude) >= 2, paste0("A base de dados deve possuir no m\u00EDnimo duas colunas. Atualmente, ela possui ", n_col, ". Foram selecionadas ", n_exclude, " para serem exclu\u00EDdas. Opera\u00E7\u00E3o cancelada" )  ))
  
  
  for (i in primary_vars) {
    
    df[[i]] <- NULL
    
  }
  
  
  attr(df, "oper_group") <- oper_group
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- primary_vars
  
  
  df
  
}




# Alteracoa de Nome de Variaveis ------------------------------------------


rename_var <- function(df, oper_group, primary_vars, new_name) {
  
  if (oper_group != "rename_var") { return(df) }
  
  validate(need(length(primary_vars) == 1, "Selecione apenas uma vari\u00E1vel por vez" ))
  validate(need(new_name, "Digite um nome v\u00E1lido")) 
  validate(need(!(new_name %in% c("Elemento", "geometry") ), "Os nomes 'Elemento' e 'geometry' s\u00E3o utilizados pelo sistema, n\u00E3o os utilize"))
  validate(need(!(new_name %in% names(df)), "Esse nome j\u00E1 est\u00E1 em uso na planilha de dados. Escolha um nome \u00FAnico. Os nomes 'Elemento' e 'geometry' s\u00E3o utilizados pelo sistema, n\u00E3o os utilize"))
  
  
  
  i <- which(names(df) %in% primary_vars)
  
  names(df)[i] <- new_name
  
  attr(df, "oper_group") <- oper_group
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- list(
    action = "rename",
    new_name = new_name,
    old_name = primary_vars
  )
  
  
  
  df
  
}


# Operacoes com a Variavel Data -------------------------------------------




oper_date_group <- function(df, 
                            oper_group, 
                            oper_specific, 
                            primary_vars,
                            fuso,
                            date_format,
                            date_suffix,
                            date_to_numeric) {
  . <- NULL
  if (oper_group != "oper_date") { return(df) }
  
  
  
  if (oper_specific == "text_to_date") {
    
    if (date_format == "my") {
      
      func <- function(x, ...) {
        
        x <- paste0("01", x)
        lubridate::dmy(x, ...)
        
      }
      
      .tz <- NULL
      
      
    } else if (date_format == "dmy") {
      
      func <- lubridate::dmy
      .tz <- NULL
      
      
    } else if (date_format == "ymd") {
      
      func <- lubridate::ymd
      .tz <- NULL
      
      
    } else if (date_format == "mdy") {
      
      func <- lubridate::mdy
      .tz <- NULL
      
      
    } else if (date_format == "dmy_hms") {
      
      func <- lubridate::dmy_hms
      .tz <- fuso
      
      
    } else if (date_format == "ymd_hms") {
      
      func <- lubridate::ymd_hms
      .tz <- fuso
      
      
    } else if (date_format == "mdy_hms") {
      
      func <- lubridate::mdy_hms
      .tz <- fuso
      
    } 
    
    
    for (i in primary_vars) {
      
      validate(need(!is.logical(df[[i]]), "Para essa opera\u00E7\u00E3o, n\u00E3o s\u00E3o aceitas vari\u00E1veis l\u00F3gicas"))
      
    }
    
    
    act_on_var <- character(0)
    for (i in primary_vars) {
      
      new_name <- paste0(i, date_suffix)
      act_on_var <- append(act_on_var, new_name)
      
      df[[new_name]] <- func(df[[i]], tz = .tz, locale = "Portuguese_Brazil.1252")
      
    }
    
  } else if (oper_specific == "date_to_numeric") {
    
    
    for (i in primary_vars) {
      
      
      cond1 <- lubridate::is.Date(df[[i]])
      cond2 <- lubridate::is.POSIXct(df[[i]])
      
      validate(need(cond1 || cond2, paste0("Para essa opera\u00E7\u00E3o, somente s\u00E3o aceitas vari\u00E1veis convertidas previamente em tipo Data. A vari\u00E1vel", i, " n\u00E3o obedece a essa regra")))
      
    }
    
    
    if (date_to_numeric == "year") {
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <- paste0(i, date_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        df[[new_name]] <- lubridate::year(df[[i]]) %>% as.integer()
        
      }
      
      
      
    } else if (date_to_numeric == "yearsemester") {
      
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <- paste0(i, date_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        yearr <- lubridate::year(df[[i]])
        ss <- lubridate::semester(df[[i]]) %>% 
          stringr::str_pad(., width = 2, pad = "0")
        
        df[[new_name]] <- paste0(yearr, ss) %>% as.integer()
        
      }
      
      
    } else if (date_to_numeric == "yearmonth") {
      
      act_on_var <- character(0)
      for (i in primary_vars) {
        
        new_name <- paste0(i, date_suffix)
        act_on_var <- append(act_on_var, new_name)
        
        yearr <- lubridate::year(df[[i]])
        monthh <- lubridate::month(df[[i]]) %>% 
          stringr::str_pad(., width = 2, pad = "0")
        
        df[[new_name]] <- paste0(yearr, monthh) %>% as.integer()
        
      }
    }
  }
  
  attr(df, "oper_group") <- oper_group
  attr(df, "source_vars") <- primary_vars
  attr(df, "act_on_var") <- act_on_var
  
  df
  
}




# FUNCOES AUXILIARES ------------------------------------------------------

# Funcoes de Apoio --------------------------------------------------------
moda <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)] %>% paste(collapse = ", ")
}

check_emp_rule <-function(x, n_dp) {
  
  mean_x <- base::mean(x)
  sd_x <- stats::sd(x)
  
  
  limit_sup <- mean_x + n_dp * sd_x 
  limit_inf <- (mean_x - n_dp * sd_x)
  
  n_between <- ((x < limit_sup) & (x > limit_inf)) %>% sum()
  n_between / length(x)
}

get_indep <- function(prop = prop) {
  
  var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
  var_dep <- prop$var_dependent
  setdiff(var_enabled, var_dep)
  
}

get_enabled <- function(prop = prop) {
  
  prop$var_enabled[prop$var_enabled] %>% names()
  
}

get_dep <- function(prop = prop) {
  
  prop$var_dependent
  
}


strucure_preview <- function (df) {
  
  df <- df[0, , drop = FALSE] 
  classes <- lapply(df, class)
  classes <- lapply(classes, paste, collapse =  " ") %>% unlist()
  
  dplyr::tibble("Vari\u00E1vel" = names(classes), "Classe" = classes) 
  
}


prev_intensity <- function(x, df) {
  
  n  <- NROW(df)
  
  if (x == 100) { return(n) }
  
  n_end <- ( (log(n) - 1) * (x/100) ) + 1 
  
  exp(n_end)
}



data_table_preview <- function(df) {
  
  
  DT::datatable(
    df,
    
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      ), 
      searching = F,
      dom = "t", #dom = "liftp",
      scrollX = TRUE,
      scrollY = TRUE,
      paging = TRUE,
      lengthMenu = FALSE,
      pageLength = -1,
      autoWidth = FALSE
    ),
    class = "display",
    callback = DT::JS("return table;"),
    rownames = FALSE,
    filter = "none",
    selection = "single"
    
  )
  
}



data_table_cor <- function(ma, round, analysis_type = "estimate", prop) {
  
  
  nomes_linhas <- rownames(ma)
  nomes_colunas <- colnames(ma)
  
  if (analysis_type == "modelling") {
    
    nomes_linhas <- vapply(nomes_linhas, apply_name_trns, FUN.VALUE = character(1), prop)
    nomes_colunas <- vapply(nomes_colunas, apply_name_trns, FUN.VALUE = character(1), prop)
    
    rownames(ma) <- nomes_linhas
    colnames(ma) <- nomes_colunas
  }
  
  
  
  ma <- ma %>% 
    tidyr::as_tibble() %>%
    dplyr::mutate("Vari\u00E1vel 1" = nomes_colunas) %>%
    tidyr::gather("Vari\u00E1vel 2", "Correla\u00E7\u00E3o", nomes_linhas) %>% 
    dplyr::mutate_at(c("Vari\u00E1vel 1", "Vari\u00E1vel 2"), as.factor)
  
  
  DT::datatable(
    ma,
    extensions = 'Buttons',
    rownames = FALSE,
    filter = "top",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      paging = TRUE,
      dom = "liftBp",
      lengthMenu = list(c(1, 2, 3, 5, 10, 25, 50, 100, -1),
                        c("1", "2", "3", "5", "10", "25", "Todos")),
      autoWidth = FALSE,
      pageLength = 5,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      buttons = list(
        'copy', 'csv', 'excel'
        
        # list(
        #   extend = 'collection',
        #   buttons = c('csv', 'excel'),
        #   text = 'Download'
        # )
      )
      
      
    )) %>%  
    
    DT::formatRound(
      "Correla\u00E7\u00E3o",
      round,
      dec.mark = ",", 
      mark = ".")
  
  
}


data_table_preview2 <- function(df) {
  
  DT::datatable(
    
    df,
    extensions = 'Buttons',
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")),
      
      dom = "liftBp",
      buttons = list(
        'copy', 'csv', 'excel'
        # list(
        #   extend = 'collection',
        #   buttons = c('csv', 'excel'),
        #   text = 'Download'
        # )
      ),
      
      scrollX = TRUE, 
      scrollY = TRUE, 
      paging = TRUE, 
      
      lengthMenu = list(
        c(5, 10, 15, 30, 50, 100, 200, -1),
        c("5", "10", "15", "30", "50", "100", "200", "Todos")),
      autoWidth = FALSE,
      pageLength = 5, 
      search = list(regex = FALSE)
    ), #fim do options
    
    class = "display",
    callback = DT::JS("return table;"),
    rownames = FALSE,
    #colnames,
    #container,
    caption = NULL,
    filter = "top",#
    escape = TRUE,
    style = "default",
    width = NULL,
    height = NULL,
    elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
    
    
    plugins = NULL,
    editable = FALSE
    
  )
  
  
  
}

make_shiny_id <- function(x, pat = "++_-_++") {
  
  x %>% stringr::str_replace_all(" ", pat) 
  
  
}


remove_shiny_id <- function(x, pat = "\\+\\+_-_\\+\\+") {
  
  x %>% stringr::str_replace_all(pat, " ") 
  
  
}



`%||%` <- function(lhs, rhs) {
  if (shiny::isTruthy(lhs)) {
    lhs
  } else {
    rhs
  }
}



### C\u00E1lculo da Moda
# moda <- function(x) {
#   
#   ux <- unique(x)
#   tab <- tabulate(match(x, ux))
#   
#   modas <- ux[tab == max(tab)]
#   
#   if (length(modas) > 1) { return(NA) }
#   modas
# }



fun_aux <- list(
  "soma" = `+`,
  "produto" = `*`,
  "divisao" = `/`,
  "subtracao" = `-`,
  "max_entre" = base::pmax,
  "min_entre" = base::pmin,
  "character" = base::as.character,
  "integer" = base::as.integer,
  "double" = base::as.double,
  "logical" = base::as.logical,
  "factor" =  base::as.factor
  
)


# 
# scale2 <- function(x, na.rm = FALSE, center = TRUE, scale = TRUE) {
#   
#   if (center) {
#     
#     x <- (x - base::mean(x, na.rm = na.rm))
#     
#   }
#   
#   if (scale) {
#     
#     x <- x / stats::sd(x, na.rm)
#     
#   }
#   
#   x
#   
# }

# 
# view_prev <- function(pp) {
#   # lim_sup <- ceiling(NROW(central$rzm) * as.numeric(input$perc_preview) / 100)
#   #
#   # prev$Preview <- prev$Preview[1:lim_sup, , drop = FALSE]
#   
#   x <- paste0(names(pp$Classe), ": ", pp$Classe)
#   cat("Classes:\n", paste(x, "\n"))
#   cat("\n")
#   
#   if (!is.null(pp$Quantidade)) {
#     
#     cat(pp$Quantidade)
#     cat("\n")
#     
#   }
#   
#   cat("Dados:\n")
# 
#   
# }

merge_vector <- function(x, y) {
  
  u <- union(names(y), names(x))
  rev(append(x, y))[u]
  
}


# 
# showNotification2 <- function (ui, action = NULL, duration = 5, closeButton = TRUE,
#                                id = NULL, type = c("default", "message", "warning", "error"),
#                                session = shiny:::getDefaultReactiveDomain()) {
#   if (is.null(id))
#     id <- shiny:::createUniqueId(8)
#   res <- shiny:::processDeps(HTML(ui), session)
#   actionRes <- shiny:::processDeps(action, session)
#   session$sendNotification("show", list(html = res$html, action = actionRes$html,
#                                         deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration *
#                                           1000, closeButton = closeButton, id = id, type = match.arg(type)))
#   id
# }






# Modulos -----------------------------------------------------------------

moduleServer <- function(id, module) {
  callModule(module, id)
}


data_panel_UI <- function(id, secundary_name = "Modelagem") {
  
  shiny::fluidRow(
    
    shinydashboardPlus::boxPlus(
      title = "Painel de Dados",
      status = "primary",
      width = 12,
      closable = FALSE,
      collapsible = TRUE,
      solidHeader = FALSE,
      collapsed = FALSE,
      enable_sidebar = TRUE,
      sidebar_start_open = FALSE,
      sidebar_width = 50,
      #sidebar_background = "#c4c4c4",
      sidebar_content = shiny::tagList(
        
        shinyWidgets::pickerInput(
          inputId = shiny::NS(id, "select_vars"),
          label = "Vari\u00E1veis a visualizar nesse painel:",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            title = "Selecione",
            actionsBox = TRUE,
            deselectAllText = "Selecionar Nenhuma",
            selectAllText = "Selecionar Todas",
            dropupAuto = FALSE,
            header = "Visualizar nesse painel:",
            liveSearch = TRUE,
            liveSearchNormalize = TRUE,
            size = 7,
            #width = "35px",
            selectedTextFormat = "count"
          )
        ),
        shiny::hr(),
        shiny::h5("Dados"),
        shiny::actionButton(
          inputId = shiny::NS(id, "enable_all"),
          label = "Habilitar todos",
          width = "100%"
        ),
        shiny::actionButton(
          inputId = shiny::NS(id, "disable_all"),
          label = "Desabilitar Todos",
          width = "100%"
        ),
        shiny::hr(),
        shiny::actionButton(
          inputId = shiny::NS(id, "send_enabled"),
          label = paste("Enviar Habilitados para", secundary_name),
          width = "100%"
        ),
        shiny::actionButton(
          inputId = shiny::NS(id, "get_enabled"),
          label = paste("Obter Habilitados da", secundary_name),
          width = "100%"
        )
        
      ), # fim do conteudo do side bar
      
      shiny::tags$div(
        style = 'overflow-x: auto; overflow-y: auto; min-height: 400px',
        
        DT::dataTableOutput(outputId = shiny::NS(id, "panel"))
        
      ) #fim do div
    ) # fim do box
  ) # fim da fluidROW
}


data_panel_SERVER <- function(
  id, 
  non_spatial_data, 
  obs_disabled_principal, 
  obs_disabled_secundary) {
  
  moduleServer(id, function(input, output, session) { 
    
    stopifnot(is.reactive(non_spatial_data))
    stopifnot(is.reactive(obs_disabled_principal))
    stopifnot(is.reactive(obs_disabled_secundary))
    send <- shiny::reactiveVal()
    n_obs_seq <- shiny::reactiveVal()
    
    observe({
      
      n <- seq_len(NROW(non_spatial_data()))
      n_obs_seq(n)
      
    })
    
    
    # PAINEL DE DADOS
    output$panel <- DT::renderDataTable({
      req(non_spatial_data())
     . <- NULL
      isolate({
        
        n_col <- length(non_spatial_data())
        
        non_spatial_data() %>% dplyr::mutate_at("Elemento", as.numeric) %>% 
          
          dplyr::mutate_at("Elemento", as.ordered) %>% 
        
        DT::datatable(
          
          .,
          
          options = list(
            columnDefs = list(
              list(visible = FALSE,  targets = seq_len(n_col - 1)),
              list(className = 'dt-center', targets = "_all")),
            
            dom = "liftpB",
            
            scrollX = TRUE, 
            scrollY = TRUE, 
            paging = TRUE, 
            
            lengthMenu = list(
              c(5, 10, 15, 30, 50, 100, 200, -1),
              c("5", "10", "15", "30", "50", "100", "200", "Todos")),
            autoWidth = FALSE,
            pageLength = 5, 
            search = list(regex = FALSE)
          ), #fim do options
          
          class = "display",
          callback = DT::JS("return table;"),
          rownames = FALSE,
          #colnames,
          #container,
          caption = NULL,
          filter = "top",#
          escape = TRUE,
          style = "default",
          width = NULL,
          height = NULL,
          elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = list(
            target = 'row', 
            selected = which(obs_disabled_principal())),
          extensions = c('FixedColumns'),
          plugins = NULL,
          editable = FALSE
          
        )
        
      })# FIM DO ISOLATE
      
    }, server = TRUE)  # FIM DO RENDER DATA TABLE
    
    # PROXY DO PAINEL
    panel_proxy <- DT::dataTableProxy("panel")
    
    
    # atualiza linhas selecionadas quando a lista principal for alterada
    observeEvent(obs_disabled_principal(), {
      
      DT::selectRows(
        DT::dataTableProxy("panel"),
        obs_disabled_principal() %>% which()
      )
      
    })
    
    
    # NOME DAS VARIAVEIS
    nms <- reactive({ 
      
      nms <- names(non_spatial_data()) 
      
      setdiff(nms,  "Elemento")
      
      
    })
    
    # OPCAO DE VARIAVEIS A EXIBIR
    observeEvent(non_spatial_data(), {
      
      #   shinyWidgets::updatePickerInput(
      #     session = session,
      #     inputId = "select_vars",
      #     selected = character(0),
      #     choices = nms())
      #   
      
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "select_vars",
        choices = nms(),
        selected = character(0),
        choicesOpt = list(
          content = nms() %>% format_choices(50)
        )
      )
    })
    
    # EXIBIR AS VARIAVEIS SELECIONADAS ACIMA
    observeEvent(
      input$select_vars, 
      ignoreInit = TRUE, 
      ignoreNULL = FALSE, {
        req(non_spatial_data())
        
        # OBTER INDICES
        indice <- c("Elemento", nms()) %in%  c("Elemento", input$select_vars) 
        indice <- which(indice) - 1
        
        all_column_idexes <- seq_along(non_spatial_data()) - 1
        
        # ATUALIZAR COLUNAS A EXIBIR
        panel_proxy %>% 
          
          DT::hideCols(
            all_column_idexes,
            reset = FALSE) %>% 
          
          DT::showCols(
            indice,
            reset =  TRUE
          )
        
      })
    
    # HABILITAR TODOS
    observeEvent(input$enable_all, {
      
      DT::selectRows(panel_proxy, selected = NULL)
      
    })
    
    # DESABILITAR TODOS
    observeEvent(input$disable_all, {
      
      DT::selectRows(panel_proxy, selected = n_obs_seq() )
      
    })
    
    
    # OBTER OS HABILITADOS DA AREA DE MODELAGEM
    observeEvent(input$get_enabled, {
      
      index <- which(isolate(obs_disabled_secundary()))
      
      DT::selectRows(panel_proxy, index)
      
    })
    
    
    # ENVIAR DADOS SELECIONADOS EM TEMPO REAL PARA O prop$obs_disabled_secundary
    observeEvent(input$send_enabled, {
      
      index <- n_obs_seq() %in% input$panel_rows_selected
      send(index)#(dados()$Elemento %in% input$explo_datatable_rows_selected )
      
      #DT::selectRows(DT::dataTableProxy("model_datatable"), selected = which(hab))
      #central$prop$obs_disabled <- hab
      
    })
    
  
    list(
      # ENVIAR DADOS SELECIONADOS EM TEMPO REAL PARA O prop$obs_disabled_principal
      #principal = shiny::reactive(n_obs_seq() %in% input$panel_rows_selected),
      principal = shiny::reactive({
         
        shiny::req(non_spatial_data()) 
        #isso \u00E9 necessario para que a expressao abaixo nao seja executada antes
        #do data frame principal acima ser criado. QUando ela eh executada sem
        #ele ser criado, assume-se o valor de null para linhas selecionadas e se
        #habilitam todas as linhas
           
        
           n_obs_seq() %in% input$panel_rows_selected
        
        
        }),
      secundary = shiny::reactive(send())
    )
    
    
  }) 
}


AE_picker_var <- function(input_id, label) {
  
  shinyWidgets::pickerInput(
    inputId = input_id,
    label = label,
    width = "100%",
    inline = FALSE,
    choices = NULL,
    multiple = TRUE,
    options = shinyWidgets::pickerOptions(
      title = "Selecione",
      #actionsBox = TRUE,
      #deselectAllText = "Selecionar Nenhuma",
      #selectAllText = "Selecionar Todas",
      dropupAuto = TRUE,
      #header = "Visualizar nesse painel:",
      liveSearch = TRUE,
      liveSearchNormalize = TRUE,
      maxOptions = 1,
      size = 7
      #selectedTextFormat = "count"
    )
  )
}
AE_picker_transf <- function(input_id, label) {
  
  shinyWidgets::pickerInput(
    inputId = input_id,
    label = label,
    width = "100%",
    inline = FALSE,
    choices = NULL,
    multiple = FALSE,
    options = shinyWidgets::pickerOptions(
      title = "Selecione",
      #actionsBox = TRUE,
      #deselectAllText = "Selecionar Nenhuma",
      #selectAllText = "Selecionar Todas",
      dropupAuto = TRUE,
      #header = "Visualizar nesse painel:",
      #liveSearch = TRUE,
      #liveSearchNormalize = TRUE,
      maxOptions = 1,
      size = 7
      #selectedTextFormat = "count"
    )
  )
}



MO_picker_var <- function(input_id, 
                          label, 
                          maxOpt = NULL, 
                          choices = NULL, 
                          selected = NULL,
                          actionsBox = FALSE,
                          deselectAllText = "Selecionar Nenhuma",
                          selectAllText = "Selecionar Todas"
                          
) {
  
  shinyWidgets::pickerInput(
    inputId = input_id,
    label = label,
    width = "100%",
    inline = FALSE,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = shinyWidgets::pickerOptions(
      title = "Selecione",
      actionsBox = actionsBox,
      deselectAllText = deselectAllText,
      selectAllText = selectAllText,
      dropupAuto = TRUE,
      #header = "Visualizar nesse painel:",
      liveSearch = TRUE,
      liveSearchNormalize = TRUE,
      maxOptions = maxOpt,
      size = 7
      #selectedTextFormat = "count"
    )
  )
}


# ANALISE EXPLORATORIA ----------------------------------------------------

plot2d <- function(df,
                   var_x, 
                   var_y, 
                   var_x_trs = NULL, 
                   var_y_trs = NULL, 
                   disabled = NULL,
                   show_disabled = TRUE,
                   cat = NULL, 
                   lm_all = TRUE,
                   lm_by_group = TRUE,
                   show_legend = TRUE,
                   marker_size = 12,
                   alpha = 1,
                   alpha_line = 1,
                   jit = 0,
                   source = NULL) {
  req(df)
  validate(need(!is.null(df$Elemento), "Coluna Elemento Ausente" ))
  . <- NULL
  .hab <- NULL
  
  if (is.null(var_x_trs)) { 
    
    var_x_trs <- "none"
    
  } 
  
  if (is.null(var_y_trs)) { 
    
    var_y_trs <- "none"
    
  } 
  
  
  if (is.null(disabled)) { 
    
    hab <- !logical(NROW(df))
    
  } else { 
    
    hab <- !disabled
    
  }
  
  if (!show_disabled) {
    
    df <- df[hab, ]
    hab <- hab[hab]
    
  }
  
  
  df <- df %>% remove_geo()
  
  validate(need(var_x, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  validate(need(var_y, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  
  validate(need(var_x_trs, "Especifique uma transformada para o eixo horizontal"))
  validate(need(var_y_trs, "Especifique uma transformada para o eixo vertical"))
  
  func_x <- lista_transf[[var_x_trs]]
  func_y <- lista_transf[[var_y_trs]]
  
  nome_x <- nome_transf[[var_x_trs]](var_x)
  nome_y <- nome_transf[[var_y_trs]](var_y)
  
  
  
  
  df_aux <- dplyr::tibble(
    ".Elemento" = df$Elemento,
    !!rlang::sym(var_x) := func_x(df[[var_x]]),
    !!rlang::sym(var_y) := func_y(df[[var_y]]),
    .hab = dplyr::case_when(
      hab == TRUE ~ "Hab.",
      hab == FALSE ~ "Desab."),
    .cat = .hab,
    .cat_name = .hab
  )
  
  if (jit != 0 && is.numeric(df_aux[[var_x]])) {
    
    df_aux[var_x] <- base::jitter(df_aux[[var_x]], jit)
    
  }
  if (jit != 0 && is.numeric(df_aux[[var_y]])) {
    
    df_aux[var_y] <- jitter(df_aux[[var_y]], jit)
    
  }
  
  
  
  if (shiny::isTruthy(cat)) {
    
    df_aux$.cat_name <- paste(df_aux$.hab, df[[cat]])
    df_aux$.cat <- df[[cat]]
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  } else {
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  }
  
  
  p <- df_aux[hab, ] %>%
    
    plotly::plot_ly(source = source) %>%
    
    plotly::add_markers(
      
      x = ~base::get(var_x),
      y = ~base::get(var_y),
      
      marker = list(size = marker_size ),
      alpha = alpha,
      color = ~I(colorir(.cat)),
      name = ~.cat_name,
      customdata = ~base::get(".Elemento"),
      visible = TRUE,#   NULL,#'legendonly',
      #name = "Observados",
      text = ~paste("<b>Elemento:", .Elemento ),
      hovertemplate = paste(
        "%{text}<br><br>",
        nome_x, ": <b>%{x}</b><br>",
        nome_y, ": <b>%{y}</b><br>"
      )
    ) %>% 
    
    plotly::layout(
      showlegend = show_legend,
      xaxis = list(title = nome_x),
      yaxis = list(title = nome_y),
      dragmode = "select"
      
    )  %>%
    
    plotly::event_register("plotly_selected") %>%
    plotly::event_register("plotly_deselect")
  
  p <- p %>%
    
    plotly::add_markers(
      data = df_aux[!hab, ],
      
      x = ~base::get(var_x),
      y = ~base::get(var_y),
      
      marker = list(size = marker_size ),
      color = ~I(colorir(.cat)),
      alpha = alpha,
      name = ~.cat_name,
      visible = 'legendonly',
      customdata = ~.Elemento,
      #legendgroup = 'Desabilitados',
      #name = "Desabilitado",
      text = ~paste("<b>Elemento:", .Elemento ),
      hovertemplate = paste(
        "%{text}<br><br>",
        nome_x, ": <b>%{x}</b><br>",
        nome_y, ": <b>%{y}</b><br>"
      )
    )
  
  all_numeric <- vapply(df_aux[, c(var_x, var_y)], is.numeric, logical(1)) %>% 
    all()
  
  
  
  if (lm_by_group && all_numeric) {
    
    df_list <- base::split(df_aux, df_aux$.cat_name)
    
    fmla <- paste0("`", var_y,"`", " ~ ", "`",var_x, "`") %>%  stats::as.formula()
    
    for (i in seq_along(df_list)) {
      
      df2 <- df_list[[i]]
      
      m <- stats::lm(data = df2, formula = fmla)
      
      p <- p %>%
        plotly::add_lines(
          data = df2,
          x = ~base::get(var_x),
          y = stats::fitted(m),
          opacity = alpha_line,
          color = ~I(colorir(.cat)),
          name = ~.cat_name,
          visible = ~ifelse(unique(.hab) == "Hab.", TRUE, "legendonly")
        )
    }
    
  }
  
  if (lm_all && all_numeric) { 
    
    pal <- leaflet::colorFactor(c("black", "grey"), levels = c("Habilitados", "Desabilitados"))
    
    df_aux <- df_aux %>% 
      dplyr::mutate(
        .hab = dplyr::case_when(
          .hab == "Hab." ~ "Habilitados",
          TRUE ~ "Desabilitados"
        )
      )
    
    df_list <- base::split(df_aux, df_aux$.hab)
    
    fmla <- paste0("`", var_y,"`", " ~ ", "`",var_x, "`") %>%  stats::as.formula()
    
    for (i in seq_along(df_list)) {
      
      df2 <- df_list[[i]]
      
      m <- stats::lm(data = df2, formula = fmla)
      
      p <- p %>%
        plotly::add_lines(
          data = df2,
          x = ~base::get(var_x),
          y = stats::fitted(m),
          
          #opacity = alpha_line,
          line = list(
            color = ~pal(.hab),
            width = 4
          ),
          
          name = ~.hab,
          visible = ~ifelse(unique(.hab) == "Habilitados", TRUE, "legendonly")
        )
    }
  }
  
  
  
  p
}

plot1d <- function(df,
                   var_x, 
                   var_x_trs = NULL, 
                   disabled = NULL,
                   show_disabled = TRUE,
                   cat = NULL, 
                   show_legend = TRUE,
                   alpha = 1,
                   barmode = "overlay",
                   nbinsx = 0,
                   histnorm  = "percent",
                   histfunc = "count",
                   cumula = FALSE,
                   show_mean_median = FALSE,
                   font_size = 0,
                   
                   source = NULL
) {
  req(df)
  validate(need(!is.null(df$Elemento), "Coluna Elemento Ausente" ))
  .hab <- NULL
  
  if (is.null(var_x_trs)) { 
    
    var_x_trs <- "none"
    
  } 
  
  
  if (is.null(disabled)) { 
    
    hab <- !logical(NROW(df))
    
  } else { 
    
    hab <- !disabled
    
  }
  
  if (!show_disabled) {
    
    df <- df[hab, ]
    hab <- hab[hab]
    
  }
  
  
  df <- df %>% remove_geo()
  
  validate(need(var_x, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  
  validate(need(var_x_trs, "Especifique uma transformada para o eixo horizontal"))
  
  func_x <- lista_transf[[var_x_trs]]
  nome_x <- nome_transf[[var_x_trs]](var_x)
  
  df_aux <- dplyr::tibble(
    ".Elemento" = df$Elemento,
    !!rlang::sym(var_x) := func_x(df[[var_x]]),
    .hab = dplyr::case_when(
      hab == TRUE ~ "Hab.",
      hab == FALSE ~ "Desab."),
    .cat = .hab,
    .cat_name = .hab
  )
  
  if (shiny::isTruthy(cat)) {
    
    df_aux$.cat_name <- paste(df_aux$.hab, df[[cat]])
    df_aux$.cat <- df[[cat]]
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  } else {
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  }
  
  
  
  p <- df_aux[hab, ] %>%
    
    plotly::plot_ly(source = source) %>%
    
    plotly::layout(
      xaxis = list(title = list(text = nome_x, font = list(size = font_size))),
      dragmode = "select",
      barmode = barmode,
      showlegend = show_legend
      
    )  %>%
    
    plotly::event_register("plotly_click") %>%
    plotly::event_register("plotly_deselect") %>% 
    
    plotly::add_histogram(
      
      x = ~base::get(var_x),
      
      color = ~I(colorir(.cat)),
      name = ~.cat_name,
      customdata = ~base::get(".Elemento"),
      visible = TRUE,
      alpha = alpha,
      histnorm = histnorm,
      nbinsx = nbinsx,
      histfunc = histfunc,
      cumulative = list(enabled = cumula)
      # ,
      # 
      # text = ~paste("<b>Elemento:", Elemento ),
      # hovertemplate = paste(
      #   "%{text}<br><br>",
      #   nome_x, ": <b>%{x}</b><br>"
      # )
    ) 
  
  
  p <- p %>%
    
    plotly::add_histogram(
      data = df_aux[!hab, ],
      
      x = ~base::get(var_x),
      
      color = ~I(colorir(.cat)),
      name = ~.cat_name,
      customdata = ~base::get(".Elemento"),
      
      alpha = alpha,
      histnorm = histnorm,
      nbinsx = nbinsx,
      histfunc = histfunc,
      cumulative = list(enabled = cumula),
      visible = 'legendonly'
      # ,
      # 
      # text = ~paste("<b>Elemento:", Elemento ),
      # hovertemplate = paste(
      #   "%{text}<br><br>",
      #   nome_x, ": <b>%{x}</b><br>"
      # )
    ) 
  
  
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      visible = TRUE,
      line = list(color = color)
    )
  }
  
  
  is_numeric <- is.numeric(df_aux[[var_x]])
  
  if (show_mean_median && is_numeric) {
    
    media <- base::mean(df_aux[[var_x]])
    mediana <- stats::median(df_aux[[var_x]])
    
    p <- p %>% plotly::layout(
      shapes = list(   
        vline(media, "blue"),
        vline(mediana, "red")
      )
    ) 
  }
  
  p
}



plot3d <- function(df,
                   var_x, 
                   var_y, 
                   var_z,
                   var_x_trs = NULL, 
                   var_y_trs = NULL, 
                   var_z_trs = NULL, 
                   disabled = NULL,
                   show_disabled = TRUE,
                   cat = NULL, 
                   #plan_by_group = TRUE,
                   plan_hab = TRUE,
                   show_legend = TRUE,
                   marker_size = 12,
                   alpha = 1,
                   alpha_plane = 1,
                   jit = 0,
                   source = NULL) {
  req(df)
  validate(need(!is.null(df$Elemento), "Coluna Elemento Ausente" ))
  .hab <- NULL
  
  if (is.null(var_x_trs)) { 
    
    var_x_trs <- "none"
    
  } 
  
  if (is.null(var_y_trs)) { 
    
    var_y_trs <- "none"
    
  } 
  
  if (is.null(var_z_trs)) { 
    
    var_z_trs <- "none"
    
  } 
  
  
  if (is.null(disabled)) { 
    
    hab <- !logical(NROW(df))
    
  } else { 
    
    hab <- !disabled
    
  }
  
  if (!show_disabled) {
    
    df <- df[hab, ]
    hab <- hab[hab]
    
  }
  
  
  df <- df %>% remove_geo()
  
  validate(need(var_x, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  validate(need(var_y, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  validate(need(var_z, "Especifique uma vari\u00E1vel para o eixo horizontal"))
  
  validate(need(var_x_trs, "Especifique uma transformada para o eixo horizontal"))
  validate(need(var_y_trs, "Especifique uma transformada para o eixo vertical"))
  validate(need(var_z_trs, "Especifique uma transformada para o eixo z"))
  
  func_x <- lista_transf[[var_x_trs]]
  func_y <- lista_transf[[var_y_trs]]
  func_z <- lista_transf[[var_z_trs]]
  
  nome_x <- nome_transf[[var_x_trs]](var_x)
  nome_y <- nome_transf[[var_y_trs]](var_y)
  nome_z <- nome_transf[[var_z_trs]](var_z)
  
  
  df_aux <- dplyr::tibble(
    ".Elemento" = df$Elemento,
    !!rlang::sym(var_x) := func_x(df[[var_x]]),
    !!rlang::sym(var_y) := func_y(df[[var_y]]),
    !!rlang::sym(var_z) := func_z(df[[var_z]]),
    
    .hab = dplyr::case_when(
      hab == TRUE ~ "Hab.",
      hab == FALSE ~ "Desab."),
    .cat = .hab,
    .cat_name = .hab
  )
  
  
  if (jit != 0 && is.numeric(df_aux[[var_x]])) {
    
    df_aux[var_x] <- jitter(df_aux[[var_x]], jit)
    
  }
  if (jit != 0 && is.numeric(df_aux[[var_y]])) {
    
    df_aux[var_y] <- jitter(df_aux[[var_y]], jit)
    
  }
  if (jit != 0 && is.numeric(df_aux[[var_z]])) {
    
    df_aux[var_z] <- jitter(df_aux[[var_z]], jit)
    
  }
  
  
  
  if (shiny::isTruthy(cat)) {
    
    df_aux$.cat_name <- paste(df_aux$.hab, df[[cat]])
    df_aux$.cat <- df[[cat]]
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  } else {
    
    colorir <- leaflet::colorFactor("Set1", levels = df_aux[[".cat"]] %>% unique)
    
  }
  
  
  
  p <- df_aux[hab, ] %>%
    
    plotly::plot_ly(source = source) %>%
    
    plotly::add_markers(
      
      x = ~base::get(var_x),
      y = ~base::get(var_y),
      z = ~base::get(var_z),
      
      marker = list(size = marker_size ),
      alpha = alpha,
      color = ~I(colorir(.cat)),
      name = ~.cat_name,
      customdata = ~base::get(".Elemento"),
      visible = TRUE,#   NULL,#'legendonly',
      #name = "Observados",
      text = ~paste("<b>Elemento:", .Elemento ),
      hovertemplate = paste(
        "%{text}<br><br>",
        nome_x, ": <b>%{x}</b><br>",
        nome_y, ": <b>%{y}</b><br>",
        nome_z, ": <b>%{z}</b><br>"
      )
    ) %>% 
    
    plotly::layout(
      showlegend = show_legend,
      scene = list(
        xaxis = list(title = nome_x),
        yaxis = list(title = nome_y),
        zaxis = list(title = nome_z)
      )
      
    )  %>%
    
    plotly::event_register("plotly_click") %>%
    plotly::event_register("plotly_deselect")
  
  p <- p %>%
    
    plotly::add_markers(
      data = df_aux[!hab, ],
      
      x = ~base::get(var_x),
      y = ~base::get(var_y),
      z = ~base::get(var_z),
      
      marker = list(size = marker_size ),
      alpha = alpha,
      color = ~I(colorir(.cat)),
      name = ~.cat_name,
      customdata = ~base::get(".Elemento"),
      visible = 'legendonly',
      #name = "Observados",
      text = ~paste("<b>Elemento:", .Elemento ),
      hovertemplate = paste(
        "%{text}<br><br>",
        nome_x, ": <b>%{x}</b><br>",
        nome_y, ": <b>%{y}</b><br>",
        nome_z, ": <b>%{z}</b><br>"
      )
    )
  
  
  all_numeric <- vapply(df_aux[, c(var_x, var_y, var_z)], is.numeric, logical(1)) %>% 
    all()
  
  # if (plan_by_group && all_numeric) {
  #   
  #   df_list <- base::split(df_aux, df_aux$.cat_name)
  #   
  #   fmla <- paste0(var_z, " ~ ", var_x, "+", var_y) %>%  stats::as.formula()
  #   
  #   for (i in seq_along(df_list)) {
  #     
  #     df2 <- df_list[[i]]
  #     
  #     m <- lm(data = df2, formula = fmla)
  #     
  #     x_plan <- seq(min(df2[, var_x]), max(df2[, var_x]), length.out = 10)
  #     y_plan <- seq(min(df2[, var_y]), max(df2[, var_y]), length.out = 10)
  #     
  #     dff <- tidyr::crossing(!!rlang::sym(var_x) := x_plan, !!rlang::sym(var_y) := y_plan)
  #     
  #     dff[[var_z]] <- stats::predict(m, dff)
  #     
  #     z_plan <- matrix(dff[[var_z]], ncol = 10)
  #     
  #     p <- p %>%
  #       plotly::add_surface(
  #         x = ~x_plan,
  #         y = ~y_plan,
  #         z = ~z_plan,
  #         colors = 'Surface',
  #         opacity = alpha_plane,
  #         #name = ~.cat_name,
  #         colorbar = list(title = "z-Calculado"),
  #         #text = ~paste("Valores Calculado" ),
  #         visible = ~ifelse(unique(.hab) == "Hab.", TRUE, "legendonly")
  #       )
  #    
  #   }
  #   
  # }
  
  if (plan_hab && all_numeric) { 
    
    pal <- leaflet::colorFactor(c("black", "grey"), levels = c("Habilitados", "Desabilitados"))
    
    df_aux <- df_aux %>% 
      dplyr::mutate(
        .hab =  dplyr::case_when(
          .hab == "Hab." ~ "Habilitados",
          TRUE ~ "Desabilitados"
        )
      )
    
    df_list <- base::split(df_aux, df_aux$.hab)
    
    fmla <- paste0("`", var_z, "`", " ~ ", "`", var_x, "`","+","`", var_y,"`") %>%  stats::as.formula()
    
    #for (i in seq_along(df_list)) {
    
    df2 <- df_list[["Habilitados"]]
    
    m <-  stats::lm(data = df2, formula = fmla)
    
    x_plan <- seq(min(df2[, var_x]), max(df2[, var_x]), length.out = 10)
    y_plan <- seq(min(df2[, var_y]), max(df2[, var_y]), length.out = 10)
    
    dff <- tidyr::crossing(!!rlang::sym(var_x) := x_plan, !!rlang::sym(var_y) := y_plan)
    
    dff[[var_z]] <- stats::predict(m, dff)
    
    z_plan <- matrix(dff[[var_z]], ncol = 10)
    
    p <- p %>%
      plotly::add_surface(
        x = ~x_plan,
        y = ~y_plan,
        z = ~z_plan,
        colors = 'Viridis',
        opacity = alpha_plane,
        name = "Plano de Regress\u00E3o",
        colorbar = list(title = "z-Calculado")#,
        #text = ~paste("Valores Calculado" ),
        #visible = ~ifelse(unique(.hab) == "Hab.", TRUE, "legendonly")
      )
    
    #}
  }
  
  
  p
}






# Funcoes Georreferenciamento ---------------------------------------------

null_map <- function() {
  
  
  leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
    
    leaflet::addTiles() %>%
    
    #leaflet::addTiles(group = "OSM (default)") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$Stamen.TonerLite, group = "Toner Lite") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap, group = "OSM (default)") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery, group = "Satelite") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.DarkMatter, group = "Escuro") %>%
    
    
    leaflet::addMeasure(
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      position = "bottomright") 
  
}




#mapa vazio
city_map <- function(df) {
  
  # configuracoes iniciais
  df_map <- df
  if (crosstalk::is.SharedData(df)) {
    
    df <- df %>% plotly::plot_ly() %>% plotly::plotly_data()
    
  }
  
  
  bbox <- df %>% sf::st_bbox()
  
  p <- leaflet::leaflet(df_map, 
                        options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
    
    leaflet::fitBounds(
      bbox[[1]],
      bbox[[2]],
      bbox[[3]],
      bbox[[4]]) %>%
    
    #leaflet::addTiles(group = "OSM (default)") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$Stamen.TonerLite, group = "Toner Lite") %>%
    
    
    
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery, group = "Satelite") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.DarkMatter, group = "Escuro") %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap, group = "OSM (default)") %>%
    
    
    
    
    leaflet::addMeasure(
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      position = "bottomright") 
  
  p
  
}

# adicao dos dados
city_map_data <- function(mapa, 
                          df, 
                          obs_disabled, 
                          cat = NULL, 
                          opacity_border = 0.6, 
                          opacity_fill = 0.6, 
                          size = 20) {
  
  # configuracoes iniciais
  df_map <- df
  if (crosstalk::is.SharedData(df)) {
    
    df <- df %>% plotly::plot_ly() %>% plotly::plotly_data()
    
  }
  
  ## criar popup
  pop <- df %>% remove_geo() %>% create_popup_tb()
  
  hab <- obs_disabled
  
  habilitados <- dplyr::case_when(
    hab == TRUE ~ "Desabilitado",
    hab == FALSE ~ "Habilitado"
  )
  
  # cores por categoria ou nao
  cat_check <- shiny::isTruthy(cat)
  if (cat_check) {
    
    categoria <- df[[cat]]
    
    pal <- leaflet::colorFactor("Set3", categoria)
    col <- pal(categoria)
    
  } else {
    
    pal <- leaflet::colorFactor(c("blue", "red"), factor(c(TRUE, FALSE)))
    col <- pal(factor(hab))
    
  }
  
  
  #criacao dos mapas
  mapa <- mapa %>%  #proxy_explo_map %>%
    
    leaflet::removeControl("legenda") %>%
    
    leaflet::clearGroup("Habilitado") %>% 
    
    leaflet::clearGroup("Desabilitado") %>% 
    
    leaflet::addCircles(
      data = df_map,
      radius = size,
      popup = pop,
      color = col,
      group = habilitados,
      weight = 5,
      opacity = opacity_border,
      fillOpacity = opacity_fill,
      popupOptions = list(maxHeight = 300),
      layerId = ~as.character(Elemento),
      highlightOptions = leaflet::highlightOptions(
        
        color = "hotpink",
        #fillColor = "blue",
        #dashArray = "4",
        opacity = 1.0,
        weight = 10,
        bringToFront = TRUE
      )
    ) 
  
  
  # adicao da legenda de cores no caso de categorizacao
  if (cat_check) {
    
    mapa <- mapa %>%
      leaflet::addLegend(
        position = "topright",
        pal = pal,
        values = categoria,
        title = cat,
        layerId = "legenda"
      )
    
  }
  
  
  mapa #%>% leaflet::showGroup("Toner Lite")
  
}


tool_draw <- function(mapa) {
  
  mapa %>% 
    
    leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>% 
    
    leaflet.extras::addDrawToolbar(
      targetGroup = 'draw',
      position = "topright",
      circleMarkerOptions = FALSE,
      singleFeature = TRUE,
      editOptions = leaflet.extras::editToolbarOptions(edit = FALSE)
    )
  
}






# Criacao da Legenda
city_map_legend <- function(mapa, 
                            obs_disabled = NULL, 
                            geo_model = list(), 
                            geo_influence = list(), 
                            geo_shp = list()) { 
  
  legenda <- character(0)
  
  
  if (!is.null(obs_disabled) && any(!obs_disabled)) {
    
    legenda <- append(legenda, "Habilitado")
    
  }
  
  if (!is.null(obs_disabled) && any(obs_disabled)) {
    
    legenda <- append(legenda, "Desabilitado")
    
  }
  
  
  map_list <- geo_model
  empty <- !length(map_list)
  
  if (!empty) {
    
    nms <- names(map_list)
    legenda <- append(legenda, nms)
  }
  
  
  map_list <- geo_shp
  empty <- !length(map_list)
  
  if (!empty) {
    
    nms <- names(map_list)
    legenda <- append(legenda, nms)
  }
  
  map_list <- geo_influence
  empty <- !length(map_list)
  
  if (!empty) {
    
    nms <- names(map_list)
    legenda <- append(legenda, nms)
  }
  
  
  
  
  
  
  mapa %>%
    
    leaflet::addLayersControl(
      baseGroups = c("OSM (default)", "Toner Lite", "Satelite", "Escuro"),
      overlayGroups = legenda,
      options = leaflet::layersControlOptions(
        collapsed = TRUE,
        position = "bottomright")
    )
}



# adicao das camadas de influencia
city_map_influence <- function(mapa, geo_influence, paleta, update = NULL) {
  
  
  empty <- !length(geo_influence)
  if (empty) return(mapa)
  
  # if (!is.null(update)) {
  #   
  #   added <- update %in% names(geo_influence)
  #   
  #   if (added) {
  #     #se for adciionadao
  #     geo_influence <- geo_influence[update]
  #     
  #     
  #   } else {
  #     # se for removido
  #     mapa <- mapa %>% clearGroup(update)
  #     return(mapa)
  #     
  #   }
  #   
  # }
  
  
  nms <- names(geo_influence)
  
  pal <- leaflet::colorFactor(paleta, levels = nms)
  
  
  for (i in nms) {
    
    cor <- pal(i)
    
    fonte <- geo_influence[[i]] %>% sf::st_transform(4326)
    tipo  <- fonte %>% sf::st_geometry_type() %>% unique() %>% as.character()
    
    pop <- fonte %>% remove_geo() %>% create_popup_tb()
    id <- seq_len(NROW(fonte))
    
    
    if (tipo %in% c("MULTIPOLYGON", "POLYGON")) {
      
      mapa <- mapa %>% 
        
        leaflet::clearGroup(i) %>% 
        
        leaflet::addPolygons(
          data = fonte,
          popup = pop,
          popupOptions = list(maxHeight = 300),
          
          layerId = id,
          group = i,
          color = cor,  #cor,
          highlightOptions = leaflet::highlightOptions(
            color = cor,
            opacity = 1.0,
            weight = 10,
            bringToFront = FALSE)
        )
      
    } else if (tipo %in% c("LINESTRING")) {
      
      mapa <- mapa %>%
        
        leaflet::clearGroup(i) %>% 
        
        leaflet::addPolylines(
          data = fonte,
          popup = pop,
          color = cor,  
          
          popupOptions = list(maxHeight = 300),
          layerId = id,
          group = i,
          highlightOptions = leaflet::highlightOptions(
            color = cor,
            opacity = 1.0,
            weight = 10,
            bringToFront = FALSE)
        )
      
    } else if (tipo %in% c("POINT", "MULTIPOINT")) {
      
      mapa <- mapa %>%
        
        leaflet::clearGroup(i) %>% 
        
        leaflet::addMarkers(
          data = fonte,
          popup = pop,
          popupOptions = list(maxHeight = 300),
          layerId = id,
          group = i
        )
      
    }
  }
  
  mapa
  
}



check_geo_name <- function(session, new_name, old_names) {
  
  check_name <- new_name %in% old_names
  
  if (any(check_name)) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Identificador j\u00E1 em uso!",
      text = "Nome de geometria j\u00E1 existente! Escolha um nome \u00FAnico, ou remova a geometria anterior",
      type = "error",
      html = TRUE
    )
    
    req(FALSE)
    
  }
}




check_download_maps <- function(session, 
                                selected, 
                                geo_influence, 
                                geo_shp, 
                                geo_model, 
                                dados) {
  
  
  if (length(selected) == 0) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Download Cancelado!",
      text = "Nenhuma camada selecionada",
      type = "info",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  
  if (is.null(names(geo_influence)) & "geo_influence" %in% selected) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Download Cancelado!",
      text = "Nenhuma elemento encontrado na camada de Polos influenciantes.<br/><br/> Desabilite o download dessa camada ou insira seus respectivos elementos." %>% shiny::HTML(),
      type = "info",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  if (is.null(names(geo_shp)) & "geo_shp" %in% selected) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Download Cancelado!",
      text = "Nenhuma elemento encontrado na camada de Mapas.<br/><br/> Desabilite o download dessa camada ou insira seus respectivos elementos." %>% shiny::HTML(),
      type = "info",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  
  if (is.null(names(geo_model)) & "geo_model" %in% selected) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Erro no download!",
      text = "Nenhuma elemento encontrado na camada de Regi\u00F5es Vinculadas ao Modelo.<br/><br/> Desabilite o download dessa camada ou insira seus respectivos elementos."  %>% shiny::HTML(),
      type = "error",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  
  
  if (is.null(dados) & "geo_obs" %in% selected) {
    
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Download Cancelado!",
      text = "Nenhuma elemento encontrado na camada de Banco de Dados.<br/><br/> Desabilite o download dessa camada ou insira seus respectivos elementos." %>% shiny::HTML(),
      type = "info",
      html = TRUE
    )
    
    req(FALSE)
    
  }
  
  
}



# OBTER OBJETO ESPACIAL

get_sf <- function(shape) {
  . <- NULL
  feature <- shape
  feature_type <- feature$properties$feature_type
  
  epsg <- 4326
  
  
  if (feature_type %in% c("rectangle","polygon")) {
    
    p <- feature$geometry$coordinates %>%
      unlist() %>%
      matrix(ncol = 2, byrow = T) %>%
      list(outer = .) %>%
      sf::st_polygon() %>%
      sf::st_sfc(crs = epsg)
    
  } else if (feature_type %in% c("polyline")) {
    
    p <-  feature$geometry$coordinates %>%
      unlist() %>%
      matrix(ncol = 2, byrow = T) %>%
      sf::st_linestring() %>%
      sf::st_sfc(crs = epsg)
    
    
    
  } else if (feature_type %in% c("marker")) {
    
    p <-  feature$geometry$coordinates %>%
      unlist() %>%
      sf::st_point() %>%
      sf::st_sfc(crs = epsg)
    
  } else if (feature_type %in% c("circle", "circlemarker")) {
    
    radius <- feature$properties$radius
    
    
    
    p <- feature$geometry$coordinates %>%
      unlist() %>%
      sf::st_point() %>%
      sf::st_sfc(crs = epsg) %>% sf::st_transform(3857)
    
    p <- sf::st_buffer(p, radius) %>% sf::st_transform(4326)
    
  }
  
  p %>% sf::st_transform(3857) %>% sf::st_transform(4326)
}




# ADCIIONAR OBJETOS ESPACIAIS AO MAPA





# POPUPS DOS MAPAS
create_popup_tb <- function(data){
  . <- NULL
  names <- names(data)
  novo <- lapply(names, function(x, df) {
    
    paste0("<tr><td align='right'>", x,":</td>","<td align='center'><b>", df[[x]], "</b></td></tr>")
    
    
  }, data) %>%
    `names<-`(., names) %>%
    do.call(cbind, .)
  
  saida <- vector("list", NROW(novo))
  for (i in seq_len(NROW(novo))) {
    
    saida[[i]] <- paste0(novo[i, ], collapse = "")
    
    saida[[i]] <- paste0("<table>", saida[[i]], "</table>")
  }
  saida %>% as.character()
}




add_geo <- function(old, new) {
  
  
  new_influences <- names(new)
  
  for (i in new_influences) {
    
    old[[i]] <- new[[i]]
    
  }
  
  old
  
}


add_geo_shp_var <- function(df, new_geo_shp) {
  
  new_shp_names <- names(new_geo_shp)
  
  for (i in new_shp_names) {
    
    shp <- new_geo_shp[[i]] 
    
    n <- names(shp) %>% setdiff("geometry") 
    
    df[n] <- NULL
    
    df <- sf::st_join(
      x = sf::st_transform(df, 3857),
      y = sf::st_transform(shp, 3857)
    ) %>% sf::st_transform(4326)
  }
  df
  
}
add_geo_influence_var <- function(df, new_geo_influence) {
  
  new_influences <- names(new_geo_influence)
  
  for (i in new_influences) {
    
    dist <- sf::st_distance(df, new_geo_influence[[i]])
    
    df[[i]] <- as.vector(dist)
    
  }
  
  df
  
}




add_spatial_variables <- function (df, spatial_list) {
  
  nms_slot <- names(spatial_list)
  
  # spatial_slot \u00E9 o elemento dentro da lista em si: o nome "geo_shp",
  # "geo_influence", "geo_model". vamos acessar cada um deles
  for (i in nms_slot) {
    nms_inside <- names(spatial_list[[i]])
    
    for (j in nms_inside) {
      spatial_element <- spatial_list[[i]][[j]]
      
      if (i == "geo_influence") {
        dist <- sf::st_distance(df, spatial_element) 
        
        df[[j]] <- dist %>% as.vector()
        
      }
      
      if (i == "geo_shp") {
        
        n <- names(spatial_element) %>% setdiff("geometry") 
        
        df[n] <- NULL
        
        df <- sf::st_join(
          x = sf::st_transform(df, 3857),
          y = sf::st_transform(spatial_element, 3857)
        ) %>% sf::st_transform(4326)
        
      }
      
    }
  }
  
  df
}

add_spatial_list <- function(prop, spatial_list) {
  
  nms_slot <- names(spatial_list)
  
  for (i in nms_slot) {
    nms_inside <- names(spatial_list[[i]])
    
    for (j in nms_inside) {
      
      prop[[i]][[j]] <- spatial_list[[i]][[j]]
      
    }
  }
  
  prop
  
}

get_spatial_names <- function (prop, unlist = FALSE) {
  
  lis <- list(
    
    geo_influence_names = names(prop$geo_influence),
    
    geo_shp_names = lapply(prop$geo_shp, names) 
    
  )
  
  
  if (unlist) {
    
    lis <- unlist( lis)
    
    
  }
  
  
  lis
  
}

criar_ponto_espacial <- function(lat, lon, epsg) {
  
  df <- dplyr::tibble(Longitude = lon, Latitude = lat)
  
  sf::st_as_sf(df, 
               coords = c("Longitude" , "Latitude"), 
               crs = epsg, 
               remove = TRUE) %>% 
    
    sf::st_transform(4326)
  
}



coletar_spatial_avaliandos <- function(avaliando, var_indep, prop) {
  #browser()
  . <- bind_cols <- NULL
  # Todos os nomes de todos os elemntos espaciais criados no Modelagem Urbana
  spatial_names_list <- get_spatial_names(prop)
  
  
  # Agora, vamos buscar os nomes dos elementos geo polos influenciantes. O
  # nome dos elementos j\u00E1 \u00E9 o pr\u00F3prio nome da variavel que tras a distancia
  # ate esse polo
  geo_influence_names <- spatial_names_list[["geo_influence_names"]]
  
  
  # Desses elementos, que sao o proprio nome das variaveis, pego somente
  # aquelas habilitadas
  geo_influence_hab <- base::intersect(geo_influence_names, var_indep)
  names(geo_influence_hab) <- geo_influence_hab
  
  # Se existirem variaveis em comum, ou seja de distancia e habilitadas,
  # procede-se as distancias do ponto em estudo
  
  if (geo_influence_hab %>% isTruthy()) {
    
    geo_influence_distance <- lapply(geo_influence_hab, function(i) {
      
      # st_distance(prop$geo_influence[[i]], ponto_avaliando())
      sf::st_distance(
        avaliando, 
        prop$geo_influence[[i]]
      ) %>% `colnames<-`(geo_influence_hab[i])
      
    }) %>%  do.call(cbind, .) %>% dplyr::as_tibble()
    
    
  } else {
    
    geo_influence_distance <- NULL
    
  }# fim do IF do geo_influence_hab
  
  
  
  
  # Pega cada Objeto Espacial com seus Respectivos Nomes de variaveis
  geo_shp_names <- spatial_names_list[["geo_shp_names"]]
  
  # Separa, desses varaiveis, as que estao habilitadas
  geo_shp_hab <- lapply(geo_shp_names, base::intersect, var_indep)
  
  # se houver var espacial habilitada procede a coleta abaixo
  if (geo_shp_hab %>% unlist() %>% isTruthy()) {
    
    geo_shp_hab_values <- lapply(names(geo_shp_hab), function(x) {
      
      var <- geo_shp_hab[[x]]
      obj <- prop$geo_shp[[x]][var]
      
      
      if (isTruthy(obj)) {
        
        # print(prop$geo_shp[[x]])
        sf::st_join(
          
          avaliando %>% sf::st_transform(3857), 
          obj %>% sf::st_transform(3857)
          
        ) %>% remove_geo()
        
      }
      
    }) %>% do.call(bind_cols, .)
    
  } else {
    
    geo_shp_hab_values <- NULL
    
  }
  
  dplyr::bind_cols(geo_influence_distance , geo_shp_hab_values)
  
}



city_map_residuals <- function(mapa, 
                               prop,
                               analysis_type, 
                               spatial_residuals, 
                               residuos_formatados, 
                               grandeza,
                               size, 
                               opacity_border,
                               opacity_fill,
                               bins
                               
) {
  
  
  # comeca a funcao aqui
  if (analysis_type == "modelling") {
    
    vars <- c("Elemento",
              
              "Var. Dep. Obs. Trns.",
              "Var. Dep. Calc. Trns.",
              
              "Res\u00EDduos Modelagem",
              "Res\u00EDduos Relativos Modelagem",
              "Res\u00EDduos Padronizados Modelagem")
    
    
  } else if (analysis_type == "estimate") {
    
    vars <- c("Elemento",
              
              "Var. Dep. Obs. Estimativa",
              "Var. Dep. Calc. Estimativa",
              
              "Res\u00EDduos Estimativa",
              "Res\u00EDduos Relativos Estimativa",
              "Res\u00EDduos Padronizados Estimativa")
    
  } else if (analysis_type == "both") {
    
    
    vars <- colnames(residuos_formatados)
    
    
  }
  
  # Identifica qual eh a varaivael dependente, a variavel independete
  var_enabled <- prop$var_enabled[prop$var_enabled] %>% names()
  var_dep <- prop$var_dependent
  var_indep <- setdiff(var_enabled, var_dep)
  
  
  # Criar Pop-up
  pop <- create_popup_tb(residuos_formatados[, vars, drop = TRUE] %>% dplyr::as_tibble())
  
  #browser()
  req(grandeza)
  dominio <- spatial_residuals[[grandeza]] #aqui o usuario escolhe
  
  validate(need(bins > 1, "Ao menos duas categorias devem ser escolhidas"))
  
  pal <- leaflet::colorBin(palette = "viridis", domain = dominio, bins = bins)
  #cores <- pal(dominio)
  
  
  #criacao dos mapas
  mapa %>%  #proxy_explo_map %>%
    
    leaflet::removeControl("legenda") %>%
    
    leaflet::clearGroup("Residuos") %>% 
    
    leaflet::addCircles(
      data = spatial_residuals,
      radius = size,
      popup = pop,
      color = ~pal(base::get(grandeza)),
      group = "Residuos",
      weight = 5,
      opacity = opacity_border,
      fillOpacity = opacity_fill,
      popupOptions = list(maxHeight = 300),
      layerId = ~as.character(Elemento),
      highlightOptions = leaflet::highlightOptions(
        
        color = "hotpink",
        #fillColor = "blue",
        #dashArray = "4",
        opacity = 1.0,
        weight = 10,
        bringToFront = TRUE
      )
    ) %>% 
    
    leaflet::addLegend(
      position = "topright",
      pal = pal,
      values = spatial_residuals[[grandeza]],
      title = grandeza,
      layerId = "legenda"
    ) 
  
}




# Funcoes de Modelagem ----------------------------------------------------

# Checa se a base \u00E9 consistente antes de entrar para regressao
check_data_conditions <- function(df, 
                                  session, 
                                  obs_disabled, 
                                  var_enabled, 
                                  var_dep,
                                  transf_for_test = NULL) {
  
  # df \u00E9 data.frame
  is_data.frame <- inherits(df, "data.frame")
  
  if (!is_data.frame) {
    
    msg <- "O objeto especificado n\u00E3o \u00E9 da classe data.frame"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
  }
  
  #verifica se todas as variaveis escolhidas sao numericas
  all_numeric <- vapply(df[var_enabled] %>% remove_geo(), is.numeric, FUN.VALUE = logical(1)) %>% all()
  if (!all_numeric) {
    
    msg <- "Todas as vari\u00E1veis selecionadas devem ser do tipo: num\u00E9rico"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
    
    
    
  }
  
  
  
  # ao menos dois dados hbailitados
  enabled <- !obs_disabled
  n_enabled <- sum(enabled)
  at_leat_2_enabled <- n_enabled >= 2
  
  
  if (!at_leat_2_enabled) {
    
    msg <- "\u00c9 necess\u00E1rio que ao menos dois dados estejam habilitados"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
  }
  
  # ao menos duas variaveis habilitadas
  n_var_enabled <- length(var_enabled)
  at_leat_2_var_enabled <- n_var_enabled >= 2
  
  if (!at_leat_2_var_enabled) {
    
    msg <- "\u00c9 necess\u00E1rio que ao menos duas vari\u00E1veis estejam habilitadas"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
  }
  
  # variavel dependente definida
  var_dep_defined <- shiny::isTruthy(var_dep)
  
  if (!var_dep_defined) {
    
    msg <- "A vari\u00E1vel dependente n\u00E3o est\u00E1 definida"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
  }
  
  var_dep_enabled <- var_dep %in% var_enabled
  
  if (!var_dep_enabled) {
    
    msg <- "A vari\u00E1vel dependente n\u00E3o est\u00E1 habilitada"
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = 2,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
  }
  
  
  
  empty_list <- !length(transf_for_test)
  
  
  
  if (!empty_list && !is.null(transf_for_test)) {
    
    
    for (i in var_enabled) {
      
      if (is.null(transf_for_test[[i]])) {
        
        msg <- paste0("A vari\u00E1vel ", i, " n\u00E3o apresenta transformadas definidas para a pesquisa")
        
        shiny::showNotification(
          ui = msg,
          type = "error",
          duration = 2,
          closeButton = TRUE, 
          session = session)
        
        validate(need(FALSE, msg))
        
      } 
      
    }
  }
  
  
  df
  
}

# filtra a base
filter_data_model <- function(df, 
                              obs_disabled, 
                              var_enabled) {
  
  
  df[!obs_disabled, var_enabled, drop = FALSE] %>% remove_geo()
  
  
  
}

# checa NAs na base filtrada
check_data_na <- function(df, session) {
  
  nms <- names(df)
  #is na
  for (i in nms) {
    
    is_na <- is.na(df[[i]]) %>% any()
    
    if (is_na) {
      
      msg <- paste0("A vari\u00E1vel ", i , " possui valores faltantes (NA)")
      
      shiny::showNotification(
        ui = msg,
        type = "error",
        duration = 2,
        closeButton = TRUE, 
        session = session)
      
      validate(need(FALSE, msg))
      
    }
  }
  
  df
  
}



# transforma a variavel
one_var_transform <- function(name, df, vars_trns_selected) {
  
  transf  <- vars_trns_selected[[name]]
  func <- lista_transf[[transf]]
  
  func(df[[name]])
  
}

# transforma o nome da variavel
one_var_transform_name <- function(name, vars_trns_selected) {
  
  transf  <- vars_trns_selected[[name]]
  func <- nome_transf[[transf]]
  
  func(name)
  
}


# transforma o nome da variavel
one_var_transform_back <- function(name, df, vars_trns_selected) {
  
  transf <- vars_trns_selected[[name]]
  func <- anti_transf[[transf]]
  
  func(df[[name]])
  
}

# transdforma todo o data frame
transform_data_model <- function(vars_trns_selected, df ) {
  
  
  if (inherits(vars_trns_selected, "data.frame")) {
    
    nms <- names(vars_trns_selected)
    vars_trns_selected <- vars_trns_selected %>% t() %>% as.vector()
    names(vars_trns_selected) <- nms
    
  }
  
  
  # df_num <- select_if(df, is.numeric) 
  # df <- select_if(df, ~!is.numeric(.)) 
  
  size <- NROW(df)
  nms <- names(df)
  
  mtz <- vapply(
    X = nms, 
    FUN = one_var_transform, 
    FUN.VALUE = numeric(size), 
    df, 
    vars_trns_selected) 
  
  mtz
}

create_model  <- function(mtz, 
                          var_dep) {
  
  
  df <- dplyr::as_tibble(mtz)
  fmla <- stats::as.formula(paste0("`", var_dep,"`", " ~ ."))
  
  m <- stats::lm(formula = fmla, data = df)
  
  
  
  names(m$coefficients) <- names(m$coefficients) %>% 
    stringr::str_replace_all("`",  "")
  
  
  
  return(m)
  
}



r2 <- function(obs, res) {
  
  v1 <- obs - base::mean(obs)
  
  1 - (res %*% res) / (v1 %*% v1)
  
}


adj_r2 <- function(r2, n_obs, n_var) {
  
  1 - (  (1 - r2) * (n_obs - 1)/(n_obs - n_var - 1 + 1 )  )
  
}

# regression_loop <- function(i, 
#                             matriz, 
#                             var_dep, 
#                             n_obs, 
#                             n_var, 
#                             nms, 
#                             all_combinations) {
#  
#   combinacao <- all_combinations[i, , drop  = TRUE]
#   
#   mtz <- vapply(nms, function(i){ 
#     
#     func <- lista_transf[[combinacao[i]]]
#     
#     func(matriz[, i]) 
#     
#   }, numeric(n_obs)) 
#   
#   
#   # ## obter o indice da var dep
#   var_dep_index <- which(var_dep == colnames(mtz))
#   # 
#   # # separa somente var dependente
#    var_dep_vector <- mtz[, var_dep_index, drop = TRUE]
#   # 
#   # # separar as variaveis independentes e adicionar o intercepto
#    mtz <- cbind(Intercepto = 1, mtz[, -var_dep_index, drop = FALSE])
#   
#   # portanto a base de dados est\u00E1 intacta e pronta para ser regredida nas
#   # variaveis: mtz e var_dep_vector
# 
#   model <- lm.fit(mtz, var_dep_vector)
#   
#   
#   
#   func_back <- anti_transf[[combinacao[var_dep]]]
#   
#   var_dep_obs_trns_scale <- var_dep_vector
#   var_dep_calc_trns_scale <- model$fitted.values
#   residuals_trns_scale <- model$residuals
#   
#   var_dep_obs_natural_scale <- matriz[, var_dep, drop = TRUE]
#   var_dep_calc_natural_scale <- func_back(var_dep_calc_trns_scale)
#   residuals_natural_scale <- var_dep_obs_natural_scale - var_dep_calc_natural_scale
#   
#   
#   r_quadrado_modelagem <- r2(var_dep_obs_trns_scale, residuals_trns_scale)
#   r_quadrado_ajus_modelagem <- adj_r2(r_quadrado_modelagem, n_obs, n_var)
#   correlacao_modelagem <- r_quadrado_modelagem^(1/2)
#   
#   r_quadrado_estimativa <- r2(var_dep_obs_natural_scale, residuals_natural_scale)
#   r_quadrado_ajus_estimativa <- adj_r2(r_quadrado_estimativa, n_obs, n_var)
#   correlacao_estimativa <- r_quadrado_estimativa^(1/2)
#   
#   
#   c(combinacao ,
#     "R\u00B2 Mod" = r_quadrado_modelagem, 
#     "R\u00B2 Adj Mod"= r_quadrado_ajus_modelagem,
#     "Correla\u00E7\u00E3o Mod" = correlacao_modelagem, 
#     "R\u00B2 Est"= r_quadrado_estimativa, 
#     "R\u00B2 Adj Est"= r_quadrado_ajus_estimativa, 
#     "Correla\u00E7\u00E3o Est" = correlacao_estimativa )
#   
#   
#   
# }


regression_loop <- function(i, 
                            matriz, 
                            var_dep, 
                            n_obs, 
                            n_var, 
                            nms, 
                            all_combinations) {
  
  combinacao <- all_combinations[i, , drop  = TRUE]
  
  mtz <- vapply(nms, function(i){ 
    
    func <- lista_transf[[combinacao[i]]]
    
    func(matriz[, i]) 
    
  }, numeric(n_obs)) 
  
  
  # ## obter o indice da var dep
  # var_dep_index <- which(var_dep == colnames(mtz))
  # 
  # # separa somente var dependente
  # var_dep_vector <- mtz[, var_dep_index, drop = TRUE]
  # 
  # # separar as variaveis independentes e adicionar o intercepto
  # mtz <- cbind(Intercepto = 1, mtz[, -var_dep_index, drop = FALSE])
  
  # portanto a base de dados est\u00E1 intacta e pronta para ser regredida nas
  # variaveis: mtz e var_dep_vector
  model <- faster_reg(mtz, var_dep)
  
  #model <- lm.fit(mtz, var_dep_vector)
  
  
  
  func_back <- anti_transf[[combinacao[var_dep]]]
  
  var_dep_obs_trns_scale <- model$observed
  var_dep_calc_trns_scale <- model$fitted.values
  residuals_trns_scale <- model$residuals
  
  var_dep_obs_natural_scale <- matriz[, var_dep, drop = TRUE]
  var_dep_calc_natural_scale <- func_back(var_dep_calc_trns_scale)
  residuals_natural_scale <- var_dep_obs_natural_scale - var_dep_calc_natural_scale
  
  
  r_quadrado_modelagem <- r2(var_dep_obs_trns_scale, residuals_trns_scale)
  r_quadrado_ajus_modelagem <- adj_r2(r_quadrado_modelagem, n_obs, n_var)
  correlacao_modelagem <- r_quadrado_modelagem^(1/2)
  
  r_quadrado_estimativa <- r2(var_dep_obs_natural_scale, residuals_natural_scale)
  r_quadrado_ajus_estimativa <- adj_r2(r_quadrado_estimativa, n_obs, n_var)
  correlacao_estimativa <- r_quadrado_estimativa^(1/2)
  
  
  c(combinacao ,
    "R\u00B2 Mod" = r_quadrado_modelagem, 
    "R\u00B2 Adj Mod"= r_quadrado_ajus_modelagem,
    "Correla\u00E7\u00E3o Mod" = correlacao_modelagem, 
    "R\u00B2 Est"= r_quadrado_estimativa, 
    "R\u00B2 Adj Est"= r_quadrado_ajus_estimativa, 
    "Correla\u00E7\u00E3o Est" = correlacao_estimativa )
  
  
  
}

# 
# regression_loop_lm <- function(combinacao, 
#                                matriz, 
#                                matriz_new,
#                                var_dep, 
#                                conf, 
#                                n_obs, 
#                                n_var, 
#                                nms,
#                                estimador_log_nep,
#                                prop) {
#   
#   
#   
#  
#   
#   mtz <- vapply(nms, function(i){ 
#     
#     func <- lista_transf[[combinacao[[i]]]]
#     
#     func(matriz[, i]) 
#     
#   }, numeric(n_obs)) %>% dplyr::as_tibble()
#   
#   
#   mtz_new <- vapply(names(matriz_new), function(i){ 
#     
#     func <- lista_transf[[combinacao[[i]]]]
#     
#     func(matriz_new[[i]]) 
#     
#   }, 1) %>% as.list() %>% dplyr::as_tibble()
#   
#   
#   # portanto a base de dados est\u00E1 intacta e pronta para ser regredida nas
#   # variaveis: mtz e var_dep_vector
#   fmla <- stats::as.formula(paste0("`", var_dep,"`", " ~ ."))
#   m <- stats::lm(formula = fmla, data = mtz)
#   
#   
#   var_dep_trns <- combinacao[[var_dep]]
#   # browser()
#   calc_new_data(mtz_new, modelo = m, conf) %>% 
#     
#     calc_back_scale_new_data( 
#       estimador_log_nep, 
#       prop, 
#       var_dep_trns) %>% dplyr::as_tibble() 
#   
#   
# }

regression_loop_lm2 <- function(combinacao, 
                                matriz, 
                                matriz_new,
                                var_dep, 
                                conf, 
                                n_obs, 
                                n_var, 
                                nms,
                                estimador_log_nep,
                                prop) {
  
  
  
  
  
  mtz <- vapply(nms, function(i){ 
    
    func <- lista_transf[[combinacao[[i]]]]
    
    func(matriz[, i]) 
    
  }, numeric(n_obs)) %>% dplyr::as_tibble()
  
  
  mtz_new <- vapply(names(matriz_new), function(i){ 
    
    func <- lista_transf[[combinacao[[i]]]]
    
    func(matriz_new[[i]]) 
    
  }, 1) %>% as.list() %>% dplyr::as_tibble()
  
  
  # portanto a base de dados est\u00E1 intacta e pronta para ser regredida nas
  # variaveis: mtz e var_dep_vector
  fmla <- stats::as.formula(paste0("`", var_dep,"`", " ~ ."))
  m <- stats::lm(formula = fmla, data = mtz)
  
  
  var_dep_trns <- combinacao[[var_dep]]
  # browser()
  new <- calc_new_data(mtz_new, modelo = m, conf) %>% 
    
    calc_back_scale_new_data( 
      estimador_log_nep, 
      prop, 
      var_dep_trns) 
  
  c(combinacao, new[1, ])
  
  
}

faster_reg <- function(mtz, var_dep){
  
  #mtz <- as.matrix(mtz)
  
  var_dep_index <- which(var_dep == colnames(mtz))
  var_dep_vector <- mtz[, var_dep_index, drop = TRUE]
  mtz <- cbind(Intercepto = 1, mtz[, -var_dep_index, drop = FALSE])
  
  m <- stats::.lm.fit(mtz, var_dep_vector)
  
  m$fitted.values <- var_dep_vector - m$residuals 
  m$observed <- var_dep_vector
  
  m
  
}



# create_multi_model  <- function(mtz, 
#                                 var_dep, 
#                                 var_dep_obs_natural_scale,
#                                 var_dep_func_back
# ) {
#   
#   if (length(out_metrics_or_model) > 1) {out_metrics_or_model = "model"} 
#   
#   #fmla <- stats::as.formula(paste0("`", var_dep,"`", " ~ ."))
#   
#   #m <- stats::lm(formula = fmla, data = df)
#   
#   # names(m$coefficients) <- names(m$coefficients) %>% 
#   # stringr::str_replace_all("`",  "")
#   
#   
#   # var_dep_index <- which(var_dep == colnames(mtz))
#   # var_dep_vector <- mtz[, var_dep_index, drop = TRUE]
#   # mtz <- cbind(Intercepto = 1, mtz[, -var_dep_index, drop = FALSE])
#   # 
#   # m <- stats::.lm.fit(mtz, var_dep_vector)
#   
#   m <- faster_reg(mtz, var_dep)
#   
#   var_dep_obs_trns_scale <- var_dep_vector
#   var_dep_calc_trns_scale <- var_dep_obs_trns_scale + m$residuals
#   
#   var_dep_obs_natural_scale <- var_dep_obs_natural_scale
#   var_dep_calc_natural_scale <- var_dep_func_back(var_dep_calc_trns_scale)
#   
#   
#   
#   
#   r.squared
#   adj.r.squared
#   sigma
#   statistic
#   p.value
#   
#   
#   if (out_metrics_or_model == "metrics") { 
#     
#     return(broom::glance(m))
#     
#   } 
#   
#   return(m)
#   
# }



create_possibilities <- function(trns_list, n, session) {
  
  
  
  trns_for_test <- tryCatch({ 
    
    
    expand.grid(trns_list, stringsAsFactors = FALSE) %>% as.matrix()
    
    
  }, 
  
  error = function(e) {
    
    #n_trns <- lapply(trns_list, length) %>% unlist()
    #n <- prod(n_trns) %>% prettyNum(big.mark = ".", decimal.mark = ",")
    
    msg <- paste0("N\u00E3o \u00E9 poss\u00EDvel criar todas as ", n, " combina\u00E7\u00F5es. Procure diminuir a quantidade de transformadas.", e)
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = NULL,
      closeButton = TRUE, 
      session = session)
    
    validate(need(FALSE, msg))
    
  })
  
  trns_for_test
  
  
} 


get_residuals <- function(modelo, 
                          df_ready, 
                          df_raw, 
                          prop) {
  
  obs_enabled <- !prop$obs_disabled
  var_dep <- prop$var_dependent
  var_dep_trns <- prop$var_trns_selected[[var_dep]] 
  func_back <- anti_transf[[var_dep_trns]] 
  
  
  Elemento <- df_raw[obs_enabled, "Elemento", drop = TRUE] %>% as.numeric()
  
  var_dep_obs_natural_scale <- df_raw[obs_enabled, var_dep, drop = TRUE]
  var_dep_obs_trns_scale <- df_ready[, var_dep, drop = TRUE] 
  var_dep_calc_trns_scale <- modelo$fitted.values 
  var_dep_calc_natural_scale <- func_back(modelo$fitted.values) 
  
  residuals_trns_scale <- modelo$residuals
  residuals_natural_scale <- var_dep_obs_natural_scale - var_dep_calc_natural_scale
  residuals_relative_trns_scale <- residuals_trns_scale / var_dep_obs_trns_scale
  residuals_relative_natural_scale <- residuals_natural_scale / var_dep_obs_natural_scale
  residuals_trns_scale_padron <- (residuals_trns_scale - base::mean(residuals_trns_scale, na.rm = TRUE)) / stats::sd(residuals_trns_scale, na.rm = TRUE)
  residuals_natural_scale_padron <- (residuals_natural_scale - base::mean(residuals_natural_scale, na.rm = TRUE)) / stats::sd(residuals_natural_scale, na.rm = TRUE)
  
  
  
  matrix(
    c(Elemento, 
      var_dep_obs_natural_scale, 
      var_dep_obs_trns_scale,
      var_dep_calc_trns_scale,
      var_dep_calc_natural_scale,
      residuals_trns_scale,
      residuals_natural_scale,
      residuals_relative_trns_scale,
      residuals_relative_natural_scale,
      residuals_trns_scale_padron,
      residuals_natural_scale_padron),
    ncol = 11
  ) %>% 
    `colnames<-`(
      c("Elemento",
        "Var. Dep. Obs. Estimativa",
        "Var. Dep. Obs. Trns.",
        "Var. Dep. Calc. Trns.",
        "Var. Dep. Calc. Estimativa",
        
        "Res\u00EDduos Modelagem",
        "Res\u00EDduos Estimativa",
        
        "Res\u00EDduos Relativos Modelagem",
        "Res\u00EDduos Relativos Estimativa",
        
        "Res\u00EDduos Padronizados Modelagem",
        "Res\u00EDduos Padronizados Estimativa") )
  
  
  
}


get_correlation <- function(df_prepared, var_dep, type = c("all", "only_indep")) {
  
  stopifnot(type %in% c("all", "only_indep")) 
  
  matrix_cor <- stats::cor(df_prepared)
  
  if (type == "all") { return(matrix_cor) }
  
  lin <- rownames(matrix_cor) != var_dep
  col <- colnames(matrix_cor) != var_dep
  
  matrix_cor2 <- matrix_cor[lin, col]
  
  matrix_cor2
  
}


calc_new_data <- function(new_data, modelo, confianca) {
 . <- NULL
  est_conf <- new_data %>%
    stats::predict.lm(
      object = modelo,
      newdata = .,
      se.fit = TRUE,
      interval = c("confidence"),
      level = confianca/100
    )
  
  est_pred <- new_data %>%
    stats::predict.lm(
      object = modelo,
      newdata = .,
      se.fit = TRUE,
      interval = c("prediction"),
      level = confianca/100
    )
  
  
  pred_inf <- est_pred$fit[, 2, drop = FALSE] %>% `colnames<-`("pred_inf")
  conf_inf <- est_conf$fit[, 2, drop = FALSE] %>% `colnames<-`("conf_inf")
  center <- est_conf$fit[, 1, drop = FALSE] %>% `colnames<-`("media")
  conf_sup <- est_conf$fit[, 3, drop = FALSE] %>% `colnames<-`("conf_sup")
  pred_sup <- est_pred$fit[, 3, drop = FALSE] %>% `colnames<-`("pred_sup")
  se_fit <- est_conf$se.fit 
  df <- est_conf$df 
  #residual_scale <- est_conf$residual.scale  # desvio padrao dos resdiduos dos dados que compoem o modelo
  
  cbind(pred_inf, 
        conf_inf, 
        center,
        conf_sup, 
        pred_sup, 
        se_fit = se_fit, 
        df = df) 
  
}


check_consistencia_inseridos <- function(new_data) {
  
  x <- new_data[1, , drop = TRUE]
  
  check <- vapply(x, shiny::isTruthy, FUN.VALUE = logical(1)) %>% all()
  
  validate(need(check, "Ao menos um valor de inserido para c\u00E1lculo n\u00E3o \u00E9 v\u00E1lido" ))
  
  new_data
  
}


check_extrapolacao <- function(mtz, data, var_nbr_type, session) {
  
  duracao <- 10
  # isolando os valores iniciais em sua escala nao transformada
  x  <- mtz[1, , drop = TRUE]
  
  # Matriz de valores de referencia: Minimos e Maximos
  min_max_sample <- apply(data, 2, range)
  
  
  # Checar quais variaveis estao acima ou abaixo de seus respectivos valores max
  # e min: Emitir Alerta q nao se Enquadra no GIII
  min_max_matrix <- vapply(names(x), FUN = function(i) {
    
    min <- min_max_sample[1, i, drop = TRUE] %>% unname()
    max <- min_max_sample[2, i, drop = TRUE] %>% unname()
    
    valor <- x[[i]]
    
    c(min_abs = valor >= min, 
      max_abs = valor <= max,
      min_50  = valor >= min * 0.5,
      min_100 = valor <= max * 2
    )
    
  }, FUN.VALUE = logical(4))
  
  
  # Verificacao GEral de extrapolacao, minimo ou maximo da amostra. Alguma
  # extrapola? se o resultado dos teste todos forem falsos, entao nenhuma
  # extrapola. Se nenhuma extrapola, n\u00E3o h\u00E1 necessidade de verifica\u00E7\u00F5es
  # suplementares
  var_q_extrapola <- (!min_max_matrix[c(1, 2), , drop = FALSE]) %>% apply(2, any)
  
  # nada extrapola, entao nao realiza nenhuma verificacao
  if (all(!var_q_extrapola)) return(mtz)
  
  
  
  
  # S\u00F3 de haver extrapola\u00E7\u00E3o de m\u00EDnimo ou m\u00E1ximo, inviabiliza-se o GIII. O
  # primeiro aviso \u00E9 sobre o GIII de fundamenta\u00E7\u00F5, portanto. 
  v <- min_max_matrix[1, , drop = TRUE]
  
  # o contrario de "ao menos um verdadeiro" eh "todos sao falsos?"
  if (any(!v)) {
    
    extrapola <- names(v)[!v] %>% paste(collapse = ", ")
    
    msg <- paste0("A(s) vari\u00E1vei(s) ", extrapola, " extrapola(m) seus respectivos valores m\u00EDnimos na amostra. Isso inviabiliza Grau III de Fundamenta\u00E7\u00E3o")
    
    shiny::showNotification(
      ui = msg,
      type = "warning",
      duration = duracao,
      closeButton = TRUE)
    
    
    # em havendo extrapolacao do valor Minimo, verifica-se quanto foi
    # extrapolado, ou seja se a extrapolacao excede menos que metade do
    # valor m\u00EDnimo. Isso inviabiliza qualquer Grau de Fundamenta\u00E7\u00E3o 
    v <- min_max_matrix[3, , drop = TRUE]
    
    # o contrario de "ao menos um verdadeiro" eh "todos sao falsos?"
    if (any(!v)) {
      
      extrapola <- names(v)[!v] %>% paste(collapse = ", ")
      
      msg <- paste0("O valor extrapolado pela(s) vari\u00E1vel(eis) ", extrapola, " \u00E9 menor que metade de seus respectivos valores m\u00EDnimos amostrais. Isso inviabiliza qualquer Grau de Fundamenta\u00E7\u00E3o")
      
      shiny::showNotification(
        ui = msg,
        type = "warning",
        duration = duracao,
        closeButton = TRUE)
      
    }
    
    
  }
  
  
  # Verificacao de Valores Maximos Extrapolados, inviabilizando GIII
  v <- min_max_matrix[2, , drop = TRUE]
  
  # o contrario de "ao menos um verdadeiro" eh "todos sao falsos?"
  if (any(!v)) {
    
    extrapola <- names(v)[!v] %>% paste(collapse = ", ")
    
    msg <- paste0("O valor extrapolado pela(s) vari\u00E1vel(eis) ", extrapola, " excede(m) seu(s) respectivo(s) valor(es) m\u00E1ximo(s) amostrai(s). Isso inviabiliza Grau de Fundamenta\u00E7\u00E3o III")
    
    shiny::showNotification(
      ui = msg,
      type = "warning",
      duration = duracao,
      closeButton = TRUE)
    
    
    
    # Havendo extrapolacao do Valor Maximo, verifica-se quanto foi
    # extrapolado, ou seja se a extrapolacao supera mais que o dobro do
    # valor maximo da amostra
    v <- min_max_matrix[ 4, , drop = TRUE]
    
    # o contrario de "ao menos um verdadeiro" eh "todos sao falsos?"
    if (any(!v)) {
      
      extrapola <- names(v)[!v] %>% paste(collapse = ", ")
      
      msg <- paste0("A(s) vari\u00E1vel(eis) ", extrapola, " excede(m) mais que o dobro de seus valores m\u00E1ximos. Isso inviabiliza Grau I, II e III de Fundamenta\u00E7\u00E3o")
      
      shiny::showNotification(
        ui = msg,
        type = "warning",
        duration = duracao,
        closeButton = TRUE)
      
    }
    
  }
  
  
  # Checar quantas variaveis estao aicma ou abaixo de seus max, min: Se
  # mais que 1, emitir alerta que nao pode no GII
  n_var_q_extrapola <- sum(var_q_extrapola)
  if (n_var_q_extrapola > 1) {
    
    msg <- "H\u00E1 mais que uma \u00FAnica vari\u00E1vel extrapolando seu respectivo limite amostral. Isso inviabiliza Grau de Fundamenta\u00E7\u00E3o II"
    
    shiny::showNotification(
      ui = msg,
      type = "warning",
      duration = duracao,
      closeButton = TRUE)
  }
  
  
  # Checar se alguma \u00E9 Dicotomica, Codigo Ajustado ou Codigo Alocado:
  # Emitir aleta de vetos com relacao a essas variaveis
  var <- names(var_q_extrapola[var_q_extrapola])
  
  var_nbr <- var_nbr_type[var] %in% c("", "dicotomic", "cod_ajus", "cod_aloc")
  
  
  if (any(var_nbr)) {
    
    var_proibido_extrapolar <- var[var_nbr] %>% paste(collapse = ", ")
    
    msg <- paste0("A NBR 14.653 veta a extrapola\u00E7\u00E3o das vari\u00E1veis Dict\u00F4micas, C\u00F3digo Ajustado, C\u00F3digo Alocado ou N\u00E3o Declaradas. As vari\u00E1vei(s) ", var_proibido_extrapolar, " desrespeita(m) esse comportamento")
    
    shiny::showNotification(
      ui = msg,
      type = "error",
      duration = duracao,
      closeButton = TRUE)
  }
  
  # 4 Se GII ou GIII verificar: 
  
  #n_var_q_extrapola
  
  
  
  ## calcular, se GII, o valor estimado com a varaivel q extrapolou em seu
  ## limite amostral (maior ou menor conforme o caso), e comparar com o
  ## valor da estimativa extrapolada com a estimativa na fronteira. ela nao
  ## pode ser maior que 15%
  
  ## calcular, se GIII
  mtz 
}







calc_back_scale_new_data <- function(vetor_estimativas, 
                                     estimador_log_sel, 
                                     prop, 
                                     trans_dep = NULL) {
  
  if (is.null(trans_dep)) {
    
    var_dep <- prop$var_dependent
    trans_dep <- prop$var_trns_selected[[var_dep]]
    
  }
  
  func_back <- anti_transf[[trans_dep]]
  # Se \u00E9 log ou nao. Se nao for a retrnasformacao \u00E9 direta
 
  if (trans_dep != "log_nep") { #se a transfromada nao for log_nep
    
    media <- moda <- mediana <- func_back(vetor_estimativas[, "media", drop = FALSE])
    conf_inf <- func_back(vetor_estimativas[, "conf_inf", drop = FALSE])
    conf_sup <- func_back(vetor_estimativas[, "conf_sup", drop = FALSE])
    
    
    if (trans_dep %in% c("inverse", "quadra_inverse", "sqrt_inverse")) {
      
      pivot <- conf_inf
      conf_inf <- conf_sup
      conf_sup <- pivot
      
    }
    
    
    pred_inf <- func_back(vetor_estimativas[, "pred_inf", drop = FALSE])
    pred_sup <- func_back(vetor_estimativas[, "pred_sup", drop = FALSE])
    
    if (trans_dep %in% c("inverse", "quadra_inverse", "sqrt_inverse")) {
      
      a <- pred_inf
      pred_inf <- pred_sup
      pred_sup <- a
      
    }
    
    se_fit_trns_scale <- vetor_estimativas[, "se_fit", drop = FALSE]
    media_trns_scale <- vetor_estimativas[, "media", drop = FALSE]
    cv <- se_fit_trns_scale/media_trns_scale
    sd_fit_for_graph <- abs(cv * media)
    
    
  } else { #se a transfromada for log_nep
    
    esti_central <- vetor_estimativas[, "media", drop = FALSE]
    se_estimativa <-  vetor_estimativas[, "se_fit", drop = TRUE]
    
    media <- back_log_estimador(esti_central, se_estimativa, "media") 
    mediana <- back_log_estimador(esti_central, se_estimativa, "mediana")
    moda <- back_log_estimador(esti_central, se_estimativa, "moda")
    
    
    conf_inf <- vetor_estimativas[, "conf_inf", drop = FALSE]
    conf_inf <- back_log_estimador(conf_inf, se_estimativa, estimador_log_sel)
    
    conf_sup <- vetor_estimativas[, "conf_sup", drop = FALSE]
    conf_sup <- back_log_estimador(conf_sup, se_estimativa, estimador_log_sel)
    
    pred_inf <- vetor_estimativas[, "pred_inf", drop = FALSE]
    pred_inf <- back_log_estimador(pred_inf, se_estimativa, estimador_log_sel)
    
    pred_sup <- vetor_estimativas[, "pred_sup", drop = FALSE]
    pred_sup <- back_log_estimador(pred_sup, se_estimativa, estimador_log_sel)
    
    sd_fit_for_graph <- se_estimativa
    
    
    
  }
  
  
  cbind(pred_inf, 
        conf_inf, 
        moda, 
        mediana, 
        media, 
        conf_sup, 
        pred_sup, 
        sd_fit_for_graph) %>% 
    `colnames<-`(c("pred_inf", 
                   "conf_inf", 
                   "moda", 
                   "mediana", 
                   "media", 
                   "conf_sup", 
                   "pred_sup", 
                   "se_fit")) 
  
}

calc_iva <- function(predicted_80,
                     estimador_log_nep, 
                     iva_central_esti,
                     prop) { 
  
  var_dep <- prop$var_dependent
  var_dep_trns <- prop$var_trns_selected[[var_dep]]
  
  if (var_dep_trns == "log_nep") { 
    
    tc <- estimador_log_nep
    
  } else {
    
    tc <- "media"
    
  }
  
  tc_inicial <- predicted_80[, tc, drop = TRUE]
  
  ca_inf <- 0.85 * tc_inicial
  ca_sup <- 1.15 * tc_inicial
  
  d_inf_80 <-  tc_inicial - predicted_80[, "conf_inf"]
  d_sup_80 <- predicted_80[, "conf_sup"] - tc_inicial
  
  new_value <- iva_central_esti
  
  validate(need(new_value >= ca_inf, "O valor arbitrado deve ser maior que o valor do campo de arb\u00EDtrio inferior para a estimativa de 80% de confian\u00E7a"))
  validate(need(new_value <= ca_sup, "O valor arbitrado deve ser menor que o valor do campo de arb\u00EDtrio superior para a estimativa de 80% de confian\u00E7a"))
  
  
  
  new_ic_inf <- new_value - d_inf_80
  new_ic_sup <- new_value + d_sup_80
  
  iva_min <- max(new_ic_inf, ca_inf)
  iva_max <- min(new_ic_sup, ca_sup)
  
  cbind(iva_min = iva_min, valor_arbitrado = iva_central_esti, iva_max)
  
  
}

prediction_graph_modelling <- function(values, confianca, incluir_ip) {
  
  
  esti_central <- values[, "media", drop = TRUE] %||% NA
  se_estimativa <- values[, "se_fit", drop = TRUE] %||% NA
  conf_inf <- values[, "conf_inf", drop = TRUE] %||% NA
  conf_sup <- values[, "conf_sup", drop = TRUE] %||% NA
  pred_inf <- values[, "pred_inf", drop = TRUE] %||% NA
  pred_sup <- values[, "pred_sup", drop = TRUE] %||% NA
  
  
  if (!incluir_ip) { pred_inf <- pred_sup <- NA }
  
  # curva de densidade
  x <- tryCatch(
    
    seq(
      from = esti_central - 3 * se_estimativa,
      to = esti_central + 3 * se_estimativa,
      length.out = 100),
    
    error = function(e) { NA })
  
  y <- stats::dnorm(x, esti_central, se_estimativa)
  #y <- dt(x, df = estimativa$df, ncp = estimativa$fit)
  
  # altura do Y central na curva de densidade
  y_esti_central <- stats::dnorm(esti_central, esti_central, se_estimativa)
  
  # intervalo de confianca
  seq_conf <- tryCatch(
    
    seq(conf_inf, conf_sup, length.out = 50),
    
    error = function(e) { NA })
  
  curva_conf <- stats::dnorm(seq_conf, esti_central, se_estimativa)
  
  # intervalo de predicao
  seq_predicao <- tryCatch(
    
    seq(pred_inf, pred_sup, length.out = 50),
    
    error = function(e) { NA })
  
  curva_predicao <- stats::dnorm(seq_predicao, esti_central, se_estimativa)
  
  
  # Plotagem
  p <- plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = x,
      y = y,
      name = "P.D.F",
      color = I("black")) %>%
    
    plotly::add_lines(
      x = seq_predicao,
      y = curva_predicao,
      name = "I. Predicao",
      color = I("lightblue"),
      fillcolor = I("lightblue"),
      fill = "tozeroy") %>%
    
    plotly::add_lines(
      x = seq_conf,
      y = curva_conf,
      name = paste0(confianca, "% de I.C."),
      color = I("blue"),
      fillcolor = I("blue"),
      fill = "tozeroy")
  
  
  if (shiny::isTruthy(esti_central)) {
    
    p <- p %>% plotly::add_segments(
      x = esti_central,
      xend = esti_central,
      y = 0,
      yend = y_esti_central,
      name = "Valor Estimado",
      color = I("red")
    ) 
  }
  
  p %>%  plotly::layout(
    title = "Curva Teorica de Previs\u00E3o na Escala do Modelo")
  
  
}





prediction_graph_natural <- function(values_back, 
                                     confianca, 
                                     prop, 
                                     estimador_log_nep, 
                                     incluir_ip) {
  
  
  
  var_dep <- prop$var_dependent
  trans_dep <- prop$var_trns_selected[[var_dep]]
  
  media_back <- values_back[, "media"] %||% NA
  mediana_back <- values_back[, "mediana"] %||% NA
  moda_back <- values_back[, "moda"] %||% NA
  conf_inf_back <- values_back[, "conf_inf"] %||% NA
  conf_sup_back <- values_back[, "conf_sup"] %||% NA
  pred_inf_back <- values_back[, "pred_inf"] %||% NA
  pred_sup_back <- values_back[, "pred_sup"] %||% NA
  
  if (!incluir_ip) { pred_inf_back <- pred_sup_back <- NA }
  
  esti_central_trns <- log(mediana_back)
  se_estimativa <- values_back[, "se_fit"] %||% NA
  
  seq_conf <- tryCatch(
    
    seq(conf_inf_back, conf_sup_back, length.out = 50),
    
    error = function(e) { NA })
  
  seq_predicao <- tryCatch(
    
    seq(pred_inf_back, pred_sup_back, length.out = 50),
    
    error = function(e) { NA })
  
  
  
  if (trans_dep == "log_nep") { # se a transformada for LOG
    
    fun <- function(x) { stats::dnorm(x, esti_central_trns, se_estimativa) }
    
    x <- tryCatch(
      
      seq(
        from = esti_central_trns - 3 * se_estimativa,
        to = esti_central_trns + 3 * se_estimativa,
        length.out = 100) %>% exp(),
      
      error = function(e) { NA })
    
  } else { # se a transformada nao \u00E9 log
    
    fun <- function(x) { stats::dnorm(x, media_back, se_estimativa) }
    
    x <- tryCatch(
      
      seq(
        from = media_back - 3 * se_estimativa,
        to = media_back + 3 * se_estimativa,
        length.out = 100),
      
      error = function(e) { NA })
    
  }
  
  y_media <- fun(media_back)
  y_mediana <- fun(mediana_back)
  y_moda <- fun(moda_back)
  
  curva_conf <- fun(seq_conf)
  curva_predicao <- fun(seq_predicao)
  
  y <- fun(x)
  
  p <- plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = seq_predicao,
      y = curva_predicao,
      name = "I. Predicao",
      color = I("lightblue"),
      fillcolor = I("lightblue"),
      fill = "tozeroy") %>%
    
    plotly::add_lines(
      x = x,
      y = y,
      name = "P.D.F.",
      color = I("black")) %>%
    
    plotly::add_lines(
      x = seq_conf,
      y = curva_conf,
      name = paste0(confianca, "% de I.C."),
      color = I("blue"),
      fillcolor = I("blue"),
      fill = "tozeroy") 
  
  
  
  if (shiny::isTruthy(moda_back)) {
    
    p <- p %>% plotly::add_segments(
      x = moda_back,
      xend = moda_back,
      y = 0,
      yend = y_moda,
      name = "Moda",
      color = I("orange")) 
    
  }
  
  if (shiny::isTruthy(mediana_back)) {
    
    p <- p %>% plotly::add_segments(
      x = mediana_back,
      xend = mediana_back,
      y = 0,
      yend = y_mediana,
      name = "Mediana",
      color = I("red"))
  }
  
  if (shiny::isTruthy(media_back)) {
    
    p <- p %>% plotly::add_segments(
      x = media_back,
      xend = media_back,
      y = 0,
      yend = y_media,
      name = "Media",
      color = I("green"))
  }
  
  p %>% plotly::layout(
    title = "Curva Teorica de Previs\u00E3o na Escala Real")
  
  
}


get_relative_values <- function(values, var_dep_trns = "", log_estimador) {
  . <- NULL
  #Calculos Valores Relativos a Tendencia Central faz a conta pra geral,
  #depois corrige os valores de volta para as informacoes de GL,
  #residual_scale e cria o coeficiente de variacao
  
  
  if (var_dep_trns == "log_nep") {
    
    tc <- log_estimador
    
  } else {
    
    tc <- "media"
    
  }
  
  vars <- c("pred_inf", 
            "conf_inf", 
            tc,
            "conf_sup", 
            "pred_sup")
  
  
  values[, vars] <- ((values[, vars]/values[, tc] - 1) * 100) %>% 
    
    formatC(x = ., 
            decimal.mark = ",",
            big.mark = ".", 
            zero.print = T, 
            flag = "+", 
            digits = 2, 
            format = "f") %>% 
    
    paste("%") 
  
  values[, "se_fit"] <- ""
  values[, tc] <- ""
  
  if (any(colnames(values) == "df")) values[, "df"] <- ""
  
  values
  
  
}


formatC2 <- function(x, digits = 2, format = "f", flag = "") {
  
  x <- as.numeric(x)
  
  if (is.na(x) | is.infinite(x)) return(x)
  
  if (x > -0.0001 & x < 0.0001  ) {
    
    format <- "E"
    digits <- NULL
    
  } else if (x > -1 & x < 1 ) {
    
    format <- "f"
    digits <- 4
    
  } 
  
  
  formatC(x = x,
          
          decimal.mark = ",",
          big.mark = ".", 
          zero.print = T, 
          flag = flag, 
          digits = digits, 
          format = format) 
  
}


format_result_matrix <- function(matrix, var_enabled, rename_prediction = FALSE) {
  
  # substitui o nome das transformadas
  mtz2 <- matrix[, var_enabled]
  
  matrix[, var_enabled] <- all_trns2()[mtz2] %>% 
    
    matrix(ncol = length(var_enabled), byrow = FALSE)
  
  non_numeric <- colnames(matrix) %in% var_enabled
  non_numeric[1] <- TRUE # adicionando a primeira coluna que as vezes chama Modelo e as Veze Combinacao
  
  # matrix[, !non_numeric] <- matrix[, !non_numeric] %>%  
  #   apply(c(1, 2), formatC2, digits = 2, format = "f") 
  # 
  
  matrix <- matrix %>% dplyr::as_tibble()
  
  matrix[ , !non_numeric] <- vapply(X = matrix[, !non_numeric], 
                                    FUN = as.numeric, 
                                    FUN.VALUE = numeric(NROW(matrix))) 
  
  
  matrix[ , non_numeric] <- lapply(X = matrix[, non_numeric], 
                                   FUN = as.factor) 
  
  
  if (rename_prediction) {
    
    matrix <- matrix %>% 
      
      dplyr::rename( 
        "Predi\u00E7\u00E3o Inferior" = "pred_inf", 
        "Confian\u00E7a Inferior" = "conf_inf", 
        "Moda" = "moda",
        "Mediana" = "mediana",
        "M\u00E9dia" = "media",
        "Confian\u00E7a Superior" = "conf_sup", 
        "Predi\u00E7\u00E3o Superior" = "pred_sup",
        "Erro-Padr\u00E3o" = "se_fit") 
  }
  
  matrix
  
}




format_pred_table <- function(valores, var_dep_trns = "", log_estimador) {
  . <- NULL
  
  if (var_dep_trns == "log_nep") {
    
    tc <- log_estimador
    
  } else {
    
    tc <- "media"
    
  }
  
  vars <- c("pred_inf", 
            "conf_inf", 
            tc,
            "conf_sup", 
            "pred_sup")
  
  
  # Formatacao dos Valores 
  valores_relativos <- get_relative_values(valores, var_dep_trns, log_estimador)
  # deteccao dos valores proveninetes quando a transformada 1/x esta
  # habilitada. As vezes ela produz valores muito pequenos, necessitando de
  # mais digitos apos a virugla. 
  
  test <- any(valores[, vars] < 1 & valores[, vars] > -1)
  if ( !is.na(test) && test ) format <- "E" else format <- "f"
  
  valores[, c(vars, "se_fit")] <- valores[, c(vars, "se_fit")] %>% 
    formatC(
      x = ., 
      decimal.mark = ",",
      big.mark = ".", 
      zero.print = T, 
      #flag = "+", 
      digits = 2, 
      format = format) 
  
  
  d <- matrix(c( "Absoluto", "Relativo"), ncol = 1) %>% `colnames<-`("Valor")
  
  
  nome <- c("media" = "M\u00E9dia", "mediana" = "Mediana", "moda" = "Moda")
  
  if ("df" %in% colnames(valores)) {
    
    df <- "df"
    nm_df <- "GL"
    
  } else {
    
    df <- NULL
    nm_df <- NULL
    
  }
  
  
  valores <- valores %>% rbind(valores_relativos) %>% cbind(d, .) 
  
  valores[, c("Valor", 
              "pred_inf", 
              "conf_inf", 
              tc,
              "conf_sup", 
              "pred_sup", 
              "se_fit", 
              df), drop = FALSE] %>% 
    
    `colnames<-`(c("Valor", 
                   "Predi\u00E7\u00E3o Inferior", 
                   "Confian\u00E7a Inferior", 
                   nome[tc], 
                   "Confian\u00E7a Superior", 
                   "Predi\u00E7\u00E3o Superior", 
                   "Erro-Padr\u00E3o", 
                   nm_df ))
}






# FUNCOES DE MODELAGEM  ---------------------------------------------------
# 
# transform_data <- function(df, prop) {
#   
#   vars <- names(df)
#   names(vars) <- vars
#   
#   matrix_calc <- vapply(vars, function(var_nome) {
#     
#     trns <- prop$var_trns_selected[[var_nome]]
#     func <- lista_transf[[trns]]
#     func(df[[var_nome]])
#     
#     
#   }, FUN.VALUE = numeric(NROW(df))) %>% as.list()
#   
#   matrix_calc
#   
# }

transform_data <- function(mtz, prop) {
  
  vars <- colnames(mtz)
  
  
  trns <- vapply(vars, function(var_nome) {
    
    trns <- prop$var_trns_selected[[var_nome]]
    func <- lista_transf[[trns]]
    
    func( mtz[, var_nome, drop = TRUE] )
    
    
  }, FUN.VALUE = numeric(NROW(mtz))) 
  
  rbind(a = mtz, trns)
  
}

transform_data2 <- function(mtz, prop) {
  
  vars <- colnames(mtz)
  
  
  trns <- vapply(vars, function(var_nome) {
    
    trns <- prop$var_trns_selected[[var_nome]]
    func <- lista_transf[[trns]]
    
    func( mtz[, var_nome, drop = TRUE] )
    
    
  }, FUN.VALUE = numeric(NROW(mtz))) 
  
  trns
  
}


back_transformation_prediction <- function(df, prop) {
  
  var_dep <- prop$var_dependent
  
  trns <- prop$var_trns_selected[[var_dep]]
  func <- anti_transf[[trns]]
  
  
  sapply(df, func, simplify = FALSE, USE.NAMES = TRUE)
  
}


# Painel de Norma Graficos ------------------------------------------------

# Histograma dos Residuos
plot_residuals_hist <- function(metrics, 
                                analysis_type, 
                                histnorm = "probability density",
                                cumula = FALSE,
                                nbinsx = 0,
                                alpha = 1,
                                show_legend = TRUE) {
  
  
  if (analysis_type == "modelling") {
    
    x_axis    <- "Res\u00EDduos Padronizados Modelagem"
    x_title <- "Res\u00EDduos na Escala da Modelagem"
    titlee <- "Res\u00EDduos da Modelagem"
    
    
  } else if (analysis_type == "estimate") {
    
    x_axis <- "Res\u00EDduos Padronizados Estimativa"
    x_title <- "Res\u00EDduos na Escala da Estimativa"
    titlee <- "Res\u00EDduos da Estimativa"
    
  }
  
  
  residuos <- metrics[, x_axis, drop = TRUE] 
  seq_x <- seq(-4, 4, length.out = length(residuos))
  des_y <- stats::dnorm(seq_x)
  
  df <- metrics[, c("Elemento", x_axis), drop = TRUE] %>% dplyr::as_tibble()
  
  
  # browser()
  df %>% 
    
    #plotly::highlight_key(~Elemento) %>% 
    
    plotly::plot_ly() %>%
    
    plotly::add_histogram(
      x = ~base::get(x_axis),
      color = ~I("black"),
      name = "Histograma",
      customdata = ~base::get("Elemento"),
      visible = TRUE,
      alpha = alpha,
      histnorm = histnorm,
      nbinsx = nbinsx,
      histfunc = "count",
      cumulative = list(enabled = cumula)
    ) %>%
    
    plotly::add_lines(
      x = seq_x,
      y = des_y,
      fill = "tozeroy",
      color = I("lightblue"),
      name = "Densidade Te\u00F3rica") %>%
    
    plotly::layout(
      title = titlee,
      xaxis = list(title = x_title),
      yaxis = list(title = ""),
      showlegend = show_legend,
      barmode = "stack"
      
    ) #%>%
  
  # plotly::highlight(
  #   on = "plotly_click", 
  #   selectize = TRUE, 
  #   dynamic = F, 
  #   persistent = T,
  #   color = "blue"
  # )
  
}

# QQ plot dos Residuos
plot_residuals_qqplot <- function(metrics, 
                                  analysis_type, 
                                  point_alpha = 0.8, 
                                  point_size = 10, 
                                  alpha_line = 0.8, 
                                  jit = 0) {
  
  .data <- residuos <- perce <-  NULL
  if (analysis_type == "modelling") {
    
    x_axis    <- "Res\u00EDduos Padronizados Modelagem"
    titulo <- "QQ Plot Modelagem"
    
    
  } else if (analysis_type == "estimate") {
    
    x_axis <- "Res\u00EDduos Padronizados Estimativa"
    titulo <- "QQ Plot Estimativa"
    
  }
  
  
  # residuos <- metrics[, x_axis, drop = TRUE] 
  # teoricos <- cume_dist(residuos) %>% qnorm()
  # 
  # inf_pos <- teoricos == Inf
  # teoricos[inf_pos] <- 3
  # 
  # inf_neg <- teoricos == -Inf
  # teoricos[inf_neg] <- -3
  
  
  # https://stats.stackexchange.com/questions/245396/how-to-read-the-x-axis-of-this-qqplot
  
  df <- metrics[, c("Elemento", x_axis), drop = FALSE]  %>%  
    
    dplyr::as_tibble() %>% 
    
    dplyr::mutate(residuos = .data[[x_axis]],
           rank = dplyr::row_number(residuos),
           perce = (rank - 0.5)/length(residuos),
           teoricos = stats::qnorm(perce)) %>% 
    plotly::highlight_key(~Elemento)
  
  
  plotly::plot_ly() %>% 
    
    plotly::add_lines(
      x = -3:3,
      y = -3:3,
      line = list(
        color = ("lightblue"), 
        width = 5),
      name = "Refer\u00EAncia",
      opacity = alpha_line
    ) %>%
    
    plotly::add_markers(
      data = df,
      x =  ~base::jitter(teoricos, jit),
      y = ~base::jitter(residuos, jit) ,
      name = "Res\u00EDduos",
      #fill = "tozeroy",
      marker = list(
        size = point_size,
        opacity = point_alpha),
      customdata = ~paste("Elemento:",  Elemento),
      text = ~paste("Elemento:",  Elemento),
      hovertemplate = paste('<i>Observado</i>: %{y:.2f}',
                            '<br><b>Te\u00F3rico</b>: %{x}<br>',
                            '<b>%{text}</b>'),
      name = "Res\u00EDduos",
      color = I("black")) %>%
    
    plotly::layout(
      title = titulo,
      xaxis = list(title = "Valores Te\u00F3ricos"),
      yaxis = list(title = "Valores Constatados"),
      showlegend = FALSE,
      barmode = "overlay"
      
    )
}


# Valores Teoricos dos Residuos
residuals_theoretical <-  function(metrics, analysis_type) {
  
  if (analysis_type == "modelling") {
    
    x_axis    <- "Res\u00EDduos Padronizados Modelagem"
    titulo <- "QQ Plot Modelagem"
    
    
  } else if (analysis_type == "estimate") {
    
    x_axis <- "Res\u00EDduos Padronizados Estimativa"
    titulo <- "QQ Plot Estimativa"
    
  }
  
  residuos <- metrics[, x_axis, drop = TRUE] 
  teoricos <- residuos #%>% pnorm() %>% qnorm()
  
  inf_pos <- teoricos == Inf
  teoricos[inf_pos] <- 3
  
  inf_neg <- teoricos == -Inf
  teoricos[inf_neg] <- -3
  
  
  obs <- c(
    dp68 = (teoricos >= -1 & teoricos <= 1) %>% base::mean(),
    
    dp90 = (teoricos >= -1.64 & teoricos <= 1.64) %>% base::mean(),
    
    dp95 = (teoricos >= -1.96 & teoricos <= 1.96) %>% base::mean()
  )
  
  
  dplyr::tibble(
    "Observado" = obs, 
    "Te\u00F3rico" = c(0.68, .90, 0.95), 
    "DP" = c("\u00B1 1", "\u00B1 1,64", "\u00B1 1,96" )
  )
  
}
# Grafico dos Residuos

plot_residuals_graph <- function(metrics, 
                                 analysis_type, 
                                 prop, 
                                 point_alpha, 
                                 point_size, 
                                 show_hist = FALSE,
                                 nbinsx = 0,
                                 histnorm  = "percent",
                                 alpha = 0.7,
                                 cumula = FALSE) {
  
  var_dep <- prop$var_dependent
  
  if (analysis_type == "modelling") {
    
    nome_trnsf <- apply_name_trns(var_dep, prop)
    
    x_axis    <- "Var. Dep. Calc. Trns."
    res_pad   <- "Res\u00EDduos Padronizados Modelagem"
    
    title_x_axis <- paste(nome_trnsf, "Calculado")
    
    
  } else if (analysis_type == "estimate") {
    
    x_axis    <- "Var. Dep. Calc. Estimativa"
    res_pad   <- "Res\u00EDduos Padronizados Estimativa"
    
    title_x_axis <- paste(var_dep, "Calculado")
    
  }
  
  #resi <- metrics # metrics[c("Elemento", x_axis, y_axis, res_pad)] %>% dplyr::as_tibble()
  
  
  
  
  if (inherits(metrics, "R6")) {
    
    df2 <- metrics %>% plotly::plot_ly() %>% plotly::plotly_data()
    df <- metrics
    
    min_x <- base::min(df2[[x_axis]], na.rm = TRUE)
    max_x <- base::max(df2[[x_axis]], na.rm = TRUE)
    
    
  } else {
    
    df <- metrics %>% dplyr::as_tibble()
    
    min_x <- base::min(df[[x_axis]], na.rm = TRUE)
    max_x <- base::max(df[[x_axis]], na.rm = TRUE)
    
    
  }
  
  
  p <- df  %>% 
    
    plotly::plot_ly() %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 0,
      yend = 0,
      color = I("#0025f5"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 2,
      yend = 2,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 1,
      yend = 1,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = -1,
      yend = -1,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = -2,
      yend = -2,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_markers(
      x = ~base::get(x_axis),
      y = ~base::get(res_pad),
      marker = list(
        size = point_size,
        opacity = point_alpha),
      customdata = ~paste("Elemento:",  Elemento),
      text = ~paste("Elemento:",  Elemento),
      hovertemplate = paste('<i>Res\u00EDduo</i>: %{y:.2f}',
                            '<br><b>X</b>: %{x}<br>',
                            '<b>%{text}</b>'),
      name = "Res\u00EDduos",
      color = I("black")) %>%
    
    plotly::layout(
      #title = title_ade,
      xaxis = list(title = title_x_axis),
      yaxis = list(title = "Res\u00EDduos Padronizados"),
      showlegend = FALSE
      
    )
  
 
  if (show_hist) {
    
    h1 <- df  %>% 
      
      plotly::plot_ly() %>%
      
      plotly::add_histogram(
        y = ~base::get(res_pad), 
        color = I("black"),
        name = "Histograma",
        histnorm = histnorm,
        nbinsy = nbinsx,
        alpha = alpha,
        cumulative = list(enabled = cumula)) %>% 
      
      plotly::layout(
        yaxis = list(title = "Res\u00EDduos Padronizados"),
        xaxis = list(autorange = "reversed"))
    
    
    p <- plotly::subplot(h1, p, 
                         nrows = 1, 
                         widths = c(0.2, 0.8),
                         shareY = TRUE, 
                         shareX = T,
                         titleX = T,
                         which_layout = 2)  
    
  } 
  
  p
  
  
}


plot_residuals_graph_indep <- function(model, 
                                       metrics,
                                       analysis_type, 
                                       prop, 
                                       var_indep, 
                                       df_select,
                                       point_alpha,
                                       point_size,
                                       show_hist,
                                       nbinsx = 0,
                                       histnorm  = "percent",
                                       alpha = 0.7,
                                       cumula = FALSE) {
  
  
  
  
  if (analysis_type == "modelling") {
    
    res_pad   <- "Res\u00EDduos Padronizados Modelagem"
    df <- model$model %>% dplyr::as_tibble()
    
    nome_trnsf <- apply_name_trns(var_indep, prop)
    title_x_axis    <- paste0(nome_trnsf)
    
  } else if (analysis_type == "estimate") {
    
    res_pad   <- "Res\u00EDduos Padronizados Estimativa"
    df <- df_select %>% dplyr::as_tibble()
    
    nome_trnsf <- apply_name_trns(var_indep, prop)
    title_x_axis  <- var_indep
    
  }
  
  resi <-  metrics[, c("Elemento", res_pad)] %>% dplyr::as_tibble()
  df <- cbind(resi, df)
  
  min_x <- base::min(df[[var_indep]], na.rm = TRUE)
  max_x <- base::max(df[[var_indep]], na.rm = TRUE)
  
  
  
  
  p <- df  %>% 
    
    plotly::plot_ly() %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 0,
      yend = 0,
      color = I("#0025f5"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 2,
      yend = 2,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = 1,
      yend = 1,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = -1,
      yend = -1,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_segments(
      x = min_x,
      xend = max_x,
      y = -2,
      yend = -2,
      color = I("#cccccc"),
      opacity = 1
    ) %>%
    
    plotly::add_markers(
      x = ~base::get(var_indep),
      y = ~base::get(res_pad),
      marker = list(
        size = point_size,
        opacity = point_alpha),
      customdata = ~paste("Elemento:",  Elemento),
      text = ~paste("Elemento:",  Elemento),
      hovertemplate = paste('<i>Res\u00EDduo</i>: %{y:.2f}',
                            '<br><b>X</b>: %{x}<br>',
                            '<b>%{text}</b>'),
      name = "Res\u00EDduos",
      color = I("black")) %>%
    
    plotly::layout(
      #title = title_ade,
      xaxis = list(title = title_x_axis),
      yaxis = list(title = "Res\u00EDduos Padronizados"),
      showlegend = FALSE
      
    )
  
  
  
  if (show_hist) {
    
    h1 <- df  %>% 
      
      plotly::plot_ly() %>%
      
      plotly::add_histogram(
        y = ~base::get(res_pad), 
        color = I("black"),
        histnorm = histnorm,
        nbinsy = nbinsx,
        alpha = alpha,
        cumulative = list(enabled = cumula)) %>% 
      
      plotly::layout(
        yaxis = list(title = "Res\u00EDduos Padronizados"),
        xaxis = list(autorange = "reversed"))
    
    
    p <- plotly::subplot(h1, p, 
                         nrows = 1, 
                         widths = c(0.2, 0.8),
                         shareY = TRUE, 
                         shareX = T,
                         titleX = T,
                         which_layout = 2)  
    
  } 
  
  p
  
  
  
}


# modelling <- function(raw_data, prop, session) {
#   
#   var_hab <- prop$var_enabled[prop$var_enabled] %>% names()
#   
#   df <- raw_data
#   df <- df[!prop$obs_disabled, var_hab, drop = FALSE]
#   df$geometry <- NULL
#   
#   
#   
#   vars <- names(df)
#   
#   shiny::validate(shiny::need(prop$var_dependent %in% vars , "A vari\u00E1vel dependente n\u00E3o est\u00E1 contida dentre as vari\u00E1veis habilitadas"))
#   
#   # matrix_calc <- vapply(vars, function(var_nome) {
#   #
#   #   trns <- prop$var_trns_selected[[var_nome]]
#   #   func <- lista_transf[[trns]]
#   #   func(df[[var_nome]])
#   #
#   #
#   # }, FUN.VALUE = numeric(NROW(df)), USE.NAMES = TRUE) %>% dplyr::as_tibble()
#   
#   
#   check_na <- lapply(df, function(i) {  any(is.na(i)) }) %>% unlist()
#   #check_na <- lapply(matrix_calc, function(i) {  any(is.na(i)) }) %>% unlist()
#   
#   check_na[check_na] %>% any()
#   
#   if (any(check_na)) {
#     
#     shinyWidgets::sendSweetAlert(
#       session = session,
#       title = "Valores Faltantes!",
#       text = shiny::HTML(paste0("H\u00E1 presen\u00E7a de valores faltantes em: ", paste(names(check_na[check_na]), collapse = " "))),
#       type = "error",
#       html = TRUE
#     )
#     
#     
#     validate(need(!any(check_na), "Valores Faltantes detectados"  ))
#     #req(FALSE)
#   }
#   
#   
#   matrix_calc <- transform_data(df, prop)
#   
#   fmla <- stats::as.formula(paste0("`", prop$var_dependent,"`", " ~ ."))
#   m <- stats::lm(formula = fmla, data = matrix_calc)
#   
#   
#   m
#   
# }


# verificar existencia do modelo

check_model <- function (prop, auto_calc) {
  
  var_check <- shiny::isTruthy(sum(prop$var_enabled) > 1)
  obs_check <- shiny::isTruthy(sum(!prop$obs_disabled) > 1)
  var_dependent_check <- shiny::isTruthy(prop$var_dependent)
  auto_calc_check <- shiny::isTruthy( auto_calc)
  
  
  var_check & obs_check & var_dependent_check & auto_calc_check
  
  
}





back_log_estimador <- function(valor, se.fit, estimador) {
  
  if (estimador == "media") {
    
    exp(valor + (se.fit^2)/2)
    
    
  } else if (estimador == "mediana") {
    
    exp(valor)
    
  } else if (estimador == "moda") {
    
    exp(valor - se.fit^2)
    
  }
  
  
  
}




prediction_graph_log_nep_transition <- function(estimativa) {
  
  x <- seq(
    from = estimativa$fit[1] - 4 * estimativa$se.fit,
    to = estimativa$fit[1] + 4 * estimativa$se.fit,
    length.out = 100)
  
  y <- exp(x)
  y2 <- stats::dnorm(x, estimativa$fit[1], estimativa$se.fit)
  y3 <- stats::dlnorm(exp(x), estimativa$fit[1], estimativa$residual.scale )
  
  p1 <- plotly::plot_ly() %>%
    
    plotly::add_paths(
      x = y2,
      y = x) %>%
    
    plotly::layout(yaxis = list(autorange = "reversed"))
  
  p2 <- plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = y,
      y = x) %>%
    
    plotly::layout(yaxis = list(autorange = "reversed"))
  
  p3 <- plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = exp(x),
      y = y3)
  
  a <- plotly::subplot(p1, p2, shareY = TRUE)
  b <- plotly::subplot(plotly::plot_ly(), p3)
  
  plotly::subplot(b, a, nrows = 2, shareX = TRUE)
  
}


#prediction_versus_sample(info_para_graficos_natural(), central$rzm)

# prediction_versus_sample <- function(raw_data, prop, values) {
#   
#   var_dep <-prop$var_dependent
#   
#   
#   df <- raw_data[!prop$obs_disabled, , drop = FALSE]
#   x <- df[[var_dep]]
#   
#   
#   
#   
#   plotly::plot_ly() %>%
#     
#     plotly::add_histogram(
#       x = x,
#       color = I("black"),
#       histnorm = "probability",
#       name = "Distribui\u00E7\u00E3o"
#     ) %>%
#     
#     plotly::add_segments(
#       x = values$moda,
#       xend = values$moda,
#       y = 0,
#       yend = 1,
#       name = "Moda",
#       color = I("orange")) %>%
#     
#     plotly::add_segments(
#       x = values$mediana,
#       xend = values$mediana,
#       y = 0,
#       yend = 1,
#       name = "Mediana",
#       color = I("red")) %>%
#     
#     plotly::add_segments(
#       x = values$media,
#       xend = values$media,
#       y = 0,
#       yend = 1,
#       name = "Media",
#       color = I("green")) %>%
#     
#     plotly::layout(xaxis = list(title = var_dep),
#                    title = "Valor Estimado versus Valores Amostrais")
#   
#   
#   
#   
#   
# }


# METRICAS ----------------------------------------------------------------
# 
# 
# get_metrics <- function(raw_data, prop, model) {
#   
#   
#   dep <- raw_data[[prop$var_dependent]][!prop$obs_disabled]
#   
#   
#   var_dep <- prop$var_dependent
#   trns <- prop$var_trns_selected[[var_dep]]
#   
#   func <- lista_transf[[trns]]
#   func_back <- anti_transf[[trns]]
#   nome_dep_trns <- nome_transf[[trns]](var_dep)
#   
#   
#   var_dep_obs_natural_scale = dep
#   var_dep_obs_trns_scale  = model$model[[prop$var_dependent]]
#   var_dep_calc_trns_scale  = model$fitted.values
#   var_dep_calc_natural_scale  = func_back(model$fitted.values)
#   
#   print(var_dep_obs_natural_scale %>% str())
#   #print(var_dep_calc_natural_scale %>% str())
#   
#   
#   residuals_trns_scale = model$residuals
#   residuals_natural_scale = var_dep_obs_natural_scale - var_dep_calc_natural_scale
#   residuals_relative_trns = (var_dep_obs_trns_scale - var_dep_calc_trns_scale) / var_dep_obs_trns_scale
#   residuals_relative_natural = (var_dep_obs_natural_scale - var_dep_calc_natural_scale) / var_dep_obs_natural_scale
#   
#   
#   summary <- summary(model)
#   significancias <- summary$coefficients[, 4]
#   distancia_cook <- stats::cooks.distance(model)
#   
#   matrix_cor <- stats::cor(model$model)
#   
#   lin <- rownames(matrix_cor) != var_dep
#   col <- colnames(matrix_cor) != var_dep
#   
#   matrix_cor2 <- matrix_cor[lin, col]
#   
#   matrix_cor_max <- matrix_cor2[lower.tri(matrix_cor2)] %>%
#     abs() %>%
#     max %>%
#     round(digits = 4)
#   
#   
#   
#   list(
#     
#     var_dep_obs_natural_scale = var_dep_obs_natural_scale,
#     var_dep_obs_trns_scale  = var_dep_obs_trns_scale,
#     var_dep_calc_trns_scale  = var_dep_calc_trns_scale,
#     var_dep_calc_natural_scale  = var_dep_calc_natural_scale,
#     nome_dep_trns = nome_dep_trns,
#     nome_var_dep = var_dep,
#     
#     Elemento = raw_data[["Elemento"]][!prop$obs_disabled],
#     nms_trns = vapply(names(raw_data), apply_name_trns, character(1), prop),
#     
#     
#     residuals_trns_scale = residuals_trns_scale,
#     residuals_trns_scale_padroni = base::scale(residuals_trns_scale, center = TRUE, scale = TRUE)[, 1],
#     residuals_natural_scale = residuals_natural_scale,
#     residuals_natural_scale_padroni = base::scale(residuals_natural_scale, center = TRUE, scale = TRUE)[, 1],
#     residuals_relative_trns = residuals_relative_trns,
#     residuals_relative_natural = residuals_relative_natural,
#     
#     
#     
#     r2_res_trns_scale = summary[["r.squared"]],
#     r2_res_natural_scale = 1 - (sum((var_dep_obs_natural_scale - var_dep_calc_natural_scale)^2) /
#                                   sum((var_dep_obs_natural_scale - base::mean(var_dep_obs_natural_scale))^2)),
#     r2_ajust_res_trns_scale = summary[["adj.r.squared"]],
#     
#     r_model_correlation = sqrt(summary[["r.squared"]]),
#     f_snedecor = summary$fstatistic,
#     distancia_cook = distancia_cook,
#     significancias = significancias,
#     
#     
#     matrix_cor = matrix_cor,
#     cor_max = matrix_cor_max,
#     sig_max = significancias %>% abs() %>% max %>% round(digits = 4),
#     cook_max = distancia_cook %>% abs() %>% max %>% round(digits = 4),
#     res_max_modelo = residuals_relative_trns %>% abs() %>% max %>% round(digits = 4),
#     res_max_escala_invertida = residuals_relative_natural %>% abs() %>% max %>% round(digits = 4)
#     
#   )
#   
# }




epsg_info <-    shiny::tags$ol(
  shiny::tags$li("Certifique-se da existencia de uma coluna chamada 'latitude'. Maiusculas e minusculas sao aceitas."),
  shiny::tags$li("Certifique-se da existencia de uma coluna chamada 'longitude'. Maiusculas e minusculas sao aceitas"),
  shiny::tags$li("Certifique-se da existencia de uma coluna chamada 'epsg'. Maiusculas e minusculas sao aceitas"),
  shiny::tags$li("Certifique-se se cada uma das colunas acima sao UNICAS, ou seja, garanta que nao haja mais de uma coluna chamada latitude/longitude/epsg"),
  shiny::tags$li("Certifique-se que nao haja valores NULOS, NA, VAZIOS em cada uma das colunas acima. Um unico valor nulo em uma unica coluna torna a base nao georreferecniada. Valores nulos e aussentes podem ser tratados no modulo de engenharia de dados"),
  shiny::tags$li("Certifique-se que as variaveis 'latitude' e 'longitude' estao expressas em notacao decimal")
)
# ,
#
# shiny::tags$p("O que e EPSG?"),
# shiny::tags$p("Todo georreferecniamento e constituido de pelo menos: um sistma de referencia, o local da origem das coordenadas nesse sistema de referencia e das coordenadas nesse sistema de referecnia."),
# shiny::tags$p("O sistema e chamado de XXXX o ponto de referecnia e chamado de datum. A associacao EPSG realizou a codificacao desses ssitemas."),
#shiny::tags$p("Coordenadas coletada no Google Earth possuem EPSG XXXX. Coordenadas coletadas no Google Maps possuem EPSG:")
#)


# Modulos -----------------------------------------------------------------



# GRAFICOS DE SAIDA -------------------------------------------------------




# Scaterr Polar -----------------------------------------------------------

polar_metrics <- function(cor_max, sig_max, cook_max, res_max_modelo, res_max_escala_invertida, max_range = 100) {
  
  
  #summary <- summary(model())
  
  tb <- dplyr::tibble(
    tick = c(
      "Correla\u00E7\u00E3o Max",
      "Signific\u00e2ncia M\u00E1x",
      "Cook M\u00E1x",
      "Res\u00EDduo Max. Mod",
      "Res\u00EDduo Max. Esti"),
    description = c(
      "Correla\u00E7\u00E3o M\u00E1xima Absoluta",
      "Signific\u00e2ncia M\u00E1xima",
      "Dist\u00e2ncia de Cook M\u00E1xima",
      "Res\u00EDduo Modelagem M\u00E1ximo",
      "Res\u00EDduo Previs\u00e2o M\u00E1ximo"
    ),
    group = c(
      "Valor M\u00E1ximo",
      "Valor M\u00E1ximo",
      "Valor M\u00E1ximo",
      "Valor M\u00E1ximo",
      "Valor M\u00E1ximo"),
    value = c(
      cor_max,#cor(back_trns()$scale_trns, back_trns()$fitted_trns)^2,
      sig_max,
      cook_max,
      res_max_modelo,
      res_max_escala_invertida
    )
  )
  
  
  tb %>% plotly::plot_ly(
    type = 'scatterpolar',
    r = ~value,
    theta = ~tick,
    fill = 'toself',
    #fillcolor = "green",
    hovertemplate = '%{theta}: %{r} <extra></extra>',
    name = "Indices M\u00E1ximos",
    
    mode = "markers",
    marker = list(
      color = 'yellow',
      size = 13,
      opacity = 1,
      line = list(
        color = 'black',
        width = 3
      )
    )
    
  ) %>%
    
    plotly::layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0, max_range)
        )
      ),
      showlegend = FALSE
    )
  
  
  
  
}
#polar_metrics(metrics())



# GRAFICOS DE COEFICIENTES ------------------------------------------------


coef_graph <- function(model, sumario, prop) {
  . <- NULL
  
  coef <- model$coefficients
  coef_names_trns <- vapply(names(coef), apply_name_trns, character(1), prop) %>%
    factor(.,levels = .)
  
  
  sig <- sumario$coefficients[, 4]
  se <- sumario$coefficients[, 2]
  
  xi <- coef - se
  xf <- coef + se
  
  xi2se <- coef - 2 * se
  xf2se <- coef + 2 * se
  
  
  
  plotly::plot_ly() %>%
    
    plotly::add_segments(
      x = xi2se,
      xend = xf2se,
      y = coef_names_trns,
      yend = coef_names_trns,
      size = 1.05,
      name = "Dispers\u00E3o 2 DP",
      color = I("#afd6fa")
    ) %>%
    
    plotly::add_segments(
      x = xi,
      xend = xf,
      y = coef_names_trns,
      yend = coef_names_trns,
      size = 1.05,
      name = "Dispers\u00E3o 1 DP",
      color = I("#0084ff")
    ) %>%
    
    plotly::add_markers(
      x = coef,
      y = coef_names_trns,
      size = 2,
      color = I("black"),
      name = "Estimativa"
    ) %>%
    
    plotly::layout(
      title = "Gr\u00E1fico dos Coeficientes", 
      showlegend = FALSE)
  
}


coef_bar_graph <- function(sumario, prop, grandeza) {
  . <- NULL
  coluna <- switch(grandeza,
                   "Coeficiente" = 1, 
                   "Erro-Padr\u00E3o" = 2, 
                   "t-Valor" = 3, 
                   "Signific\u00e2ncia" = 4
  )
  
  vetor <- sumario$coefficients[, coluna, drop = TRUE]
  
  coef_names_trns <- vapply(names(vetor), apply_name_trns, character(1), prop) %>%
    factor(.,levels = .)
  
  
  
  
  plotly::plot_ly() %>%
    
    plotly::add_bars(
      y = vetor,
      x = stats::reorder(coef_names_trns, vetor),
      color = ~I("black"),
      width = 0.5,
      name = grandeza) %>%
    
    plotly::layout(
      # xaxis = list(range = c(0,1)), 
      title = "Gr\u00E1fico de Barras",
      yaxis = list(title = grandeza),
      xaxis = list(autorange = "reversed", 
                   title = "")
    )
  
}


# tabela de coeficientes

coef_tab <- function(sumario, prop, dig) {
  . <- NULL
  tb <- sumario
  nomes <- rownames(tb$coefficients)
  
  tb <- tb$coefficients %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename("Coeficiente" = "Estimate", 
                  "Erro-Padr\u00E3o" = "Std. Error", 
                  "t-Valor" =  "t value", 
                  "Signific\u00e2ncia" = "Pr(>|t|)")
  
  coef_names_trns <- vapply(nomes, apply_name_trns, character(1), prop) %>%
    factor(.,levels = .)
  
  
  tb <- tb %>%
    dplyr::mutate("Par\u00e2metro" = coef_names_trns) %>%
    dplyr::select("Par\u00e2metro", dplyr::everything())
  
  
  DT::datatable(
    tb,
    rownames = FALSE,
    filter = "top",
    extensions = 'Buttons',
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
        
      ),
      scrollX = TRUE,
      scrollY = TRUE,
      paging = TRUE,
      dom = "liftBp",
      buttons = list(
        'copy', 'csv', 'excel'
        
        # list(
        #   extend = 'collection',
        #   buttons = c('csv', 'excel'),
        #   text = 'Download'
        # )
      ),
      lengthMenu = list(c( 3, 5, 10, 25, 50, 100, -1),
                        c("3", "5", "10", "25", "Todos")),
      autoWidth = FALSE)) %>% 
    
    DT::formatPercentage(
      "Signific\u00e2ncia", 
      digits = dig, 
      dec.mark = ",", 
      mark = ".") %>% 
    
    DT::formatRound(
      c("Coeficiente", "Erro-Padr\u00E3o", "t-Valor"),
      digits = dig, 
      dec.mark = ",", 
      mark = "."
    )
  
  
}





# HEATMAP DAS CORRELACOES -------------------------------------------------

plot_cor_heatmap <- function(m, simetric = FALSE, diag = T,analysis_type = "estimate", prop) {
  
  
  if (!simetric) {
    
    m[lower.tri(m)] <- NA
    
  }
  
  if (!diag) {
    
    diag(m) <- NA
    
  }
  
  palette <- grDevices::colorRampPalette(c("red",                      
                                           "yellow",
                                           "green",
                                           "yellow", 
                                           "red"))
  
  if (analysis_type == "modelling") {
    
    rownames(m) <- rownames(m) %>% vapply(apply_name_trns, character(1), prop)
    colnames(m) <- colnames(m) %>% vapply(apply_name_trns, character(1), prop)
    
  }
  
  
  plotly::plot_ly() %>% 
    plotly::add_heatmap(
      x = rownames(m),
      y = colnames(m),
      z = m,
      colors = palette(10), 
      zauto = FALSE, 
      zmin = -1, 
      zmax = 1
    )
  
}



# 
# plot_corr_heat_map <- function(metrics) {
#   
#   
#   palette <- grDevices::colorRampPalette(c("#ff3b3b",  # vermelho
#                                            "#0073e6",  # azul escuro
#                                            "#47a3ff",  # azul claro
#                                            "#d1e8ff",  # branco
#                                            "#47a3ff",  # azul claro
#                                            "#0073e6",  # azul escuro
#                                            "#ff3b3c")) # vermelho
#   
#   m <- metrics$matrix_cor
#   coef_names_trns <- metrics$nms_trns
#   
#   col <- colnames(m)
#   row <- rownames(m)
#   
#   colnames(m) <- coef_names_trns[col] # %>% factor(.,levels = .)
#   rownames(m) <- coef_names_trns[row] # %>% factor(.,levels = .)
#   
#   #  m[upper.tri(m)] <- NA
#   #  diag(m) <- NA
#   #
#   #
#   #  p <- plotly::plot_ly() %>%
#   #
#   #    plotly::add_heatmap(
#   #      x = colnames(m),
#   #      y = rownames(m),
#   #      z = m,
#   #      colors = palette(10)
#   #      ) %>%
#   #
#   #    plotly::layout(title = "Matriz de Correla\u00E7\u00F5es")
#   #
#   # p
#   a <- expand.grid(rownames(m), colnames(m), stringsAsFactors = FALSE )
#   b <- m %>% as.vector()
#   c <- data.frame(a, b)
#   
#   pal <- leaflet::colorNumeric("Set3", (b))
#   #pal <- colorRampPalette(c("#f54242","#f7f7f7",  "#006aff"), abs(b))
#   
#   d <-  c %>% dplyr::mutate(
#     
#     col_ring = dplyr::case_when(
#       Var1 == Var2 ~ "white",
#       abs(b) >= 0.8  ~ "red",
#       TRUE ~ "white"),
#     
#     col_marker = dplyr::case_when(
#       Var1 == Var2 ~ "white",
#       TRUE ~ pal(b)
#     )
#     
#   )
#   
#   marker = list(
#     color =  d$col_marker,
#     size = abs(b)*30,
#     line = list(
#       color = d$col_ring,
#       width = 3
#     )
#   )
#   
#   plotly::plot_ly(d) %>%
#     
#     plotly::add_markers(
#       x = ~Var1,
#       y = ~Var2,
#       marker = marker,
#       text = ~b,
#       name = "Correla\u00E7\u00E3o"
#       
#     ) %>% plotly::add_lines(
#       x = ~(Var1),
#       y = ~Var1,
#       name = "Diagonal",
#       color = I("black")) %>%
#     
#     plotly::hide_legend() %>%
#     
#     plotly::layout(
#       xaxis = list(title = ""),
#       yaxis = list(title = ""))
#   
#   
#   
#   
#   
# }
#plot_corr_heat_map(metrics())

# tabela de correlacoes


corr_table <- function(metrics) {
  
  m <- metrics$matrix_cor
  nomes_linhas <- rownames(m)
  
  m %>%
    tidyr::as_tibble() %>%
    dplyr::mutate("Vari\u00E1vel 1" = nomes_linhas) %>%
    tidyr::gather("Vari\u00E1vel 2", "Correla\u00E7\u00E3o", colnames(m)) %>%
    
    DT::datatable(
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        paging = TRUE,
        lengthMenu = list(c(1, 2, 3, 5, 10, 25, 50, 100, -1),
                          c("1", "2", "3", "5", "10", "25", "Todos")),
        autoWidth = FALSE,
        pageLength = 3,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
        
        
      ))
  
  # %>%
  #   DT::formatPercentage(
  #     c(8,11), 3) # %>% DT::formatSignif(c(2,3,4,5,6,7,9,10),4)
}




plot_corr_corrplot <- function(metrics) {
  
  
  palette <- grDevices::colorRampPalette(c("#ff3b3b",  # vermelho
                                           "#0073e6",  # azul escuro
                                           "#47a3ff",  # azul claro
                                           "#d1e8ff",  # branco
                                           "#47a3ff",  # azul claro
                                           "#0073e6",  # azul escuro
                                           "#ff3b3c")) # vermelho
  
  # m <- metrics$matrix_cor
  # coef_names_trns <- metrics$nms_trns
  #
  # col <- colnames(m)
  # row <- rownames(m)
  #
  # colnames(m) <- coef_names_trns[col] %>% factor(.,levels = .)
  # rownames(m) <- coef_names_trns[row] %>% factor(.,levels = .)
  #
  #
  #
  # corrplot::corrplot(m)
  
  
  
  
  
  
  
  
}



# REGRESSAO LINEAR DOS RESIDUOS -------------------------------------------

# GERADOR DE PONTOS PARA REGRESSAO LINEAR NO GRAFICO DE RESIDUOS
generate_mini_lm  <- function(data, x, y) {
  
  fmla <- stats::as.formula(paste(y, "~", x))
  
  model_mod <- stats::lm(fmla, data)
  
  p <- stats::predict.lm(model_mod, newdata = data[, x])
  
  
  x_range = range(data[[x]])
  y_calc_range = range(p)
  
  
  list(
    ideal_min = pmin(x_range[[1]], y_calc_range[[1]]),
    ideal_max = pmax(x_range[[2]], y_calc_range[[2]]),
    x_range = x_range,
    y_calc_range = y_calc_range
  )
  
}




# ADERENCIA DOS VALORES CALCULADOS ----------------------------------------

plot_residuals_ade <- function(resi, 
                               prop, 
                               analysis_type, 
                               point_alpha, 
                               point_size) {
  #browser()
  . <- NULL
  var_dep <- prop$var_dependent
  nome_dep_trns <- apply_name_trns(var_dep, prop)
  
  title_x_axis <- paste(nome_dep_trns, "Calculado")
  title_y_axis <- paste(nome_dep_trns, "Observado")
  
  if (analysis_type == "modelling") {
    
    y_axis    <- "Var. Dep. Obs. Trns."
    x_axis    <- "Var. Dep. Calc. Trns."
    res_pad   <- "Res\u00EDduos Padronizados Modelagem"
    
    title_x_axis <- paste(nome_dep_trns, "Calculado")
    title_y_axis <- paste(nome_dep_trns, "Observado")
    
    title_ade <- "Ader\u00EAncia da Modelagem"
    title_res <- "Res\u00EDduos Modelagem"
    title_hist <- "Histograma dos Erros Modelagem"
    
  } else if (analysis_type == "estimate") {
    
    y_axis    <- "Var. Dep. Obs. Estimativa"
    x_axis    <- "Var. Dep. Calc. Estimativa"
    res_pad   <- "Res\u00EDduos Padronizados Estimativa"
    
    title_x_axis <- paste(var_dep, "Calculado")
    title_y_axis <- paste(var_dep, "Observado")
    
    title_ade <- "Ader\u00EAncia da Estimativa"
    title_res <- "Res\u00EDduos Estimativa"
    title_hist <- "Histograma dos Erros Estimativa"
    
  }
  
  
  #resi <- metrics # metrics[c("Elemento", x_axis, y_axis, res_pad)] %>% dplyr::as_tibble()
  
  mat <- resi %>%
    plotly::plot_ly() %>%
    plotly::plotly_data() %>%
    .[, c(x_axis, y_axis) , drop = FALSE] %>%
    as.matrix() %>%
    stats::na.omit()


  mini_reg <- faster_reg(mat, var_dep = x_axis)
  
  ampli_x_axis <- range(mat[, y_axis, drop = TRUE])
  ampli_fitted <- range(mini_reg$fitted.values)
  
  general_min <- min(ampli_x_axis[1], ampli_fitted[1])
  general_max <- max(ampli_x_axis[2], ampli_fitted[2])
  
  
  
  
  resi %>% 
    
    plotly::plot_ly() %>%
    
    plotly::add_segments(
      x = general_min,
      y = general_min,
      xend = general_max,
      yend = general_max,
      color = I("#0025f5"),
      opacity = 1,
      name = "Bissetriz") %>%
    
    plotly::add_lines(
      y = mat[, y_axis, drop = TRUE],
      x = mini_reg$fitted.values,
      color = I("#f58f00"),
      opacity = 1,
      name = "Massa de dados"
    ) %>%
    
    # plotly::add_segments(
    #   y = ampli_x_axis[[1]],
    #   x = ampli_fitted[[1]],
    #   xend = ampli_x_axis[[2]],
    #   yend = ampli_fitted[[2]],
    #   color = I("#f58f00"),
    #   opacity = 1,
    #   name = "Massa de dados"
    # ) %>%
    
    plotly::add_markers(
      x = ~base::get(x_axis),
      y = ~base::get(y_axis),
      marker = list(
        size = point_size,
        opacity = point_alpha),
      customdata = ~Elemento,
      text = ~paste("Elemento:",  Elemento),
      hovertemplate = paste('<i>Observado</i>: %{y:.2f}',
                            '<br><i>Calculado</i>: %{x}<br>',
                            '<b>%{text}</b>'),
      name = "Ader\u00EAncia",
      color = I("black"),
      opacity = 1) %>%
    
    plotly::layout(
      #title = title_ade,
      xaxis = list(title = title_x_axis),
      yaxis = list(title = title_y_axis),
      showlegend = FALSE
      
    )
  
  
}


# GRAFICO DOS RESIDUOS ----------------------------------------------------


cook_graph <- function(metrics, n = NULL) {
  cook <- NULL
  
  if (!shiny::isTruthy(n)) {
    
    n <- dim(metrics)[1]
    
    
  }
  
  metrics %>% 
    
    dplyr::as_tibble() %>% 
    
    dplyr::arrange(dplyr::desc(cook)) %>% 
    
    dplyr::slice(seq_len(n)) %>% 
    
    plotly::plot_ly() %>%
    
    plotly::add_bars(
      x = ~reorder(Elemento, dplyr::desc(cook)),
      y = ~cook,
      color = I("black"),
      name = "Cook",
      customdata = ~paste("Elemento:",  Elemento),
      #text = ~paste("Elemento:",  Elemento),
      hovertemplate = paste('<i>Dist\u00e2ncia de Cook</i>: %{y:.2f}',
                            '<br><b>Elemento</b>: %{x}<br>')
      
    ) %>%
    
    plotly::layout(
      title = "Dist\u00e2ncia de Cook",
      xaxis = list(title = "Elemento"),
      yaxis = list(title = "Dist\u00e2ncia de Cook"))
  
}


# PLOTAR F_SNEDECOR -------------------------------------------------------
plot_fsnedecor <- function(sumario) {
  
  fsned <- sumario$fstatistic
  
  x <- seq_len(fsned[1])
  density <- stats::df(x, df1 = fsned[2], df2 = fsned[3])
  
  plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = x,
      y = density,
      fill = "tozeroy",
      name = "Distribui\u00E7\u00E3o F Te\u00F3rica") %>%
    
    plotly::add_segments(
      x = fsned[1],
      xend = fsned[1],
      color = I("red"),
      y = 0,
      yend = 0.2,
      #size = 2,
      
      name = "F-valor")
  
}


# DATA TABLE DOS RESIDUOS -------------------------------------------------


table_hist_res <- function(metrics) {
  
  
  resi <- metrics[c("Elemento", #1
                    "var_dep_obs_natural_scale", #2
                    "var_dep_obs_trns_scale" , #4
                    "var_dep_calc_trns_scale", #5
                    "var_dep_calc_natural_scale", #3
                    "residuals_trns_scale", #6
                    "residuals_trns_scale_padroni", #7
                    "residuals_relative_trns", #8
                    "residuals_natural_scale", #9
                    "residuals_natural_scale_padroni", #10
                    "residuals_relative_natural" #11
  )] %>%
    dplyr::as_tibble()
  
  
  names(resi) <- c("Elemento", # 1
                   "Var. Dep. Obs. Escala Natural", #2
                   "Var. Dep. Obs. Escala Transf.", #4
                   "Var. Dep. Calc. Escala Transf.", #5
                   "Var. Dep. Calc. Escala Natural", # 3
                   "Res\u00EDduos Escala Transf.", #6
                   "Res\u00EDduos Escala Transf. Padron.", #7
                   "Res. Relativos Escala Transf.", #8
                   "Res\u00EDduos Escala Natural", #9
                   "Res\u00EDduos Escala Natural Padron.", #10
                   "Res. Relativos Escala Natural" #11
  )
  
  
  DT::datatable(
    resi,
    rownames = FALSE,
    filter = "top",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      paging = TRUE,
      lengthMenu = list(c(1, 2, 3, 5, 10, 25, 50, 100, -1),
                        c("1", "2", "3", "5", "10", "25", "Todos")),
      autoWidth = FALSE,
      pageLength = 3,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
      
      
    )) %>%
    DT::formatPercentage(
      c(8,11), 3) # %>% DT::formatSignif(c(2,3,4,5,6,7,9,10),4)
}

plot_data_grid <- function(var_dep, 
                           var_x, 
                           df_grid,
                           ic_show, 
                           ip_show, 
                           show_obs, 
                           df_obs,
                           df_obs_trns, 
                           point_size,
                           point_jit,
                           point_opacity,
                           estimador = "media") {
  
  # plota o grafico
  p <- df_grid %>%
    
    plotly::plot_ly() %>%
    
    plotly::add_lines(
      x = ~base::get(var_x),
      y = ~base::get(estimador),
      name = "Estimativa Central",
      color = I("black")
    ) %>%
    
    plotly::layout(
      xaxis = list(title = var_x),
      yaxis = list(title = var_dep)
    )
  
  if (ic_show) {
    
    p <- p %>%
      plotly::add_lines(
        x = ~base::get(var_x),
        y = ~conf_inf,
        #fill = "tonexty",
        color = I("#b5b3b1"),
        name = "Confian\u00E7a Inferior") %>%
      
      plotly::add_lines(
        x = ~base::get(var_x),
        y = ~conf_sup,
        color = I("#b5b3b1"),
        fill = "tonexty",
        name = "Confian\u00E7a Superior")
  }
  
  if (ip_show) {
    
    p <- p %>%
      
      plotly::add_lines(
        x = ~base::get(var_x),
        y = ~pred_inf,
        visible = T,
        color = I("#e3e2e1"),
        name = "Predi\u00E7\u00E3o Inferior") %>%
      
      plotly::add_lines(
        x = ~base::get(var_x),
        y = ~pred_sup,
        visible = T,
        color = I("#e3e2e1"),
        name = "Predi\u00E7\u00E3o Superior"
      )
  }
  
  if (show_obs) {
    
    
    p <- p %>%
      
      plotly::add_markers(
        #data = df_obs %>% dplyr::as_tibble(),
        x = base::jitter(df_obs[[var_x]], point_jit),
        y = base::jitter(df_obs_trns[[var_dep]], point_jit),
        name = "Observados",
        alpha = point_opacity,
        marker = list(size = point_size),
        color = I("blue"),
        customdata = ~paste("Elemento:",  df_obs_trns$Elemento),
        text = ~paste("Elemento:",  df_obs_trns$Elemento),
        hovertemplate = paste('<i>Y</i>: %{y:.2f}',
                              '<br><b>X</b>: %{x}<br>',
                              '<b>%{text}</b>')
      )
  }
  
  p
}





