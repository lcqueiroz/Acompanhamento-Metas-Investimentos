if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('BETS')) install.packages('BETS'); library('BETS')
if (!require('googledrive')) install.packages('googledrive'); library('googledrive')
if (!require('googlesheets4')) install.packages('googlesheets4'); library('googlesheets4')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')

url <-'cole aqui o url do arquivo googlesheets em seu próprio drive'

ssid <- as_sheets_id(url)

# Ler tabelas de metas
df_metas <- sheets_read(ssid, sheet = 'Metas')

# Ler tabelas de saldo
df_saldos <- sheets_read(ssid, sheet = 'Saldos')

# Funcao para procurar dados de ipca em pacote da FGV
#BETS_search <- BETSsearch(description = "ipca",view = F)


# Download de dados do ipca
df_ipca <- BETSget(433, data.frame = T) %>% filter(date >= date("2010-01-01"))
df_cdi <- BETSget(4391, data.frame = T) %>% filter(date >= date("2019-01-01"))

#Funcao para calcular a media ponderada do ipca 
media_ipca <- function(numero_meses_media, df=df_ipca){
  #vetor <- seq(1,numero_meses_media)
  #vetor <- vetor/sum(vetor)
  vetor <- rep(1, numero_meses_media) #media sem pesos
  missing_rows <- nrow(df)-length(vetor)
  if (missing_rows > 0){
    vetor <- c(rep(0,missing_rows), vetor)}
  else {
    vetor <- tail(vetor, n=nrow(df))
  }
  df$vetor <- vetor
  return(df)
}

printCurrency <- function(value, currency.sym="R$", digits=2, sep=".", decimal=",") {
  paste(
    currency.sym,
    formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal),
    sep=""
  )
}

#Data do dia
today <- as_date(today())

## Definindo data de inicio e fim
data_min <- min(df_saldos$Data)
data_max <- max(df_saldos$Data)


# Somando diversos investimentos por META
df_agrupados_data_meta <- df_saldos %>% group_by(Data, Meta) %>% 
  summarise(Saldo = sum(Saldo), Aporte = sum(Aporte_do_mes), Total=sum(Total))

# Adicionando a coluna com rendimentos
df_agrupados_data_meta <- df_agrupados_data_meta %>% group_by(Meta) %>%
  mutate(Rendimentos = Saldo - lag(Total))
df_agrupados_data_meta$Rendimentos[is.na(df_agrupados_data_meta$Rendimentos)] <- 0

##### Função para filtrar tabela e adicionar quota
calculando_quota <- function(data_in, data_fim){
  df <- df_agrupados_data_meta %>% filter(Data >= data_in, Data <= data_fim)
  meta_loop <- unique(df_metas$Meta)[!is.na(unique(df_metas$Meta))]
  result <- list()
  for (j in meta_loop) {
    
    datas_loop <- unique(df$Data[df$Meta==j])[!is.na(unique(df$Data[df$Meta==j]))]
    datas_loop <- sort.POSIXlt(datas_loop)
    v_quota <- c(1)
    qtd_quota <- c(sum(df$Total[df$Data==datas_loop[1] & df$Meta==j])/1)
    
    if (length(datas_loop)>1) {
      
      for (i in datas_loop[-1]){
        valor<-tail(v_quota, n=1)
        qtd <- tail(qtd_quota, n=1)
        
        aporte <- sum(df$Aporte[df$Data==i & df$Meta==j])
        valor_final <- sum(df$Saldo[df$Data==i & df$Meta==j])/qtd
        
        new_qty <- qtd + aporte/valor
        
        v_quota <- c(v_quota, valor_final)
        qtd_quota <- c(qtd_quota, new_qty)
      }
      final <- data.frame(datas_loop, v_quota, qtd_quota)
      final$Meta <- j
      result[[j]] <- final
    }
    
    #################################
    datas_loop <- unique(df$Data)[!is.na(unique(df$Data))]
    datas_loop <- sort.POSIXlt(datas_loop)
    v_quota <- c(1)
    qtd_quota <- c(sum(df$Total[df$Data==datas_loop[1]])/1)
    
    if (length(datas_loop)>1) {
      
      for (i in datas_loop[-1]){
        valor<-tail(v_quota, n=1)
        qtd <- tail(qtd_quota, n=1)
        
        aporte <- sum(df$Aporte[df$Data==i])
        valor_final <- sum(df$Saldo[df$Data==i])/qtd
        
        new_qty <- qtd + aporte/valor
        
        v_quota <- c(v_quota, valor_final)
        qtd_quota <- c(qtd_quota, new_qty)
      }
      final <- data.frame(datas_loop, v_quota, qtd_quota)
      final$Meta <- "Total"
      result[["Total"]] <- final
    }
  }  
  df_result <- bind_rows(result)
  colnames(df_result) <- c("Data", "v_quota", "qtd_quota", "Meta")
  return(df_result)
}

#filtrando apenas a última data
df_agrupados_data_max <- df_agrupados_data_meta %>% filter(Data==data_max)

filtrado_com_ipca <- function(data_in, data_fim){
  df <- calculando_quota(data_in, data_fim) %>% mutate(Juros= (v_quota-1)*100)
  df <- df %>% left_join(df_agrupados_data_meta, by=c("Data", "Meta"))
  ipca <- df_ipca %>% filter(date >= data_in, date <= data_fim) 
  
  
  #Ajuste na tabela IPCA
  if (nrow(filter(ipca, date==data_fim))==0) {
    df_0 <- data.frame("date"=lubridate::as_date(data_fim), "value"=0)
    ipca <- ipca %>% bind_rows(df_0)
  }
  
  colnames(ipca) <- c("Data", "Juros")
  ipca$Meta <- "IPCA"
  ipca$Cum_Rend <- cumsum(ipca$Juros)
  
  cdi <- df_cdi %>% filter(date >= data_in, date <= data_fim) 
  
  #Ajuste na tabela CDI
  if (nrow(filter(cdi, date==data_fim))==0) {
    df_0 <- data.frame("date"=lubridate::as_date(data_fim), "value"=0)
    cdi <- cdi %>% bind_rows(df_0)
  }
  
  colnames(cdi) <- c("Data", "Juros")
  cdi$Meta <- "CDI"
  cdi$Cum_Rend <- cumsum(cdi$Juros)
  
  df$Data <- as.Date(df$Data)
  #df$Juros <- df$Juros %>% replace(is.na(.), 0)
  df <- df %>% group_by(Meta) %>%
    mutate(Cum_Rend = Juros)
  df <- bind_rows(df, ipca, cdi)
  return(df)  
}

#rendimento_medio <- 0.0065

###################################################################################

ui <- fluidPage(
  titlePanel("Overview dos Investimentos"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "mesesipca",
                  label = "Número de meses no cálculo da inflação:",
                  min = 1,
                  max = 100,
                  value = 20),
      sliderInput(inputId = "rendimentomedio",
                  label = "Valor médio dos rendimentos mensais em %:",
                  min = 0.1,
                  max = 2,
                  value = 0.65),
      dateRangeInput("datas", "Intervalo de datas", start=data_min, end = data_max,
                     min= data_min, max= data_max, language = "pt-BR")
    ,width=3),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      textOutput("texto_ipca"),
      
      # Output: Tabela ----
      tableOutput("view1"),
      
      # Output: Grafico1 ----
      plotlyOutput("plot1"),
      
      # Output: Tabela ----
      tableOutput("view2"),
      
      # Output: Grafico2 ----
      plotlyOutput("plot2")
      
    )
  )
)

#####################################################################################

server <- function(input, output, session) {
  
  avg_ipca <- reactive({
    
    df_new <- media_ipca(as.numeric(input$mesesipca))
    
    #Calculo da media do ipca
    weighted_average_ipca <- sum(df_new$value*df_new$vetor)/sum(df_new$vetor)
    return(weighted_average_ipca)
  })
  
  rendimento_anual <- reactive({
    
    rendimento_medio <- as.numeric(input$rendimentomedio)/100
    
    #Calculo do retorno anual
    retorno_anual <- round((rendimento_medio+1)**12-1,4)*100
    return(retorno_anual)
  })
  
  #Tratar dados da tabela
  metas <- reactive({
    
    rendimento_medio <- as.numeric(input$rendimentomedio)/100
    
    df_metas$Meses_ate_hoje   <- round(as.numeric(difftime(today, df_metas$Data_Inicio, units = 'days'))/30,0)
    df_metas$Meses_Faltando   <- round(as.numeric(difftime(df_metas$Data_Fim, today, units = 'days'))/30,0)
    df_metas$PV_Objetivo <- df_metas$Objetivo*(1+avg_ipca()/100)**df_metas$Meses_ate_hoje
    df_metas$FV_Objetivo <- df_metas$Objetivo*(1+avg_ipca()/100)**(df_metas$Meses_ate_hoje + df_metas$Meses_Faltando)
    
    ## Calculo do aporte mensal
    df <- df_agrupados_data_max %>% select(c("Meta", "Total"))
    colnames(df) <- c("Meta", "Total Acumulado")
    df_metas <- df_metas %>% left_join(df, by="Meta")
    df_metas$`Total Acumulado`[is.na(df_metas$`Total Acumulado`)] <- 0
    df_metas$Valor_Faltando <- df_metas$FV_Objetivo - df_metas$`Total Acumulado`
    
    df_metas$Aporte_Mensal <- (df_metas$Valor_Faltando*rendimento_medio)/ ((1+rendimento_medio)**df_metas$Meses_Faltando - 1)
    
    df_metas <- df_metas %>% select(-c("PV_Objetivo", "Meses_ate_hoje"))
    
    ###Ajuste de strings para tabela
    df_metas$Data_Fim <- format(df_metas$Data_Fim, "%b/%Y")
    df_metas$Data_Fim <- as.character(df_metas$Data_Fim)
    df_metas$Data_Inicio <- format(df_metas$Data_Inicio, "%d/%m/%Y")
    df_metas$Data_Inicio <- as.character(df_metas$Data_Inicio)
    df_metas$FV_Objetivo <- Map(printCurrency, df_metas$FV_Objetivo)
    df_metas$Objetivo <- Map(printCurrency, df_metas$Objetivo)
    df_metas$Aporte_Mensal <- Map(printCurrency, df_metas$Aporte_Mensal)
    df_metas$Valor_Faltando<- Map(printCurrency, df_metas$Valor_Faltando)
    df_metas$`Total Acumulado` <- Map(printCurrency, df_metas$`Total Acumulado`)
    df_metas$Meses_Faltando <- gsub('.00', '', as.character(df_metas$Meses_Faltando))
    colnames(df_metas) <- gsub("_", " ",colnames(df_metas))
    return(df_metas)
  })
  
  output$texto_ipca <- renderText({
    print(paste0("A inflação média considerada é de ",as.character(round(avg_ipca(),3)),"% ao mês. ",
    "O valor escolhido dos rendimentos dos investimentos daria ", 
                 as.character(rendimento_anual()),"% ao ano." ))
  })
  
  output$view1 <- renderTable({
    metas()
  })
  
  dates_input <- reactive({
    data_inicio <- input$datas[1]
    data_final <- input$datas[2]
    
    x <- filtrado_com_ipca(data_inicio, data_final)
    return(x)
  })
  
  p1 <- reactive({ggplot(dates_input()) + geom_line(aes(x=Data, y=Cum_Rend, color=Meta)) + 
    labs(color = "Legenda", y="% acumulado")+
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
    ggtitle("Rendimentos acumulados")
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(p1())
  })
  
  inflation <- reactive({
    df <- dates_input()
    inflation <- df$Cum_Rend[df$Meta=="IPCA" & df$Data==input$datas[2]][1]
    return(inflation)
  })
    
  tabela_rend <- reactive({
    n_meses <- round(as.numeric(difftime(input$datas[2], input$datas[1], units = 'days'))/30,0)
    
    df <- dates_input() %>% filter(!Meta %in% c("IPCA", "CDI")) %>% group_by(Meta) %>%
      summarise(Ultima_data = max(Data), Rendimentos_do_periodo = sum(Rendimentos)) %>%
      left_join(dates_input(), by=c("Ultima_data"="Data", "Meta"="Meta"))
    
    df <- df %>% select(Meta, Rendimentos_do_periodo, Juros) #Filtrando as colunas a serem mostradas
    
    df$Rendimentos_do_periodo[df$Meta=="Total"] <- sum(df$Rendimentos_do_periodo[!df$Meta=="Total"])
    df$Rendimentos_do_periodo <- Map(printCurrency, df$Rendimentos_do_periodo)
    
    
    df$Juros_real <- df$Juros - inflation()
    
    df$Taxa_mensal_equivalente <- ((1+df$Juros/100)**(1/n_meses) - 1)*100
    
    df$Taxa_mensal_equivalente <- paste0(as.character(round(df$Taxa_mensal_equivalente, 3)),"%")
    df$Juros <- paste0(as.character(round(df$Juros, 3)),"%")
    df$Juros_real <- paste0(as.character(round(df$Juros_real, 3)),"%")
    match_order <- c(df$Meta=="Total")
    df <- rbind(df[!match_order,], df[match_order,])
    colnames(df) <- gsub("_", " ",colnames(df))
    return(df)
  })
  
  output$view2 <- renderTable({
    tabela_rend()
  })
  
  p2 <- reactive({ggplot(dates_input(), aes(x=Data, y=Total, fill=Meta)) + geom_bar(stat = "sum", show.legend=c(size=FALSE)) + 
      labs(y="R$")+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
      ggtitle("Saldo")
  })
  output$plot2 <- renderPlotly({
    ggplotly(p2(), tooltip = c("y"))
  })
}

shinyApp(ui, server)