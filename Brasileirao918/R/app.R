#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import glue
#' @import keys
#' @import plotly
#' @import gganimate
#' @import gifski

#' @export
app <- function(){
  tab_arbitro <- arbitro()
  tab_campeonato <- campeonato()
  tab_confrontos <- confrontos()
  tab_geral <- geral()
  tab_time <- time()
  tab_tecnico <- tecnic()

  cores = c("#3b5998", "#3b5998", "#3b5998", "#8b9dc3", "#3b5998",
            "#3b5998", "#8b9dc3", "#3b5998", "#f0f0f0", "black", "red", "black")

  ###Ignore ####
  tosupress = paste0("a#ui-tab-",1:1000, collapse = ", ")

  aplica_tema = tags$head(tags$style(HTML(glue(.trim = FALSE, .open = " <", .close = " >", '

/* COR ATRAS DO TITULO DO TRABALHO */
.skin-blue .main-header .logo {
    background-color:  <cores[1] >;
    color: white;}

/* COR ATRAS DO TITULO DO TRABALHO QUANDO VOCE PASSA O MOUSE */
.skin-blue .main-header .logo:hover {
    background-color:  <cores[2] >;
    color: white;}

/* BARRA DO LADO DE CIMA */
.skin-blue .main-header .navbar {
    background-color:  <cores[3] >;}

/* COR DA BARRA DO MENU NA ESQUERDA */
.skin-blue .main-sidebar {
    background-color:  <cores[4] >;}

/* ABA ATIVA NO MENU DA ESQUERDA */
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
    background-color:  <cores[5] >;}

/* ABAS QUE NAO ESTÃO ATIVAS NO MENU DA ESQUERDA */
.skin-blue .main-sidebar .sidebar .sidebar-menu a {
    background-color:  <cores[6] >;}

/* PASSAR O MOUSE NO MENU QUE FICA DO LADO ESQUERDO (ABAS QUE ESTAO ATIVAS)*/
.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
    background-color:  <cores[7] >;}

/* PASSAR O MOUSE NO MENU QUE FICA DO LADO ESQUERDA (ABAS QUE NÃO ESTÃO ATIVAS) */
.skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color:  <cores[8] >;}

/* CORPO DO DASHBOARD (LUGAR ONDE FICA OS GRAFICOS) */
.content-wrapper,
.right-side {
    background-color:  <cores[9] >;}

/* COR DAS LETRAS DA BARRA /*
.skin-blue .main-sidebar .sidebar .sidebar-menu a {
  color: white;}

/* RAPOSO, NÃO TOQUE! */
.form-control {
    border: 3px;
    width: 75% !important;
    border-color:  <cores[11] >}

.content-wrapper,
.right-side, .dygraph-legend{
    background-color:  <cores[9] > !important;}
.container-fluid{
    background-color:  <cores[9] > !important;}'))))


  ui <- fluidPage(dashboardPage(
    dashboardHeader(title = "Análise do Brasileirão"),
    #### Sidebar ##################################################
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Confrontos", tabName = "Confrontos"),
        menuItem("Time", tabName = "Time"),
        menuItem("Campeonato", tabName = "Campeonato"),
        menuItem("Técnico", tabName = "Técnico"),
        menuItem("Árbitro", tabName = "Árbitro"),
        menuItem("Geral", tabName = "Geral")
      )
    ),
    #### Body ##################
    dashboardBody(
      aplica_tema,
      tabItems(
        tabItem(tabName = "Confrontos",
                fluidPage(
                  fluidRow(
                    tab_confrontos
                  )
                )
        ),
        tabItem(tabName = "Time",
                fluidPage(
                  tab_time
                )
        ),
        tabItem(tabName = "Campeonato",
                fluidPage(
                  fluidRow(
                    tab_campeonato
                  )
                )
        ),
        tabItem(tabName = "Técnico",
                fluidPage(
                  fluidRow(
                    tab_tecnico$tab_tecnico
                  )
                )
        ),
        tabItem(tabName = "Árbitro",
                fluidPage(
                  fluidRow(
                    tab_arbitro
                  )
                )
        ),
        tabItem(tabName = "Geral",
                fluidPage(
                  fluidRow(
                    tab_geral
                  )
                )
        )
      )
    ),
  ),
  #### Mover Slide com o teclado #################################
  useKeys(),
  keysInput("keys", c("left", "right")))


  ## ---------------------------------------------
  server <- function(input, output, session) {
    ### cores dos times -----
    corBr <- c(`América-MG` = "#007f4e",
               `América-RN` = "#E41920",
               `Athletico-PR` = "#E5050F",
               `Atlético-GO` = "#DC1212",
               `Atlético-MG` = "black",
               `Atlético-PR` = "#E5050F",
               `Avaí FC` = "#00679A",
               `Barueri` = "#FECE02",
               `Botafogo` = "black",
               `Brasiliense-DF` = "#FFDE00",
               `Ceará SC` = "black",
               `Chapecoense` = "#00813A",
               `Corinthians` = "#231f20",
               `Coritiba FC` = "#00534C",
               `Criciúma EC` = "#FDD116",
               `Cruzeiro` = "#2F529E",
               `CSA` = "#0A275D",
               `Cuiabá-MT` = "#FBD800",
               `EC Bahia` = "#006CB5",
               `EC Vitória` = "#ff1100",
               `Figueirense FC` = "black",
               `Flamengo` = "#C52613",
               `Fluminense` = "#870a28",
               `Fortaleza` = "#006cb5",
               `Goiás` = "#01481E",
               `Goiás EC` = "#01481E",
               `Grêmio` = "#0d80bf",
               `Guarani` = "#006C51",
               `Internacional` = "#c20c18",
               `Ipatinga FC` = "#48887B",
               `Joinville-SC` = "#343232",
               `Juventude` = "#00922E",
               `Náutico` = "#ED1C24",
               `Palmeiras` = "#006437",
               `Paraná` = "#1C4189",
               `Paysandu SC` = "#0075C9",
               `Ponte Preta` = "black",
               `Portuguesa` = "#EE221C",
               `RB Bragantino` = "#D61E3F",
               `Santa Cruz` = "#B11C18",
               `Santo André` = "#0A2572",
               `Santos` = "black",
               `Santos FC` = "black",
               `São Caetano` = "#222568",
               `São Paulo` = "#fe0000",
               `Sport Recife` = "#ffd900",
               `Vasco da Gama` = "black")
######### Defina aqui suas variáveis reativas BOTOES -------------------------------------

    tecnicos <- reactive(input$tecnico)
    time1 <- reactive(input$time1)
    time2 <- reactive(input$time2)
    periodo <- reactive(input$periodo_conf)
    Ano <- reactive(input$Ano)
    AnoArbitro <- reactive(input$AnoArbitro)
    arbitros <- reactive(input$arbitro)
    times_f <- reactive(input$times_f)

    ################# Atualiza times #####
    observe({
    times <- unique(brasileirao$time_visitante)

          # Change values for input$inSelect
          updateSelectInput(session, "time2", choices = times[times != time1()], selected = time2())
      updateSelectInput(session, "time1", choices = times[times != time2()], selected = time1())
})


    ######### Confrontos ###################
    output$historico_conf <- renderTable({
      res <- brasileirao %>% mutate(resultado =
                                      ifelse(gols_mandante > gols_visitante,
                                             "Vitória",
                                             ifelse(gols_mandante == gols_visitante,
                                                    "Empate", "Derrota"))) %>%
        select(ano_campeonato, time_mandante, time_visitante,
               gols_mandante, gols_visitante, resultado)

      MV <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time1(),
                           time_visitante == time2())
      VM <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time2(),
                           time_visitante == time1()) %>%
        mutate(resultado = ifelse(resultado == "Vitória", "Derrota",
                                  ifelse(resultado == "Derrota", "Vitória", "Empate")))

      conf_df <- rbind(MV, VM)
      conf_df %>% mutate(resultado = factor(resultado,
                                                         levels = c("Vitória", "Empate", "Derrota"),
                                                         labels = c(paste("Vitórias do", time1()), "Empates", paste("Vitórias do", time2())), ordered = T)) %>%
        group_by(resultado) %>% summarise(n = n()) %>% pivot_wider(names_from = "resultado", values_from = "n")
    }, striped = T, na = " ", align = 'c')

    output$gols_conf <- renderTable({
      res <- brasileirao %>% mutate(resultado =
                                      ifelse(gols_mandante > gols_visitante,
                                             "Vitória",
                                             ifelse(gols_mandante == gols_visitante,
                                                    "Empate", "Derrota"))) %>%
        select(ano_campeonato, time_mandante, time_visitante,
               gols_mandante, gols_visitante, resultado)

      MV <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time1(),
                           time_visitante == time2())
      VM <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time2(),
                           time_visitante == time1()) %>%
        mutate(gols_mandante1 = gols_visitante,
               gols_visitante = gols_mandante,
               gols_mandante = gols_mandante1) %>% select(-gols_mandante1)

      conf_df <- rbind(MV, VM)
      aux <- conf_df %>% summarise(gols1 = sum(gols_mandante),
                            gols2 = sum(gols_visitante))
    colnames(aux) <- c(paste("Gols do", time1()), paste("Gols do", time2()))
      aux
      }, striped = T, na = " ", align = 'c')
    output$jogos_s_gols <- renderTable({
      res <- brasileirao %>% mutate(resultado =
                                      ifelse(gols_mandante > gols_visitante,
                                             "Vitória",
                                             ifelse(gols_mandante == gols_visitante,
                                                    "Empate", "Derrota"))) %>%
        select(ano_campeonato, time_mandante, time_visitante,
               gols_mandante, gols_visitante, resultado)

      MV <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time1(),
                           time_visitante == time2())
      VM <- res %>% filter(ano_campeonato %in% periodo()[1]:periodo()[2],
                           time_mandante == time2(),
                           time_visitante == time1()) %>%
        mutate(gols_mandante1 = gols_visitante,
               gols_visitante = gols_mandante,
               gols_mandante = gols_mandante1) %>% select(-gols_mandante1)

      conf_df <- rbind(MV, VM)
      aux <- conf_df %>% summarise(gols1 = sum(gols_visitante == 0),
                                   gols2 = sum(gols_mandante == 0))
      colnames(aux) <- c("Jogos sem sofrer gols", "Jogos sem sofrer gols")
      aux
    }, striped = T, na = " ", align = 'c')

####################### FIm do confrontos ###############

    ######### Tecnico #####

    ############### GRÁFICOS
    output$top_pontos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_pontos})
    output$top_aproveitamentos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_aprov})

    #tabelas
    output$tmn <- renderTable({
      brasileirao %>% filter(tecnico_mandante %in% tecnicos()) %>%
        group_by(time_mandante) %>%
        summarise(GolFeito = sum(gols_mandante, na.rm = T),
                  GolSofrido = sum(gols_visitante, na.rm = T),
                  SaldodeGols = sum(gols_mandante, na.rm = T)-sum(gols_visitante, na.rm = T)) %>% rename("Saldo Final" = "SaldodeGols", "Time" = "time_mandante", "Gols Feitos" = "GolFeito", "Gols Sofridos" = "GolSofrido")
    }, striped = T, na = " ", align = 'c')

    output$tvs <- renderTable({
      brasileirao %>% filter(tecnico_visitante %in% tecnicos()) %>%
        group_by(time_visitante) %>%
        summarise(GolFeito = sum(gols_visitante, na.rm = T),
                  GolSofrido = sum(gols_mandante, na.rm = T),
                  SaldodeGols = sum(gols_visitante, na.rm = T)-sum(gols_mandante, na.rm = T)) %>% rename("Saldo Final" = "SaldodeGols", "Time" = "time_visitante", "Gols Feitos" = "GolFeito", "Gols Sofridos" = "GolSofrido")
    }, striped = T, na = ' ', align = 'c')

    ################# Campeonato ######################
    output$top3ArbApi <- renderTable({
      brasileirao %>%
        filter(ano_campeonato == Ano()) %>%
        count(arbitro) %>%
        arrange(-n) %>% top_n(n, n = 3) %>%
        rename("Partidas" = "n", "Árbitro" = "arbitro")
    }, striped = T, na = " ", align = 'c')

    output$gif <- renderImage({
      arqv <- file.path("evolucao", glue(Ano(), ".gif"))
      if(!file.exists(arqv)){
        dir.create("evolucao")
        arqv <- file.path("evolucao", glue(Ano(), ".gif"))

        res <- brasileirao %>% filter(ano_campeonato == Ano()) %>%
          mutate(resultado =
                   ifelse(gols_mandante > gols_visitante,
                          "Vitória",
                          ifelse(gols_mandante == gols_visitante,
                                 "Empate", "Derrota"))) %>%
          select(ano_campeonato, time_mandante, time_visitante,
                 rodada, resultado, gols_mandante, gols_visitante)
        M <- res %>% select(-time_visitante) %>%
          mutate(time = time_mandante, gols = gols_mandante,
                 gols_sof = gols_visitante) %>%
          select(-time_mandante, -gols_mandante, -gols_visitante)
        V <- res %>% select(-time_mandante) %>%
          mutate(resultado = ifelse(resultado == "Vitória", "Derrota",
                                    ifelse(resultado == "Derrota",
                                           "Vitória", "Empate")),
                 time = time_visitante, gols = gols_visitante,
                 gols_sof = gols_mandante) %>%
          select(-time_visitante, -gols_visitante, -gols_mandante)
        jogos <- rbind(M,V)
        final <- jogos %>%
          mutate(pontos = ifelse(resultado == "Vitória", 3,
                                 ifelse(resultado == "Empate", 1, 0))) %>%
          group_by(time) %>% arrange(rodada) %>%
          mutate(total = cumsum(pontos),
                 vitorias = cumsum(resultado == "Vitória"),
                 gols_marcados = cumsum(gols),
                 gols_sof = cumsum(gols_sof),
                 sg = gols_marcados - gols_sof)

        aux <- final %>% group_by(rodada) %>%
          arrange(-total,-vitorias,-sg,gols_marcados) %>%
          mutate(posicao = 20:1)

        anim <-
          aux %>% drop_na() %>%
          ggplot(aes(posicao, group = time)) +
          geom_tile(aes(y = total/2, height = total,
                        width = 0.9, fill = time), show.legend = F) +
          theme_bw()+
          scale_fill_manual(values = corBr)+
          theme(axis.text.y = element_blank(),
                axis.title.y = element_blank()) +
          transition_states(rodada, transition_length = 1,
                            state_length = 4) +
          geom_text(aes(y = 0, label = time), hjust = "right") +
          scale_y_continuous(name = "Pontos",
                             breaks = c(0,20,40,60,80,100),
                             limits = c(-20,NA)) +
          coord_flip(clip = "off") +
          labs(title = 'Classificação por Rodada : {closest_state}',
               x = "Pontos")

        animate(anim, nframes = 38*4, renderer = gifski_renderer(arqv))

      }
      list(src = arqv,
           contentType = "image/gif")
    })

    output$Posicoes8 <- renderTable({
      M <- brasileirao %>% filter(ano_campeonato == Ano(), rodada == 38) %>%
        select(time_mandante, colocacao_mandante) %>%
        rename("Time" = "time_mandante", "Posição" = "colocacao_mandante")
      V <- brasileirao %>% filter(ano_campeonato == Ano(), rodada == 38) %>%
        select(time_visitante, colocacao_visitante) %>%
        rename("Time" = "time_visitante", "Posição" = "colocacao_visitante")

      posi <- rbind(M, V) %>%
        filter(!`Posição` %in% c(5:16)) %>% arrange(`Posição`)
      posi
    }, striped = T, na = " ", align = 'c')

    output$PubMetade0 <- renderTable({
      brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        summarise(MediaPublico = mean(publico, na.rm = T),
                  SomaTotPublico = sum(publico, na.rm = T),
                  MaxPublico = max(publico, na.rm = T),
                  MinPublico = min(publico, na.rm = T)) %>%
        ungroup() %>% filter(rodada == c(1:19)) %>%
        summarise(MediaPublico = mean(MediaPublico),
                  PublicoTotal = sum(SomaTotPublico),
                  MaxPublico = max(MaxPublico),
                  MinPublico = min(MinPublico)) %>%
        rename("Média de Público" = "MediaPublico",
               "Público Total" = "PublicoTotal",
               "Maior Público" = "MaxPublico",
               "Menor Público" = "MinPublico")
    }, striped = T, na = " ", align = 'c')

    output$PubMetade1 <- renderTable({
      brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        summarise(MediaPublico = mean(publico, na.rm = T),
                  SomaTotPublico = sum(publico, na.rm = T),
                  MaxPublico = max(publico, na.rm = T),
                  MinPublico = min(publico, na.rm = T)) %>%
        ungroup() %>% filter(rodada == c(20:38)) %>%
        summarise(MediaPublico = mean(MediaPublico),
                  PublicoTotal = sum(SomaTotPublico),
                  MaxPublico = max(MaxPublico),
                  MinPublico = min(MinPublico)) %>%
        rename("Média de Público" = "MediaPublico",
               "Público Total" = "PublicoTotal",
               "Maior Público" = "MaxPublico",
               "Menor Público" = "MinPublico")
    }, striped = T, na = " ", align = 'c')

    output$GolsResRod <- renderTable({
      GolsResRod0 <- brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        mutate(GolsTot = gols_mandante+gols_visitante) %>%
        summarise(SomaGols = sum(GolsTot, na.rm = T),
                  MediaGols = mean(GolsTot, na.rm = T),
                  MaxGolsTot = max(GolsTot, na.rm = T)) %>%
        ungroup() %>% filter(rodada == c(1:19)) %>%
        summarise(Fase = c("1º Turno"),
                  GolsTotal = sum(SomaGols),
                  MediaGols = mean(MediaGols),
                  MaxGolsTot = max(MaxGolsTot)) %>%
        rename("Total de Gols" = "GolsTotal",
               "Média de Gols" = "MediaGols",
               "Máximo de Gols em uma Partida" = "MaxGolsTot")
      GolsResRod1 <- brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        mutate(GolsTot = gols_mandante+gols_visitante) %>%
        summarise(SomaGols = sum(GolsTot, na.rm = T),
                  MediaGols = mean(GolsTot, na.rm = T),
                  MaxGolsTot = max(GolsTot, na.rm = T)) %>%
        ungroup() %>% filter(rodada == c(20:38)) %>%
        summarise(Fase = c("2º Turno"),
                  GolsTotal = sum(SomaGols),
                  MediaGols = mean(MediaGols),
                  MaxGolsTot = max(MaxGolsTot)) %>%
        rename("Total de Gols" = "GolsTotal",
               "Média de Gols" = "MediaGols",
               "Máximo de Gols em uma Partida" = "MaxGolsTot")
      GolsResRod <- rbind(GolsResRod0, GolsResRod1)
      GolsResRod
    }, striped = T, na = " ", align = 'c')

    output$GolsResCamp <- renderTable({
      brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        mutate(GolsTot = gols_mandante+gols_visitante) %>%
        summarise(SomaGols = sum(GolsTot, na.rm = T),
                  MediaGols = mean(GolsTot, na.rm = T),
                  MaxGolsTot = max(GolsTot, na.rm = T)) %>%
        summarise(QuantidadeTotal = sum(SomaGols),
                  MediaTotal = mean(MediaGols)) %>%
        rename("Soma Total de Gols" = "QuantidadeTotal",
               "Média Total de Gols" = "MediaTotal")
    }, striped = T, na = " ", align = 'c')

    output$GolsPartMaior <- renderTable({
      brasileirao %>% filter(ano_campeonato == Ano()) %>%
        group_by(rodada) %>%
        mutate(GolsTot = gols_mandante+gols_visitante) %>%
        mutate(MaxGolsTot = max(GolsTot, na.rm = T)) %>%
        filter(GolsTot == max(MaxGolsTot)) %>% ungroup() %>%
        filter(MaxGolsTot == max(MaxGolsTot)) %>%
        select(rodada,time_mandante,gols_mandante,
               gols_visitante,time_visitante) %>%
        rename("Mandante" = "time_mandante",
               "Gols Casa" = "gols_mandante",
               "Gols Visitante" = "gols_visitante",
               "Visitante" = "time_visitante")
    }, striped = T, na = " ", align = 'c')

    ########## Fim do Campeonato
    ################ Arbitro #######################
    output$GolsAp <- renderTable({brasileirao %>%
        filter(arbitro == arbitros()) %>%
        summarise(gols = sum(gols_mandante+gols_visitante),
                  MediaGols = mean(gols_mandante+gols_visitante)) %>%
        rename("Gols Apitados" = "gols",
               "Média de Gol por Partida" = "MediaGols")
    }, striped = T, na = " ", align = 'c')

    output$Top5EstAp <- renderPlot({
      top5est <- brasileirao %>%
        filter(arbitro == arbitros()) %>%
        count(estadio) %>% arrange(-n) %>% top_n(n, n = 5)
      top5est[1:5,] %>%
        ggplot(aes(x = estadio, y = n))+
        geom_bar(stat = "identity", fill = "#3b5998")+
        coord_flip()+
        labs(x = "Jogos Apitados", y = "Estádio")+theme_bw()
    })

    output$FaltasAp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros()) %>%
        drop_na() %>%
        summarise(faltasTot = sum(faltas_mandante+faltas_visitante),
                  faltasMedia = mean(faltas_mandante+faltas_visitante)) %>%
        rename("Faltas Apitadas" = "faltasTot",
               "Média de Faltas por Partida" = "faltasMedia")
    }, striped = T, na = " ", align = 'c')

    output$GolsApTemp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros(),
                             ano_campeonato == AnoArbitro()) %>%
        summarise(gols = sum(gols_mandante+gols_visitante),
                  MediaGols = mean(gols_mandante+gols_visitante)) %>%
        rename("Gols Apitados" = "gols",
               "Média de Gol por Partida" = "MediaGols")
    }, striped = T, na = " ", align = 'c')
output$FaltasApTemp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros(),
                             ano_campeonato == AnoArbitro()) %>%
        drop_na() %>%
        summarise(faltasTot = sum(faltas_mandante+faltas_visitante),
                  faltasMedia = mean(faltas_mandante+faltas_visitante))%>%
        rename("Faltas Apitadas" = "faltasTot",
               "Média de Faltas por Partida" = "faltasMedia")
    }, striped = T, na = " ", align = 'c')

    output$TimesApTemp <- renderTable({
      res <- brasileirao %>% filter(ano_campeonato == AnoArbitro()) %>%
        mutate(resultado =
                 ifelse(gols_mandante > gols_visitante,
                        "Vitória",
                        ifelse(gols_mandante == gols_visitante,
                               "Empate", "Derrota"))) %>%
        select(ano_campeonato, time_mandante, time_visitante, arbitro,
               rodada, resultado, gols_mandante, gols_visitante)
      M <- res %>% select(-time_visitante) %>% mutate(time = time_mandante, gols = gols_mandante, gols_sof = gols_visitante) %>%
        select(-time_mandante, -gols_mandante, -gols_visitante)
      V <- res %>% select(-time_mandante) %>%
        mutate(resultado = ifelse(resultado == "Vitória", "Derrota",
                                  ifelse(resultado == "Derrota", "Vitória", "Empate")),
               time = time_visitante, gols = gols_visitante,
               gols_sof = gols_mandante) %>% select(-time_visitante, -gols_visitante, -gols_mandante)
      jogos <- rbind(M,V)
      final <- jogos %>% mutate(pontos = ifelse(resultado == "Vitória", 3,
                                                ifelse(resultado == "Empate", 1, 0))) %>%
        group_by(time) %>% arrange(rodada) %>% mutate(total = cumsum(pontos), vitorias = sum(resultado == "Vitória"), gols_marcados = cumsum(gols), gols_sof = cumsum(gols_sof), sg = gols_marcados - gols_sof)

      timesApAno <- final %>% filter(arbitro == arbitros()) %>%
        count(time) %>% arrange(-n) %>% rename("Jogos Apitados" = "n",
                                               "Time" = "time")
      timesApAno[1:5,]
    }, striped = T, na = " ", align = 'c')
    ######### FIm do árbitro ###############
    #######################  Time ##########################
    output$MediaPub <- renderTable({brasileirao %>%
        filter(time_mandante == times_f()) %>%
        summarise('Total de público' = sum(publico,na.rm=T),
                  'Média de público' = round(mean(publico,na.rm=T)))
    }, striped = T, na = " ", align = 'c')
    output$MaiorGolCasa <- renderTable({brasileirao %>%
        filter(time_mandante == times_f()) %>%
        mutate(MaxGol_Casa = max(gols_mandante,na.rm=T)) %>%
        filter(gols_mandante == MaxGol_Casa) %>%
        summarise('Máximo de Gols Mandante' = MaxGol_Casa, Ano = ano_campeonato,
                  Contra = time_visitante)
    }, striped = T, na = " ", align = 'c')

    output$MaiorGolFora <- renderTable({brasileirao %>%
        filter(time_visitante == times_f()) %>%
        mutate(MaxGol_Fora = max(gols_visitante,na.rm=T)) %>%
        filter(gols_visitante == MaxGol_Fora) %>%
        summarise('Máximo de Gols Visitante' = MaxGol_Fora, Ano = ano_campeonato,
                  Contra = time_mandante)
    }, striped = T, na = " ", align = 'c')

    output$GolPartida <- renderTable({
      golstot_mandante <- brasileirao %>%
        filter(time_mandante == times_f()) %>% summarise('gol_mand'=sum(gols_mandante,na.rm=T))
      golstot_visitante <- brasileirao %>%
        filter(time_visitante == times_f()) %>% summarise('gol_visit'=sum(gols_visitante,na.rm=T))
      tam <- brasileirao %>% filter(time_mandante == times_f() | time_visitante == times_f()) %>%
        summarise(n())
      data.frame((golstot_mandante+golstot_visitante),tam) %>%
        summarise('Gols por partida' = gol_mand/tam,
                  'Total de gols' = gol_mand)
    }, striped = T, na = " ", align = 'c')

    output$valor_time <- renderPlotly({time_data <- brasileirao %>%
      filter(time_mandante == times_f(), !is.na(valor_equipe_titular_mandante))

    colnames(time_data)[14] <- c('Valor_do_time')
    if(length(time_data > 0)){
      plot_valores <- ggplotly(
        ggplot(time_data, aes(x = data, y = Valor_do_time)) +
          geom_line(color = '#3b5998') +
          scale_y_continuous(name = 'Valor do time')+theme_bw())
    }
    })
    ####################### Fim do Time ##########################
    
    ########################### Geral ############################
    output$Pub_total <- renderTable({brasileirao %>% summarise('Público_Total' = sum(publico, na.rm=T),
                                                               'Média_Público' = mean(publico, na.rm=T))},
                                    striped = T, na = " ", align = 'c')
    
    output$Gols_total <- renderTable({brasileirao %>% summarise('Total Mandante' = sum(gols_mandante,na.rm=T),
                                                                'Total Visitante' = sum(gols_visitante,na.rm=T))},
                                     striped = T, na = " ", align = 'c')
    
    output$Ano_gols_max <- renderTable({brasileirao %>% group_by(ano_campeonato) %>%
        mutate('Gols_ano' = sum(c(gols_mandante,gols_visitante),na.rm = T)) %>%
        select(Gols_ano) %>%
        ungroup() %>% filter(Gols_ano == max(Gols_ano)) %>%
        distinct() %>% summarise('Máximo de Gols' = Gols_ano[1],'Ano' = ano_campeonato[1])},
        striped = T, na = " ", align = 'c')
    
    output$Ano_gols_min <- renderTable({brasileirao %>% group_by(ano_campeonato) %>%
        mutate('Gols_ano' = sum(c(gols_mandante,gols_visitante),na.rm = T)) %>%
        select(Gols_ano) %>%
        ungroup() %>% filter(Gols_ano == min(Gols_ano)) %>%
        distinct() %>% summarise('Mínimo de Gols' = Gols_ano[1],'Ano' = ano_campeonato[1])},
        striped = T, na = " ", align = 'c')
    
    output$Time_valor_min <- renderTable({brasileirao %>% group_by(time_mandante) %>%
        filter(valor_equipe_titular_mandante != is.na(valor_equipe_titular_mandante)) %>%
        ungroup() %>% select(ano_campeonato, time_mandante, valor_equipe_titular_mandante) %>%
        filter(valor_equipe_titular_mandante == min(valor_equipe_titular_mandante)) %>%
        distinct() %>% rename('Time' = 'time_mandante',
                              'Valor do Time' = 'valor_equipe_titular_mandante', 'Ano' = 'ano_campeonato')},
        striped = T, na = " ", align = 'c')
    
    output$Time_valor_max <- renderTable({brasileirao %>% group_by(time_mandante) %>%
        filter(valor_equipe_titular_mandante != is.na(valor_equipe_titular_mandante)) %>%
        ungroup() %>% select(ano_campeonato, time_mandante, valor_equipe_titular_mandante) %>%
        filter(valor_equipe_titular_mandante == max(valor_equipe_titular_mandante)) %>%
        distinct() %>%  rename('Time' = 'time_mandante',
                               'Valor do Time' = 'valor_equipe_titular_mandante',
                               'Ano' = 'ano_campeonato')},
        striped = T, na = " ", align = 'c')
    
    output$valor_times <- renderPlotly({banco <- brasileirao %>%
      group_by(ano_campeonato,time_mandante) %>% arrange(desc(data)) %>%
      select(time_mandante,ano_campeonato,valor_equipe_titular_mandante) %>%
      distinct(time_mandante,.keep_all = T) %>% ungroup() %>% group_by(ano_campeonato) %>%
      summarise('Valor_Total' = sum(valor_equipe_titular_mandante,na.rm = T)) %>%
      rename('Ano' = 'ano_campeonato')
    
    banco <- banco %>% summarise(Ano,'Valor_Total' = Valor_Total/1000000)
    
    plot_valor <- ggplotly(ggplot(banco)+
                             geom_line(aes(x = Ano,y = Valor_Total),color = '#3b5998')+theme_bw()+
                             scale_y_continuous(name = 'Valor Total (em Milhões)'))
    })
    ###################### Fim do Geral ##########################
    
    ##Mover o slide com as setas do teclado#######################
    id_tab = reactiveVal(1)
    observeEvent(input$keys, {
      todas_tabs = c("Confrontos", "Time", "Campeonato", "Técnico",
                     "Árbitro", "Geral")
      inputado = input$keys
      id = isolate(id_tab())
      if(inputado == "left"){
        if(id > 1){
          id = id - 1
        }
      }
      if(inputado == "right"){
        if(id < length(todas_tabs)){
          id = id + 1
        }
      }
      id_tab(id)

      updateTabItems(session, "tabs", selected = todas_tabs[id])
    })
  }


  ## ---------------------------------------------
  #### GERAR DASHBOARD #######################################
  shinyApp(ui = ui, server = server)
}



