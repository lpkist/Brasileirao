#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import glue
#' @import keys
#' @export
app <- function(){
  tab_arbitro <- arbitro()
  tab_campeonato <- campeonato()
  tab_confrontos <- confrontos()
  tab_geral <- geral()
  tab_time <- time()
  tab_tecnico <- tecnic()

  cores = c("#21A366", "#21A366", "#21A366", "#185C37", "#10793F",
            "#10793F", "#185C37", "#10793F", "#3A3A3A", "white", "#33c481", "black")

  ###Ignore
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

/* FONTES, SE FOR MUDAR, MUDA TUDO DE UMA VEZ */
.content-wrapper,
.right-side, * {
    color:  <cores[12] >;}
.content-wrapper,
.right-side, h1, h2, h3, h4, h5,
.c1, .c2, .c3, .c4, .c5, .c6,
.c7, .c8, .c9, .c10, .active{
    color:  <cores[12] > !important;}
.content-wrapper,
.right-side,  <tosupress >{
    color:  <cores[11] >;}
.nav > li > a:focus, .nav > li > a:hover {
    color:  <cores[9] >;
    background-color:  <cores[9] >;}
.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
    color:  <cores[9] >;
    background-color:  <cores[9] >;}

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
    #### Body #####################################################
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

######### Defina aqui suas variáveis reativas -------------------------------------


    tecnicos <- reactive(input$tecnico)
    time1 <- reactive(input$time1)
    time2 <- reactive(input$time2)
    periodo <- reactive(input$periodo_conf)
    anoArbitro <- reactive(input$AnoArbitro)
    arbitros <- reactive(input$arbitro)

    ################# Atualiza times
    observe({
    times <- unique(brasileirao$time_visitante)

          # Change values for input$inSelect
          updateSelectInput(session, "time2", choices = times[times != time1()], selected = time2())
      updateSelectInput(session, "time1", choices = times[times != time2()], selected = time1())
})


    ######### TABELAS ###################
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
    }, striped = T)

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
      }, striped = T)
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
    }, striped = T)
    ############### GRÁFICOS
    output$top_pontos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_pontos})
    output$top_aproveitamentos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_aprov})

####################### FIm do confrontos ###############
    output$GolsAp <- renderTable({brasileirao %>%
        filter(arbitro == arbitros()) %>%
        summarise(gols = sum(gols_mandante+gols_visitante),
                  MediaGols = mean(gols_mandante+gols_visitante))
    }, striped = T)
    output$Top5EstAp <- renderPlot({
      top5est <- brasileirao %>%
        filter(arbitro == arbitros()) %>%
        count(estadio) %>% arrange(-n) %>% top_n(n, n = 5)
      top5est[1:5,] %>%
        ggplot(aes(x = estadio, y = n), fill = "blue")+
        geom_bar(stat = "identity")+
        labs(x = "Estadio", y = "Jogos Apitados")+theme_bw()
    })
    output$FaltasAp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros()) %>%
        drop_na() %>%
        summarise(faltasTot = sum(faltas_mandante+faltas_visitante),
                  faltasMedia = mean(faltas_mandante+faltas_visitante))
    }, striped = T)
    output$GolsApTemp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros(),
                             ano_campeonato == anoArbitro()) %>%
        summarise(gols = sum(gols_mandante+gols_visitante),
                  MediaGols = mean(gols_mandante+gols_visitante))
    }, striped = T)
    output$FaltasApTemp <- renderTable({
      brasileirao %>% filter(arbitro == arbitros(),
                             ano_campeonato == anoArbitro()) %>%
        drop_na() %>%
        summarise(faltasTot = sum(faltas_mandante+faltas_visitante),
                  faltasMedia = mean(faltas_mandante+faltas_visitante))
    }, striped = T)
    output$TimesApTemp <- renderTable({
      res <- brasileirao %>% filter(ano_campeonato == anoArbitro()) %>%
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
    }, striped = T)



    ####################### FIm do árbitro ###############
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
