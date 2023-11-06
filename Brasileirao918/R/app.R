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

    tecnicos <- reactive(input$tecnico)

    output$top_pontos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_pontos})
    output$top_aproveitamentos <- renderPlot({
      tab_tecnico <- tecnic(tecnico = tecnicos())
      tab_tecnico$plot_aprov})



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
