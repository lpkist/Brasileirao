#' @import shiny
#' @import shinydashboard
arbitro <- function(){
  # suppressPackageStartupMessages(library(shiny))
  # suppressPackageStartupMessages(library(shinydashboard))
  tab_arbitro <- fluidRow(column(2,
                                 h1("Árbitro"),
                                 selectInput(
                                   inputId = "arbitro",
                                   label = "Escolha o Árbitro:",
                                   choices = unique(brasileirao$arbitro)),
                                 sliderInput(inputId = "AnoArbitro",
                                             label = "Escolha um Ano:",
                                             min = 2003, max = 2023,
                                             value = 2013, step = 1, sep = ""),
                                 h3("Hitórico Geral(2003 - 2023)"),
                                 tableOutput("GolsAp"),
                                 tableOutput("FaltasAp")),
                          column(7,
                                 plotOutput("Top5EstAp")),
                          column(3,
                                 h3("Análise Por Temporada Escolhida"),
                                 tableOutput("GolsApTemp"),
                                 tableOutput("FaltasApTemp"),
                                 tableOutput("TimesApTemp"))
  )

  return(list(tab_arbitro = tab_arbitro))
}
