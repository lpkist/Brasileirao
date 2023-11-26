#' @import shiny
#' @import shinydashboard
confrontos <- function(time1 = "Grêmio", time2 = "Internacional", periodo= c(2003,2023)){
  tab_confrontos <- fluidRow(column(5,
                                    h1("Confrontos"),
                                    selectInput(inputId = "time1",
                                                label = "Escolha o 1º time",
                                                choices = unique(brasileirao$time_visitante),
                                                "Grêmio"),
                                    selectInput(inputId = "time2",
                                                label = "Escolha o 2º time",
                                                choices = unique(brasileirao$time_mandante),
                                                "Internacional"),
                                    sliderInput("periodo_conf",
                                                "Selecione o período do confronto",
                                                min = 2003,
                                                max = max(brasileirao$ano_campeonato),
                                                value = c(2003,2023),
                                                step = 1, sep = "")),
                             column(7,
                                    h4("Histórico dos Confrontos"),
                                    tableOutput("historico_conf"),
                                    tableOutput("gols_conf"),
                                    tableOutput("jogos_s_gols")

                             ))
  return(list(tab_confrontos = tab_confrontos))
}
