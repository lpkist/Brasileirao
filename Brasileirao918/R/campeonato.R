campeonato <- function(){
  tab_campeonato <- fluidRow(column(2,
                                    h1("Campeonato"),
                                    sliderInput(inputId = "Ano",
                                                label = "Escolha um Ano:",
                                                min = 2003, max = 2023,
                                                value = 2013, step = 1, sep = ""),
                                    h4("Árbitros que mais Apitaram"),
                                    tableOutput("top3ArbApi"),
                                    h4("Colocação Final"),
                                    p("4 Primeiros e os 4 Rebaixados"),
                                    tableOutput("Posicoes8")
  ),
  column(5,
         h3("Evolução dos Times, ao longo das Rodadas"),
         imageOutput("gif")),
  column(5,
         h4("Análise de Público no 1º Turno"),
         tableOutput("PubMetade0"),
         h4("Análise de Público no 2º Turno"),
         tableOutput("PubMetade1"),
         h4("Análise de Gols por Turno do Campeonato"),
         tableOutput("GolsResRod"),
         h4("Análise de Gols do Campeonato Completo"),
         tableOutput("GolsResCamp"),
         h4("Partida(s) com maior Quantidade de Gols"),
         tableOutput("GolsPartMaior")
  ))
  return(list(tab_campeonato = tab_campeonato))
}
