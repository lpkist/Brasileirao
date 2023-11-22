campeonato <- function(){
  tab_campeonato <- fluidRow(column(2,
                                    h1("Campeonato"),
                                    sliderInput(inputId = "Ano",
                                                label = "Escolha um Ano:",
                                                min = 2003, max = 2023,
                                                value = 2013, step = 1,
                                                sep = ""),
                                    tableOutput("top3ArbApi"),
                                    tableOutput("Posicoes8")
  ),
  column(6,
         h3("Evolução dos Times, ao longo das Rodadas"),
         imageOutput("gif")),
  column(4,
         h4("Análise de Público 1ª Metade"),
         tableOutput("PubMetade0"),
         h4("Análise de Público 2ª Metade"),
         tableOutput("PubMetade1"),
         h4("Análise de Gols por Fase do Campeonato"),
         tableOutput("GolsResRod"),
         h4("Análise de Gols do Campeonato Completo"),
         tableOutput("GolsResCamp"),
         h4("Partida(s) com maior Saldo de Gols"),
         tableOutput("GolsPartMaior")
  )
  # Conteúdo do Slide de Modelos aqui
  )
  return(list(tab_campeonato = tab_campeonato))
}
