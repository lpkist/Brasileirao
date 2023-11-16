campeonato <- function(){
tab_campeonato <- fluidRow(column(2,
                      h1("Campeonato"),
                      sliderInput(inputId = "Ano",
                                  label = "Escolha um Ano:",
                                  min = 2003, max = 2023,
                                  value = 2013, step = 1),
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

###pacotes pra iniciar o shiny
library(keys)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(glue)
library(Brasileirao918)
library(tidyverse)
library(gganimate)
library(gifski)

## Gols ############

#da pra virar grafico
# GolsResRod <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   mutate(GolsTot = gols_mandante+gols_visitante) %>%
#   summarise(SomaGols = sum(GolsTot, na.rm = T),
#             MediaGols = mean(GolsTot, na.rm = T),
#             MaxGolsTot = max(GolsTot, na.rm = T))

# GolsResRod0 <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   mutate(GolsTot = gols_mandante+gols_visitante) %>%
#   summarise(SomaGols = sum(GolsTot, na.rm = T),
#             MediaGols = mean(GolsTot, na.rm = T),
#             MaxGolsTot = max(GolsTot, na.rm = T)) %>%
#   ungroup() %>% filter(rodada == c(1:19)) %>%
#   summarise(ParteCampeonato = c("1ªMetade"),
#             GolsTotal = sum(SomaGols),
#             MediaGols = mean(MediaGols),
#             MaxGolsTot = max(MaxGolsTot))
# GolsResRod1 <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   mutate(GolsTot = gols_mandante+gols_visitante) %>%
#   summarise(SomaGols = sum(GolsTot, na.rm = T),
#             MediaGols = mean(GolsTot, na.rm = T),
#             MaxGolsTot = max(GolsTot, na.rm = T)) %>%
#   ungroup() %>% filter(rodada == c(20:38)) %>%
#   summarise(ParteCampeonato = c("2ªMetade"),
#             GolsTotal = sum(SomaGols),
#             MediaGols = mean(MediaGols),
#             MaxGolsTot = max(MaxGolsTot))
# GolsResRod <- rbind(GolsResRod0, GolsResRod1)

# GolsResCamp <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   mutate(GolsTot = gols_mandante+gols_visitante) %>%
#   summarise(SomaGols = sum(GolsTot, na.rm = T),
#             MediaGols = mean(GolsTot, na.rm = T),
#             MaxGolsTot = max(GolsTot, na.rm = T)) %>%
#   summarise(QuantidadeTotal = sum(SomaGols),
#             MediaTotal = mean(MediaGols))

# GolsPartMaior <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   mutate(GolsTot = gols_mandante+gols_visitante) %>%
#   mutate(MaxGolsTot = max(GolsTot, na.rm = T)) %>%
#   filter(GolsTot == max(MaxGolsTot)) %>% ungroup() %>%
#   filter(MaxGolsTot == max(MaxGolsTot)) %>%
#   select(rodada,time_mandante,
#          time_visitante,MaxGolsTot,gols_mandante,gols_visitante)

## Publico ############

# PubMetade0 <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   summarise(MediaPublico = mean(publico, na.rm = T),
#          SomaTotPublico = sum(publico, na.rm = T),
#          MaxPublico = max(publico, na.rm = T),
#          MinPublico = min(publico, na.rm = T)) %>%
#   ungroup() %>% filter(rodada == c(1:19)) %>%
#   summarise(MediaPublico = mean(MediaPublico),
#          PublicoTotal = sum(SomaTotPublico),
#          MaxPublico = max(MaxPublico),
#          MinPublico = min(MinPublico))
#
# PubMetade1 <- brasileirao %>% filter(ano_campeonato == 2016) %>%
#   group_by(rodada) %>%
#   summarise(MediaPublico = mean(publico, na.rm = T),
#             SomaTotPublico = sum(publico, na.rm = T),
#             MaxPublico = max(publico, na.rm = T),
#             MinPublico = min(publico, na.rm = T)) %>%
#   ungroup() %>% filter(rodada == c(20:38)) %>%
#   summarise(MediaPublico = mean(MediaPublico),
#             PublicoTotal = sum(SomaTotPublico),
#             MaxPublico = max(MaxPublico),
#             MinPublico = min(MinPublico))

## Arbitro ############
# top3ArbApi <- brasileirao %>%
#   filter(ano_campeonato == 2016) %>%
#   count(arbitro) %>%
#   arrange(-n) %>% top_n(n, n = 3)

###Colocacao
# M <- brasileirao %>% filter(ano_campeonato == 2016, rodada == 38) %>%
#   select(time_mandante, colocacao_mandante) %>%
#   rename("Time" = "time_mandante", "Posicao" = "colocacao_mandante")
# V <- brasileirao %>% filter(ano_campeonato == 2016, rodada == 38) %>%
#   select(time_visitante, colocacao_visitante) %>%
#   rename("Time" = "time_visitante", "Posicao" = "colocacao_visitante")
#
# posi <- rbind(M, V) %>%
#   filter(!Posicao %in% c(5:16)) %>% arrange(Posicao)

