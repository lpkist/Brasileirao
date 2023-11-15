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
                         h3("Evolução dos Times, ao longo das Rodadas")),
                  column(4,

                         )
                      # Conteúdo do Slide de Modelos aqui
)
return(list(tab_campeonato = tab_campeonato))
}

library(Brasileirao918)
library(tidyverse)
library(gganimate)
library(gifski)

## Gols ############

GolsResRod<- brasileirao %>% filter(ano_campeonato == 2016) %>%
  group_by(rodada) %>%
  mutate(GolsTot = gols_mandante+gols_visitante) %>%
  summarise(SomaGols = sum(GolsTot, na.rm = T),
            MediaGols = mean(GolsTot, na.rm = T),
            MaxGolsTot = max(GolsTot, na.rm = T))

GolsResCamp <- GolsResRod %>%
  summarise(QuantidadeTotal = sum(SomaGols),
            MediaTotal = mean(MediaGols))

GolsResRod %>%
  filter(MaxGolsTot == max(MaxGolsTot)) %>%
  select(rodada,MaxGolsTot)

GolsPartMaior <- brasileirao %>% filter(ano_campeonato == 2016) %>%
  group_by(rodada) %>%
  mutate(GolsTot = gols_mandante+gols_visitante) %>%
  mutate(MaxGolsTot = max(GolsTot, na.rm = T)) %>%
  filter(GolsTot == max(MaxGolsTot)) %>% ungroup() %>%
  filter(MaxGolsTot == max(MaxGolsTot)) %>%
  select(rodada,time_mandante,
         time_visitante,MaxGolsTot,gols_mandante,gols_visitante)

## Publico ############

PubResCamp <- brasileirao %>% filter(ano_campeonato == 2016) %>%
  group_by(rodada) %>%
  summarise(MediaPublico = mean(publico, na.rm = T),
         SomaTotPublico = sum(publico, na.rm = T),
         MaxPublico = max(publico, na.rm = T),
         MinPublico = min(publico, na.rm = T)) %>%
  View()



## Arbitro ############
# top3ArbApi <- brasileirao %>%
#   filter(ano_campeonato == 2016) %>%
#   count(arbitro) %>%
#   arrange(-n) %>% top_n(n, n = 3)

###Colocacao
M <- brasileirao %>% filter(ano_campeonato == 2016, rodada == 38) %>%
  select(time_mandante, colocacao_mandante) %>%
  rename("Time" = "time_mandante", "Posicao" = "colocacao_mandante")
V <- brasileirao %>% filter(ano_campeonato == 2016, rodada == 38) %>%
  select(time_visitante, colocacao_visitante) %>%
  rename("Time" = "time_visitante", "Posicao" = "colocacao_visitante")

posi <- rbind(M, V) %>%
  filter(!Posicao %in% c(5:16)) %>% arrange(Posicao)

