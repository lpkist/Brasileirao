campeonato <- function(){
tab_campeonato <- column(3,
                      h1("Campeonato")
                      # Conteúdo do Slide de Modelos aqui
)
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
top3ArbApi <- brasileirao %>%
  filter(ano_campeonato == 2016) %>%
  count(arbitro) %>%
  arrange(-n) %>% top_n(n, n = 3)

###Colocacao
