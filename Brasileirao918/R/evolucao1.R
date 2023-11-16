library(gganimate)
library(tidyverse)
library(gifski)
library(Brasileirao918)

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

res <- brasileirao %>% filter(ano_campeonato == 2016) %>%
  mutate(resultado =
           ifelse(gols_mandante > gols_visitante,
                  "Vitória",
                  ifelse(gols_mandante == gols_visitante,
                         "Empate", "Derrota"))) %>%
  select(ano_campeonato, time_mandante, time_visitante,
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
  group_by(time) %>% arrange(rodada) %>% mutate(total = cumsum(pontos), vitorias = cumsum(resultado == "Vitória"), gols_marcados = cumsum(gols), gols_sof = cumsum(gols_sof), sg = gols_marcados - gols_sof)

aux <- final %>% group_by(rodada) %>% arrange(-total,-vitorias,-sg,gols_marcados) %>% mutate(posicao = 20:1)


anim <-
  aux %>%
  ggplot(aes(posicao, group = time)) +
  geom_tile(aes(y = total/2, height = total,
                width = 0.9, fill = time), show.legend = F) +
  theme_bw()+
  scale_fill_manual(values = corBr)+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  transition_states(rodada, transition_length = 1, state_length = 4) +
  geom_text(aes(y = 0, label = time), hjust = "right") +
  scale_y_continuous(name = "Pontos", breaks = c(0,20,40,60,80,100), limits = c(-20,NA)) +
  coord_flip(clip = "off") +
  labs(title = 'Classificação por Rodada : {closest_state}',
       x = "Pontos")
  #ggtitle("{closest_state}")

animate(anim, nframes = 38*4, renderer = gifski_renderer("evolucao_tabela.gif"))

