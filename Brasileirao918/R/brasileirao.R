#' @title Dados históricos do Brasileirão de pontos corridos
#' @description
#' `data.frame` com informações de todos os jogos do Brasilerão de pontos corridos (desde 2003)
#'
#' @source https://basedosdados.org/dataset/c861330e-bca2-474d-9073-bc70744a1b23?table=18835b0d-233e-4857-b454-1fa34a81b4fa
#' @format um banco de dados com 8016 linhas (04/11/2023) e 35 colunas.
#' \describe{
#'   \item{ano_campeonato}{Ano do campeonato}
#'   \item{data}{Data do jogo (formato aaaa-mm-dd)}
#'   \item{rodada}{Rodada do campeonato}
#'   \item{estadio}{Nome do estádio em que foi realizado o jogo}
#'   \item{arbitro}{Nome do árbitro que apitou o jogo}
#'   \item{publico}{Público presente no jogo}
#'   \item{publico_max}{Público máximo}
#'   \item{time_mandante}{Time mandante}
#'   \item{time_visitante}{Time visitante}
#'   \item{tecnico_mandante}{Ténico do time mandante}
#'   \item{tecnico_visitante}{Técnico do time visitante}
#'   \item{colocacao_mandante}{Posição do time mandante}
#'   \item{colocacao_visitante}{Posição do time visitante}
#'   \item{valor_equipe_titular_mandante}{Valor da equipe titular do time mandante}
#'   \item{valor_equipe_titular_visitante}{Valor da equipe titular do time visitante}
#'   \item{idade_media_titular_mandante}{Média das idades dos jogadores da equipe titular do time mandante}
#'   \item{idade_media_titular_visitante}{Média das idades dos jogadores da equipe titular do time visitante}
#'   \item{gols_mandante}{Número de gols marcados pelo time mandante}
#'   \item{gols_visitante}{Número de gols marcados pelo time visitante}
#'   \item{gols_1_tempo_mandante}{Número de gols marcados pelo time mandante no 1º tempo}
#'   \item{gols_1_tempo_visitante}{Número de gols marcados pelo time visitante no 1º tempo}
#'   \item{escanteios_mandante}{Número de escanteios marcados para o time mandante}
#'   \item{escanteios_visitante}{Número de escanteios marcados para o time visitante}
#'   \item{faltas_mandante}{Número de faltas cometidas pelo time mandante}
#'   \item{faltas_visitante}{Número de faltas cometidas pelo time visitante}
#'   \item{chutes_bola_parada_mandante}{Número de chutes de bola parada do time mandante}
#'   \item{chutes_bola_parada_visitante}{Número de chutes de bola parada do time visitante}
#'   \item{defesas_mandante}{Número de defesas do time mandante}
#'   \item{defesas_visitante}{Número de defesas do time visitante}
#'   \item{impedimentos_mandante}{Número de impedimentos do time mandante}
#'   \item{impedimentos_visitante}{Número de impedimentos do time visitante}
#'   \item{chutes_mandante}{Número de chutes do time mandante}
#'   \item{chutes_visitante}{Número de chutes do time visitante}
#'   \item{chutes_fora_mandante}{Número de chutes para fora do time mandante}
#'   \item{chutes_fora_visitante}{Número de chutes para fora do time visitante}
#' }
"brasileirao"
