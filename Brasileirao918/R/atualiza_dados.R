#' @title Atualiza dados do Brasileirão
#' @description
#' Atualiza o `data.frame brasileirao` com os dados mais recentes
#'  disponíveis na fonte.
#' @param project_id ID do seu projeto do Google Cloud. Ele deve possuir algumas autorizações específicas para conseguir importar os dados. Para mais informações, ver referências.
#' @references https://basedosdados.org/dataset/c861330e-bca2-474d-9073-bc70744a1b23?table=18835b0d-233e-4857-b454-1fa34a81b4fa
#' @import basedosdados
#' @export
atualiza_dados <- function(project_id){
  if(!is.character(project_id)) stop("`project_id` deve ser do tipo character")
  library(basedosdados)

  # Defina o seu projeto no Google Cloud
  set_billing_id(project_id)

  # Para carregar o dado direto no R
  query <- bdplyr("mundo_transfermarkt_competicoes.brasileirao_serie_a")
  brasileirao <<- bd_collect(query)
}
