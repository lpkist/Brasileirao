#' @import rsconnect
#' @param name Parâmetro `name` do shinyapps.io
#' @param token Parâmetro `token` do shinyapps.io
#' @param secret Parâmetro `secret` shinyapps.io
#' @param fath Caminho até uma pasta que contenha o arquivo que se deseja
#'  salvar. É necessário que ele tenha o nome `app.R`
#' @param app_name Nome que você deseja salvar seu dashboard
#' @title Salva dashboards no shinyapps.io
#' @description
#' A partir dos dados da sua conta no shinyapps.io, a função salva um
#'  dashboard com o nome que você desejar
#' @references https://www.shinyapps.io
#' @export

salva_app <- function(name, token, secret, fpath, app_name){
  setwd(fpath)
  setAccountInfo(name = name,
                 token = token,
                 secret = secret)
  deployApp(appName = app_name)
  print("App salvo com sucesso!")
}
