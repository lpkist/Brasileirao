% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/salva_app.R
\name{salva_app}
\alias{salva_app}
\title{Salva dashboards no shinyapps.io}
\usage{
salva_app(name, token, secret, fpath, app_name)
}
\arguments{
\item{name}{Parâmetro `name` do shinyapps.io}

\item{token}{Parâmetro `token` do shinyapps.io}

\item{secret}{Parâmetro `secret` shinyapps.io}

\item{app_name}{Nome que você deseja salvar seu dashboard}

\item{fath}{Caminho até uma pasta que contenha o arquivo que se deseja
salvar. É necessário que ele tenha o nome `app.R`}
}
\description{
A partir dos dados da sua conta no shinyapps.io, a função salva um
 dashboard com o nome que você desejar
}
\references{
https://www.shinyapps.io
}
