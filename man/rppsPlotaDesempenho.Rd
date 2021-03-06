% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rppsPlotaDesempenho.R
\name{rppsPlotaDesempenho}
\alias{rppsPlotaDesempenho}
\title{Plota o desempenho da carteira de investimentos em fundos de um RPPS.}
\usage{
rppsPlotaDesempenho(retornos, opcao = 0, numColLeg = 3)
}
\arguments{
\item{retornos}{um objeto do tipo xts_dair contendo os retornos do portfolio das aplicacoes financeiras do RPPS.}

\item{opcao}{objeto do tipo inteiro especificando um dentre as seguintes opcoes de retorno:
\itemize{
  \item \code{opcao = 0} (padrao) objeto do tipo gtable contendo um grafico em que os retornos acumulados sao exibidos acima de uma legenda.
  \item \code{opcao = 1} environment contendo dois objetos: legenda (do tipo \code{grob}) e grafico (do tipo \code{grob})
}}

\item{numColLeg}{numero de colunas na legenda (por padrao, sao 3)}
}
\value{
um objeto conforme especificado pelo parametro \code{opcao}
}
\description{
Esta funcao recebe um objeto do tipo xts_dair, contendo o retorno da carteira de um RPPS, bem como
dos retornos dos fundos que a compoem, e retorna um grafico do tipo ggplot por meio de um
objeto gtable
}
\examples{
\dontrun{
tsRetornos <- rppsDesempenho(dtCvm, dtDair)
rppsPlotaDesempenho(retornos = tsRetornos, opcao = 0, numColLeg = 3)
}
}
\author{
Bruno M. S. S. Melo
}
\seealso{
\code{PerformanceAnalytics}, \code{xts}, \code{ggplot2}, \code{gridExtra}
}

