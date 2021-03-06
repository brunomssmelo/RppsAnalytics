% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rppsAcoplaReferencia.R
\name{rppsAcoplaReferencia}
\alias{rppsAcoplaReferencia}
\title{Acopla indice de referencia a serie temporal de retornos dos investimentos em cotas de fundos de um RPPS.}
\usage{
rppsAcoplaReferencia(retornos, referencia, nome.ref, taxa, periodo.taxa)
}
\arguments{
\item{retornos}{objeto xts contendo os retornos mensais da carteira de investimentos em fundos do RPPS, conforme
gerado pela funcao \code{rppsDesempenho}}

\item{referencia}{data.frame contendo a serie historica da variacao do indice de referencia. Assume-se que
o data.frame possui as seguintes colunas: ANO, MES e VALOR. Faz-se necessario que a ordem das colunas
seja obedecida.}

\item{nome.ref}{nome do indice de referencia.}

\item{taxa}{taxa a ser acrescentada ao indice de referencia.}

\item{periodo.taxa}{periodo a ser considerado para a taxa. As opcoes disponiveis sao: "ANO" e "MES". Caso o periodo
da taxa nao coincida com o periodo da serie historica do indice, sera calculada taxa equivalente a ser
adicionada a mesma.}
}
\value{
um objeto do tipo data.frame contendo os retornos do portfolio das aplicacoes financeiras do RPPS.
}
\description{
Esta funcao acopla um indice de referencia a serie temporal de retornos da carteiras de investimentos
em cotas de fundos obtida a partir da funcao \code{rppsDesempenho}.
}
\details{
Obs.: Mais de um indice de referencia podera ser acoplado a uma serie temporal de retornos. No entanto, so podera
ser incluido um indice a cada chamada desta funcao.
}
\examples{
\dontrun{
tsRetornos <- rppsDesempenho(dtCvm, dtDair)
tsRetornos <- rppsAcoplaReferencia(retornos = tsRetornos,
                                   referencia = dfIpca,
                                   nome.ref = "IPCA + 6\% a.a.",
                                   taxa = 6,
                                   periodo.taxa = "ANO")  # IPCA + 6\% a.a.
}
}
\author{
Bruno M. S. S. Melo
}
\seealso{
\code{RppsWrangler} e \code{RcvmWrangler}
}

