#' Calculo do desempenho da carteira de investimentos em fundos de um RPPS.
#'
#' Esta funcao calcula, a partir dos dados contidos no Demonstrativo de Aplicacoes e Investimentos dos Recursos (DAIR)
#' dos RPPS e dos informes diarios dos fundos de investimentos disponibilizados pela CVM, o retorno da carteira de
#' investimentos em fundos de um RPPS.
#'
#' @param dtDair data.frame contendo dados extraidos do "DAIR", com a mesma estrutura retornada pela funcao
#'        de extracao de DAIR do pacote \code{RppsWrangler}.
#' @param dtCvm data.frame contendo dados extraidos dos informes diarios de fundos da CVM, com a mesma
#'        estrutura retornada pela funcao de extracao de informes do pacote \code{RcvmWrangler}.
#' @return um objeto do tipo data.frame contendo os retornos do portfolio das aplicacoes financeiras do RPPS.
#' @author Bruno M. S. S. Melo
#' @details
#' Obs.: O desempenho calculado esta aproximado pois as informacoes da composicao da carteira do RPPS so
#' sao informados com periodicidade bimestral.
#'
#' @examples
#' \dontrun{
#' tsRetornos <- rppsDesempenho(dtCvm, dtDair)
#' }
#' @seealso \code{RppsWrangler} e \code{RcvmWrangler}
#' @export
rppsDesempenho <- function(dtCvm, dtDair) {

  library(data.table)

  dtDair <- data.table::data.table(dtDair)
  dtCvm <- data.table::data.table(dtCvm)

  # Extrai informacoes cadastrais dos fundos extraidos dos DAIR
  colCadastro <- c("ENTE",
                   "SEGMENTO",
                   "TIPO_ATIVO",
                   "INSTITUICAO_FINANCEIRA",
                   "CNPJ_INSTITUICAO_FINANCEIRA",
                   "FUNDO",
                   "CNPJ_FUNDO",
                   "INDICE_REFERENCIA")
  dtCadastro <- data.table::data.table(as.data.frame(dtDair)[,colCadastro])
  dtCadastro <- unique(dtCadastro[order(dtCadastro$FUNDO),])

  # Preserva apenas numeros do CNPJ dos Fundos
  # dtDair$CNPJ_FUNDO <- gsub("[./-]", "", as.character(dtDair$CNPJ_FUNDO))
  # dtDair$CNPJ_FUNDO <- gsub("(?<![0-9])0+", "", dtDair$CNPJ_FUNDO, perl = TRUE)

  # Preserva apenas numeros do CNPJ das Instituicoes financeiras
  # dtDair$CNPJ_INSTITUICAO_FINANCEIRA <- gsub("[./-]", "", as.character(dtDair$CNPJ_INSTITUICAO_FINANCEIRA))
  # dtDair$CNPJ_INSTITUICAO_FINANCEIRA <- gsub("(?<![0-9])0+", "", dtDair$CNPJ_INSTITUICAO_FINANCEIRA, perl = TRUE)

  # Caso exista mais de uma aplicacao num mesmo fundo, agrupa numa so
  dtDair <- dtDair[,.(QTD_COTAS = sum(QTD_COTAS), VALOR_TOTAL_ATUAL = sum(VALOR_TOTAL_ATUAL)),
                   by = .(ENTE, SEGMENTO, TIPO_ATIVO, DATA_POSICAO,
                          INSTITUICAO_FINANCEIRA, CNPJ_INSTITUICAO_FINANCEIRA,
                          FUNDO, CNPJ_FUNDO, VALOR_ATUAL_COTA, INDICE_REFERENCIA, PL_FUNDO)]

  # Extrai lista de todos os fundos encontrados nos DAIR
  fundos <- unique(dtDair$CNPJ_FUNDO)

  # VERIFICAR --> ALGUNS FUNDOS NAO FORAM ENCONTRADOS
  # idEncontrados <- which(dtDair$CNPJ_FUNDO %in% dtCvm$CNPJ_FDO)
  # dtDair <- dtDair[idEncontrados,]

  # Faz um merge das aplicacoes em fundos extraidas do DAIR com os informes diarios obtidos da CVM
  dtCvmDair <- merge(x = dtCvm[,.(CNPJ_FDO, DT_COMPTC, VL_QUOTA)],
                     y = unique(dtDair[,.(FUNDO, CNPJ_FUNDO,
                                          INSTITUICAO_FINANCEIRA, CNPJ_INSTITUICAO_FINANCEIRA,
                                          TIPO_ATIVO, SEGMENTO, INDICE_REFERENCIA)]),
                     by.x = "CNPJ_FDO",
                     by.y = "CNPJ_FUNDO",
                     all = T)

  # estima valores do fundos ausentes dos informes da CVM a partir do proprio DAIR:
  dtAusentesCvm <- dtCvmDair[is.na(dtCvmDair$DT_COMPTC),]
  dtAusentesDair <- dtDair[CNPJ_FUNDO %in% dtAusentesCvm$CNPJ_FDO,]

  dtCvmDair <- rbind(
    dtCvmDair[!is.na(dtCvmDair$DT_COMPTC),],
    dtAusentesDair[,.(CNPJ_FDO = CNPJ_FUNDO,
                                      DT_COMPTC = DATA_POSICAO,
                                      VL_QUOTA = VALOR_ATUAL_COTA,
                                      FUNDO, INSTITUICAO_FINANCEIRA, CNPJ_INSTITUICAO_FINANCEIRA,
                                      TIPO_ATIVO, SEGMENTO, INDICE_REFERENCIA)]
  )


  # cria uma serie temporal a partir do data.table dtCvmDair
  tsCvmDair <- data.table::dcast.data.table(data = dtCvmDair[,.(DT_COMPTC, FUNDO, VL_QUOTA)],
                                            formula = DT_COMPTC ~ FUNDO,
                                            value.var = "VL_QUOTA")

  # transforma a serie temporal tsCvmDair numa serie temoral do tipo xts
  colDate <- tsCvmDair$DT_COMPTC
  tsCvmDair <- xts::xts(x = tsCvmDair[, !"DT_COMPTC", with=F], order.by = colDate)


  # interpola os valores faltantes (NAs)
  # tsCvmDair <- na.spline(tsCvmDair)
  tsCvmDair <- zoo::na.locf(tsCvmDair, maxgap = Inf)
  tsCvmDair <- zoo::na.locf(tsCvmDair, maxgap = Inf, fromLast = T)
  tsCvmDair <- xts::to.period(tsCvmDair, OHLC = F, period = 'months')

  # a multiplicacao de matrizes abaixo cria uma coluna com o valor da "quota equivalente"
  # da carteira do RPPS.

  # Incluir os calculos abaixo num looping que será definido pela data da posicao da carteira do DAIR.
  # Assume-se que a posicao dos investimentos do DAIR permaneceu inalterada entre a data do ultimo DAIR
  # e a do atual.

  dataPosicao <- sort(unique(c(dtDair$DATA_POSICAO, Sys.Date())))

  # Calcula o retorno dos fundos no periodo analisado
  tsCvmDair <- tsCvmDair[zoo::index(tsCvmDair)>dataPosicao[1],]

  posRpps <- NULL
  for (i in 1:(length(dataPosicao)-1)) {

    id <- which(zoo::index(tsCvmDair)>dataPosicao[i] & zoo::index(tsCvmDair)<=dataPosicao[i+1])

    matFundos <- data.matrix(as.data.frame(tsCvmDair[id,]))

    dtDairAtual <- dtDair[DATA_POSICAO == dataPosicao[i] & FUNDO %in% unlist(dimnames(tsCvmDair)),]
    dtDairAtual <- merge(
      x = dtDairAtual,
      y = dtCadastro,
      by.x = colCadastro,
      by.y = colCadastro,
      all.y = T
    )
    dtDairAtual[is.na(QTD_COTAS),]$QTD_COTAS <- 0
    dtDairAtual <- dtDairAtual[order(FUNDO)]

    vecCotas <- as.matrix(dtDairAtual$QTD_COTAS)

    posRpps <- rbind(
      posRpps,
      matFundos %*% vecCotas
    )
  }
  row.names(posRpps) <- NULL

  tsCvmDair <- cbind(posRpps, tsCvmDair)
  names(tsCvmDair)[1] <- "RPPS"

  # Calcula o retorno dos fundos no periodo analisado
  rtCvmDair <- PerformanceAnalytics::Return.calculate(tsCvmDair, method = "discrete")

  rtCvmDair[1,] <- 0

  # Cria classe xts_dair a partir da classe xts
  class(rtCvmDair) <- c("xts_dair", class(rtCvmDair))

  # um objeto da classe xts_dair contem os seguintes atributos:
  #
  # col_rpps -> indica a coluna correspondente a serie historica da carteira do RPPS
  # col_fundos -> colunas relativas aos fundos que compoem a carteira do RPPS
  # col_indices_ref -> colunas relativas a indices de referencia
  # col_livre_risco -> coluna correspondente a taxa livre de risco
  # segmento_fundo -> segmento do fundo (RENDA FIXA ou RENDA VARIAVEL)
  # cnpj_fundo -> cnpj dos fundos
  # fundo_estimado -> indica de o fundo estava ausente dos informes da CVM
  # tipo_ativo -> tipo do ativo segundo Resolução CMN 3.244/2004


  # cria um data.frame de atributos:
  dfAttrib <- unique(as.data.frame(dtDair)[,c("ENTE", "FUNDO", "SEGMENTO", "TIPO_ATIVO")])
  dfAttrib <- dfAttrib[order(dfAttrib$FUNDO),]

  attr(x = rtCvmDair, which = "ente") <- iconv(enc2native(as.character(dfAttrib$ENTE[1])), to = "ASCII//TRANSLIT")
  attr(x = rtCvmDair, which = "col_rpps") <- 1
  attr(x = rtCvmDair, which = "col_fundos") <- (1:length(dfAttrib$FUNDO))+1
  attr(x = rtCvmDair, which = "col_indices_ref") <- numeric()
  attr(x = rtCvmDair, which = "col_livre_risco") <- numeric()
  attr(x = rtCvmDair, which = "segmento_fundo") <- dfAttrib$SEGMENTO
  attr(x = rtCvmDair, which = "cnpj_fundo") <- dfAttrib$CNPJ_FUNDO
  attr(x = rtCvmDair, which = "fundo_estimado") <- dfAttrib$CNPJ_FUNDO %in% dtAusentesCvm$CNPJ_FDO
  attr(x = rtCvmDair, which = "tipo_ativo") <- dfAttrib$TIPO_ATIVO

  rtCvmDair
}
