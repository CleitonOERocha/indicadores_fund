

#########################################################################################################
############################ Obtendo indicadores fundamentalistas #######################################
#########################################################################################################
#####   ESCRITO POR: Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL: cleitonotavio058@gmail.com 
#####   LICENÇA: GPLv3
#####   Data: 20/01/2021
#########################################################################################################
#########################################################################################################

library(tidyverse)
library(ralger)
library(lubridate)

#########################################################################
##### Função de leitura -------------------------------------------------
#########################################################################

# argumento da função é 'cod_acao' = ticker do ativo, ex. PETR4, BBDC3, etc.

fn_obter_indicadores_acao = function(cod_acao){
  
  tryCatch({
  
  # obtendo tabela com dados fundamentalistas
  dados_fundamentus <- table_scrap(paste0("https://www.fundamentus.com.br/detalhes.php?papel=",cod_acao),choose = 3)
  
  # retirando colunas não necessárias, que tratam de oscilações do preço da ação
  dados_fundamentus <- dados_fundamentus[,-c(1,2)]
  
  dados_fundamentus1 <- dados_fundamentus[,1:2]
  dados_fundamentus2 <- dados_fundamentus[,3:4]
  
  dados_fundamentus_all <- rbind(dados_fundamentus1, dados_fundamentus2)
  
  dados_fundamentus_all <- dados_fundamentus_all %>% `colnames<-`(c("indicador", "valor"))
  
  # retirando "?" no começo de cada termo
  dados_fundamentus_all$indicador <- sub("^.", "", dados_fundamentus_all$indicador)
  
  # renomeando algumas colunas
  dados_fundamentus_all$indicador <- dplyr::recode(dados_fundamentus_all$indicador,
                                                    `Cres. Rec (5a)`= "Cres.Rec.Ultimos.5anos",
                                                    `EV / EBIT` = "EV_EBIT",
                                                    `Marg. EBIT` = "Marg.EBIT",
                                                    `Div. Yield` = "Div.Yield",
                                                    `Marg. Líquida` = "Marg.Liquida",
                                                    `Liquidez Corr` = "Liquidez.Corrente",
                                                    `EBIT / Ativo` = "EBIT_Ativo",
                                                    `Div Br/ Patrim` = "Div.Bruta_Patrimonio",
                                                    `P/Cap. Giro` = "P_Cap.Giro",
                                                    `Marg. Bruta` = "Margem.Bruta",
                                                    `Giro Ativos` = "Giro.Ativos",
                                                    `P/Ativ Circ Liq` = "P_Ativo.Circ.Liq",
                                                    `EV / EBITDA` = "EV_EBITDA")
  
  
  # criando coluna com nome da ação e alterando a estrutura do banco de dados
  dados_fundamentus_all <- dados_fundamentus_all %>%
    dplyr::mutate(Acao = cod_acao) %>%
    tidyr::spread(key = indicador, value = valor)
  
  return(dados_fundamentus_all)
  
  }, error = function(e){message("Erro no cód: ", cod_acao, " | O ticker pode não existir mais")}
  
  )
  
  
}

#########################################################################
##### Função de tratamento ----------------------------------------------
#########################################################################

# argumento da função é 'vetor_ativos' = um ou mais ativos (em formato de vetor) para serem coletados

fn_tratar_indicadores_acao = function(vetor_ativos){
  
  if(length(vetor_ativos) > 1){

  # aplicando loop para obter os indicadores das ações armazenados em um só dataframe
  indicadores_fundamentalistas <- purrr::map_df(vetor_ativos, suppressMessages(fn_obter_indicadores_acao))
  
  } else {
    
    indicadores_fundamentalistas = fn_obter_indicadores_acao(vetor_ativos)
    
  }
  
  # Transformando "-" em NA
  indicadores_fundamentalistas[indicadores_fundamentalistas == "-"] <- NA
  
  # retirando "%"
  indicadores_fundamentalistas <- indicadores_fundamentalistas %>%
                                      dplyr::mutate(dplyr::across(dplyr::everything(), 
                                                                  ~str_replace_all(.x, "%", "")))
  
  # convertendo colunas em númericas
  indicadores_fundamentalistas <- indicadores_fundamentalistas %>%
                                      dplyr::mutate(dplyr::across(-1, ~gsub(",", ".", 
                                                                            gsub("\\.", "", .x)) %>% as.numeric))
  
  
  indicadores_fundamentalistas = indicadores_fundamentalistas %>% dplyr::mutate(data_execucao = Sys.Date())
  
  return(indicadores_fundamentalistas)

}


#########################################################################
##### Execução ----------------------------------------------------------
#########################################################################

# vetor com ações listadas no ibovespa ou no indice small caps
bovespa <- c("VALE3", "ITUB4","B3SA3","PETR4", "BBDC4", "PETR3","ABEV3",
             "BBAS3","MGLU3", "ITSA4", "WEGE3", "JBSS3", "LREN3", "GNDI3",
             "NTCO3", "SUZB3", "RENT3", "BBDC3", "EQTL3", "RADL3", "RAIL3",
             "VVAR3", "LAME4", "BTOW3", "UGPA3","VIVT4", "SBSP3", "BBSE3",
             "CCRO3", "BRFS3", "GGBR4", "BRDT3","BPAC11", "KLBN11","HAPV3",
             "HYPE3", "SULA11", "COGN3","ENGI11", "TIMP3", "ELET3", "CSAN3",
             "PCAR3", "EGIE3","CMIG4", "SANB11", "YDUQ3", "TOTS3", "BRML3",
             "IRBR3", "ELET6", "QUAL3","BRAP4", "CRFB3", "FLRY3","CSNA3",
             "CYRE3", "AZUL4", "BRKM5", "TAEE11", "EMBR3","MULT3","MRVE3",
             "CIEL3", "ENBR3", "CPFE3", "MRFG3", "GOAU4", "USIM5", "BEEF3",
             "IGTA3", "CVCB3", "GOLL4","ECOR3", "HGTX3","AALR3","ABCB4","ALSO3",
             "ALUP11","AMAR3","ANIM3", "ARZZ3","BIDI11","BIDI4", "BKBR3","BMGB4",
             "BPAN4","BRPR3", "BRSR6","CAML3","CEAB3","CESP6","CNTO3","CSMG3",
             "DIRR3","DMMO3","DTEX3","ENAT3","ENEV3","EVEN3","EZTC3","GFSA3",
             "GRND3","GUAR3", "HBOR3", "JHSF3","LCAM3","LEVE3","LIGT3","LINX3",
             "LOGG3","LOGN3","LPSB3","LWSA3", "MDIA3","MEAL3","MILS3", "MOVI3",
             "MYPK3","ODPV3","OMGE3","PARD3","POMO4","POSI3","PRIO3","RAPT4","RLOG3" ,
             "SAPR11","SAPR4","SEER3", "SIMH3","SLCE3","SMLS3","SMTO3","SQIA3",
             "STBP3","TASA4","TCSA3","TEND3","TGMA3","TIET11","TRIS3","TUPY3",
             "VIVA3", "VLID3","VULC3","WIZS3" 
)

# Aplicando para um ativo
#indicadores_fundamentalistas = fn_tratar_indicadores_acao('GGBR4')

# Aplicando para um vetor de ativos 'bovespa'
indicadores_fundamentalistas = fn_tratar_indicadores_acao(bovespa)


#########################################################################
##### Exemplo de análises rápidas com o dataframe gerado ----------------
#########################################################################

# procurando empresas cuja multiplicação do P/L pelo P/VP seja menor que 22,5
indicadores_fundamentalistas$descontadas <- ifelse(indicadores_fundamentalistas$`P/L`*indicadores_fundamentalistas$`P/VP` <= 22.5 &
                                                     indicadores_fundamentalistas$`P/L`*indicadores_fundamentalistas$`P/VP` > 0, 1, 0)

# criando novo dataset com as empresas descontadas
empresas_descontadas <- indicadores_fundamentalistas %>% dplyr::filter(descontadas == 1)

# brincando com os indicadores
empresas_descontadas <- empresas_descontadas %>% dplyr::filter(ROE > 10 &
                                                               Cres.Rec.Ultimos.5anos > 0 &
                                                               Div.Bruta_Patrimonio <= 1.5 &
                                                               Div.Bruta_Patrimonio > 0)




