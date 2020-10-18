#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# Script   : PAM_importação.R
# Descrição: Produção Agrícola Municipal - oferece opções e filtros geográficos
#	     para visualização de dados da tabela 5457 do SIDRA (IBGE)
# Criado em: 18 de setembro de 2020
# Autor    : João Paulo G. Garcia <joaopauloggarcia@gmail.com>
#-------------------------------------------------------------------------------
# Software  : R 4.0.2
# Requisitos: Arquivos csv da tabela 5457 entre 2002 e 2018;
#	      Se você trabalha com o Windows, talvez seja necessário instalar o
#	      Rtools, o que pode ser feito através do pacote "installr";
# Instrução : Os arquivos csv da tabela 5457 de 2002 a 2018 podem ser baixados
#             no site do SIDRA, ou no link <https://drive.google.com/file/d/1oP2fNwh_XjzKqgqeei-voXhfEv_3U-az/view?usp=sharing>
#	      Criar pasta de arquivos "Entrada" (arquivos de dados baixados) e 
#             "Saida" (exportação de tabelas e gráficos) OU alterar os caminhos
#             utilizados no script;
#	      Para atualizar a base de dados será necessário editar o script.
#-------------------------------------------------------------------------------

# Edite a linha abaixo para o diretório de trabalho desejado
#setwd("/home/jpgg/R/PAC-PAM-PIA")

# Carregar, instalar e atualizar pacotes
pacotes <- c("vroom", "openxlsx", "dplyr", "stringr", "ggplot2")
#pacotes <- c(pacotes, "plotly")		# utilizar plotly opcionalmente
#install.packages(pacotes)			# instalar/atualizar pacotes
lapply(pacotes, library, character.only = TRUE)

# Estrutura
#...1 - IMPORTAÇÃO
#.... 1.1 - Tabela 5457 - Unidades Federativas
#.... 1.2 - Tabela 5457 - Municípios (BR)
#.... 1.3 - Tabela de Regiões Intermediárias por Municípios
#...2 - MANIPULAÇÃO
#...3 - VISUALIZAÇÃO

#============================== 1. IMPORTAÇÃO ====================================
# Importar os arquivos  de dados do IBGE utilizados.
#=================================================================================

# 1.1 Tabela 5457 - Unidades Federativas -----------------------------------------
# importar dados
t5457_UF <- vroom("Entrada/t5457_UF.csv",		# a linha abaixo faz o download do arquivo
		  #"https://sidra.ibge.gov.br/geratabela?format=us.csv&name=t5457_UF.csv&terr=NC&rank=-&query=t/5457/n1/all/n3/11,12,13,14,15,16,21,22,23,24,17,25,26,27,28,29,31,32,33,35,41,42,43,51,52,53,50/v/112,214,215,216/p/last%2017/c782/all/l/,,p%2Bt%2Bv%2Bc782",
		  skip = 1,
		  na = c("-", "..", "..."),
		  col_types = "ncfffn")
colnames(t5457_UF) <- c("Ano", "Cod", "UF", "Variavel", "Produto", "Valor")

# 1.2 Tabela 5457 - Municípios (BR) ----------------------------------------------
# lista com os nomes dos arquivos
arquivos_csv <- paste0("Entrada/", "tab", "_", "5457", "_", 2002:2018, ".csv")

# importar arquivos e 
t5457_mun <- vroom(file = arquivos_csv,
		   skip = 1,
		   n_max = 1602144,		# retirar linhas de comentários
		   na = c("-","..","..."),
		   col_types = "ncfffn")
		  
colnames(t5457_mun) <- c("Cod", "Municipio", "Ano", "Variavel", "Produto", "Valor")

# 1.3 Tabela com as Regiões Intermediárias ---------------------------------------
regints <- read.xlsx("Entrada/regints.xlsx")		# a linha abaixo faz o download do arquivo
		     #"ftp://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/divisao_regional_do_brasil/divisao_regional_do_brasil_em_regioes_geograficas_2017/tabelas/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")

#============================== 2. MANIPULAÇÃO ===================================
# Manipulação de dados que serão usados na elaboração dos gráficos
#=================================================================================

# 2.1 UF - 10 maiores produtos por variável --------------------------------------
uf_10maiores <- t5457_UF %>%
  filter(str_detect(UF, "Minas"),
	 str_detect(Variavel, "Área")) %>%
  group_by(Ano, UF) %>%
  arrange(Valor) %>%
  slice(1:10)


#============================== 3. VISUALIZAÇÃO ==================================
# Elaboração de gráficos
#=================================================================================
