#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# Script   : PAC-PAM-PIA/pam_regint.R
# Descrição: Exporta a tabela 5457 (Produção Agrícola Municipal) por RegInt
# Criado em: 28 de setembro de 2020
# Autor    : João Paulo G. Garcia <joaopauloggarcia@gmail.com>
#-------------------------------------------------------------------------------
# Software  : R 4.0.2
# Requisitos: Se você trabalha com o Windows, talvez seja necessário instalar o
#	      Rtools, o que pode ser feito através do pacote "installr";
# Instrução : Criar pasta de arquivos "Entrada" (arquivos de dados baixados) e 
#            "Saida" (exportação de tabelas e gráficos) OU alterar os caminhos
#	      utilizados no script.
#-------------------------------------------------------------------------------

# Definir diretório de trabalho
setwd("/home/jpgg/R/PAC-PAM-PIA")	     # mude para o seu diretório

# Carregar, instalar e atualizar pacotes
pacotes <- c("data.table", "openxlsx", "dtplyr", "dplyr", "stringr")
#install.packages(pacotes)			# instalar/atualizar pacotes
lapply(pacotes, library, character.only = TRUE)

# Estrutura
#...1 - IMPORTAÇÃO
#...2 - MANIPULAÇÃO
#...3 - Exportação

#============================= 1. IMPORTAÇÃO ===================================
# primeiro criar um vetor com os nomes das tabelas
arquivos_csv <- paste0("Entrada/tab", "_", "5457", "_", 2002:2018, ".csv") # ex: "tab_5457_2012"
# 
# criar uma lista que vamos preencher com as 17 tabelas importadas utilizanda
tabelas <- vector("list", 17)
for (i in 1:17) {
  tabelas[[i]] <- fread(file = arquivos_csv[[i]],
			skip = 1,
			nrows = 1602144,
			na.strings = c('"-"','".."','"..."'),
			colClasses = c(list("factor" = c(1:5), "numeric" = 1)), 
			col.names = c("Cod", "Municipio", "Ano",
				      "Variavel", "Produto", "Valor"))
}
# importar os dados para uma tabela
geo_cod <- read.xlsx("Entrada/regints.xlsx")

#============================= 2. MANIPULAÇÃO ==================================

# criar uma tabela única com todos os dados de município (BR)
PAM_mun <- tabelas %>%
  rbindlist()

# criar as coluna com a função mutate
PAM_mun <- PAM_mun %>%
  mutate(
	 "UF" = str_sub(PAM_mun$Município,-5.-2),
	 "RegInt" = geo_cod$nome_rgint[match(PAM_mun$Cód.,
					     geo_cod$CD_GEOCODI)],
	 "Cod.RegInt" = geo_cod$cod_rgint[match(PAM_mun$Cód.,
						 geo_cod$CD_GEOCODI)]
  )
# agregar os dados municipais por RegInt
PAM_regint <- PAM_mun %>%
  group_by(Ano, Cod.RegInt, RegInt, Variável, Produto) %>%
  summarise("Valor" = sum(Valor, na.rm = TRUE))

#============================= 3. Exportação =================================

# exportar a tabela 5457 por RegInts
fwrite(PAM_regint, "Saida/t5457_regint.csv")
