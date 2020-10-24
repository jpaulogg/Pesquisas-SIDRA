#-------------------------------------------------------------------------------
#
tag
#
#       Autor : João Paulo G. Garcia 
#       email : <joaopauloggarcia@gmail.com>
#                                             24 de outubro de 2020
#-------------------------------------------------------------------------------
# Software  : R 4.0.2
# Requisitos: Rtools (Windows, pode ser instalado pelo 'installr')
#-------------------------------------------------------------------------------
# Descrição : tag
#
#-------------------------------------------------------------------------------

# PACOTES ------------------------------------------------------------------{{{
pacotes <- list("data.table", "forcats", "magrittr",
                "ggplot2", "plotly", "RColorBrewer")

pacotes_instalados <- pacotes %in% rownames(installed.packages())

# instalar pacotes
if (any(pacotes_instalados == FALSE)) {
  install.packages(pacotes[!installed_packages])
}
# carregar pacotes
lapply(pacotes, library, character.only=TRUE)                              #}}}

# ESTRUTURA ----------------------------------------------------------------{{{
#...1 - Importação
#...2 - Manipulação
#...3 - Visualização
#...4 - Exportação                                                          }}}

# -----------------------------------------------------------------------------
#                                 1. BASE DE DADOS

# caminho para o arquivo de entrada/ ou o endereço de download.
setwd("~/R/Pesquisas-IBGE/")

# tabela 1757 do SIDRA                                                      {{{
url_1757 <-  "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1757.csv&terr=N&rank=-&query=t/1757/n1/all/n2/all/n3/all/v/410,630,631,634,637,673,1239,1240,1242/p/all/c319/104030/l/,,t%2Bc319%2Bp%2Bv"

if (file.exists("Entrada/tab_1757.csv")) {
  entrada <- "Entrada/tab_1757.csv"
} else {
  entrada <- url_1757
}

tab_1757 <- fread(file = entrada,
		     na.strings = c('"-"', '"X"', '".."', '"..."'),
		     colClasses = list("factor" = c(1:4)),
		     col.names = c("Territorio", "Pessoal.Ocupado",
                                   "Ano", "Var", "Valor"))                 #}}} 

# -----------------------------------------------------------------------------
#                                 2. MANIPULAÇÃO
# EXEMPLO 1                                                                 {{{
# 1 Território, 1 Faixa de Pessoal ocupado (5 ou mais) e "N" Variáveis

# primeiro filtramos os dados
n_divisao <- tab_1407[Territorio %like% "Minas" #& Var %like% "empresas|Receita"
                      ][, Rank := frank(-Valor, na.last = "keep"), by = Ano] #}}}

# EXEMPLO 2                                                                 {{{
# "N" Territórios, 1 Faixa de Pessoal ocupado (5 ou mais) e 1 Variável

# primeiro filtramos os dados
n_territorio <- tab_1407[Territorio %like% "Minas|Janeiro|Paulo|Sudeste" &
                         Var %like% "remunerações"]                         #}}}

