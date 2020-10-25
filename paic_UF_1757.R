#-------------------------------------------------------------------------------
#
#                   PESQUISA ANUAL DA INDÚSTRIA DO COMÉRCIO
#
#       Autor : João Paulo G. Garcia 
#       email : <joaopauloggarcia@gmail.com>
#                                             24 de outubro de 2020
#-------------------------------------------------------------------------------
# Software  : R 4.0.2
# Requisitos: Rtools (Windows, pode ser instalado pelo 'installr')
#-------------------------------------------------------------------------------
# Descrição : O script faz o download da base de dados (para trabalhar com
#             arquivos do HD é preciso criar um pasta "Entrada" com os arquivos
#             csv); oferece algumas opções de filtros e gera alguns gráficos.
#
#-------------------------------------------------------------------------------

# PACOTES                                                                   #{{{
pacotes <- list("data.table", "forcats", "magrittr",
                "ggplot2", "plotly", "RColorBrewer")
#
pacotes_instalados <- pacotes %in% rownames(installed.packages())
#
# instalar pacotes
if (any(pacotes_instalados == FALSE)) {
  install.packages(pacotes[!installed_packages])
}
# carregar pacotes
lapply(pacotes, library, character.only=TRUE)                               #}}}

# ESTRUTURA                                                                 #{{{
#...1 - Importação
#...2 - Manipulação
#...3 - Visualização
#...4 - Exportação                                                          #}}}

# -----------------------------------------------------------------------------
#                                 1. BASE DE DADOS

# caminho para o arquivo de entrada/ ou o endereço de download.
setwd("~/R/Pesquisas-IBGE/")

# tabela 1757 do SIDRA                                                      #{{{
url_1757 <-  "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1757.csv&terr=N&rank=-&query=t/1757/n1/all/n2/all/n3/all/v/410,630,631,634,637,673,1239,1240,1242/p/all/c319/104030/l/,,t%2Bc319%2Bp%2Bv"

if (file.exists("Entrada/tab_1757.csv")) {
  entrada <- "Entrada/tab_1757.csv"
} else {
  entrada <- url_1757
}

tab_1757 <- fread(entrada,
                  na.strings = c('"-"', '"X"', '".."', '"..."'),
                  colClasses = list("factor" = c(1:4)),
                  col.names = c("Territorio", "Pessoal.Ocupado",
                                "Ano", "Var", "Valor"))                     #}}}

# -----------------------------------------------------------------------------
#                                 2. MANIPULAÇÃO
# EXEMPLO 1                                                                 #{{{
# 1 Território, 1 Faixa de Pessoal ocupado (5 ou mais) e "N" Variáveis

# primeiro filtramos os dados
n_divisao <- tab_1757[Territorio %like% "Minas" #& Var %like% "empresas|Receita"
                      ][, Rank := frank(-Valor, na.last = "keep"), by = Ano]#}}}

# EXEMPLO 2                                                                 {{{
# "N" Territórios, 1 Faixa de Pessoal ocupado (5 ou mais) e 1 Variável

# primeiro filtramos os dados
n_territorio <- tab_1757[Territorio %like% "Minas|Janeiro|Paulo|Sudeste" &
                         Var %like% "remunerações"]                         #}}}

# EXEMPLO 3                                                                #{{{
# Todos os Estados, 1 Faixa de Pessoal ocupado (5 ou mais), 2 Variável e 6 anos

wide_div <- tab_1407[!(Territorio %like% "Brasil|Região") &
                     Var %like% "líquida|Salários" &
                     Ano %in% as.factor(2013:2018)] %>%
                       dcast(Ano + Territorio ~ Var, value.var = "Valor")

# simplificar nomes de colunas
colnames(wide_div)[3:4] = c("Receita.liquida", "Salarios.e.remuneracoes")

# criar colunas de rank
wide_div[, `:=` (Rank_V = frank(-Receita.liquida, na.last = "keep"),
                 Rank_C = frank(-Salarios.e.remuneracoes, na.last = "keep")),
         by = .(Ano, Territorio)]                                          #}}}

# -----------------------------------------------------------------------------
#                                 3. VISUALIZAÇÃO 

# GRÁFICO DE BOLHAS...............................................         #}}}
# As 10 maiores Variáveis's por Pessoal.Ocupado, Territorio e Ano.

# CANE's em ordem crescente
n_divisao$Var <- fct_reorder(n_divisao$Var, -n_divisao$Rank)

g_bolha <-  n_divisao[Rank <= 10] %>%
  ggplot(aes(x = Ano, y = Var, text = paste("Rank: ", Rank))) +
  geom_point(
    aes(size = Valor, color = as.factor(Rank)),
    show.legend = F
  ) +
  scale_size(range = c(3, 12)) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Variável") 

# visualizar com o plotly
ggplotly(g_bolha, tooltip = c("text", "y", "size")) %>% hide_guides()      #}}}

# GRÁFICO DE CAIXA................................................         #{{{ 
# 1 Estado, 1 Faixa de Pessoal ocupado, todos os Anos e todas as Variáveis

# usando os ranks podemos escolher a abrangência das Var's
distribuicao <- n_divisao[Rank %between% c(20,50)] %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_jitter(aes(text = paste("Varável: ", Var)),
              fill = "pink", alpha = 0.3, shape = 21) +
  geom_boxplot(aes(fill = Ano), alpha = 0.6) +
  theme_classic()

# visualizar com o plotly
ggplotly(distribuicao, tooltip = c("text", "y")) %>% hide_guides()         #}}}

# GRÁFICO DE CORRELAÇÃO...........................................

# para possíveis gráficos de correlação utilizar o filtro "wide_div"

# SÉRIE HISTÓRICA.................................................         #{{{

# elaboração do gráfico
serie_h <- n_divisao %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_line(aes(color = Var, group = Var)) +
  ylab("Variáveis")

# visualizar
plotly(serie_h, tooltip = c("color", "y", "x")) %>% hide_guides()          #}}}

# -----------------------------------------------------------------------------
