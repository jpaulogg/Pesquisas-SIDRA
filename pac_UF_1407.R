#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# Script   : pac_sidra.R
# Descrição: Pesquisa Anual de Comércio;
#     	     tabela 1407 do SIDRA - IBGE
# Criado em: 10 de outubro de 2020
# Autor    : João Paulo G. Garcia <joaopauloggarcia@gmail.com>
#------------------------------------------------------------------------------
# Software  : R 4.0.2
#
# Requisitos: Se você trabalha  com o Windows, talvez seja  necessário instalar
#	            o Rtools, o que pode ser feito através do pacote "installr";
#
# Instrução : Opcionalmente, criar pasta de arquivos "Entrada" (arquivos de
#             dados baixados) e "Saida" (exportação de tabelas e gráficos)
#------------------------------------------------------------------------------
# PACOTES ---------------------------------------------------------------------
pacotes <- list("data.table", "forcats", "magrittr",
                "ggplot2", "plotly", "RColorBrewer")

# instalar pacotes
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(pacotes, library, character.only=TRUE)

# ESTRUTURA ------------------------------------------------------------------
#...1 - Importação
#...2 - Manipulação
#...3 - Visualização
#...4 - Exportação

#============================= 1.IMPORTAÇÃO ==================================

setwd("~/R/PAC-PAM-PIA")

# objeto com a entrada de dados a serem importados
entrada  <- if (file.exists("Entrada/tab_1407.csv")) {
  "Entrada/tab_1407.csv"
} else {
  "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1407.csv&terr=N&rank=-&query=t/1407/n1/all/v/312/p/all/c12354/all/c11066/allxt/l/,,p%2Bt%2Bc12354%2Bv%2Bc11066"
}

# importar tabela
tab_1407 <- fread(entrada,
                  na.strings = c('"-"','"X"'),
                  colClasses = c(list("factor" = c(1:5))),
                  col.names = c("Ano", "Brasil", "Territorio",
                                "Var", "Divisao", "Valor"))

#============================= 2. MANIPULAÇÃO ================================

# EXEMPLO 1 -------------------------------------------------------------------
# 1 Região, 1 Variável (Pessoal ocupado) e "N" Divisões de comércio (...)

# primeiro filtramos os dados
n_divisao <- tab_1407[Territorio %like% "Minas" #& Divisão %like% "atacado"
                      ][, Rank := frank(-Valor, na.last = "keep"), by = Ano]


# EXEMPLO 2 -------------------------------------------------------------------	
# "N" Territórios, 1 Variável(Pessoal ocupado) e 1 Divisão

# primeiro filtramos os dados
n_territorio <- tab_1407[Territorio %like% "Minas|Janeiro|Paulo|Sudeste" &
                         Divisao %like% "varejista"]

# EXEMPLO 3 -------------------------------------------------------------------
#  Todos os Estados, 1 Variável, 2 Divisões (em formato wide) e 6 anos

wide_div <- tab_1407[!(Territorio %like% "Brasil|Região") &
                     Divisao %like% "veículos,|4.4Combustíveis" &
                     Ano %in% as.factor(2013:2018)] %>%
                       dcast(Ano + Territorio ~ Divisao, value.var = "Valor")

# simplificar nomes de colunas
colnames(wide_div)[3:4] = c("Comercio.de.veiculos", "Combustiveis")

# criar colunas de rank
wide_div[, `:=` (Rank_V = frank(-Comercio.de.veiculos, na.last = "keep"),
                 Rank_C = frank(-Combustiveis, na.last = "keep")),
         by = .(Ano, Territorio)]

#---------------------------- 3. VISUALIZAÇÃO --------------------------------

# GRÁFICO DE BOLHAS ----------------------------------------------------------
# As 10 maiores Divisao's por Variável, Estado e Ano.

# CANE's em ordem crescente
n_divisao$Divisao <- fct_reorder(n_divisao$Divisao, -n_divisao$Rank)

g_bolha <-  n_divisao[Rank <= 10] %>%
  ggplot(aes(x = Ano, y = Divisao, text = paste("Rank: ", Rank))) +
  geom_point(
    aes(size = Valor, color = as.factor(Rank)),
    show.legend = F
  ) +
  scale_size(range = c(3, 12)) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Divisão de comércio e grupo de atividade") 

# visualizar com o plotly
ggplotly(g_bolha, tooltip = c("text", "y", "size")) %>% hide_guides()

# GRÁFICO DE CAIXA -----------------------------------------------------------
# 1 Estado, 1 Variável, todos os Anos e todas as Divisao's

# usando os ranks podemos escolher a abrangência das Divisao's
distribuicao <- n_divisao[Rank %between% c(20,50)] %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_jitter(aes(text = paste("Divisao: ", Divisao)),
              fill = "pink", alpha = 0.3, shape = 21) +
  geom_boxplot(aes(fill = Ano), alpha = 0.6) +
  theme_classic()

# visualizar com o plotly
ggplotly(distribuicao, tooltip = c("text", "y")) %>% hide_guides()

# GRÁFICO DE CORRELAÇÃO ------------------------------------------------------

# para possíveis gráficos de correlação utilizar o filtro "wide_div"

# SÉRIE HISTÓRICA ------------------------------------------------------------

# elaboração do gráfico
serie_h <- n_divisao %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_line(aes(color = Divisao, group = Divisao)) +
  ylab("Divisão de comércio e grupo de atividade")

# visualizar
plotly(serie_h, tooltip = c("color", "y", "x")) %>% hide_guides()
