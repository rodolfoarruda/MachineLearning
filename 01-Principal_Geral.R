#########################################################################################################################################################
#########################################################################################################################################################
############################################################## Principal ################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

#################################################### Geração do banco de dados
# Pacotes
library(tm)
library(SnowballC)
library(wordcloud)
library(stringdist)
library(RODBC)
library(dplyr)
library(tidytext)

options(scipen=10)

# Banco de dados 
CON <- odbcConnect("RServer1",uid="rodolfo.arruda.telefonica",pwd="#Ro150286")
dados <- sqlQuery(CON,'SELECT *, [texto] = CAST(SU01A AS VARCHAR(8000))
                       FROM DB_GRP_GOVERNANCA_KPI..RODS_TM_FIXA_B2B_INPUT')


# Filtro de colunas e definição de nome padrão
#dados<-vivo2[,c("IDTNS","SU01A","ISC","PERIODO")]

dados<-as.list(dados_1804)
dados$texto<-toupper(dados$texto)
# Remover acentuação
source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\02-RemoveAcentos_Geral.R");dados$texto<-rm_accent(dados$texto) 
# Marca clientes que não sugeriram
source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\03-SemSugestao_Geral.R")
# Corretor de palavras
source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\04-TratamentoTexto_Geral.R")

# Palavras Chave - Altere para o produto específico

source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\05-Keywords_Geral_B2CFixa.R")

dados_B2B_fixa_aux1<-dados_final
# Salvar no SQL
CON <- odbcConnect("RServer1",uid="rodolfo.arruda.telefonica",pwd="#Ro150286")
sqlSave(CON, dados_B2B_fixa_aux1, rownames = FALSE,append = FALSE, varTypes=c(SU01A="varchar(8000)",texto="varchar(8000)"))


#################################################### Word Cloud

#Filtro de Processo / Produto:
source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\06-FunctionKeyWord.R")

# Definir os filtro de público
banco_filtrado<-dados_final

#################Parâmetros:
# dados<-bd(produto,prod_unico)

#produto:
#"Geral"      = 0
#"BL"         = 1
#"Linha"      = 2
#"TV"         = 3
#"Atendimento"= 4
#"Fatura"     = 5
#"Web"        = 6
#"Suporte"    = 7
#"Preco"      = 8  

#prod_unico: Determina se o cliente reclamou de apenas 1 ítem.
#0: Não 
#1: Sim

#Seleção de Processo/Produto
dados<-bd(produto=0,prod_unico=0);rm(banco_filtrado)

# Banco de dados com termos por ID:
source("Z:\\01.Governança Corporativa\\Estudos\\TextMining\\01 - Programas\\07-TermosCitados.R")

PB_dados_B2B_fixa_aux1<-PB_dados
# Salvar no SQL
CON <- odbcConnect("RServer1",uid="rodolfo.arruda.telefonica",pwd="#Ro150286")
sqlSave(CON,PB_dados_B2B_fixa_aux1, rownames = FALSE,append = FALSE, varTypes=c(SU01A="varchar(8000)",texto="varchar(8000)"))

