###################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES DO BRASILEIRA - SERIE A
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 02/12/2023
# CPU 1:      Desktop HP; processor i7-3770 @ 3.40GHz, RAM: 16.0 GB
#  OS 1:      Windows 7 Professional
# CPU 2:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
#  OS 2:      Windows 10 Home Single Language
# Versão R: 4.3.1(x64) 
#
# OBS.1: Rotina para gera as chances dos times de acordo com técnicas naïve, 
# 		 os pareamentos (qual time enfrenta qual) não importam.
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (UnA-MATE), 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem alfabética dos times:

times <- c('America', 'Atletico', 'Athletico', 'Bahia', 'Botafogo', 'Bragantino', 'Corinthians', 'Coritiba', 'Cruzeiro', 'Cuiaba',
	'Flamengo', 'Fluminense', 'Fortaleza', 'Goias', 'Gremio', 'Internacional', 'Palmeiras', 'Santos', 'Sao Paulo', 'Vasco')

# Abreviatura dos times:

abr <- c('AME', 'CAM', 'CAP', 'BAH', 'BOT', 'BGT', 'COR', 'CTB', 'CRU', 'CUI', 'FLA', 'FLU', 'FOR', 'GOI', 'GRE', 'INT', 'PAL', 'SAN', 'SAO', 'VAS')

# Pontos na rodada atual de acordo com a oderm alfabética

pontos <- c(21, 66, 53, 41, 63, 59, 47, 30, 45, 48, 63, 56, 48, 35, 62, 52, 66, 43, 50, 42)

# Vamos fazer uma tabela de acordo com a classificação:

tab <- data.frame(matrix(c(abr, pontos), nrow = 20, ncol = 2))
names(tab) <- c("equipe", "pontos")

# Para ordenar pelos pontos,
# Ainda não tem critério de desempate:

# AQUI há mais imputs importantes, repare que já está quase na ordem da tabela porém sem critério de desempate.

tab <- tab[order(tab[,2], decreasing = TRUE),]
rodadas <- c(37, 36, 36, 36, 36, 36, 36, 36, 37, 37, 36, 36, 37, 36, 36, 36, 36, 36, 36, 36)
vitorias <- c(19, 19, 18, 18, 19, 16, 16, 13, 14, 13, 13, 13, 11, 11, 11, 11, 11, 8, 8, 4)
empates <- c(9, 9, 9, 9, 5, 11, 8, 14, 10, 11, 9, 9, 14, 12, 10, 9, 8, 11, 6, 9)
derrotas <- rodadas - (vitorias + empates)

# Juntando na tabela:

tab <- cbind(tab, rodadas, vitorias, empates, derrotas)

# Reordenando com critério de vitórias como desempate:

tab <- tab[order(tab[,2], tab[,4], decreasing = TRUE),]
rownames(tab) <- 1:nrow(tab)

# Em alguns casos o critério de vitórias não é suficiente então para o Saldo de Gols, mais dois vetores de inputs:
# Atenção à ordem dos dados:

GP <- c(51, 62, 57, 54, 59, 47, 49, 48, 43, 39, 36, 42, 45, 34, 38, 39, 44, 35, 41, 39) # Gols Pro
GC <- c(28, 32, 34, 40, 54, 33, 43, 40, 44, 38, 37, 43, 48, 31, 59, 49, 49, 52, 70, 78) # Gols Contra
Saldo <- GP - GC

# Juntando na tabela do brasileirão:

tab <- cbind(tab, GP, GC, Saldo)

# Corrigindo o fato de que o valor dos pontos não ficou como número na minha tabela:

tab[,2] <- as.numeric(tab[,2])

# O aproveitamento pode ser obtido a partir dos dados:

Aproveitamento <- round(tab[,2]/(3*tab[,3]),3)
tab <- cbind(tab, Aproveitamento)

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam:
#
###################################################

# Rodadas que ainda faltam (inclui as adiadas)

rodada_37 <- list(c('FLA', 'CTB', 'EMP'),
				  c('PAL', 'FLU', 'EMP'),
				  c('BOT', 'CRU', 'EMP'),
				  c('BGT', 'CTB', 'EMP'),
				  c('GRE', 'VAS', 'EMP'),
				  c('CAP', 'SAN', 'EMP'),
				  c('FOR', 'GOI', 'EMP'),
				  c('AME', 'BAH', 'EMP')
				  )

rodada_38 <- list(c('GOI', 'AME', 'EMP'),
				  c('FLU', 'GRE', 'EMP'),
				  c('VAS', 'BGT', 'EMP'),
				  c('SAO', 'FLA', 'EMP'),
				  c('SAN', 'FOR', 'EMP'),
				  c('CRU', 'PAL', 'EMP'),
				  c('INT', 'BOT', 'EMP'),
				  c('CTB', 'COR', 'EMP'),
				  c('BAH', 'CAM', 'EMP'),
				  c('CUI', 'CAP', 'EMP')
				  )

#####################################################
#
# Simulação Naïve (ingênua), 
# Considera as chances de vitória, derrota e empate 
# como sendo 1/3 para cada um dos times:
#
#####################################################

iter <- list(rodada_37, rodada_38)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados_naive <- matrix(0, nrow = 20, ncol = n)

for(i in 1:n)
{
	# Uma tabela sem GP, SC e Saldo nos será mais útil no momento:

	tab2 <- tab[,-c(7,8,9,10)]

	for(j in 1:2)
	{
	result <- lapply(iter[[j]], sample, size = 3)
		for(k in 1:length(result)){
			if(result[[k]][1] == "EMP"){
				tab2[tab2[,1] == result[[k]][2], 2:6] <- tab2[tab2[,1] == result[[k]][2], 2:6] + c(1,1,0,1,0)
				tab2[tab2[,1] == result[[k]][3], 2:6] <- tab2[tab2[,1] == result[[k]][3], 2:6] + c(1,1,0,1,0)
			}else{
				ganhou <- result[[k]][1]
				tab2[tab2[,1] == ganhou, 2:6] <- tab2[tab2[,1] == ganhou, 2:6] + c(3, 1, 1, 0, 0)
				perdeu <- which(result[[k]] == "EMP")
				perdeu <- result[[k]][-perdeu]
				perdeu <- perdeu[2]
				tab2[tab2[,1] == perdeu, 2:6] <- tab2[tab2[,1] == perdeu, 2:6] + c(0, 1, 0, 0, 1)
			}
		}
	}
tab3 <- tab2[order(tab2[,2], tab2[,4], decreasing = TRUE),]
resultados_naive[,i]  <- tab3[,1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- (table(resultados_naive[1,])/n)*100
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Brasileiro 2023", ylim = c(0,110))
text(0.7, 7, "4%")
text(1.9, 34, "31%")
text(3.1, 6.5, "3.5%")
text(4.35, 5.5, "2.5%")
text(5.5, 62, "59%")

# Para ver as chances de ficar entre os 6 primeiros colocados:
# Olhar os times que ficam entre os 6 primeiros (Libertadores e Pré-liberda, independente da Copa do Brasil)

chances_libertadores <- sort((table(resultados_naive[1:6,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores, main = "Probabilidades de Ficar entre os 6 primeiros Serie A Brasileiro 2023", ylim = c(0,110))

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados_naive[17:20,])/n)*100, decreasing = TRUE)
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie A Brasileiro 2023", ylim = c(0,110))
text(0.7, 102, "100%")
text(1.9, 102, "100%")
text(3.1, 102, "100%")
text(4.35, 61, "58%")
text(5.5, 31, "28%")
text(6.7, 16, "13%")
text(7.9, 5, "1%")
text(9.1, 3, "~0%")

## FIM DA ROTINA!