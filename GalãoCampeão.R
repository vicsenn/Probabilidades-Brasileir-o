###################################################################################################################
#
# ** GALÃO CAMPEÃO ** !!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 25/11/2021
# CPU 1:      Desktop HP; processor i7-3770 @ 3.40GHz, RAM: 16.0 GB
#  OS 1:      Windows 7 Professional
# CPU 2:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
#  OS 2:      Windows 10 Home Single Language
# Versão R: 4.0.5(x64) 
#
# OBS.1: Rotina para gera as chances dos times de acordo com duas técnicas diferentes. 
# 		 Na primeira técnica, naïve, os pareamentos (qual time enfrenta qual) não importam.
#		 Com a segunda técnica, sofisticada, os resultados dos times no campeonado ate o presente momento possuem importância.
#		 Bem como qual time enfrenta qual, influencia chance de vitória ou não.
#		 No futuro pretende-se considerar o fator 'casa' para uma terceira técnica.
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.0.5 para Windows 7, 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem alfabética dos times:

times <- c('America', 'Atletico GO', 'Atletico', 'Athletico', 'Bahia', 'Bragantino', 'Ceara', 'Chapecoense', 'Corinthians', 'Cuiaba',
	'Flamengo', 'Fluminense', 'Fortaleza', 'Gremio', 'Internacional', 'Juventude', 'Palmeiras', 'Santos', 
	'Sao Paulo', 'Sport')

# Abreviatura dos times:

abr <- c('AME', 'ACG', 'CAM', 'CAP', 'BAH', 'BGT', 'CEA', 'CHA', 'COR', 'CUI', 'FLA', 'FLU', 'FOR', 'GRE', 'INT', 'JUV', 'PAL', 'SAN', 'SAO', 'SPT')

# Pontos na rodada atual de acordo com a oderm alfabética

pontos <- c(49, 44, 81, 42, 40, 53, 49, 15, 56, 43, 70, 51, 52, 39, 48, 43, 62, 46, 45, 33)

# Vamos fazer uma tabela de acordo com a classificação:

tab <- data.frame(matrix(c(abr, pontos), nrow = 20, ncol = 2))
names(tab) <- c("equipe", "pontos")

# Para ordenar pelos pontos,
# Ainda não tem critério de desempate:

# AQUI há mais imputs importantes, repare que já está quase na ordem da tabela porém sem critério de desempate.

tab <- tab[order(tab[,2], decreasing = TRUE),]
rodadas <- c(36, 35, 36, 36, 36, 35, 36, 36, 36, 36, 36, 36, 35, 35, 35, 35, 36, 36, 35, 35)
vitorias <- c(25, 21, 19, 15, 13, 15, 14, 12, 11, 12, 11, 10, 10, 9, 10, 12, 10, 11, 8, 1)
empates <- c(6, 7, 5, 11, 14, 7, 9, 13, 16, 12, 13, 15, 14, 16, 13, 6, 10, 6, 9, 12)
derrotas <- rodadas - (vitorias + empates)

# Juntando na tabela:

tab <- cbind(tab, rodadas, vitorias, empates, derrotas)

# Reordenando com critério de vitórias como desempate:

tab <- tab[order(tab[,2], tab[,4], decreasing = TRUE),]
rownames(tab) <- 1:nrow(tab)

# Em alguns casos o critério de vitórias não é suficiente então para o Saldo de Gols, mais dois vetores de imputs:
# Atenção à ordem dos dados:

GP <- c(60, 68, 57, 39, 51, 41, 35, 39, 39, 43, 33, 28, 28, 34, 32, 39, 39, 38, 21, 27) # Gols Pro
GC <- c(27, 32, 43, 34, 42, 43, 36, 37, 37, 39, 39, 35, 35, 40, 35, 44, 49, 47, 35, 62) # Gols Contra
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

rodada_35 <- list(c('CAP', 'CUI', 'EMP'),
				  c('SPT', 'FLA', 'EMP')
				  )

rodada_36 <- list(c('CUI', 'PAL', 'EMP'),
				  c('FOR', 'JUV', 'EMP')
				  )

rodada_37 <- list(c('COR', 'GRE', 'EMP'),
				  c('CAM', 'BGT', 'EMP'),
				  c('BAH', 'FLU', 'EMP'),
				  c('INT', 'ACG', 'EMP'),
				  c('CEA', 'AME', 'EMP'),
				  c('SAO', 'JUV', 'EMP'),
				  c('CAP', 'PAL', 'EMP'),
				  c('FLA', 'SAN', 'EMP'),
				  c('CHA', 'SPT', 'EMP'),
				  c('CUI', 'FOR', 'EMP')
				  )

rodada_38 <- list(c('FLU', 'CHA', 'EMP'),
				  c('PAL', 'CEA', 'EMP'),
				  c('SAN', 'CUI', 'EMP'),
				  c('AME', 'SAO', 'EMP'),
				  c('GRE', 'CAM', 'EMP'),
				  c('FOR', 'BAH', 'EMP'),
				  c('SPT', 'CAP', 'EMP'),
				  c('BGT', 'INT', 'EMP'),
				  c('JUV', 'COR', 'EMP'),
				  c('ACG', 'FLA', 'EMP')
				  )

#####################################################
#
# Simulação Naïve (ingênua), 
# Considera as chances de vitória, derrota e empate 
# como sendo 1/3 para cada um dos times:
#
#####################################################

iter <- list(rodada_35, rodada_36, rodada_37, rodada_38)

# Número de simulações:

n <- 13000000 # Se não quiser esperar tanto coloca 13 mil simulações que tá valendo

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados_naive <- matrix(0, nrow = 20, ncol = n)

for(i in 1:n)
{
	# Uma tabela sem GP, SC e Saldo nos será mais útil no momento:

	tab2 <- tab[,-c(7,8,9,10)]

	for(j in 1:4)
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
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Brasileiro 2021", ylim = c(0,110))
text(0.7, 102, "100%")

# Para ver as chances de ficar entre os 6 primeiros colocados:
# Olhar os times que ficam entre os 6 primeiros (Libertadores e Pré-liberda, independente da Copa do Brasil)

chances_libertadores <- sort((table(resultados_naive[1:6,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores, main = "Probabilidades de Ficar entre os 6 primeiros Serie A Brasileiro 2021", ylim = c(0,110))

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados_naive[17:20,])/n)*100, decreasing = TRUE)
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie A Brasileiro 2021", ylim = c(0,110))

#####################################################
#
# Simulação Sofisticada (Mais realista), 
# Considera as chances de vitória, derrota e de acordo com o aproveitamento 
#
#####################################################

prob_35 <- list(
c(tab[tab[,1] == 'CAP',4]/tab[tab[,1] == 'CAP',3], tab[tab[,1] == 'CUI',4]/tab[tab[,1] == 'CUI',3], (tab[tab[,1] == 'CAP',5] + tab[tab[,1] == 'CUI',5])/(tab[tab[,1] == 'CAP',3] + tab[tab[,1] == 'CUI',3])),
c(tab[tab[,1] == 'SPT',4]/tab[tab[,1] == 'SPT',3], tab[tab[,1] == 'FLA',4]/tab[tab[,1] == 'FLA',3], (tab[tab[,1] == 'SPT',5] + tab[tab[,1] == 'FLA',5])/(tab[tab[,1] == 'SPT',3] + tab[tab[,1] == 'FLA',3])))

prob_36 <- list(
c(tab[tab[,1] == 'CUI',4]/tab[tab[,1] == 'CUI',3], tab[tab[,1] == 'PAL',4]/tab[tab[,1] == 'PAL',3], (tab[tab[,1] == 'CUI',5] + tab[tab[,1] == 'PAL',5])/(tab[tab[,1] == 'CUI',3] + tab[tab[,1] == 'PAL',3])),
c(tab[tab[,1] == 'FOR',4]/tab[tab[,1] == 'FOR',3], tab[tab[,1] == 'JUV',4]/tab[tab[,1] == 'JUV',3], (tab[tab[,1] == 'FOR',5] + tab[tab[,1] == 'JUV',5])/(tab[tab[,1] == 'FOR',3] + tab[tab[,1] == 'JUV',3])))

prob_37 <- list(
c(tab[tab[,1] == 'COR',4]/tab[tab[,1] == 'COR',3], tab[tab[,1] == 'GRE',4]/tab[tab[,1] == 'GRE',3], (tab[tab[,1] == 'COR',5] + tab[tab[,1] == 'GRE',5])/(tab[tab[,1] == 'COR',3] + tab[tab[,1] == 'GRE',3])),
c(tab[tab[,1] == 'CAM',4]/tab[tab[,1] == 'CAM',3], tab[tab[,1] == 'BGT',4]/tab[tab[,1] == 'BGT',3], (tab[tab[,1] == 'CAM',5] + tab[tab[,1] == 'BGT',5])/(tab[tab[,1] == 'CAM',3] + tab[tab[,1] == 'BGT',3])),
c(tab[tab[,1] == 'BAH',4]/tab[tab[,1] == 'BAH',3], tab[tab[,1] == 'FLU',4]/tab[tab[,1] == 'FLU',3], (tab[tab[,1] == 'BAH',5] + tab[tab[,1] == 'FLU',5])/(tab[tab[,1] == 'BAH',3] + tab[tab[,1] == 'FLU',3])),
c(tab[tab[,1] == 'INT',4]/tab[tab[,1] == 'INT',3], tab[tab[,1] == 'ACG',4]/tab[tab[,1] == 'ACG',3], (tab[tab[,1] == 'INT',5] + tab[tab[,1] == 'ACG',5])/(tab[tab[,1] == 'INT',3] + tab[tab[,1] == 'ACG',3])),
c(tab[tab[,1] == 'CEA',4]/tab[tab[,1] == 'CEA',3], tab[tab[,1] == 'AME',4]/tab[tab[,1] == 'AME',3], (tab[tab[,1] == 'CEA',5] + tab[tab[,1] == 'AME',5])/(tab[tab[,1] == 'CEA',3] + tab[tab[,1] == 'AME',3])),
c(tab[tab[,1] == 'SAO',4]/tab[tab[,1] == 'SAO',3], tab[tab[,1] == 'JUV',4]/tab[tab[,1] == 'JUV',3], (tab[tab[,1] == 'SAO',5] + tab[tab[,1] == 'JUV',5])/(tab[tab[,1] == 'SAO',3] + tab[tab[,1] == 'JUV',3])),
c(tab[tab[,1] == 'CAP',4]/tab[tab[,1] == 'CAP',3], tab[tab[,1] == 'PAL',4]/tab[tab[,1] == 'PAL',3], (tab[tab[,1] == 'CAP',5] + tab[tab[,1] == 'PAL',5])/(tab[tab[,1] == 'CAP',3] + tab[tab[,1] == 'PAL',3])),
c(tab[tab[,1] == 'FLA',4]/tab[tab[,1] == 'FLA',3], tab[tab[,1] == 'SAN',4]/tab[tab[,1] == 'SAN',3], (tab[tab[,1] == 'FLA',5] + tab[tab[,1] == 'SAN',5])/(tab[tab[,1] == 'FLA',3] + tab[tab[,1] == 'SAN',3])),
c(tab[tab[,1] == 'CHA',4]/tab[tab[,1] == 'CHA',3], tab[tab[,1] == 'SPT',4]/tab[tab[,1] == 'SPT',3], (tab[tab[,1] == 'CHA',5] + tab[tab[,1] == 'SPT',5])/(tab[tab[,1] == 'CHA',3] + tab[tab[,1] == 'SPT',3])),
c(tab[tab[,1] == 'CUI',4]/tab[tab[,1] == 'CUI',3], tab[tab[,1] == 'FOR',4]/tab[tab[,1] == 'FOR',3], (tab[tab[,1] == 'CUI',5] + tab[tab[,1] == 'FOR',5])/(tab[tab[,1] == 'CUI',3] + tab[tab[,1] == 'FOR',3])))

prob_38 <- list(
c(tab[tab[,1] == 'FLU',4]/tab[tab[,1] == 'FLU',3], tab[tab[,1] == 'CHA',4]/tab[tab[,1] == 'CHA',3], (tab[tab[,1] == 'FLU',5] + tab[tab[,1] == 'CHA',5])/(tab[tab[,1] == 'FLU',3] + tab[tab[,1] == 'CHA',3])),
c(tab[tab[,1] == 'PAL',4]/tab[tab[,1] == 'PAL',3], tab[tab[,1] == 'CEA',4]/tab[tab[,1] == 'CEA',3], (tab[tab[,1] == 'PAL',5] + tab[tab[,1] == 'CEA',5])/(tab[tab[,1] == 'PAL',3] + tab[tab[,1] == 'CEA',3])),
c(tab[tab[,1] == 'SAN',4]/tab[tab[,1] == 'SAN',3], tab[tab[,1] == 'CUI',4]/tab[tab[,1] == 'CUI',3], (tab[tab[,1] == 'SAN',5] + tab[tab[,1] == 'CUI',5])/(tab[tab[,1] == 'SAN',3] + tab[tab[,1] == 'CUI',3])),
c(tab[tab[,1] == 'AME',4]/tab[tab[,1] == 'AME',3], tab[tab[,1] == 'SAO',4]/tab[tab[,1] == 'SAO',3], (tab[tab[,1] == 'AME',5] + tab[tab[,1] == 'SAO',5])/(tab[tab[,1] == 'AME',3] + tab[tab[,1] == 'SAO',3])),
c(tab[tab[,1] == 'GRE',4]/tab[tab[,1] == 'GRE',3], tab[tab[,1] == 'CAM',4]/tab[tab[,1] == 'CAM',3], (tab[tab[,1] == 'GRE',5] + tab[tab[,1] == 'CAM',5])/(tab[tab[,1] == 'GRE',3] + tab[tab[,1] == 'CAM',3])),
c(tab[tab[,1] == 'FOR',4]/tab[tab[,1] == 'FOR',3], tab[tab[,1] == 'BAH',4]/tab[tab[,1] == 'BAH',3], (tab[tab[,1] == 'FOR',5] + tab[tab[,1] == 'BAH',5])/(tab[tab[,1] == 'FOR',3] + tab[tab[,1] == 'BAH',3])),
c(tab[tab[,1] == 'SPT',4]/tab[tab[,1] == 'SPT',3], tab[tab[,1] == 'CAP',4]/tab[tab[,1] == 'CAP',3], (tab[tab[,1] == 'SPT',5] + tab[tab[,1] == 'CAP',5])/(tab[tab[,1] == 'SPT',3] + tab[tab[,1] == 'CAP',3])),
c(tab[tab[,1] == 'BGT',4]/tab[tab[,1] == 'BGT',3], tab[tab[,1] == 'INT',4]/tab[tab[,1] == 'INT',3], (tab[tab[,1] == 'BGT',5] + tab[tab[,1] == 'INT',5])/(tab[tab[,1] == 'BGT',3] + tab[tab[,1] == 'INT',3])),
c(tab[tab[,1] == 'JUV',4]/tab[tab[,1] == 'JUV',3], tab[tab[,1] == 'COR',4]/tab[tab[,1] == 'COR',3], (tab[tab[,1] == 'JUV',5] + tab[tab[,1] == 'COR',5])/(tab[tab[,1] == 'JUV',3] + tab[tab[,1] == 'COR',3])),
c(tab[tab[,1] == 'ACG',4]/tab[tab[,1] == 'ACG',3], tab[tab[,1] == 'FLA',4]/tab[tab[,1] == 'FLA',3], (tab[tab[,1] == 'ACG',5] + tab[tab[,1] == 'FLA',5])/(tab[tab[,1] == 'ACG',3] + tab[tab[,1] == 'FLA',3])))

# Juntando as probabilidades:

probs <- list(prob_35, prob_36, prob_37, prob_38)

Sprobs <- list()

for(i in 1:4){
	Sprobs[[i]] <- lapply(probs[[i]], sum)
}

probs_ls <- probs

aux1 <- c(rep(1,2), rep(2,2), rep(3,10), rep(4,10))
aux2 <- c(1, 2, 1, 2, rep(1:10,2))

for(i in 1:24){
	probs_ls[[aux1[i]]][[aux2[i]]] <- unlist(probs[[aux1[i]]][[aux2[i]]])/unlist(Sprobs[[aux1[i]]][[aux2[i]]])
}

resultados_sofisticado <- matrix(0, nrow = 20, ncol = n)

# Deixei o Loop mais eficiente:

result1 <- NULL

for(i in 1:n)
{
	# Uma tabela sem GP, SC e Saldo nos será mais útil no momento:

	tab2 <- tab[,-c(7,8,9,10)]

	for(j in 1:24)
	{
		result1 <- sample(iter[[aux1[j]]][[aux2[j]]], size = 3, prob = probs_ls[[aux1[j]]][[aux2[j]]])
		if(result1[1] == "EMP")
		{
			tab2[tab2[,1] == result1[2], 2:6] <- tab2[tab2[,1] == result1[2], 2:6] + c(1,1,0,1,0)
			tab2[tab2[,1] == result1[3], 2:6] <- tab2[tab2[,1] == result1[3], 2:6] + c(1,1,0,1,0)
		}else
		{
			ganhou <- result1[1]
			tab2[tab2[,1] == ganhou, 2:6] <- tab2[tab2[,1] == ganhou, 2:6] + c(3, 1, 1, 0, 0)
			perdeu <- which(result1 == "EMP")
			perdeu <- result1[-perdeu]
			perdeu <- perdeu[2]
			tab2[tab2[,1] == perdeu, 2:6] <- tab2[tab2[,1] == perdeu, 2:6] + c(0, 1, 0, 0, 1)
		}

	}	
tab3 <- tab2[order(tab2[,2], tab2[,4], decreasing = TRUE),]
resultados_sofisticado[,i]  <- tab3[,1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao2 <- sort((table(resultados_sofisticado[1,])/n)*100, decreasing = TRUE)
barplot(chances_campeao2, main = "Probabilidades de Campeão Serie A Brasileiro 2021, Modelo Melhor", ylim = c(0,110), cex.main = 0.95)
text(0.7, 103, "100%")

# Para ver as chances de ficar entre os 6 primeiros colocados:
# Olhar os times que ficam entre os 6 primeiros (Libertadores e Pré-liberda, independente da Copa do Brasil)

chances_libertadores2 <- sort((table(resultados_sofisticado[1:4,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores2, main = "Probabilidades de Ficar entre os 4 primeiros Serie A Brasileiro 2021", ylim = c(0,110))

chances_libertadores3 <- sort((table(resultados_sofisticado[1:6,])/n)*100, decreasing = TRUE)
barplot(chances_libertadores3, main = "Probabilidades de Ficar entre os 6 primeiros Serie A Brasileiro 2021", ylim = c(0,110))

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento2 <- sort((table(resultados_sofisticado[17:20,])/n)*100, decreasing = TRUE)
barplot(chances_rebaixamento2[1:15], main = "Probabilidades de Ficar entre os 4 últimos Serie A Brasileiro 2021", ylim = c(0,110))

## FIM DA ROTINA!