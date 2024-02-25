###################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES DA LIGA GRAND PRIX DO CLUBE DE XADREZ DE PEÇANHA - SERIE A, B e C
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
#
# DATA da última versão: 24/02/2024
# CPU 1:      ASUS; processor intel Xeon CPU X3430 @ 2.40GHz x4, RAM: 16.0 GB
#  OS 1:      Linux Mint 20.3 Una 64-bit
#
# Versão R: 4.3.1(x64) 
#
# OBS.1: Rotina para gera as chances dos jogadores de acordo com o ELO-Chess em rápidas de cada um,
#                E_b = Esperança de vitória do jogador de brancas
#                E_p = Esperança de vitória do jogador de pretas
#                Q_b = Cálculo da força relativa, baseada no rating, para jogador das brancas Q_b = 10^(R_b/400) 
#                Q_p = Cálculo da força relativa, baseada no rating, para jogador das pretas Q_p = 10^(R_p/400)
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (UnA-MATE), 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem alfabética dos times:

serieA <- c('Vigg-Sama', 'Hyoukami', 'DouglasRoots', 'ronyfreitas', 'olivmaicon33', 'jardindemonet', 'Alexcapas', 'Alanpsc', 'Careca_De_Capa', 'BispodaAvareza',
	'luancarlison', 'Sovinski', 'Dragon-dorf', 'denis_duarte', 'AkiYuuto', 'FelipeViana')

# Pontos na rodada atual de acordo com a oderm alfabética

pontosA <- c(5, 2, 2.5, 3, 3, 3, 0, 1, 4, 3, 2.5, 2, 2, 1.5, 2.5, 1, 2)
A_TB1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
A_TB2 <- c(12.5, 12.5, 14.5, 12, 10.5, 10, 14, 13, 10, 12, 11, 10.5, 10, 11, 12, 8)
A_TB3 <- c(15, 13.5, 16.5, 14, 11.5, 12, 16.5, 14, 11, 14, 11, 10.5, 10, 11, 12, 9)

# Vamos fazer uma tabela de acordo com a classificação:

tabA <- data.frame(matrix(c(seriaA, pontosA, A_TB1, A_TB2, A_TB3), nrow = 16, ncol = 2))
names(tabA) <- c("jogador", "pontos", "TB1", "TB2", "TB3")

# Reordenando com os critérios de desempate:

tabA <- tabA[order(tabA[,2], tabA[,4], decreasing = TRUE),]
rownames(tabA) <- 1:nrow(tabA)

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
	desempate_1 <- rbinom(n = 20, size = 1, prob = 0.5)
	desempate_2 <- rbinom(n = 20, size = 1, prob = 0.5)
	tab2[,7:8] <- cbind(desempate_1, desempate_2)

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
tab3 <- tab2[order(tab2[,2], tab2[,4], tab2[,7], tab2[,8], decreasing = TRUE),]
resultados_naive[,i]  <- tab3[,1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- (table(resultados_naive[1,])/n)*100
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Brasileiro 2023", ylim = c(0,110))
text(0.7, 10, "6.5%")
text(1.9, 27, "24%")
text(3.1, 9, "6%")
text(4.35, 5.5, "2.5%")
text(5.5, 64, "61%")

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
text(4.35, 57, "54%")
text(5.5, 32, "29%")
text(6.7, 18, "15%")
text(7.9, 5, "2%")
text(9.1, 3, "~0%")

## FIM DA ROTINA!
