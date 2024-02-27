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
#                k = uma constante para ajudar a calcular probabilidades de empate.
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (UnA-MATE), 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# DADOS DE ENTRADA IMPORTANTES:

# Ordem de classificação na atual rodada:

serieA <- c('Vigg-Sama', 'Alanpsc', 'Careca_De_Capa', 'jardindemonet', 'ronyfreitas', 'olivmaicon33', 'BispodaAvareza', 'DouglasRoots', 
	'denis_duarte', 'Hyoukami', 'FelipeViana', 'luancarlison', 'Sovinski', 'Dragon-dorf', 'AkiYuuto', 'Alexcapas')

# Pontos na rodada atual de acordo com a oderm alfabética

pontosA <- c(5, 4, 3, 3, 3, 3, 2.5, 2.5, 2.5, 2, 2, 2, 2, 1.5, 1, 0)
ratingA <- c(2144, 1911, 1906, 1945, 1989, 1983, 1887, 1999, 1842, 2045, 1801, 1877, 1864, 1850, 1837, 1913)
A_TB1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
A_TB2 <- c(12.5, 12.5, 14.5, 12, 10.5, 10, 14, 13, 10, 12, 11, 10.5, 10, 11, 12, 8)
A_TB3 <- c(15, 13.5, 16.5, 14, 11.5, 12, 16.5, 14, 11, 14, 11, 10.5, 10, 11, 12, 9)

# Vamos fazer uma tabela de acordo com a classificação:

tabA <- data.frame(matrix(c(serieA, ratingA, pontosA, A_TB1, A_TB2, A_TB3), nrow = 16, ncol = 6))
names(tabA) <- c("jogador", "rating", "pontos", "TB1", "TB2", "TB3")

# Reordenando com os critérios de desempate:

tabA <- tabA[order(tabA[,3], tabA[,4], tabA[,5], tabA[,6], decreasing = TRUE),]
rownames(tabA) <- 1:nrow(tabA)

# Corrigindo o fato de que o valor dos pontos não ficaram como números:

tabA[,2] <- as.numeric(tabA[,2])
tabA[,3] <- as.numeric(tabA[,3])
tabA[,4] <- as.numeric(tabA[,4])
tabA[,5] <- as.numeric(tabA[,5])
tabA[,6] <- as.numeric(tabA[,6])

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam:
#
###################################################

# Rodadas que ainda faltam, primeiros nomes de brancas,
# Segundo nome de pretas
# Terceiro é o empate.

rodada_6A <- list(c('ronyfreitas', 'Vigg-Sama', 'EMP'),
				  c('olivmaicon33', 'Alanpsc', 'EMP'),
				  c('Careca_De_Capa', 'jardindemonet', 'EMP'),
				  c('BispodaAvareza', 'denis_duarte', 'EMP'),
				  c('FelipeViana', 'DouglasRoots', 'EMP'),
				  c('luancarlison', 'Sovinski', 'EMP'),
				  c('Alexcapas', 'Hyoukami', 'EMP'),
				  c('AkiYuuto', 'Dragon-dorf', 'EMP')
				  )

#####################################################
#
# Calculo dos Qs de cada jogador
#
#####################################################

QA <- 10^(tabA[,2]/400)

tabA[,7] <- QA
names(tabA)[7] <- "Q_valor"

# Constante para a série A:

kA <- 60000

# Lista para obter as probabilidades:

prob_6A <- list(c(tabA[5,7]/(tabA[5,7] + kA + tabA[1,7]), tabA[1,7]/(tabA[5,7] + kA + tabA[1,7]), kA/(tabA[5,7] + kA + tabA[1,7])),
			c(tabA[6,7]/(tabA[6,7] + kA + tabA[2,7]), tabA[2,7]/(tabA[6,7] + kA + tabA[2,7]), kA/(tabA[6,7] + kA + tabA[2,7])),
			c(tabA[3,7]/(tabA[3,7] + kA + tabA[4,7]), tabA[4,7]/(tabA[3,7] + kA + tabA[4,7]), kA/(tabA[3,7] + kA + tabA[4,7])),
			c(tabA[7,7]/(tabA[7,7] + kA + tabA[9,7]), tabA[9,7]/(tabA[7,7] + kA + tabA[9,7]), kA/(tabA[7,7] + kA + tabA[9,7])),
			c(tabA[11,7]/(tabA[11,7] + kA + tabA[8,7]), tabA[8,7]/(tabA[11,7] + kA + tabA[8,7]), kA/(tabA[11,7] + kA + tabA[8,7])),
			c(tabA[12,7]/(tabA[12,7] + kA + tabA[13,7]), tabA[13,7]/(tabA[12,7] + kA + tabA[13,7]), kA/(tabA[12,7] + kA + tabA[13,7])),
			c(tabA[16,7]/(tabA[16,7] + kA + tabA[10,7]), tabA[10,7]/(tabA[16,7] + kA + tabA[10,7]), kA/(tabA[16,7] + kA + tabA[10,7])),
			c(tabA[15,7]/(tabA[15,7] + kA + tabA[14,7]), tabA[14,7]/(tabA[15,7] + kA + tabA[14,7]), kA/(tabA[15,7] + kA + tabA[14,7]))
		)

#####################################################
#
# Simulação 
# Considera as chances de vitória, derrota e empate 
# depende da relação de rating CXP
#
#####################################################

iter <- list(rodada_6A)
probs <- list(prob_6A)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados <- matrix(0, nrow = 16, ncol = n)

result <- matrix(0, nrow = 8, ncol = 3)

for(i in 1:n)
{
	# Considera os desempates tal como estavam antes da atual rodada
	tabA_aux <- tabA

		for(k in 1:8){
			result[k,] <- sample(unlist(iter[[1]][k]), size = 3, prob = unlist(probs[[1]][k]))
		}
		for(k in 1:8){
			if(result[k,1] == "EMP"){
				tabA_aux[tabA_aux[,1] == result[k,2], 3:7] <- tabA_aux[tabA_aux[,1] == result[k,2], 3:7] + c(0.5, 0, 0, 0, 0)
				tabA_aux[tabA_aux[,1] == result[k,3], 3:7] <- tabA_aux[tabA_aux[,1] == result[k,3], 3:7] + c(0.5, 0, 0, 0, 0)
			}else{
				ganhou <- result[k,1]
				tabA_aux[tabA_aux[,1] == ganhou, 3:7] <- tabA_aux[tabA_aux[,1] == ganhou, 3:7] + c(1, 0, 0, 0, 0)
			}
		}
resultados[,i] <- tabA_aux[order(tabA_aux[,3], tabA_aux[,4], tabA_aux[,5], tabA_aux[,6], decreasing = TRUE),1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- sort((table(resultados[1,])/n)*100, decreasing = TRUE)
png("Campeão SerieA_CXP LigaKaspa_3Ed.png")
barplot(chances_campeao, main = "Probabilidades de Campeão Serie A Liga Kasparov 3A Ed CXP", ylim = c(0,110))
text(0.7, 103, "100%")
dev.off()

# Para ver as chances de ficar entre os 4 primeiros colocados:

chances_G4 <- sort((table(resultados[1:4,])/n)*100, decreasing = TRUE)
png("SerieA_CXP LigaKaspa_3Ed.png")
barplot(chances_G4, main = "Probabilidades de Ficar entre os 4 Serie A Liga Kasparov 3A Ed CXP", ylim = c(0,110))
text(0.7, 103, "100%")
text(1.9, 103, "100%")
text(3.1, 62, "59%")
text(4.35, 45, "42%")
text(5.5, 42, "39%")
text(6.7, 30, "27%")
text(7.9, 19, "15%")
text(9.1, 18, "15%")
text(10.3, 4, "1%")
dev.off()

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados[13:16,])/n)*100, decreasing = TRUE)
png("SerieA_CXP LigaKaspa_3Ed_Z4.png")
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie A Liga Kasparov 3A Ed CXP", ylim = c(0,110))
text(0.7, 103, "100%")
text(1.9, 94, "91%")
text(3.1, 78, "75%")
text(4.35, 54, "51%")
text(5.5, 41, "38%")
text(6.7, 36, "33%")
text(7.9, 12, "9%")
text(9.1, 6, "3%")
dev.off()

#################################################
#
#
# SÉRIE B 
#
#
#################################################

serieB <- c('Luor4ng', 'Lucas_Correa22', 'franciscohailton', 'WesleyMelo10', 'dama_de_ouro', 'tdmsilvino', 'jpsudarte', 'CapivaraSelvagem', 
	'Alekhine-94', 'Dutracorredor', 'Nego_drama45', 'Kimimaryy', 'gsmorgan', 'danielvieirasousa', 'americoata1981', 'JehanPedroso')

# Pontos na rodada atual de acordo com a oderm alfabética

pontosB <- c(4, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1.5, 1.5, 0)
ratingB <- c(1895, 1783, 1792, 1758, 1837, 1808, 1789, 1711, 50, 1835, 1798, 1797, 1785, 1817, 1709, 1832)
B_TB1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0)
B_TB2 <- c(14, 12.5, 12, 13, 12.5, 12, 12, 11, 13, 11, 9.5, 8.5, 6.5, 9.5, 7, 10)
B_TB3 <- c(16.5, 14, 14, 14.5, 14, 13.5, 13.5, 12, 14.5, 12.5, 11, 9.5, 7, 11, 8.5, 10)

# Vamos fazer uma tabela de acordo com a classificação:

tabB <- data.frame(matrix(c(serieB, ratingB, pontosB, B_TB1, B_TB2, B_TB3), nrow = 16, ncol = 6))
names(tabB) <- c("jogador", "rating", "pontos", "TB1", "TB2", "TB3")

# Reordenando com os critérios de desempate:

tabB <- tabB[order(tabB[,3], tabB[,4], tabB[,5], tabB[,6], decreasing = TRUE),]
rownames(tabB) <- 1:nrow(tabB)

# Corrigindo o fato de que o valor dos pontos não ficaram como números:

tabB[,2] <- as.numeric(tabB[,2])
tabB[,3] <- as.numeric(tabB[,3])
tabB[,4] <- as.numeric(tabB[,4])
tabB[,5] <- as.numeric(tabB[,5])
tabB[,6] <- as.numeric(tabB[,6])

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam:
#
###################################################

# Rodadas que ainda faltam, primeiros nomes de brancas,
# Segundo nome de pretas
# Terceiro é o empate.

rodada_6B <- list(c('Lucas_Correa22', 'franciscohailton', 'EMP'),
				  c('jpsudarte', 'Luor4ng', 'EMP'),
				  c('dama_de_ouro', 'tdmsilvino', 'EMP'),
				  c('WesleyMelo10', 'gsmorgan', 'EMP'),
				  c('americoata1981', 'CapivaraSelvagem', 'EMP'),
				  c('Nego_drama45', 'Dutracorredor', 'EMP'),
				  c('Kimimaryy', 'danielvieirasousa', 'EMP'),
				  c('Alekhine-94', 'JehanPedroso', 'EMP')
				  )

#####################################################
#
# Calculo dos Qs de cada jogador
#
#####################################################

QB <- 10^(tabB[,2]/400)

tabB[,7] <- QB
names(tabB)[7] <- "Q_valor"

# Constante para a série B:

kB <- 20000

# Lista para obter as probabilidades:

prob_6B <- list(c(tabB[2,7]/(tabB[2,7] + kB + tabB[3,7]), tabB[3,7]/(tabB[2,7] + kB + tabB[3,7]), kB/(tabB[2,7] + kB + tabB[3,7])),
			c(tabB[7,7]/(tabB[7,7] + kB + tabB[1,7]), tabB[1,7]/(tabB[7,7] + kB + tabB[1,7]), kB/(tabB[7,7] + kB + tabB[1,7])),
			c(tabB[5,7]/(tabB[5,7] + kB + tabB[6,7]), tabB[6,7]/(tabB[5,7] + kB + tabB[6,7]), kB/(tabB[5,7] + kB + tabB[6,7])),
			c(tabB[4,7]/(tabB[4,7] + kB + tabB[11,7]), tabB[11,7]/(tabB[4,7] + kB + tabB[11,7]), kB/(tabB[4,7] + kB + tabB[11,7])),
			c(tabB[15,7]/(tabB[15,7] + kB + tabB[8,7]), tabB[8,7]/(tabB[15,7] + kB + tabB[8,7]), kB/(tabB[15,7] + kB + tabB[8,7])),
			c(tabB[9,7]/(tabB[9,7] + kB + tabB[13,7]), tabB[13,7]/(tabB[9,7] + kB + tabB[13,7]), kB/(tabB[9,7] + kB + tabB[13,7])),
			c(tabB[10,7]/(tabB[10,7] + kB + tabB[14,7]), tabB[14,7]/(tabB[10,7] + kB + tabB[14,7]), kB/(tabB[10,7] + kB + tabB[14,7])),
			c(tabB[12,7]/(tabB[12,7] + kB + tabB[16,7]), tabB[16,7]/(tabB[12,7] + kB + tabB[16,7]), kB/(tabB[12,7] + kB + tabB[16,7]))
		)

#####################################################
#
# Simulação 
# Considera as chances de vitória, derrota e empate 
# depende da relação de rating CXP
#
#####################################################

iter <- list(rodada_6B)
probs <- list(prob_6B)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados <- matrix(0, nrow = 16, ncol = n)

result <- matrix(0, nrow = 8, ncol = 3)

for(i in 1:n)
{
	# Considera os desempates tal como estavam antes da atual rodada
	tabB_aux <- tabB

		for(k in 1:8){
			result[k,] <- sample(unlist(iter[[1]][k]), size = 3, prob = unlist(probs[[1]][k]))
		}
		for(k in 1:8){
			if(result[k,1] == "EMP"){
				tabB_aux[tabB_aux[,1] == result[k,2], 3:7] <- tabB_aux[tabB_aux[,1] == result[k,2], 3:7] + c(0.5, 0, 0, 0, 0)
				tabB_aux[tabB_aux[,1] == result[k,3], 3:7] <- tabB_aux[tabB_aux[,1] == result[k,3], 3:7] + c(0.5, 0, 0, 0, 0)
			}else{
				ganhou <- result[k,1]
				tabB_aux[tabB_aux[,1] == ganhou, 3:7] <- tabB_aux[tabB_aux[,1] == ganhou, 3:7] + c(1, 0, 0, 0, 0)
			}
		}
resultados[,i] <- tabB_aux[order(tabB_aux[,3], tabB_aux[,4], tabB_aux[,5], tabB_aux[,6], decreasing = TRUE),1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- sort((table(resultados[1,])/n)*100, decreasing = TRUE)
png("Campeão SerieB_CXP LigaKaspa_3Ed.png")
barplot(chances_campeao, main = "Probabilidades de Campeão Serie B Liga Kasparov 3A Ed CXP", ylim = c(0,110), col = "lightblue")
text(0.7, 60, "57%")
text(1.9, 28, "25%")
text(3.1, 22, "18%")
dev.off()

# Para ver as chances de ficar entre os 4 primeiros colocados:

chances_G4 <- sort((table(resultados[1:4,])/n)*100, decreasing = TRUE)
png("SerieB_CXP LigaKaspa_3Ed.png")
barplot(chances_G4, main = "Probabilidades de Ficar entre os 4 Serie B Liga Kasparov 3A Ed CXP", ylim = c(0,110), col =  "lightblue")
text(0.7, 103, "100%")
text(1.9, 103, "100%")
text(3.1, 98, "95%")
text(4.35, 41, "37%")
text(5.5, 39, "36%")
text(6.7, 27, "24%")
text(7.9, 7, "4%")
text(9.1, 7, "4%")
dev.off()

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados[13:16,])/n)*100, decreasing = TRUE)
png("SerieB_CXP LigaKaspa_3Ed_Z4.png")
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie B Liga Kasparov 3A Ed CXP", ylim = c(0,110),
col = "lightblue")
text(0.7, 103, "100%")
text(1.9, 52, "49%")
text(3.1, 51, "48%")
text(4.35, 48, "45%")
text(5.5, 47, "44%")
text(6.7, 46, "43%")
text(7.9, 42, "39%")
text(9.1, 36, "33%")
dev.off()

#################################################
#
#
# SÉRIE C 
#
#
#################################################

serieC <- c('livia200215', 'AnginhoChess', 'zeckqi', 'Villander', '7GabrielRibeiro', 'CRVGsalguerin', 'luanalindadasilva', 'MCoura', 
	'Jojowdapartebaixa', 'danilopmj', 'emanuelsena', 'dinhoptu', 'Eturin16', 'camidere', 'AntonioVermeio', 
	'PatyE3', 'HendricksonRogers', 'Joanderdiego', 'Alexs157', 'Daniel_mate7')

# Pontos na rodada atual de acordo com a oderm alfabética

pontosC <- c(5, 3.5, 3.5, 3.5, 3, 3, 3, 3, 3, 2.5, 2, 2, 2, 1.5, 1, 1, 1, 1, 0, 0)
ratingC <- c(1811, 1822, 1800, 1725, 1796, 1787, 1741, 1800, 1800, 1756, 1716, 1814, 1682, 1800, 1740, 1645, 1800, 1812, 10, 1800)
C_TB1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C_TB2 <- c(12, 14, 10.5, 10, 13, 12, 12, 9, 7, 11.5, 13.5, 12.5, 12, 9, 13, 11, 10.5, 10, 11.5, 9.5)
C_TB3 <- c(13, 15, 12, 11, 14.5, 13, 13, 10, 8, 12.5, 15, 14.5, 13.5, 10, 14.5, 11.5, 12, 10.5, 12.5, 10.5)

# Vamos fazer uma tabela de acordo com a classificação:

tabC <- data.frame(matrix(c(serieC, ratingC, pontosC, C_TB1, C_TB2, C_TB3), nrow = 20, ncol = 6))
names(tabC) <- c("jogador", "rating", "pontos", "TB1", "TB2", "TB3")

# Reordenando com os critérios de desempate:

tabC <- tabC[order(tabC[,3], tabC[,4], tabC[,5], tabC[,6], decreasing = TRUE),]
rownames(tabC) <- 1:nrow(tabC)

# Corrigindo o fato de que o valor dos pontos não ficaram como números:

tabC[,2] <- as.numeric(tabC[,2])
tabC[,3] <- as.numeric(tabC[,3])
tabC[,4] <- as.numeric(tabC[,4])
tabC[,5] <- as.numeric(tabC[,5])
tabC[,6] <- as.numeric(tabC[,6])

###################################################
#
# Vamos fazer listas das rodadas que ainda faltam:
#
###################################################

# Rodadas que ainda faltam, primeiros nomes de brancas,
# Segundo nome de pretas
# Terceiro é o empate.

rodada_6C <- list(c('livia200215', 'zeckqi', 'EMP'),
				  c('AnginhoChess', 'Villander', 'EMP'),
				  c('luanalindadasilva', 'Jojowdapartebaixa', 'EMP'),
				  c('7GabrielRibeiro', 'CRVGsalguerin', 'EMP'),
				  c('MCoura', 'danilopmj', 'EMP'),
				  c('Eturin16', 'dinhoptu', 'EMP'),
				  c('camidere', 'emanuelsena', 'EMP'),
				  c('PatyE3', 'Joanderdiego', 'EMP'),
				  c('HendricksonRogers', 'AntonioVermeio', 'EMP'),
				  c('Alexs157', 'Daniel_mate7', 'EMP')
				  )

#####################################################
#
# Calculo dos Qs de cada jogador
#
#####################################################

QC <- 10^(tabC[,2]/400)

tabC[,7] <- QC
names(tabC)[7] <- "Q_valor"

# Constante para a série C:

kC <- 30000

# Lista para obter as probabilidades:

prob_6C <- list(c(tabC[1,7]/(tabC[1,7] + kC + tabC[3,7]), tabC[3,7]/(tabC[1,7] + kC + tabC[3,7]), kC/(tabC[1,7] + kC + tabC[3,7])),
			c(tabC[2,7]/(tabC[2,7] + kC + tabC[4,7]), tabC[4,7]/(tabC[2,7] + kC + tabC[4,7]), kC/(tabC[2,7] + kC + tabC[4,7])),
			c(tabC[9,7]/(tabC[9,7] + kC + tabC[6,7]), tabC[6,7]/(tabC[9,7] + kC + tabC[6,7]), kC/(tabC[9,7] + kC + tabC[6,7])),
			c(tabC[7,7]/(tabC[7,7] + kC + tabC[8,7]), tabC[8,7]/(tabC[7,7] + kC + tabC[8,7]), kC/(tabC[7,7] + kC + tabC[8,7])),
			c(tabC[5,7]/(tabC[5,7] + kC + tabC[10,7]), tabC[10,7]/(tabC[5,7] + kC + tabC[10,7]), kC/(tabC[5,7] + kC + tabC[10,7])),
			c(tabC[13,7]/(tabC[13,7] + kC + tabC[12,7]), tabC[12,7]/(tabC[13,7] + kC + tabC[12,7]), kC/(tabC[13,7] + kC + tabC[12,7])),
			c(tabC[14,7]/(tabC[14,7] + kC + tabC[11,7]), tabC[11,7]/(tabC[14,7] + kC + tabC[11,7]), kC/(tabC[14,7] + kC + tabC[11,7])),
			c(tabC[16,7]/(tabC[16,7] + kC + tabC[18,7]), tabC[18,7]/(tabC[16,7] + kC + tabC[18,7]), kC/(tabC[16,7] + kC + tabC[18,7])),
			c(tabC[17,7]/(tabC[17,7] + kC + tabC[15,7]), tabC[15,7]/(tabC[17,7] + kC + tabC[15,7]), kC/(tabC[17,7] + kC + tabC[15,7])),
			c(tabC[20,7]/(tabC[20,7] + kC + tabC[19,7]), tabC[19,7]/(tabC[20,7] + kC + tabC[19,7]), kC/(tabC[20,7] + kC + tabC[19,7]))
		)

#####################################################
#
# Simulação 
# Considera as chances de vitória, derrota e empate 
# depende da relação de rating CXP
#
#####################################################

iter <- list(rodada_6C)
probs <- list(prob_6C)

# Número de simulações:

n <- 13000

# Note que quanto mais rodadas precisarem ser simuladas o ideal é que esse número 'n' aumente de acordo.

resultados <- matrix(0, nrow = 20, ncol = n)

result <- matrix(0, nrow = 10, ncol = 3)

for(i in 1:n)
{
	# Considera os desempates tal como estavam antes da atual rodada
	tabC_aux <- tabC

		for(k in 1:10){
			result[k,] <- sample(unlist(iter[[1]][k]), size = 3, prob = unlist(probs[[1]][k]))
		}
		for(k in 1:10){
			if(result[k,1] == "EMP"){
				tabC_aux[tabC_aux[,1] == result[k,2], 3:7] <- tabC_aux[tabC_aux[,1] == result[k,2], 3:7] + c(0.5, 0, 0, 0, 0)
				tabC_aux[tabC_aux[,1] == result[k,3], 3:7] <- tabC_aux[tabC_aux[,1] == result[k,3], 3:7] + c(0.5, 0, 0, 0, 0)
			}else{
				ganhou <- result[k,1]
				tabC_aux[tabC_aux[,1] == ganhou, 3:7] <- tabC_aux[tabC_aux[,1] == ganhou, 3:7] + c(1, 0, 0, 0, 0)
			}
		}
resultados[,i] <- tabC_aux[order(tabC_aux[,3], tabC_aux[,4], tabC_aux[,5], tabC_aux[,6], decreasing = TRUE),1]
}

# Para obter as chances de campeão é só olhar quantas vezes cada time aparece na primeira linha:

chances_campeao <- sort((table(resultados[1,])/n)*100, decreasing = TRUE)
png("Campeão SerieC_CXP LigaKaspa_3Ed.png")
barplot(chances_campeao, main = "Probabilidades de Campeão Serie C Liga Kasparov 3A Ed CXP", ylim = c(0,110), col = "pink")
text(0.7, 103, "100%")
dev.off()

# Para ver as chances de ficar entre os 4 primeiros colocados:

chances_G4 <- sort((table(resultados[1:4,])/n)*100, decreasing = TRUE)
png("SerieC_CXP LigaKaspa_3Ed.png")
barplot(chances_G4, main = "Probabilidades de Ficar entre os 4 Serie C Liga Kasparov 3A Ed CXP", ylim = c(0,110), col =  "pink")
text(0.7, 103, "100%")
text(1.9, 84, "81%")
text(3.1, 62, "59%")
text(4.35, 45, "42%")
text(5.5, 43, "40%")
text(6.7, 35, "32%")
text(7.9, 25, "22%")
text(9.1, 15, "12%")
text(10.3, 14, "11%")
dev.off()

# Para o rebaixamento:
# Verificar entre os quatro últimos (Z4):

chances_rebaixamento <- sort((table(resultados[17:20,])/n)*100, decreasing = TRUE)
png("SerieC_CXP LigaKaspa_3Ed_Z4.png")
barplot(chances_rebaixamento, main = "Probabilidades de Ficar entre os 4 últimos Serie C Liga Kasparov 3A Ed CXP", ylim = c(0,110),
col = "pink")
text(0.7, 103, "100%")
text(1.9, 103, "100%")
text(3.1, 57, "54%")
text(4.35, 54, "51%")
text(5.5, 47, "44%")
text(6.7, 40, "37%")
text(7.9, 17, "14%")
dev.off()

## FIM DA ROTINA!
