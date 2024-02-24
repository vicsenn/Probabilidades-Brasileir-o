############################################################################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES EM CAMPEONATOS DE XADREZ FORMATO SUÍÇO
# Scripts para Calcular as Probabilidades de Vitória dos jogadores 
#
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 03/12/2023
# CPU:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
#  OS:      Windows 10 Home Single Language
# Versão R: 4.3.1(x64) 
#
# OBS.1: 
#    Rotina para gera as chances dos times de acordo com duas técnicas diferentes. 
# 	 Na primeira técnica, naïve, os pareamentos (qual time enfrenta qual) não importam.
#		 
#    Com a segunda técnica, sofisticada, os resultados dos times no campeonado ate o presente momento possuem importância.
#		 Bem como qual time enfrenta qual, influencia chance de vitória ou não.
#		 No futuro pretende-se considerar o fator 'casa' para uma terceira técnica.
#		 Para maiores detalhes ou dúvidas entre em contato com o autor que aparece no e-mail acima.
# 
# OBS.2: 
#   Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (Una-MATE), 64 Bits, para rodá-la em outros SOs alguns detalhes devem ser observados.
#
# OBS.3:
# Criei uma atualização para considerar melhor o desempate de saldo de gols. Não é, na verdade, o saldo de gols, mas simplesmente uma série binomial (0s e 1s) com # n = 20 (os 20 times do brasileirão). Acho que já deve ser bom o suficiente, em especial quando os times estão com saldos similares. No futuro deve ser possível # desenvolver um critério de desempate que leve em conta o saldo existente. Grato ao meu primo, Henrique Maia Menezes, por me apontar essa questão de critérios de # desempate, bastante relevante para o campeonato de 2023. Considere então que os códigos de 2021 estão sem esse desempate para além do número de vitórias e o     # novo código de 2023 já possui o critério de desempate aleatório mais simples. 
#
###################################################################################################################################################################
