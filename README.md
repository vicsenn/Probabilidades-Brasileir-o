############################################################################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES DO BRASILEIRA - SERIE A
# Dois Scripts para Calcular as Probabilidades de Vitória dos times no Brasileirão.
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
#   Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (Una-MATE), 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################################################################
