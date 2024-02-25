############################################################################################################################################################################
#
# ROTINA PARA GERAR PROBABILIDADES DOS CAMPEÕES EM CAMPEONATOS DE XADREZ FORMATO SUÍÇO
# Scripts para Calcular as Probabilidades de Vitória dos jogadores 
#
# Rotina em R
#
# AUTOR: VICTOR MAIA S. DELGADO (UFOP)
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 24/02/2024
# CPU:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
# Versão R: 4.3.1(x64) 
#
# OBS.1: 
#   Essa Rotina foi gerada primordialmente no R 4.3.1 para Linux Mint (Una-MATE), 64 Bits, para rodá-la em outros SOs alguns detalhes devem ser observados.
# OBS.2:
#   As probabilidades de vitória foram calculadas segundo a logística ELO com E_b = Q_b/(Q_b + Q_p) e Q_b = 10^(R_b/400)
#   E_b expectativa de vitória das Brancas e Q_b conta auxiliar.
#   O mesmo pode ser feito para E_p e Q_p paras pretas
#
###################################################################################################################################################################
