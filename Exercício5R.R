##########################################
# Lista de exercícios 5
# Autor: Juelmo Gomes
# Data: 07/04/2026
##########################################

# Removendo os objetos ativos
rm(list = ls())

##########################################
# Função para cálculo do LDL-c (Friedewald)
##########################################

calcula_LDL <- function(CT, HDLc, TG, jejum = TRUE){
  
  # Cálculo do VLDL
  VLDLc <- TG/5
  
  # Cálculo do LDL
  LDLc <- CT - HDLc - VLDLc
  # (a) Aviso para hipertrigliceridemia
  if(TG >= 400){
    warning("TG >= 400 mg/dL. A equação de Friedewald torna-se imprecisa.")
  }
  # (b) Hiperlipidemia mista
  if(jejum == TRUE){
    if(LDLc >= 160 & TG >= 150){
      warning("Possível hiperlipidemia mista (LDLc elevado e TG elevado com jejum).")
    }
  } else {
    if(LDLc >= 160 & TG >= 175){
      warning("Possível hiperlipidemia mista (LDLc elevado e TG elevado sem jejum).")
    }
  }
  # (c) Hipercolesterolemia isolada
  if(LDLc >= 160){
    if(!((jejum == TRUE & TG >= 150) | (jejum == FALSE & TG >= 175))){
      warning("Possível hipercolesterolemia isolada (LDLc elevado).")
    }
  }
  
  # Retornando o valor de LDLc
  return(LDLc)
}

