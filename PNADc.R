
rm(list=ls())

#Funções primeiro nivel----
initiate_libs <- function(){
  library(convey)
  library(PNADcIBGE)
  library(survey)
  library(magrittr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  
}

get_offline_pnadc <- function(year, quarter=NULL, vars){
  #get_pnadc offline que já prepara para ser deflator
  #Não pega entrevistas, para isso, por enquanto vai ser online
  data <- read_pnadc(microdata=paste("D:/Pesquisa_IC/1.Iniciacao Cientifica_R/Base_PNADC/", "PNADC_0", quarter, year, ".txt", sep=""), input_txt ="D:/Pesquisa_IC/1.Iniciacao Cientifica_R/ferramentas/input_PNADC_trimestral.txt", vars)
  data <- pnadc_labeller(data, "D:/Pesquisa_IC/1.Iniciacao Cientifica_R/ferramentas/dicionario_PNADC_microdados_trimestral.xls")
  data <- pnadc_deflator(data_pnadc=data, deflator.file="D:/Pesquisa_IC/1.Iniciacao Cientifica_R/ferramentas/deflator_PNADC_2023_trimestral_040506.xls")
  data <- pnadc_design(data)
  return(data)
  
}

initiate_analysis <- function(teste_lista="", teste=FALSE, year=2022, modo="online", anual=FALSE){
  initiate_libs()
  
  ano <- year
  if(anual==FALSE){
    lista_generalista = c("UF", "RM_RIDE", "VD4003", "VD4002", "VD4016", "V2007", "VD4017", "VD4020", "VD4035", "VD2006", 'VD4010')
    lista_gini = c("UF", "RM_RIDE", "Capital", "VD4020", "VD4019", "V2007", "VD4016")
    lista_setores = c("UF", "RM_RIDE", "Capital", "VD4002", "VD4010", "VD4016")
    lista_categoria = c("UF", "RM_RIDE", "Capital", "VD4002", "VD4009", "VD4016")
    lista_idade = c("UF", "RM_RIDE", "Capital", "VD4002", "V2009", "VD4016", "VD2006")
    lista_instrucao= c("UF", "RM_RIDE", "Capital", "VD4002", "VD3004", "VD4016")
    lista_cor = c("UF", "RM_RIDE", "Capital", "VD4002", "V2010", "VD4016")
    lista_segsocial = c("UF", "RM_RIDE", "Capital", "VD4002", "VD4012", "VD4016", "V2007")
    lista_sexo = c("UF", "RM_RIDE", "Capital", "VD4002", "VD4001", "V2007", "VD4016")
    lista_regW = c("UF", "RM_RIDE", "Capital", "VD4002", "VD4016", "VD3004", "V2010", "V2009", "VD4009", "VD4010")
    #lista <- lista_regW
    
    if (teste_lista == "setorial") {
      lista <- lista_setores    
    } else if (teste_lista == "geral"){
      lista <- lista_generalista
    } else if(teste_lista=="gini"){
      lista <- lista_gini
    } else if(teste_lista=="categoria"){
      lista <- lista_categoria
    } else if(teste_lista=="idade"){
      lista <- lista_idade
    } else if(teste_lista=="instrucao"){
      lista <- lista_instrucao
    } else if(teste_lista=="cor"){
      lista <- lista_cor
    } else if(teste_lista=="segsocial"){
      lista <- lista_segsocial
    } else if(teste_lista=="sexo"){
      lista <- lista_sexo
    } else {
      print("Lista nao selecionada") 
    }
    
    if(teste==TRUE){
      #dados1 <<- get_pnadc(year=year, quarter=2, vars=lista, deflator=TRUE)
      #Download, label, deflate and create survey design. Thats the steps
      if(modo=="online"){
        dados1 <<- get_pnadc(year=ano, quarter=1, vars=lista, deflator=TRUE)
      } else{
        dados1 <<- get_offline_pnadc(year=ano, quarter=1, vars=lista)
        
      }
      
      #Deflacionamento
      dados1$variables <<- transform(dados1$variables, VD4016_real=VD4016*Habitual)
      
    } else {
      if(modo=="online"){
        dados1 <<- get_pnadc(year=ano, quarter=1, vars=lista, deflator=TRUE)
        dados2 <<- get_pnadc(year=ano, quarter=2, vars=lista, deflator=TRUE)
        dados3 <<- get_pnadc(year=ano, quarter=3, vars=lista, deflator=TRUE)
        dados4 <<- get_pnadc(year=ano, quarter=4, vars=lista, deflator=TRUE)
      } else {
        dados1 <<- get_offline_pnadc(year=ano, quarter=1, vars=lista)
        dados2 <<- get_offline_pnadc(year=ano, quarter=2, vars=lista)
        dados3 <<- get_offline_pnadc(year=ano, quarter=3, vars=lista)
        dados4 <<- get_offline_pnadc(year=ano, quarter=4, vars=lista)
      
      }    
      
    if(teste_lista=="gini"){
      # #Deflaciona gini
      dados1$variables <- transform(dados1$variables,VD4019_real=VD4019*Habitual)
      dados2$variables <- transform(dados2$variables,VD4019_real=VD4019*Habitual)
      dados3$variables <- transform(dados3$variables,VD4019_real=VD4019*Habitual)
      dados4$variables <- transform(dados4$variables,VD4019_real=VD4019*Habitual)
    } else{
      dados1$variables <- transform(dados1$variables,VD4016_real=VD4016*Habitual)
      dados2$variables <- transform(dados2$variables,VD4016_real=VD4016*Habitual)
      dados3$variables <- transform(dados3$variables,VD4016_real=VD4016*Habitual)
      dados4$variables <- transform(dados4$variables,VD4016_real=VD4016*Habitual)
      
    }

    }
  } else { #Se for anual
    #Aqui já considero como domiciliar PER CAPITA VD5005
    lista_gini_anual <- c("UF", "RM_RIDE", "Capital", "VD5008", "V2007")
    dados1 <<- get_pnadc(year=ano, interview=5, vars=lista_gini_anual, deflator=TRUE)
   
    }
     
  }

#Funções-estimativas (interface)----
#-> Ocupados idade é algo que estou trabalhando ainda.
ocupados_idade <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("idade", teste, ano, modo)

  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }

  #Tarefa: Trabalhar idades em faixas


  #Brasil
  setor1_br <- svyby(formula=~VD2006, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor2_br <- svyby(formula=~VD2006, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    setor3_br <- svyby(formula=~VD2006, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    setor4_br <- svyby(formula=~VD2006, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)

    setores_ano_br <- full_join(setor1_br, setor2_br)
    setores_ano_br <- full_join(setores_ano_br,setor3_br)
    setores_ano_br <- full_join(setores_ano_br,setor4_br)
  }else{
    setores_ano_br <- setor1_br
  }
  setores_ano_br <- setores_ano_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)

  for (i in 2:13){
    colnames(setores_ano_br)[i] <- substring(colnames(setores_ano_br)[i], 7)
  }

  #Estados
  setor1 <- svyby(formula=~VD2006, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor2 <- svyby(formula=~VD2006, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor3 <- svyby(formula=~VD2006, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor4 <- svyby(formula=~VD2006, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)

    setores_ano_uf <- full_join(setor1, setor2)
    setores_ano_uf <- full_join(setores_ano_uf,setor3)
    setores_ano_uf <- full_join(setores_ano_uf,setor4)
  }else{
    setores_ano_uf <- setor1
  }
  setores_ano_uf <- setores_ano_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)

  for (i in 2:13){
    colnames(setores_ano_uf)[i] <- substring(colnames(setores_ano_uf)[i], 7)
  }

  #RMs
  setor_rm1 <- svyby(formula=~VD2006, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor_rm2 <- svyby(formula=~VD2006, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor_rm3 <- svyby(formula=~VD2006, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor_rm4 <- svyby(formula=~VD2006, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)

    setores_ano_rm <- full_join(setor_rm1, setor_rm2)
    setores_ano_rm <- full_join(setores_ano_rm,setor_rm3)
    setores_ano_rm <- full_join(setores_ano_rm,setor_rm4)
  }else{
    setores_ano_rm <- setor_rm1
  }
  setores_ano_rm <- setores_ano_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)

  for (i in 2:13){
    colnames(setores_ano_rm)[i] <- substring(colnames(setores_ano_rm)[i], 7)
  }

  lista <- rep(c(), 13)

  for (i in 2:13){
    lista[i-1] <- colnames(setores_ano_rm)[i]
  }

  #Merge tibbles
  setores_ano <- full_join(setores_ano_br, setores_ano_uf)
  setores_ano <- full_join(setores_ano, setores_ano_rm)
  setores_ano <- unite(setores_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)

  setores_ano['Ano'] <- ano
  lista_colunas <- c("Ano","Regiao")
  lista_colunas <- append(lista_colunas, colnames(setores_ano))
  setores_ano <- select(setores_ano, all_of(lista_colunas))

  path_f <<- paste(path_downloads, "ocupados_setorial.xlsx", sep="")
  result <- setores_ano
}

ocupados_setor <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("setorial", teste, ano, modo) #deflaciona, organiza os dados no tipo survey design
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  setor1_br <- svyby(formula=~VD4010, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor2_br <- svyby(formula=~VD4010, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    setor3_br <- svyby(formula=~VD4010, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    setor4_br <- svyby(formula=~VD4010, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    setores_ano_br <- full_join(setor1_br, setor2_br)
    setores_ano_br <- full_join(setores_ano_br,setor3_br)
    setores_ano_br <- full_join(setores_ano_br,setor4_br)
  }else{
    setores_ano_br <- setor1_br
  }
  setores_ano_br <- setores_ano_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)
  
  for (i in 2:13){
    colnames(setores_ano_br)[i] <- substring(colnames(setores_ano_br)[i], 7)
  }
  
  #Estados - UFs
  setor1 <- svyby(formula=~VD4010, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor2 <- svyby(formula=~VD4010, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor3 <- svyby(formula=~VD4010, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor4 <- svyby(formula=~VD4010, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
 
    setores_ano_uf <- full_join(setor1, setor2)
    setores_ano_uf <- full_join(setores_ano_uf,setor3)
    setores_ano_uf <- full_join(setores_ano_uf,setor4)
  }else{
    setores_ano_uf <- setor1
  }
  setores_ano_uf <- setores_ano_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)
  
  for (i in 2:13){
    colnames(setores_ano_uf)[i] <- substring(colnames(setores_ano_uf)[i], 7)
  }
  
  #RMs
  setor_rm1 <- svyby(formula=~VD4010, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    setor_rm2 <- svyby(formula=~VD4010, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor_rm3 <- svyby(formula=~VD4010, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    setor_rm4 <- svyby(formula=~VD4010, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    setores_ano_rm <- full_join(setor_rm1, setor_rm2)
    setores_ano_rm <- full_join(setores_ano_rm,setor_rm3)
    setores_ano_rm <- full_join(setores_ano_rm,setor_rm4)
  }else{
    setores_ano_rm <- setor_rm1
  }
  setores_ano_rm <- setores_ano_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) %>%
    select(1:13)
  
  for (i in 2:13){
    colnames(setores_ano_rm)[i] <- substring(colnames(setores_ano_rm)[i], 7)
  }
  
  lista <- rep(c(), 13)
  
  for (i in 2:13){
     lista[i-1] <- colnames(setores_ano_rm)[i]
    }

  #Merge tibbles
  setores_ano <- full_join(setores_ano_br, setores_ano_uf)
  setores_ano <- full_join(setores_ano, setores_ano_rm)
  setores_ano <- unite(setores_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  
  setores_ano['Ano'] <- ano
  lista_colunas <- c("Ano","Regiao")
  lista_colunas <- append(lista_colunas, colnames(setores_ano))
  setores_ano <- select(setores_ano, all_of(lista_colunas))

  path_f <<- paste(path_downloads, "ocupados_setorial.xlsx", sep="")
  result <- setores_ano
}

rendimentos_setor <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("setorial", teste, ano, modo)
  
  #Brasil
  setor1_br <- svyby(formula=~VD4016_real, by=~interaction(VD4010, Ano), design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    setor2_br <- svyby(formula=~VD4016_real, by=~interaction(VD4010, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    setor3_br <- svyby(formula=~VD4016_real, by=~interaction(VD4010, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    setor4_br <- svyby(formula=~VD4016_real, by=~interaction(VD4010, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    setores_ano_br <- full_join(setor1_br, setor2_br)
    setores_ano_br <- full_join(setores_ano_br,setor3_br)
    setores_ano_br <- full_join(setores_ano_br,setor4_br)
  }
  else{
    setores_ano_br <- setor1_br
  }
  setores_ano_br <- setores_ano_br %>%
    group_by(`interaction(VD4010, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  setores1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, UF), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    setores2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    setores3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    setores4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    setores_ano_uf_rendi <- full_join(setores1_rendi, setores2_rendi)
    setores_ano_uf_rendi <- full_join(setores_ano_uf_rendi,setores3_rendi)
    setores_ano_uf_rendi <- full_join(setores_ano_uf_rendi,setores4_rendi)
  }
  else{
    setores_ano_uf_rendi <- setores1_rendi
  }
  setores_ano_uf_rendi <- setores_ano_uf_rendi %>%
    group_by(`interaction(VD4010, UF)`) %>%
    summarise_all(mean)
    
  #RMs
  setor_rm1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, RM_RIDE), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    setor_rm2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, RM_RIDE), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    setor_rm3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, RM_RIDE), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    setor_rm4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4010, RM_RIDE), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    setores_ano_rm_rendi <- full_join(setor_rm1_rendi, setor_rm2_rendi)
    setores_ano_rm_rendi <- full_join(setores_ano_rm_rendi,setor_rm3_rendi)
    setores_ano_rm_rendi <- full_join(setores_ano_rm_rendi,setor_rm4_rendi)
  }else{
    setores_ano_rm_rendi <- setor_rm1_rendi
  }
      
  setores_ano_rm_rendi <- setores_ano_rm_rendi %>%
    group_by(`interaction(VD4010, RM_RIDE)`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  setores_ano <- full_join(setores_ano_br, setores_ano_uf_rendi)
  setores_ano <- full_join(setores_ano, setores_ano_rm_rendi)
  setores_ano <- unite(setores_ano, Regiao, `interaction(VD4010, UF)`, `interaction(VD4010, RM_RIDE)`,na.rm=TRUE)
  setores_ano <- unite(setores_ano, Regiao, `interaction(VD4010, Ano)`, `Regiao`,na.rm=TRUE)
  
  setores_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(setores_ano))
  setores_ano <- select(setores_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_setorial.xlsx", sep="")
  result <- setores_ano
}

ocupados_categoria <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("categoria", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  categoria1_br <- svyby(formula=~VD4009, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    categoria2_br <- svyby(formula=~VD4009, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    categoria3_br <- svyby(formula=~VD4009, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    categoria4_br <- svyby(formula=~VD4009, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    categoria_br <- full_join(categoria1_br, categoria2_br)
    categoria_br <- full_join(categoria_br,categoria3_br)
    categoria_br <- full_join(categoria_br,categoria4_br)
  } else{
    categoria_br <- categoria1_br
  }
  categoria_br <- categoria_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) %>%
    select(1:11)
  
  for (i in 2:11){
    colnames(categoria_br)[i] <- substring(colnames(categoria_br)[i], 7)
  }

  #Estados
  categoria1 <- svyby(formula=~VD4009, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    categoria2 <- svyby(formula=~VD4009, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    categoria3 <- svyby(formula=~VD4009, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    categoria4 <- svyby(formula=~VD4009, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    categoria_uf <- full_join(categoria1, categoria2)
    categoria_uf <- full_join(categoria_uf,categoria3)
    categoria_uf <- full_join(categoria_uf,categoria4)
  } else{
    categoria_uf <- categoria1
  }
  categoria_uf <- categoria_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) %>%
    select(1:11)
  
  for (i in 2:11){
    colnames(categoria_uf)[i] <- substring(colnames(categoria_uf)[i], 7)
  }
  
  #RMs
  categoria_rm1 <- svyby(formula=~VD4009, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    categoria_rm2 <- svyby(formula=~VD4009, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    categoria_rm3 <- svyby(formula=~VD4009, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    categoria_rm4 <- svyby(formula=~VD4009, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    categoria_rm <- full_join(categoria_rm1, categoria_rm2)
    categoria_rm <- full_join(categoria_rm,categoria_rm3)
    categoria_rm <- full_join(categoria_rm,categoria_rm4)
  }else{
    categoria_rm <- categoria_rm1
  }  
  categoria_rm <- categoria_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) %>%
    select(1:11)
  
  for (i in 2:11){
    colnames(categoria_rm)[i] <- substring(colnames(categoria_rm)[i], 7)
  }
  
  
  #Merge two tibbles
  categoria_ano <- full_join(categoria_br, categoria_uf)
  categoria_ano <- full_join(categoria_ano, categoria_rm)
  categoria_ano <- unite(categoria_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  
  categoria_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(categoria_ano))
  categoria_ano <- select(categoria_ano, all_of(lista_colunas))

  path_f <<- paste(path_downloads, "ocupados_categoria.xlsx", sep="")
  result <- categoria_ano
}

rendimentos_categoria <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("categoria", teste, ano, modo)

  #Brasil
  categoria1_br <- svyby(formula=~VD4016_real, by=~interaction(VD4009, Ano), design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    categoria2_br <- svyby(formula=~VD4016_real, by=~interaction(VD4009, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    categoria3_br <- svyby(formula=~VD4016_real, by=~interaction(VD4009, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    categoria4_br <- svyby(formula=~VD4016_real, by=~interaction(VD4009, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    categoria_br_ano <- full_join(categoria1_br, categoria2_br)
    categoria_br_ano <- full_join(categoria_br_ano,categoria3_br)
    categoria_br_ano <- full_join(categoria_br_ano,categoria4_br)
  }else{
    categoria_br_ano <- categoria1_br
  }
  categoria_br_ano <- categoria_br_ano %>%
    group_by(`interaction(VD4009, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  categoria1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, UF), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    categoria2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    categoria3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    categoria4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    categoria_uf_rendi <- full_join(categoria1_rendi, categoria2_rendi)
    categoria_uf_rendi <- full_join(categoria_uf_rendi,categoria3_rendi)
    categoria_uf_rendi <- full_join(categoria_uf_rendi,categoria4_rendi)
  }else{
    categoria_uf_rendi <- categoria1_rendi
  }
  categoria_uf_rendi <- categoria_uf_rendi %>%
    group_by(`interaction(VD4009, UF)`) %>%
    summarise_all(mean)
  
  #RMs
  categoria_rm1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, RM_RIDE), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    categoria_rm2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, RM_RIDE), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    categoria_rm3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, RM_RIDE), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    categoria_rm4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD4009, RM_RIDE), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    categoria_ano_rm_rendi <- full_join(categoria_rm1_rendi, categoria_rm2_rendi)
    categoria_ano_rm_rendi <- full_join(categoria_ano_rm_rendi,categoria_rm3_rendi)
    categoria_ano_rm_rendi <- full_join(categoria_ano_rm_rendi,categoria_rm4_rendi)
  }else{
    categoria_ano_rm_rendi <- categoria_rm1_rendi
  }
  categoria_ano_rm <- categoria_ano_rm_rendi %>%
    group_by(`interaction(VD4009, RM_RIDE)`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  categoria_ano <- full_join(categoria_br_ano, categoria_uf_rendi)
  categoria_ano <- full_join(categoria_ano, categoria_ano_rm)
  categoria_ano <- unite(categoria_ano, Regiao, `interaction(VD4009, UF)`, `interaction(VD4009, RM_RIDE)`,na.rm=TRUE)
  categoria_ano <- unite(categoria_ano, Regiao, `interaction(VD4009, Ano)`, Regiao,na.rm=TRUE)
  
    
  categoria_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(categoria_ano))
  categoria_ano <- select(categoria_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_categoria.xlsx", sep="")
  result <- categoria_ano
}

indice_gini <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("gini", teste, ano, modo)

  #Brasil
  gini_br1 <- convey_prep(dados1) %>%
    svyby(formula=~VD4019, by=~Ano, FUN=svygini, na.rm=T)
  if(teste==FALSE){
    gini_br2 <- convey_prep(dados2) %>%
      svyby(formula=~VD4019, by=~Ano, FUN=svygini, na.rm=T)

    gini_br3 <- convey_prep(dados3) %>%
      svyby(formula=~VD4019, by=~Ano, FUN=svygini, na.rm=T)

    gini_br4 <- convey_prep(dados4) %>%
      svyby(formula=~VD4019, by=~Ano, FUN=svygini, na.rm=T)


    gini_br <- full_join(gini_br1, gini_br2)
    gini_br <- full_join(gini_br,gini_br3)
    gini_br <- full_join(gini_br,gini_br4)
  }else{
    gini_br <- gini_br1
  }
  gini_br_ano <- gini_br %>%
    group_by(`Ano`) %>%
    summarise_all(mean)


  #Estado
  gini1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD4019, by=~UF, FUN=svygini, na.rm=T)
  if(teste==FALSE){
    gini2 <- subset(dados2, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~UF, FUN=svygini, na.rm=T)

    gini3 <- subset(dados3, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~UF, FUN=svygini, na.rm=T)

    gini4 <- subset(dados4, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~UF, FUN=svygini, na.rm=T)

    gini_uf <- full_join(gini1, gini2)
    gini_uf <- full_join(gini_uf,gini3)
    gini_uf <- full_join(gini_uf,gini4)
  }else{
    gini_uf <- gini1
  }
  gini_uf_ano <- gini_uf %>%
    group_by(`UF`) %>%
    summarise_all(mean)

  #RMs
  gini_rm1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD4019, by=~RM_RIDE, FUN=svygini, na.rm=T)
  if(teste==FALSE){
    gini_rm2 <- subset(dados2, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~RM_RIDE, FUN=svygini, na.rm=T)

    gini_rm3 <- subset(dados3, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~RM_RIDE, FUN=svygini, na.rm=T)

    gini_rm4 <- subset(dados4, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~RM_RIDE, FUN=svygini, na.rm=T)

    gini_rm <- full_join(gini_rm1, gini_rm2)
    gini_rm <- full_join(gini_rm,gini_rm3)
    gini_rm <- full_join(gini_rm,gini_rm4)
  }else{
    gini_rm <- gini_rm1
  }
  gini_rm_ano <- gini_rm %>%
    group_by(`RM_RIDE`) %>%
    summarise_all(mean)

  #Capital
  gini_rm1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
  if(teste==FALSE){
    gini_rm2 <- subset(dados2, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)

    gini_rm3 <- subset(dados3, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)

    gini_rm4 <- subset(dados4, UF %in% lista_ufs) %>%
      convey_prep() %>%
      svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)

    gini_cap <- full_join(gini_rm1, gini_rm2)
    gini_cap <- full_join(gini_cap,gini_rm3)
    gini_cap <- full_join(gini_cap,gini_rm4)
  }else{
    gini_cap <- gini_rm1
  }
  gini_cap_ano <- gini_cap %>%
    group_by(`Capital`) %>%
    summarise_all(mean)
# 
#   #Periferia
#   gini_rm1 <- subset(dados1, RM_RIDE %in% c("Região Metropolitana de Rio de Janeiro (RJ)", "Região Metropolitana de Sâo Paulo (SP)") & Capital %in% "NAN") %>%
#     convey_prep() %>%
#     svyby(formula=~VD4019, by=~Ano, FUN=svygini, na.rm=T)
#   if(teste==FALSE){
#     gini_rm2 <- subset(dados2, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_rm3 <- subset(dados3, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_rm4 <- subset(dados4, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_cap <- full_join(gini_rm1, gini_rm2)
#     gini_cap <- full_join(gini_cap,gini_rm3)
#     gini_cap <- full_join(gini_cap,gini_rm4)
#   }else{
#     gini_cap <- gini_rm1
#   }
#   gini_cap_ano <- gini_cap %>%
#     group_by(`Capital`) %>%
#     summarise_all(mean)
#   
#   #Interior
#   gini_rm1 <- subset(dados1, UF %in% lista_ufs) %>%
#     convey_prep() %>%
#     svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#   if(teste==FALSE){
#     gini_rm2 <- subset(dados2, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_rm3 <- subset(dados3, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_rm4 <- subset(dados4, UF %in% lista_ufs) %>%
#       convey_prep() %>%
#       svyby(formula=~VD4019, by=~Capital, FUN=svygini, na.rm=T)
#     
#     gini_cap <- full_join(gini_rm1, gini_rm2)
#     gini_cap <- full_join(gini_cap,gini_rm3)
#     gini_cap <- full_join(gini_cap,gini_rm4)
#   }else{
#     gini_cap <- gini_rm1
#   }
#   gini_cap_ano <- gini_cap %>%
#     group_by(`Capital`) %>%
#     summarise_all(mean)
  
  
  #Merge tibbles
  gini_ano <- full_join(gini_br_ano, gini_uf_ano)
  gini_ano <- full_join(gini_ano, gini_rm_ano)
  gini_ano <- full_join(gini_ano, gini_cap_ano)
  gini_ano <- unite(gini_ano, Regiao, `UF`, `RM_RIDE`,na.rm=TRUE)

  gini_ano['Ano'] <- ano
  lista_colunas <- c('Ano')
  lista_colunas <- append(lista_colunas, colnames(gini_ano))
  gini_ano <- select(gini_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "indice_gini.xlsx", sep="")
  result <- gini_ano
}

indice_gini_domiciliar <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("gini", teste, ano, modo, anual=TRUE)
  
  #Brasil
  gini_br1 <- convey_prep(dados1) %>%
    svyby(formula=~VD5008, by=~Ano, FUN=svygini, na.rm=T)
  # if(teste==FALSE){
  #   gini_br5 <- convey_prep(dados5) %>%
  #     svyby(formula=~VD5008, by=~Ano, FUN=svygini, na.rm=T)
    
    # gini_br <- full_join(gini_br1, gini_br5)
  # }else{
  gini_br <- gini_br1
  # }
  gini_br_ano <- gini_br %>%
    group_by(`Ano`) %>%
    summarise_all(mean)

  #Estado
  gini1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD5008, by=~UF, FUN=svygini, na.rm=T)
  # if(teste==FALSE){
  #   gini5 <- subset(dados5, UF %in% lista_ufs) %>%
  #     convey_prep() %>%
  #     svyby(formula=~VD5008, by=~UF, FUN=svygini, na.rm=T)
  # 
  #   gini_uf <- full_join(gini1, gini5)
  # }else{
  gini_uf <- gini1
  # }
  gini_uf_ano <- gini_uf %>%
    group_by(`UF`) %>%
    summarise_all(mean)

  #RMs
  gini_rm1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD5008, by=~RM_RIDE, FUN=svygini, na.rm=T)
  # if(teste==FALSE){
  #   gini_rm5 <- subset(dados5, UF %in% lista_ufs) %>%
  #     convey_prep() %>%
  #     svyby(formula=~VD5008, by=~RM_RIDE, FUN=svygini, na.rm=T)
  # 
  # 
  #   gini_rm <- full_join(gini_rm1, gini_rm5)
  # }else{
  gini_rm <- gini_rm1
  # }
  gini_rm_ano <- gini_rm %>%
    group_by(`RM_RIDE`) %>%
    summarise_all(mean)

  #Capital
  gini_cap1 <- subset(dados1, UF %in% lista_ufs) %>%
    convey_prep() %>%
    svyby(formula=~VD5008, by=~Capital, FUN=svygini, na.rm=T)
  # if(teste==FALSE){
  #   gini_cap5 <- subset(dados5, UF %in% lista_ufs) %>%
  #     convey_prep() %>%
  #     svyby(formula=~VD5008, by=~Capital, FUN=svygini, na.rm=T)
  # 
  #   gini_cap <- full_join(gini_cap1, gini_cap5)
  # }else{
  gini_cap <- gini_cap1
  # }
  gini_cap_ano <- gini_cap %>%
    group_by(`Capital`) %>%
    summarise_all(mean)

  #Merge tibbles
  gini_ano <- full_join(gini_br_ano, gini_uf_ano)
  gini_ano <- full_join(gini_ano, gini_rm_ano)
  gini_ano <- full_join(gini_ano, gini_cap_ano)
  gini_ano <- unite(gini_ano, Regiao, `UF`, `RM_RIDE`,na.rm=TRUE)

  gini_ano['Ano'] <- ano
  lista_colunas <- c('Ano')
  lista_colunas <- append(lista_colunas, colnames(gini_ano))
  gini_ano <- select(gini_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "indice_gini_domiciliar.xlsx", sep="")
  result <- gini_ano
}

ocupados_previdencia <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("segsocial", teste, ano, modo)

  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~VD4012, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~VD4012, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~VD4012, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~VD4012, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) #%>%
    
#    select(1:5)
  
 # for (i in 2:11){
  #  colnames(previdencia_br)[i] <- substring(colnames(previdencia_br)[i], 7)
  #}
  
  #Estados
  previdencia1 <- svyby(formula=~VD4012, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~VD4012, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~VD4012, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~VD4012, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) #%>%
 #   select(1:11)
  
#  for (i in 2:11){
#    colnames(previdencia_uf)[i] <- substring(colnames(previdencia_uf)[i], 7)
#  }
  
  #RMs
  previdencia_rm1 <- svyby(formula=~VD4012, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~VD4012, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~VD4012, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~VD4012, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) #%>%
  #  select(1:11)
  
#  for (i in 2:11){
#    colnames(previdencia_rm)[i] <- substring(colnames(previdencia_rm)[i], 7)
#  }
  
  #Merge two tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))

  path_f <<- paste(path_downloads, "ocupados_previdencia.xlsx", sep="")
  result <- previdencia_ano
}

rendimento_instrucao <-function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("instrucao", teste, ano, modo)
  
  #Brasil
  instrucao1_br <- svyby(formula=~VD4016_real, by=~interaction(VD3004, Ano), design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_br <- svyby(formula=~VD4016_real, by=~interaction(VD3004, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao3_br <- svyby(formula=~VD4016_real, by=~interaction(VD3004, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao4_br <- svyby(formula=~VD4016_real, by=~interaction(VD3004, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    instrucao_br_ano <- full_join(instrucao1_br, instrucao2_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao3_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao4_br)
  }else{
    instrucao_br_ano <- instrucao1_br
  }
  instrucao_br_ano <- instrucao_br_ano %>%
    group_by(`interaction(VD3004, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  instrucao1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, UF), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    instrucao_uf_rendi <- full_join(instrucao1_rendi, instrucao2_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao3_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao4_rendi)
  }else{
    instrucao_uf_rendi <- instrucao1_rendi
  }
  instrucao_uf_rendi <- instrucao_uf_rendi %>%
    group_by(`interaction(VD3004, UF)`) %>%
    summarise_all(mean)
  
  #RMs
  instrucao_rm1_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, RM_RIDE), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao_rm2_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, RM_RIDE), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm3_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, RM_RIDE), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm4_rendi <- svyby(formula=~VD4016_real, by=~interaction(VD3004, RM_RIDE), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    intrucao_ano_rm_rendi <- full_join(instrucao_rm1_rendi, instrucao_rm2_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm3_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm4_rendi)
  }else{
    intrucao_ano_rm_rendi <- instrucao_rm1_rendi
  }
  instrucao_ano_rm <- intrucao_ano_rm_rendi %>%
    group_by(`interaction(VD3004, RM_RIDE)`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  instrucao_ano <- full_join(instrucao_br_ano, instrucao_uf_rendi)
  instrucao_ano <- full_join(instrucao_ano, instrucao_ano_rm)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(VD3004, UF)`, `interaction(VD3004, RM_RIDE)`,na.rm=TRUE)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(VD3004, Ano)`, Regiao,na.rm=TRUE)
  
  
  instrucao_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(instrucao_ano))
  instrucao_ano <- select(instrucao_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_intrucao.xlsx", sep="")
  result <- instrucao_ano
}

rendimento_total <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  #Brasil
  instrucao1_br <- svyby(formula=~VD4016_real, by=~Ano, design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_br <- svyby(formula=~VD4016_real, by=~Ano, design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao3_br <- svyby(formula=~VD4016_real, by=~Ano, design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao4_br <- svyby(formula=~VD4016_real, by=~Ano, design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    instrucao_br_ano <- full_join(instrucao1_br, instrucao2_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao3_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao4_br)
  }else{
    instrucao_br_ano <- instrucao1_br
  }
  instrucao_br_ano <- instrucao_br_ano %>%
    group_by(`Ano`) %>%
    summarise_all(mean)
  
  #Estados
  instrucao1_rendi <- svyby(formula=~VD4016_real, by=~UF, design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_rendi <- svyby(formula=~VD4016_real, by=~UF, design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao3_rendi <- svyby(formula=~VD4016_real, by=~UF, design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao4_rendi <- svyby(formula=~VD4016_real, by=~UF, design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    instrucao_uf_rendi <- full_join(instrucao1_rendi, instrucao2_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao3_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao4_rendi)
  }else{
    instrucao_uf_rendi <- instrucao1_rendi
  }
  instrucao_uf_rendi <- instrucao_uf_rendi %>%
    group_by(`UF`) %>%
    summarise_all(mean)
  
  #RMs
  instrucao_rm1_rendi <- svyby(formula=~VD4016_real, by=~RM_RIDE, design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao_rm2_rendi <- svyby(formula=~VD4016_real, by=~RM_RIDE, design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm3_rendi <- svyby(formula=~VD4016_real, by=~RM_RIDE, design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm4_rendi <- svyby(formula=~VD4016_real, by=~RM_RIDE, design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    intrucao_ano_rm_rendi <- full_join(instrucao_rm1_rendi, instrucao_rm2_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm3_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm4_rendi)
  }else{
    intrucao_ano_rm_rendi <- instrucao_rm1_rendi
  }
  instrucao_ano_rm <- intrucao_ano_rm_rendi %>%
    group_by(`RM_RIDE`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  instrucao_ano <- full_join(instrucao_br_ano, instrucao_uf_rendi)
  instrucao_ano <- full_join(instrucao_ano, instrucao_ano_rm)
  instrucao_ano <- unite(instrucao_ano, Regiao, `UF`, `RM_RIDE`,na.rm=TRUE)
  instrucao_ano <- unite(instrucao_ano, Regiao, `Ano`, Regiao,na.rm=TRUE)
  
  
  instrucao_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(instrucao_ano))
  instrucao_ano <- select(instrucao_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_total.xlsx", sep="")
  result <- instrucao_ano
}

rendimento_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  #Brasil
  instrucao1_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao3_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao4_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    instrucao_br_ano <- full_join(instrucao1_br, instrucao2_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao3_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao4_br)
  }else{
    instrucao_br_ano <- instrucao1_br
  }
  instrucao_br_ano <- instrucao_br_ano %>%
    group_by(`interaction(V2007, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  instrucao1_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao3_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao4_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    instrucao_uf_rendi <- full_join(instrucao1_rendi, instrucao2_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao3_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao4_rendi)
  }else{
    instrucao_uf_rendi <- instrucao1_rendi
  }
  instrucao_uf_rendi <- instrucao_uf_rendi %>%
    group_by(`interaction(V2007, UF)`) %>%
    summarise_all(mean)
  
  #RMs
  instrucao_rm1_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, RM_RIDE), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao_rm2_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, RM_RIDE), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm3_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, RM_RIDE), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm4_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, RM_RIDE), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    intrucao_ano_rm_rendi <- full_join(instrucao_rm1_rendi, instrucao_rm2_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm3_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm4_rendi)
  }else{
    intrucao_ano_rm_rendi <- instrucao_rm1_rendi
  }
  instrucao_ano_rm <- intrucao_ano_rm_rendi %>%
    group_by(`interaction(V2007, RM_RIDE)`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  instrucao_ano <- full_join(instrucao_br_ano, instrucao_uf_rendi)
  instrucao_ano <- full_join(instrucao_ano, instrucao_ano_rm)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2007, UF)`, `interaction(V2007, RM_RIDE)`,na.rm=TRUE)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2007, Ano)`, Regiao,na.rm=TRUE)
  
  
  instrucao_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(instrucao_ano))
  instrucao_ano <- select(instrucao_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_sexo.xlsx", sep="")
  result <- instrucao_ano
}

rendimento_cor <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("cor", teste, ano, modo)
  
  #Brasil
  instrucao1_br <- svyby(formula=~VD4016_real, by=~interaction(V2010, Ano), design=subset(dados1, VD4016_real != "NA"), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_br <- svyby(formula=~VD4016_real, by=~interaction(V2010, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao3_br <- svyby(formula=~VD4016_real, by=~interaction(V2010, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao4_br <- svyby(formula=~VD4016_real, by=~interaction(V2010, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    instrucao_br_ano <- full_join(instrucao1_br, instrucao2_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao3_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao4_br)
  }else{
    instrucao_br_ano <- instrucao1_br
  }
  instrucao_br_ano <- instrucao_br_ano %>%
    group_by(`interaction(V2010, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  instrucao1_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, UF), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao2_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao3_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao4_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    instrucao_uf_rendi <- full_join(instrucao1_rendi, instrucao2_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao3_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao4_rendi)
  }else{
    instrucao_uf_rendi <- instrucao1_rendi
  }
  instrucao_uf_rendi <- instrucao_uf_rendi %>%
    group_by(`interaction(V2010, UF)`) %>%
    summarise_all(mean)
  
  #RMs
  instrucao_rm1_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, RM_RIDE), design=subset(dados1, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
  if(teste==FALSE){
    instrucao_rm2_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, RM_RIDE), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm3_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, RM_RIDE), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao_rm4_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2010, RM_RIDE), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    intrucao_ano_rm_rendi <- full_join(instrucao_rm1_rendi, instrucao_rm2_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm3_rendi)
    intrucao_ano_rm_rendi <- full_join(intrucao_ano_rm_rendi,instrucao_rm4_rendi)
  }else{
    intrucao_ano_rm_rendi <- instrucao_rm1_rendi
  }
  instrucao_ano_rm <- intrucao_ano_rm_rendi %>%
    group_by(`interaction(V2010, RM_RIDE)`) %>%
    summarise_all(mean)
  
  #Merge two tibbles
  instrucao_ano <- full_join(instrucao_br_ano, instrucao_uf_rendi)
  instrucao_ano <- full_join(instrucao_ano, instrucao_ano_rm)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2010, UF)`, `interaction(V2010, RM_RIDE)`,na.rm=TRUE)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2010, Ano)`, Regiao,na.rm=TRUE)
  
  
  instrucao_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(instrucao_ano))
  instrucao_ano <- select(instrucao_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "rendimentos_cor.xlsx", sep="")
  result <- instrucao_ano
}

ocupados_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000)
  
  #Estados
  previdencia1 <- svyby(formula=~V2007, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & RM_RIDE %in% c("Região Metropolitana de Rio de Janeiro (RJ)", "Região Metropolitana de Sâo Paulo (SP)") & Capital =="NAN"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~V2007, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~V2007, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~V2007, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000)
  
  #RMs
  previdencia_rm1 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000)
  
  #Capital
  previdencia_rm1 <- svyby(formula=~V2007, by=~Capital, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~Capital, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~Capital, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~Capital, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_cap <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm3)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm4)
  }else{
    previdencia_cap <- categoria_rm1
  }  
  previdencia_cap <- previdencia_cap %>%
    group_by(Capital) %>%
    summarise_all(media_divide1000)
  
  #Merge tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- full_join(previdencia_ano, previdencia_cap)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  previdencia_ano <- unite(previdencia_ano, Regiao, Regiao, Capital,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "ocupados_sexo.xlsx", sep="")
  result <- previdencia_ano
}

desocupados_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados1, VD4002=="Pessoas desocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados2, VD4002=="Pessoas desocupadas"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados3, VD4002=="Pessoas desocupadas"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados4, VD4002=="Pessoas desocupadas"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000)

  #Estados
  previdencia1 <- svyby(formula=~V2007, by=~UF, design=subset(dados1, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~V2007, by=~UF, design=subset(dados2, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~V2007, by=~UF, design=subset(dados3, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~V2007, by=~UF, design=subset(dados4, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000)
  
  #RMs
  previdencia_rm1 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) 
  #Capital
  previdencia_rm1 <- svyby(formula=~V2007, by=~Capital, design=subset(dados1, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~Capital, design=subset(dados2, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~Capital, design=subset(dados3, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~Capital, design=subset(dados4, VD4002=="Pessoas desocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_cap <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm3)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm4)
  }else{
    previdencia_cap <- categoria_rm1
  }  
  previdencia_cap <- previdencia_cap %>%
    group_by(Capital) %>%
    summarise_all(media_divide1000)
  
  #Merge tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- full_join(previdencia_ano, previdencia_cap)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  previdencia_ano <- unite(previdencia_ano, Regiao, Regiao, Capital,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "desocupados_sexo.xlsx", sep="")
  result <- previdencia_ano
}

pea_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados1, VD4001=="Pessoas na força de trabalho"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados2, VD4001=="Pessoas na força de trabalho"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados3, VD4001=="Pessoas na força de trabalho"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados4, VD4001=="Pessoas na força de trabalho"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000)

  #Estados
  previdencia1 <- svyby(formula=~V2007, by=~UF, design=subset(dados1, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~V2007, by=~UF, design=subset(dados2, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~V2007, by=~UF, design=subset(dados3, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~V2007, by=~UF, design=subset(dados4, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000)
  
  #RMs
  previdencia_rm1 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados1, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados2, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados3, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados4, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000)
  
  #Capital
  previdencia_rm1 <- svyby(formula=~V2007, by=~Capital, design=subset(dados1, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~Capital, design=subset(dados2, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~Capital, design=subset(dados3, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~Capital, design=subset(dados4, VD4001=="Pessoas na força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_cap <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm3)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm4)
  }else{
    previdencia_cap <- categoria_rm1
  }  
  previdencia_cap <- previdencia_cap %>%
    group_by(Capital) %>%
    summarise_all(media_divide1000)
  
  #Merge tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- full_join(previdencia_ano, previdencia_cap)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  previdencia_ano <- unite(previdencia_ano, Regiao, Regiao, Capital,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "pea_sexo1.xlsx", sep="")
  result <- previdencia_ano
}

naopea_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("sexo", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados1, VD4001=="Pessoas fora da força de trabalho"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados2, VD4001=="Pessoas fora da força de trabalho"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados3, VD4001=="Pessoas fora da força de trabalho"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~V2007, by=~Ano, design=subset(dados4, VD4001=="Pessoas fora da força de trabalho"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000)
  
  #Estados
  previdencia1 <- svyby(formula=~V2007, by=~UF, design=subset(dados1, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~V2007, by=~UF, design=subset(dados2, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~V2007, by=~UF, design=subset(dados3, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~V2007, by=~UF, design=subset(dados4, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000)
  
  #RMs
  previdencia_rm1 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados1, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados2, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados3, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~RM_RIDE, design=subset(dados4, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000)
  
  #Capital
  previdencia_rm1 <- svyby(formula=~V2007, by=~Capital, design=subset(dados1, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2007, by=~Capital, design=subset(dados2, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2007, by=~Capital, design=subset(dados3, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2007, by=~Capital, design=subset(dados4, VD4001=="Pessoas fora da força de trabalho" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_cap <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm3)
    previdencia_cap <- full_join(previdencia_cap,previdencia_rm4)
  }else{
    previdencia_cap <- categoria_rm1
  }  
  previdencia_cap <- previdencia_cap %>%
    group_by(Capital) %>%
    summarise_all(media_divide1000)
  
  #Merge tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- full_join(previdencia_ano, previdencia_cap)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  previdencia_ano <- unite(previdencia_ano, Regiao, Regiao, Capital,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "naopea_sexo.xlsx", sep="")
  result <- previdencia_ano
}

ocupados_cor <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("cor", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~V2010, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~V2010, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~V2010, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~V2010, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) #%>%
  
  #    select(1:5)
  
  # for (i in 2:11){
  #  colnames(previdencia_br)[i] <- substring(colnames(previdencia_br)[i], 7)
  #}
  
  #Estados
  previdencia1 <- svyby(formula=~V2010, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~V2010, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~V2010, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~V2010, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) #%>%
  #   select(1:11)
  
  #  for (i in 2:11){
  #    colnames(previdencia_uf)[i] <- substring(colnames(previdencia_uf)[i], 7)
  #  }
  
  #RMs
  previdencia_rm1 <- svyby(formula=~V2010, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~V2010, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~V2010, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~V2010, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) #%>%
  #  select(1:11)
  
  #  for (i in 2:11){
  #    colnames(previdencia_rm)[i] <- substring(colnames(previdencia_rm)[i], 7)
  #  }
  
  #Merge two tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "ocupados_cor.xlsx", sep="")
  result <- previdencia_ano
}

ocupados_instrucao <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  ano <- anothis
  initiate_analysis("instrucao", teste, ano, modo)
  
  media_divide1000 <- function(observed) {
    result <- mean(observed) / 1000
  }
  
  #Brasil
  previdencia1_br <- svyby(formula=~VD3004, by=~Ano, design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2_br <- svyby(formula=~VD3004, by=~Ano, design=subset(dados2, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia3_br <- svyby(formula=~VD3004, by=~Ano, design=subset(dados3, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    previdencia4_br <- svyby(formula=~VD3004, by=~Ano, design=subset(dados4, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
    
    previdencia_br <- full_join(previdencia1_br, previdencia2_br)
    previdencia_br <- full_join(previdencia_br,previdencia3_br)
    previdencia_br <- full_join(previdencia_br,previdencia4_br)
  } else{
    previdencia_br <- previdencia1_br
  }
  previdencia_br <- previdencia_br %>%
    group_by(Ano) %>%
    summarise_all(media_divide1000) #%>%
  
  #    select(1:5)
  
  # for (i in 2:11){
  #  colnames(previdencia_br)[i] <- substring(colnames(previdencia_br)[i], 7)
  #}
  
  #Estados
  previdencia1 <- svyby(formula=~VD3004, by=~UF, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia2 <- svyby(formula=~VD3004, by=~UF, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia3 <- svyby(formula=~VD3004, by=~UF, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia4 <- svyby(formula=~VD3004, by=~UF, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_uf <- full_join(previdencia1, previdencia2)
    previdencia_uf <- full_join(previdencia_uf,previdencia3)
    previdencia_uf <- full_join(previdencia_uf,previdencia4)
  } else{
    previdencia_uf <- previdencia1
  }
  previdencia_uf <- previdencia_uf %>%
    group_by(UF) %>%
    summarise_all(media_divide1000) #%>%
  #   select(1:11)
  
  #  for (i in 2:11){
  #    colnames(previdencia_uf)[i] <- substring(colnames(previdencia_uf)[i], 7)
  #  }
  
  #RMs
  previdencia_rm1 <- svyby(formula=~VD3004, by=~RM_RIDE, design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    previdencia_rm2 <- svyby(formula=~VD3004, by=~RM_RIDE, design=subset(dados2, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm3 <- svyby(formula=~VD3004, by=~RM_RIDE, design=subset(dados3, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    previdencia_rm4 <- svyby(formula=~VD3004, by=~RM_RIDE, design=subset(dados4, VD4002=="Pessoas ocupadas" & UF %in% lista_ufs), FUN=svytotal, na.rm=T)
    
    previdencia_rm <- full_join(previdencia_rm1, previdencia_rm2)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm3)
    previdencia_rm <- full_join(previdencia_rm,previdencia_rm4)
  }else{
    previdencia_rm <- categoria_rm1
  }  
  previdencia_rm <- previdencia_rm %>%
    group_by(RM_RIDE) %>%
    summarise_all(media_divide1000) #%>%
  #  select(1:11)
  
  #  for (i in 2:11){
  #    colnames(previdencia_rm)[i] <- substring(colnames(previdencia_rm)[i], 7)
  #  }
  
  #Merge two tibbles
  previdencia_ano <- full_join(previdencia_br, previdencia_uf)
  previdencia_ano <- full_join(previdencia_ano, previdencia_rm)
  previdencia_ano <- unite(previdencia_ano, Regiao, UF, RM_RIDE,na.rm=TRUE)
  
  previdencia_ano['Ano'] <- ano
  lista_colunas <- c("Ano", "Regiao")
  lista_colunas <- append(lista_colunas, colnames(previdencia_ano))
  previdencia_ano <- select(previdencia_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "ocupados_instrucao.xlsx", sep="")
  result <- previdencia_ano
}

#F. estimativa - Extra
formal_sexo <- function(anothis=2022, teste=FALSE, modo="online", lista_ufs=c("")){
  
  #Brasil
  instrucao1_br <- svyby(formula=~VD4012, by=~interaction(V2007, Ano), design=subset(dados1, VD4002=="Pessoas ocupadas"), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    instrucao2_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados2, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao3_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados3, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    instrucao4_br <- svyby(formula=~VD4016_real, by=~interaction(V2007, Ano), design=subset(dados4, VD4016_real != "NA"), FUN=svymean, na.rm=T)
    
    instrucao_br_ano <- full_join(instrucao1_br, instrucao2_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao3_br)
    instrucao_br_ano <- full_join(instrucao_br_ano,instrucao4_br)
  }else{
    instrucao_br_ano <- instrucao1_br
  }
  instrucao_br_ano <- instrucao_br_ano %>%
    group_by(`interaction(V2007, Ano)`) %>%
    summarise_all(mean)
  
  #Estados
  instrucao1_rendi <- svyby(formula=~VD4012, by=~interaction(V2007, UF), design=subset(dados1, VD4002=="Pessoas ocupadas" & UF %in% c("Rio de Janeiro", "São Paulo")), FUN=svytotal, na.rm=T)
  if(teste==FALSE){
    instrucao2_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados2, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao3_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados3, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    instrucao4_rendi <- svyby(formula=~VD4016_real, by=~interaction(V2007, UF), design=subset(dados4, VD4016_real != "NA" & UF %in% lista_ufs), FUN=svymean, na.rm=T)
    
    instrucao_uf_rendi <- full_join(instrucao1_rendi, instrucao2_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao3_rendi)
    instrucao_uf_rendi <- full_join(instrucao_uf_rendi,instrucao4_rendi)
  }else{
    instrucao_uf_rendi <- instrucao1_rendi
  }
  instrucao_uf_rendi <- instrucao_uf_rendi %>%
    group_by(`interaction(V2007, UF)`) %>%
    summarise_all(mean)
  
  
  #Merge two tibbles
  instrucao_ano <- full_join(instrucao_br_ano, instrucao_uf_rendi)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2007, UF)`, na.rm=TRUE)
  instrucao_ano <- unite(instrucao_ano, Regiao, `interaction(V2007, Ano)`, Regiao,na.rm=TRUE)
  
  
  instrucao_ano['Ano'] <- ano
  lista_colunas <- 'Ano'
  lista_colunas <- append(lista_colunas, colnames(instrucao_ano))
  instrucao_ano <- select(instrucao_ano, all_of(lista_colunas))
  
  path_f <<- paste(path_downloads, "formal_sexo.xlsx", sep="")
  writexl::write_xlsx(instrucao_ano, path=path_f)
  result <- instrucao_ano
}

# Loop anos -----
loop_anos <- function(vetor_anos, FUN, teste=FALSE, how="online", ufs=c("Rio de Janeiro", "São Paulo")){
  for (i in vetor_anos){
    gc() #limpa memória
    if (which(vetor_anos == i) == 1){ #vetor_anos = (2012, 2013, 2014)
      tibble_resultado <<- FUN(i, teste, modo=how, ufs)
        
    } else {
      rm(list = ls()[grep("dados", ls())]) #?
      tibble_resultado <<- full_join(tibble_resultado, FUN(i, teste, modo=how, ufs))    

    }

  }

  writexl::write_xlsx(tibble_resultado, path=path_f)

}


#Rodando estatísticas no intervalo de ano----
rm(list = setdiff(ls(), lsf.str()))
#rm(list=ls()) rm(list = ls()[grep("dados", ls())])
gc()
path_downloads <- "C:/Users/magju/Downloads/"
intervalo_anos <- c(2022)

# #IDADE----
# #-> Idade está em construção
# loop_anos(intervalo_anos, ocupados_idade, FALSE, "offline")
# 
# loop_anos(intervalo_anos, rendimentos_idade, FALSE, "offline")

#Rendimento total-----
loop_anos(intervalo_anos, rendimento_total, FALSE, "offline")

#Instrucao----
loop_anos(intervalo_anos, ocupados_instrucao, FALSE, "offline")

#Cor----
loop_anos(intervalo_anos, ocupados_cor, FALSE, "offline")

#Sexo-----
loop_anos(intervalo_anos, naopea_sexo, FALSE, "offline")

loop_anos(intervalo_anos, pea_sexo, FALSE, "offline")

loop_anos(intervalo_anos, desocupados_sexo, FALSE, "offline")

loop_anos(intervalo_anos, ocupados_sexo, FALSE, "offline")

#Rendimento Cor-----
loop_anos(intervalo_anos, rendimento_cor, FALSE, "offline")

#Rendimento Sexo-----
loop_anos(intervalo_anos, rendimento_sexo, FALSE, "offline")

#Rendimento Instrucao-----
loop_anos(intervalo_anos, rendimento_instrucao, FALSE, "offline")

#Ocupados PREVIDENCIA-----
loop_anos(intervalo_anos, ocupados_previdencia, FALSE, "offline")

#SETOR-----
loop_anos(intervalo_anos, ocupados_setor, TRUE, "offline")

loop_anos(intervalo_anos, rendimentos_setor, FALSE, "offline")

#CATEGORIA-----
loop_anos(intervalo_anos, ocupados_categoria, FALSE, "offline")

loop_anos(intervalo_anos, rendimentos_categoria, FALSE, "offline")

#Índice Gini trabalho---------
loop_anos(intervalo_anos, indice_gini, FALSE, how="offline")

# Gini domiciliar ----
# VER NA TABELA DE ENTREVISTAS QUAIS ANOS PEGAR ETC
loop_anos(intervalo_anos, indice_gini_domiciliar, FALSE)


#Testes de hipóteses----
#Testando se rendimento médio de salários das mulheres é igual ao dos homens a partir das estatísticas
svyttest(formula=VD4016_real~V2007, design=dados1)

#Modelos----

# rm(a,b,c,d,e,f,g,h,i,j,k,l)
# rm(dados2017)
gc()
initiate_libs()

lista <- c("UF", "VD4002", "VD4016", "VD4019", "VD4017", "VD3004", "V2010", 'V3006A', "V3009", "V3009A", "V3013", "V2009", "VD4009", "VD4010", "V2007", "V2010", "V1022", "V1023", "V4040", "V40401", "VD4011", "VD4035", "VD4031", "V4039")

# dados1 <- get_offline_pnadc(year=c(2019,2020), quarter=c(1,2,3,4), vars=lista)

#teste
gc()
dados2017 <- get_offline_pnadc(year=c(2017), quarter=c(1,2,3,4), vars=lista)
gc()
dados2019 <- get_offline_pnadc(year=c(2019), quarter=c(1,2,3,4), vars=lista)
gc()
dados2020 <- get_offline_pnadc(year=c(2022), quarter=c(1,2,3,4), vars=lista)
gc()

#Deflaciona variaveis de rendimento
# dados1$variables <- transform(dados1$variables, VD4016_real=VD4016*Habitual)
# dados1$variables <- transform(dados1$variables, VD4019_real=VD4019*Habitual)

#teste
# dados2017$variables <- transform(dados2017$variables, VD4016_real=VD4016*Habitual)
# dados2017$variables <- transform(dados2017$variables, VD4019_real=VD4019*Habitual)
# dados2019$variables <- transform(dados2019$variables, VD4016_real=VD4016*Habitual)
# dados2019$variables <- transform(dados2019$variables, VD4019_real=VD4019*Habitual)
dados2020$variables <- transform(dados2020$variables, VD4016_real=VD4016*Habitual)
dados2020$variables <- transform(dados2020$variables, VD4019_real=VD4019*Habitual)
gc()


#Dummy Homem
# dados1$variables <- transform(dados1$variables,V2007=factor(V2007, levels=c("Mulher", "Homem")))

#teste
# dados2017$variables <- transform(dados2017$variables,V2007=factor(V2007, levels=c("Mulher", "Homem")))
# dados2019$variables <- transform(dados2019$variables,V2007=factor(V2007, levels=c("Mulher", "Homem")))
dados2020$variables <- transform(dados2020$variables,V2007=factor(V2007, levels=c("Mulher", "Homem")))
gc()

#Dummy Branco
# dados1$variables <- transform(dados1$variables,V2010_dummy=ifelse(V2010=="Não aplicável","Others", V2010))
# dados1$variables <- transform(dados1$variables,V2010_dummy=ifelse(V2010_dummy==1, "Branca", "Others"))
# dados1$variables <- transform(dados1$variables,V2010_dummy=factor(V2010_dummy, levels=c("Others", "Branca")))

#teste
# dados2017$variables <- transform(dados2017$variables,V2010_dummy=ifelse(V2010=="Não aplicável","Others", V2010))
# dados2017$variables <- transform(dados2017$variables,V2010_dummy=ifelse(V2010_dummy==1, "Branca", "Others"))
# dados2017$variables <- transform(dados2017$variables,V2010_dummy=factor(V2010_dummy, levels=c("Others", "Branca")))
# dados2019$variables <- transform(dados2019$variables,V2010_dummy=ifelse(V2010=="Não aplicável","Others", V2010))
# dados2019$variables <- transform(dados2019$variables,V2010_dummy=ifelse(V2010_dummy==1, "Branca", "Others"))
# dados2019$variables <- transform(dados2019$variables,V2010_dummy=factor(V2010_dummy, levels=c("Others", "Branca")))
dados2020$variables <- transform(dados2020$variables,V2010_dummy=ifelse(V2010=="Não aplicável","Others", V2010))
dados2020$variables <- transform(dados2020$variables,V2010_dummy=ifelse(V2010_dummy==1, "Branca", "Others"))
dados2020$variables <- transform(dados2020$variables,V2010_dummy=factor(V2010_dummy, levels=c("Others", "Branca")))
 gc()


#Dummy formal-informal
# dados1$variables <- transform(dados1$variables,VD4009_dummy=ifelse(VD4009=="Não aplicável","Others", VD4009))
# dados1$variables <- transform(dados1$variables,VD4009_dummy=ifelse(VD4009_dummy==1|VD4009_dummy==3|VD4009_dummy==5|VD4009_dummy==8, "Formal","Informal"))
# dados1$variables <- transform(dados1$variables,VD4009_dummy=factor(VD4009_dummy, levels=c("Informal", "Formal")))

#teste
# dados2017$variables <- transform(dados2017$variables,VD4009_dummy=ifelse(VD4009=="Não aplicável","Others", VD4009))
# dados2017$variables <- transform(dados2017$variables,VD4009_dummy=ifelse(VD4009_dummy==1|VD4009_dummy==3|VD4009_dummy==5|VD4009_dummy==8, "Formal","Informal"))
# dados2017$variables <- transform(dados2017$variables,VD4009_dummy=factor(VD4009_dummy, levels=c("Informal", "Formal")))
# dados2019$variables <- transform(dados2019$variables,VD4009_dummy=ifelse(VD4009=="Não aplicável","Others", VD4009))
# dados2019$variables <- transform(dados2019$variables,VD4009_dummy=ifelse(VD4009_dummy==1|VD4009_dummy==3|VD4009_dummy==5|VD4009_dummy==8, "Formal","Informal"))
# dados2019$variables <- transform(dados2019$variables,VD4009_dummy=factor(VD4009_dummy, levels=c("Informal", "Formal")))
dados2020$variables <- transform(dados2020$variables,VD4009_dummy=ifelse(VD4009=="Não aplicável","Others", VD4009))
dados2020$variables <- transform(dados2020$variables,VD4009_dummy=ifelse(VD4009_dummy==1|VD4009_dummy==3|VD4009_dummy==5|VD4009_dummy==8, "Formal","Informal"))
dados2020$variables <- transform(dados2020$variables,VD4009_dummy=factor(VD4009_dummy, levels=c("Informal", "Formal")))
gc()



#Dummy Urbano
# dados1$variables <- transform(dados1$variables,V1022_dummy=ifelse(V1022=="Não aplicável","Others", V1022))
# dados1$variables <- transform(dados1$variables,V1022_dummy=ifelse(V1022_dummy==1, "Urbana", "Others"))
# dados1$variables <- transform(dados1$variables,V1022_dummy=factor(V1022_dummy, levels=c("Others", "Urbana")))

#teste
# dados2017$variables <- transform(dados2017$variables,V1022_dummy=ifelse(V1022=="Não aplicável","Others", V1022))
# dados2017$variables <- transform(dados2017$variables,V1022_dummy=ifelse(V1022_dummy==1, "Urbana", "Others"))
# dados2017$variables <- transform(dados2017$variables,V1022_dummy=factor(V1022_dummy, levels=c("Others", "Urbana")))
# dados2019$variables <- transform(dados2019$variables,V1022_dummy=ifelse(V1022=="Não aplicável","Others", V1022))
# dados2019$variables <- transform(dados2019$variables,V1022_dummy=ifelse(V1022_dummy==1, "Urbana", "Others"))
# dados2019$variables <- transform(dados2019$variables,V1022_dummy=factor(V1022_dummy, levels=c("Others", "Urbana")))
dados2020$variables <- transform(dados2020$variables,V1022_dummy=ifelse(V1022=="Não aplicável","Others", V1022))
dados2020$variables <- transform(dados2020$variables,V1022_dummy=ifelse(V1022_dummy==1, "Urbana", "Others"))
dados2020$variables <- transform(dados2020$variables,V1022_dummy=factor(V1022_dummy, levels=c("Others", "Urbana")))
gc()

#Dummy Capital e RM(sem capital)
# dados1$variables <- transform(dados1$variables,V1023_dummy=ifelse(V1023=="Não aplicável","Others", V1023))
# dados1$variables <- transform(dados1$variables,V1023_dummy=ifelse(V1023_dummy==1, "Capital",ifelse(V1023_dummy==2, "RM sem capital", "Others")))
# dados1$variables <- transform(dados1$variables,V1023_dummy=factor(V1023_dummy, levels=c("Others", "Capital", "RM sem capital")))

#teste
# dados2017$variables <- transform(dados2017$variables,V1023_dummy=ifelse(V1023=="Não aplicável","Others", V1023))
# dados2017$variables <- transform(dados2017$variables,V1023_dummy=ifelse(V1023_dummy==1, "Capital",ifelse(V1023_dummy==2, "RM sem capital", "Others")))
# dados2017$variables <- transform(dados2017$variables,V1023_dummy=factor(V1023_dummy, levels=c("Others", "Capital", "RM sem capital")))
# dados2019$variables <- transform(dados2019$variables,V1023_dummy=ifelse(V1023=="Não aplicável","Others", V1023))
# dados2019$variables <- transform(dados2019$variables,V1023_dummy=ifelse(V1023_dummy==1, "Capital",ifelse(V1023_dummy==2, "RM sem capital", "Others")))
# dados2019$variables <- transform(dados2019$variables,V1023_dummy=factor(V1023_dummy, levels=c("Others", "Capital", "RM sem capital")))
dados2020$variables <- transform(dados2020$variables,V1023_dummy=ifelse(V1023=="Não aplicável","Others", V1023))
dados2020$variables <- transform(dados2020$variables,V1023_dummy=ifelse(V1023_dummy==1, "Capital",ifelse(V1023_dummy==2, "RM sem capital", "Others")))
dados2020$variables <- transform(dados2020$variables,V1023_dummy=factor(V1023_dummy, levels=c("Others", "Capital", "RM sem capital")))
gc()


#Dummy nivel instrução
# dados1$variables <- transform(dados1$variables,V3009A_dummy=ifelse(V3009A=="Não aplicável","Others", V3009A))
# dados1$variables <- transform(dados1$variables,V3013_dummy=ifelse(V3013=="Não aplicável","Others", V3013))
# 
# dados1$variables <- transform(dados1$variables,V3009A_dummy=ifelse(V3009A_dummy==5 | (V3009A_dummy==7 & V3013_dummy%in%c(1:5)), "Fundamental I", 
#                                                                    ifelse(V3009A_dummy==6 | V3009A_dummy==8 |(V3009A_dummy==7 & V3013_dummy%in%c(6:9)), "Fundamental II",
#                                                                           ifelse(V3009A_dummy==9 | V3009A_dummy==10| V3009A_dummy==11, "Ensino Médio", 
#                                                                                  ifelse(V3009A_dummy==12|V3009A_dummy==13|V3009A_dummy==14|V3009A_dummy==15, "Superior", "Others")))))
# 
# dados1$variables <- transform(dados1$variables,V3009A_dummy=factor(V3009A_dummy, levels=c("Others", "Fundamental I", "Fundamental II", "Ensino Médio", "Superior")))

# teste
# dados2017$variables <- transform(dados2017$variables,V3009A_dummy=ifelse(V3009A=="Não aplicável","Others", V3009A))
# dados2017$variables <- transform(dados2017$variables,V3013_dummy=ifelse(V3013=="Não aplicável","Others", V3013))
# 
# dados2017$variables <- transform(dados2017$variables,V3009A_dummy=ifelse(V3009A_dummy==5 | (V3009A_dummy==7 & V3013_dummy%in%c(1:5)), "Fundamental I",
#                                                                           ifelse(V3009A_dummy==6 | V3009A_dummy==8 |(V3009A_dummy==7 & V3013_dummy%in%c(6:9)), "Fundamental II",
#                                                                                  ifelse(V3009A_dummy==9 | V3009A_dummy==10| V3009A_dummy==11, "Ensino Médio",
#                                                                                         ifelse(V3009A_dummy==12|V3009A_dummy==13|V3009A_dummy==14|V3009A_dummy==15, "Superior", "Others")))))
# 
# dados2017$variables <- transform(dados2017$variables,V3009A_dummy=factor(V3009A_dummy, levels=c("Others", "Fundamental I", "Fundamental II", "Ensino Médio", "Superior")))
#
# dados2019$variables <- transform(dados2019$variables,V3009A_dummy=ifelse(V3009A=="Não aplicável","Others", V3009A))
# dados2019$variables <- transform(dados2019$variables,V3013_dummy=ifelse(V3013=="Não aplicável","Others", V3013))
# 
# dados2019$variables <- transform(dados2019$variables,V3009A_dummy=ifelse(V3009A_dummy==5 | (V3009A_dummy==7 & V3013_dummy%in%c(1:5)), "Fundamental I",
#                                                                    ifelse(V3009A_dummy==6 | V3009A_dummy==8 |(V3009A_dummy==7 & V3013_dummy%in%c(6:9)), "Fundamental II",
#                                                                           ifelse(V3009A_dummy==9 | V3009A_dummy==10| V3009A_dummy==11, "Ensino Médio",
#                                                                                  ifelse(V3009A_dummy==12|V3009A_dummy==13|V3009A_dummy==14|V3009A_dummy==15, "Superior", "Others")))))
# 
# dados2019$variables <- transform(dados2019$variables,V3009A_dummy=factor(V3009A_dummy, levels=c("Others", "Fundamental I", "Fundamental II", "Ensino Médio", "Superior")))
# #
dados2020$variables <- transform(dados2020$variables,V3009A_dummy=ifelse(V3009A=="Não aplicável","Others", V3009A))
dados2020$variables <- transform(dados2020$variables,V3013_dummy=ifelse(V3013=="Não aplicável","Others", V3013))

dados2020$variables <- transform(dados2020$variables,V3009A_dummy=ifelse(V3009A_dummy==5 | (V3009A_dummy==7 & V3013_dummy%in%c(1:5)), "Fundamental I",
                                                                         ifelse(V3009A_dummy==6 | V3009A_dummy==8 |(V3009A_dummy==7 & V3013_dummy%in%c(6:9)), "Fundamental II",
                                                                                ifelse(V3009A_dummy==9 | V3009A_dummy==10| V3009A_dummy==11, "Ensino Médio",
                                                                                       ifelse(V3009A_dummy==12|V3009A_dummy==13|V3009A_dummy==14|V3009A_dummy==15, "Superior", "Others")))))

dados2020$variables <- transform(dados2020$variables,V3009A_dummy=factor(V3009A_dummy, levels=c("Others", "Fundamental I", "Fundamental II", "Ensino Médio", "Superior")))
gc()


#Dummy faixas idade
# dados1$variables <- transform(dados1$variables,V2009_faixa=ifelse(V2009<25,0,ifelse((V2009>=25)&(V2009<=39),"25-39",ifelse((V2009>=40)&(V2009<=59),"40-59",ifelse((V2009>=60),"60+",0)))))
# # dados1$variables <- transform(dados1$variables,V2009_faixa=as.factor(V2009_faixa))
# dados1$variables <- transform(dados1$variables,V2009_faixa=factor(V2009_faixa, levels=c(0, "25-39", "40-59", "60+")))

#teste
# dados2017$variables <- transform(dados2017$variables,V2009_faixa=ifelse(V2009<25,0,ifelse((V2009>=25)&(V2009<=39),"25-39",ifelse((V2009>=40)&(V2009<=59),"40-59",ifelse((V2009>=60),"60+",0)))))
# dados2017$variables <- transform(dados2017$variables,V2009_faixa=factor(V2009_faixa, levels=c(0, "25-39", "40-59", "60+")))

# dados2019$variables <- transform(dados2019$variables,V2009_faixa=ifelse(V2009<25,0,ifelse((V2009>=25)&(V2009<=39),"25-39",ifelse((V2009>=40)&(V2009<=59),"40-59",ifelse((V2009>=60),"60+",0)))))
# dados2019$variables <- transform(dados2019$variables,V2009_faixa=factor(V2009_faixa, levels=c(0, "25-39", "40-59", "60+")))

dados2020$variables <- transform(dados2020$variables,V2009_faixa=ifelse(V2009<25,0,ifelse((V2009>=25)&(V2009<=39),"25-39",ifelse((V2009>=40)&(V2009<=59),"40-59",ifelse((V2009>=60),"60+",0)))))
dados2020$variables <- transform(dados2020$variables,V2009_faixa=factor(V2009_faixa, levels=c(0, "25-39", "40-59", "60+")))
gc()

#Dummy de experiencia em mesmo trabalho
# dados1$variables <- transform(dados1$variables,V4040_dummy=ifelse(V4040=="Não aplicável","Others", V4040))
# dados1$variables <- transform(dados1$variables,V4040_dummy=ifelse(V4040_dummy==2 & V40401>=2, "De 2 meses a menos de 1 ano",ifelse(V4040_dummy==3, "De 1 ano a menos de 2 anos", ifelse(V4040_dummy==4, "2 anos ou mais", 0))))
# dados1$variables <- transform(dados1$variables,V4040_dummy=factor(V4040_dummy, levels=c(0, "De 2 meses a menos de 1 ano", "De 1 ano a menos de 2 anos", "2 anos ou mais")))

#testes
# dados2017$variables <- transform(dados2017$variables,V4040_dummy=ifelse(V4040=="Não aplicável","Others", V4040))
# dados2017$variables <- transform(dados2017$variables,V4040_dummy=ifelse(V4040_dummy==2 & V40401>=2, "De 2 meses a menos de 1 ano",ifelse(V4040_dummy==3, "De 1 ano a menos de 2 anos", ifelse(V4040_dummy==4, "2 anos ou mais", 0))))
# dados2017$variables <- transform(dados2017$variables,V4040_dummy=factor(V4040_dummy, levels=c(0, "De 2 meses a menos de 1 ano", "De 1 ano a menos de 2 anos", "2 anos ou mais")))

# dados2019$variables <- transform(dados2019$variables,V4040_dummy=ifelse(V4040=="Não aplicável","Others", V4040))
# dados2019$variables <- transform(dados2019$variables,V4040_dummy=ifelse(V4040_dummy==2 & V40401>=2, "De 2 meses a menos de 1 ano",ifelse(V4040_dummy==3, "De 1 ano a menos de 2 anos", ifelse(V4040_dummy==4, "2 anos ou mais", 0))))
# dados2019$variables <- transform(dados2019$variables,V4040_dummy=factor(V4040_dummy, levels=c(0, "De 2 meses a menos de 1 ano", "De 1 ano a menos de 2 anos", "2 anos ou mais")))

dados2020$variables <- transform(dados2020$variables,V4040_dummy=ifelse(V4040=="Não aplicável","Others", V4040))
dados2020$variables <- transform(dados2020$variables,V4040_dummy=ifelse(V4040_dummy==2 & V40401>=2, "De 2 meses a menos de 1 ano",ifelse(V4040_dummy==3, "De 1 ano a menos de 2 anos", ifelse(V4040_dummy==4, "2 anos ou mais", 0))))
dados2020$variables <- transform(dados2020$variables,V4040_dummy=factor(V4040_dummy, levels=c(0, "De 2 meses a menos de 1 ano", "De 1 ano a menos de 2 anos", "2 anos ou mais")))
gc()

#Dummy Setor
# dados1$variables <- transform(dados1$variables,VD4010_dummy=ifelse(VD4010=="Não aplicável","Others", VD4010))
# dados1$variables <- transform(dados1$variables,VD4010_dummy=as.factor(VD4010_dummy))

#testes
# dados2017$variables <- transform(dados2017$variables,VD4010_dummy=ifelse(VD4010=="Não aplicável","Others", VD4010))
# dados2017$variables <- transform(dados2017$variables,VD4010_dummy=as.factor(VD4010_dummy))

# dados2019$variables <- transform(dados2019$variables,VD4010_dummy=ifelse(VD4010=="Não aplicável","Others", VD4010))
# dados2019$variables <- transform(dados2019$variables,VD4010_dummy=as.factor(VD4010_dummy))

dados2020$variables <- transform(dados2020$variables,VD4010_dummy=ifelse(VD4010=="Não aplicável","Others", VD4010))
dados2020$variables <- transform(dados2020$variables,VD4010_dummy=as.factor(VD4010_dummy))
gc()

#Dummy Tipo
# dados1$variables <- transform(dados1$variables,VD4011_dummy=ifelse(VD4011=="Não aplicável","Others", VD4011))
# dados1$variables <- transform(dados1$variables,VD4011_dummy=as.factor(VD4011_dummy))
# dados1$variables <- transform(dados1$variables,VD4011_dummy=factor(VD4011_dummy, levels=c(6,1,2,3,4,5,7,8,9,10,11)))

#testes
# dados2017$variables <- transform(dados2017$variables,VD4011_dummy=ifelse(VD4011=="Não aplicável","Others", VD4011))
# dados2017$variables <- transform(dados2017$variables,VD4011_dummy=as.factor(VD4011_dummy))
# dados2017$variables <- transform(dados2017$variables,VD4011_dummy=factor(VD4011_dummy, levels=c(6,1,2,3,4,5,7,8,9,10,11)))

# dados2019$variables <- transform(dados2019$variables,VD4011_dummy=ifelse(VD4011=="Não aplicável","Others", VD4011))
# dados2019$variables <- transform(dados2019$variables,VD4011_dummy=as.factor(VD4011_dummy))
# dados2019$variables <- transform(dados2019$variables,VD4011_dummy=factor(VD4011_dummy, levels=c(6,1,2,3,4,5,7,8,9,10,11)))

dados2020$variables <- transform(dados2020$variables,VD4011_dummy=ifelse(VD4011=="Não aplicável","Others", VD4011))
dados2020$variables <- transform(dados2020$variables,VD4011_dummy=as.factor(VD4011_dummy))
dados2020$variables <- transform(dados2020$variables,VD4011_dummy=factor(VD4011_dummy, levels=c(6,1,2,3,4,5,7,8,9,10,11)))

gc()
# 
# modelo2017 <- svyglm(formula=log(VD4016_real)~V2007
#                      +V2010_dummy+VD4009_dummy+V1022_dummy
#                      +V1023_dummy+V3009A_dummy+V2009_faixa
#                      +V4040_dummy+VD4010_dummy+VD4011_dummy
#                      +UF+V4039, design=dados2017)
# 
# gc()
# modelo2017_rj <- svyglm(formula=log(VD4016_real)~V2007
#                      +V2010_dummy+VD4009_dummy+V1022_dummy
#                      +V1023_dummy+V3009A_dummy+V2009_faixa
#                      +V4040_dummy+VD4010_dummy+VD4011_dummy
#                      +V4039, design=subset(dados2017, UF=="Rio de Janeiro"))
# 
# gc()

# modelo2017_sp <- svyglm(formula=log(VD4016_real)~V2007
#                         +V2010_dummy+VD4009_dummy+V1022_dummy
#                         +V1023_dummy+V3009A_dummy+V2009_faixa
#                         +V4040_dummy+VD4010_dummy+VD4011_dummy
#                         +V4039, design=subset(dados2017, UF=="São Paulo"))
# 
# gc()


# modelo2019 <- svyglm(formula=log(VD4016_real)~V2007
#                      +V2010_dummy+VD4009_dummy+V1022_dummy
#                      +V1023_dummy+V3009A_dummy+V2009_faixa
#                      +V4040_dummy+VD4010_dummy+VD4011_dummy
#                      +UF+V4039, design=dados2019)
# 
# gc()
# 
# modelo2019_rj <- svyglm(formula=log(VD4016_real)~V2007
#                         +V2010_dummy+VD4009_dummy+V1022_dummy
#                         +V1023_dummy+V3009A_dummy+V2009_faixa
#                         +V4040_dummy+VD4010_dummy+VD4011_dummy
#                         +V4039, design=subset(dados2019, UF=="Rio de Janeiro"))
# 
# gc()
# 
# modelo2019_sp <- svyglm(formula=log(VD4016_real)~V2007
#                         +V2010_dummy+VD4009_dummy+V1022_dummy
#                         +V1023_dummy+V3009A_dummy+V2009_faixa
#                         +V4040_dummy+VD4010_dummy+VD4011_dummy
#                         +V4039, design=subset(dados2019, UF=="São Paulo"))
# 
# gc()

# modelo2020 <- svyglm(formula=log(VD4016_real)~V2007
#                      +V2010_dummy+VD4009_dummy+V1022_dummy
#                      +V1023_dummy+V3009A_dummy+V2009_faixa
#                      +V4040_dummy+VD4010_dummy+VD4011_dummy
#                      +UF++V4039, design=dados2020)
# 
# gc()
# 
# modelo2020_rj <- svyglm(formula=log(VD4016_real)~V2007
#                         +V2010_dummy+VD4009_dummy+V1022_dummy
#                         +V1023_dummy+V3009A_dummy+V2009_faixa
#                         +V4040_dummy+VD4010_dummy+VD4011_dummy
#                         +V4039, design=subset(dados2020, UF=="Rio de Janeiro"))
# 
# gc()

modelo2020_sp <- svyglm(formula=log(VD4016_real)~V2007
                        +V2010_dummy+VD4009_dummy+V1022_dummy
                        +V1023_dummy+V3009A_dummy+V2009_faixa
                        +V4040_dummy+VD4010_dummy+VD4011_dummy
                        +V4039, design=subset(dados2020, UF=="São Paulo"))

gc()


#Calculo do R^2
#calculo dos desvios

#Médias das variáveis

gc()
# a2017 <- svyby(formula=~VD4016_real, by=~Ano, design=dados2017, FUN=svymean, na.rm=T)
gc()
# a2019 <- svyby(formula=~log(VD4016_real), by=~Ano, design=dados2019, FUN=svymean, na.rm=T)
# gc()
# a2020 <- svyby(formula=~log(VD4016_real), by=~Ano, design=dados2020, FUN=svymean, na.rm=T)
gc()
# b2017 <- svyby(formula=~V2007, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# b2019 <- svyby(formula=~V2007, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
b2020 <- svyby(formula=~V2007, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# c2017 <- svyby(formula=~V2010_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# c2019 <- svyby(formula=~V2010_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
c2020 <- svyby(formula=~V2010_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# d2017 <- svyby(formula=~VD4009_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# d2019 <- svyby(formula=~VD4009_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
d2020 <- svyby(formula=~VD4009_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# e2017 <- svyby(formula=~V1022_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# e2019 <- svyby(formula=~V1022_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
e2020 <- svyby(formula=~V1022_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# f2017 <- svyby(formula=~V1023_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# f2019 <- svyby(formula=~V1023_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
f2020 <- svyby(formula=~V1023_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# g2017 <- svyby(formula=~V3009A_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# g2019 <- svyby(formula=~V3009A_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
g2020 <- svyby(formula=~V3009A_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# h2017 <- svyby(formula=~V2009_faixa, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# h2019 <- svyby(formula=~V2009_faixa, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
h2020 <- svyby(formula=~V2009_faixa, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# i2017 <- svyby(formula=~V4040_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# i2019 <- svyby(formula=~V4040_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
i2020 <- svyby(formula=~V4040_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# j2017 <- svyby(formula=~VD4010_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# j2019 <- svyby(formula=~VD4010_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
j2020 <- svyby(formula=~VD4010_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# k2017 <- svyby(formula=~VD4011_dummy, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# k2019 <- svyby(formula=~VD4011_dummy, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
k2020 <- svyby(formula=~VD4011_dummy, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
# gc()
# l2017 <- svyby(formula=~V4039, by=~Ano, design=subset(dados2017, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# l2019 <- svyby(formula=~V4039, by=~Ano, design=subset(dados2019, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
l2020 <- svyby(formula=~V4039, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()
# m2017 <- svyby(formula=~UF, by=~Ano, design=dados2017, FUN=svymean, na.rm=T)
gc()
# m2019 <- svyby(formula=~UF, by=~Ano, design=dados2019, FUN=svymean, na.rm=T)
# gc()
m2020 <- svyby(formula=~UF, by=~Ano, design=subset(dados2020, UF=="São Paulo"), FUN=svymean, na.rm=T)
gc()


#ajuste de coeficientes
adju_coefs <- function(coefs, bool_ajuste){
  adj_coef <- list()
  if(bool_ajuste==TRUE){
    
    for(i in (1:length(coefs))){
      # print(i)
      if(i==1){
        adj_coef <- coefs[i]+mean(coefs) 
        print(coefs[i])
      }else if(i<length(coefs)){
        adj_coef <- append(adj_coef, coefs[i]-mean(coefs))
        print(coefs[i])
      }else{
        adj_coef <- append(adj_coef, coefs[i])
      }
    }
  }else{
    adj_coef <- coefs
  }
  result <-adj_coef
}

modelo2019_adj <- adju_coefs(modelo2019$coefficients,FALSE)
modelo2017_adj <- adju_coefs(modelo2017$coefficients,FALSE)
modelo2020_adj <- adju_coefs(modelo2020$coefficients,FALSE)

modelo2019_adj_rj <- adju_coefs(modelo2019_rj$coefficients,FALSE)
modelo2017_adj_rj <- adju_coefs(modelo2017_rj$coefficients,FALSE)
modelo2020_adj_rj <- adju_coefs(modelo2020_rj$coefficients,FALSE)

modelo2019_adj_sp <- adju_coefs(modelo2019_sp$coefficients,FALSE)
modelo2017_adj_sp <- adju_coefs(modelo2017_sp$coefficients,FALSE)
modelo2020_adj_sp <- adju_coefs(modelo2020_sp$coefficients,FALSE)

media_coeficientesA <- 0.5*as.vector(modelo2019_adj)+0.5*as.vector(modelo2017_adj)
media_coeficientesB <- 0.5*as.vector(modelo2020_adj)+0.5*as.vector(modelo2019_adj)


#teste
joined_2017_sp <- list()
joined_2017_sp <- c(1,
                 list(as.vector(b2017)[3])[[1]][[1]],
                 list(as.vector(c2017)[3])[[1]][[1]],
                 list(as.vector(d2017)[3])[[1]][[1]],
                 list(as.vector(e2017)[3])[[1]][[1]],
                 list(as.vector(f2017)[3])[[1]][[1]],
                 list(as.vector(f2017)[4])[[1]][[1]],
                 list(as.vector(g2017)[3])[[1]][[1]],
                 list(as.vector(g2017)[4])[[1]][[1]],
                 list(as.vector(g2017)[5])[[1]][[1]],
                 list(as.vector(g2017)[6])[[1]][[1]],
                 list(as.vector(h2017)[3])[[1]][[1]],
                 list(as.vector(h2017)[4])[[1]][[1]],
                 list(as.vector(h2017)[5])[[1]][[1]],
                 list(as.vector(i2017)[3])[[1]][[1]],
                 list(as.vector(i2017)[4])[[1]][[1]],
                 list(as.vector(i2017)[5])[[1]][[1]],
                 list(as.vector(j2017)[3])[[1]][[1]],
                 list(as.vector(j2017)[4])[[1]][[1]],
                 list(as.vector(j2017)[5])[[1]][[1]],
                 list(as.vector(j2017)[6])[[1]][[1]],
                 list(as.vector(j2017)[7])[[1]][[1]],
                 list(as.vector(j2017)[8])[[1]][[1]],
                 list(as.vector(j2017)[9])[[1]][[1]],
                 list(as.vector(j2017)[10])[[1]][[1]],
                 list(as.vector(j2017)[11])[[1]][[1]],
                 list(as.vector(j2017)[12])[[1]][[1]],
                 list(as.vector(j2017)[13])[[1]][[1]],
                 list(as.vector(k2017)[3])[[1]][[1]],
                 list(as.vector(k2017)[4])[[1]][[1]],
                 list(as.vector(k2017)[5])[[1]][[1]],
                 list(as.vector(k2017)[6])[[1]][[1]],
                 list(as.vector(k2017)[7])[[1]][[1]],
                 list(as.vector(k2017)[8])[[1]][[1]],
                 list(as.vector(k2017)[9])[[1]][[1]],
                 list(as.vector(k2017)[10])[[1]][[1]],
                 list(as.vector(k2017)[11])[[1]][[1]],
                 list(as.vector(k2017)[12])[[1]][[1]],
                 list(as.vector(l2017)[2])[[1]][[1]])

joined_2019 <- list()
joined_2019 <- c(1,
                 list(as.vector(b2019)[3])[[1]][[1]],
                 list(as.vector(c2019)[3])[[1]][[1]],
                 list(as.vector(d2019)[3])[[1]][[1]],
                 list(as.vector(e2019)[3])[[1]][[1]],
                 list(as.vector(f2019)[3])[[1]][[1]],
                 list(as.vector(f2019)[4])[[1]][[1]],
                 list(as.vector(g2019)[3])[[1]][[1]],
                 list(as.vector(g2019)[4])[[1]][[1]],
                 list(as.vector(g2019)[5])[[1]][[1]],
                 list(as.vector(g2019)[6])[[1]][[1]],
                 list(as.vector(h2019)[3])[[1]][[1]],
                 list(as.vector(h2019)[4])[[1]][[1]],
                 list(as.vector(h2019)[5])[[1]][[1]],
                 list(as.vector(i2019)[3])[[1]][[1]],
                 list(as.vector(i2019)[4])[[1]][[1]],
                 list(as.vector(i2019)[5])[[1]][[1]],
                 list(as.vector(j2019)[3])[[1]][[1]],
                 list(as.vector(j2019)[4])[[1]][[1]],
                 list(as.vector(j2019)[5])[[1]][[1]],
                 list(as.vector(j2019)[6])[[1]][[1]],
                 list(as.vector(j2019)[7])[[1]][[1]],
                 list(as.vector(j2019)[8])[[1]][[1]],
                 list(as.vector(j2019)[9])[[1]][[1]],
                 list(as.vector(j2019)[10])[[1]][[1]],
                 list(as.vector(j2019)[11])[[1]][[1]],
                 list(as.vector(j2019)[12])[[1]][[1]],
                 list(as.vector(j2019)[13])[[1]][[1]],
                 list(as.vector(k2019)[3])[[1]][[1]],
                 list(as.vector(k2019)[4])[[1]][[1]],
                 list(as.vector(k2019)[5])[[1]][[1]],
                 list(as.vector(k2019)[6])[[1]][[1]],
                 list(as.vector(k2019)[7])[[1]][[1]],
                 list(as.vector(k2019)[8])[[1]][[1]],
                 list(as.vector(k2019)[9])[[1]][[1]],
                 list(as.vector(k2019)[10])[[1]][[1]],
                 list(as.vector(k2019)[11])[[1]][[1]],
                 list(as.vector(k2019)[12])[[1]][[1]],
                 list(as.vector(m2019)[3])[[1]][[1]],
                 list(as.vector(m2019)[4])[[1]][[1]],
                 list(as.vector(m2019)[5])[[1]][[1]],list(as.vector(m2019)[6])[[1]][[1]],
                 list(as.vector(m2019)[7])[[1]][[1]],list(as.vector(m2019)[8])[[1]][[1]],
                 list(as.vector(m2019)[9])[[1]][[1]],
                 list(as.vector(m2019)[10])[[1]][[1]],list(as.vector(m2019)[11])[[1]][[1]],
                 list(as.vector(m2019)[12])[[1]][[1]],list(as.vector(m2019)[13])[[1]][[1]],
                 list(as.vector(m2019)[14])[[1]][[1]],
                 list(as.vector(m2019)[15])[[1]][[1]],list(as.vector(m2019)[16])[[1]][[1]],
                 list(as.vector(m2019)[17])[[1]][[1]],list(as.vector(m2019)[18])[[1]][[1]],
                 list(as.vector(m2019)[19])[[1]][[1]],list(as.vector(m2019)[20])[[1]][[1]],
                 list(as.vector(m2019)[21])[[1]][[1]],list(as.vector(m2019)[22])[[1]][[1]],
                 list(as.vector(m2019)[23])[[1]][[1]],
                 list(as.vector(m2019)[24])[[1]][[1]],list(as.vector(m2019)[25])[[1]][[1]],
                 list(as.vector(m2019)[26])[[1]][[1]],
                 list(as.vector(m2019)[27])[[1]][[1]],
                 list(as.vector(m2019)[28])[[1]][[1]],
                 list(as.vector(l2019)[2])[[1]][[1]]
                  )

joined_2020_sp <- list()
joined_2020_sp <- c(1,
                 list(as.vector(b2020)[3])[[1]][[1]],
                 list(as.vector(c2020)[3])[[1]][[1]],
                 list(as.vector(d2020)[3])[[1]][[1]],
                 list(as.vector(e2020)[3])[[1]][[1]],
                 list(as.vector(f2020)[3])[[1]][[1]],
                 list(as.vector(f2020)[4])[[1]][[1]],
                 list(as.vector(g2020)[3])[[1]][[1]],
                 list(as.vector(g2020)[4])[[1]][[1]],
                 list(as.vector(g2020)[5])[[1]][[1]],
                 list(as.vector(g2020)[6])[[1]][[1]],
                 list(as.vector(h2020)[3])[[1]][[1]],
                 list(as.vector(h2020)[4])[[1]][[1]],
                 list(as.vector(h2020)[5])[[1]][[1]],
                 list(as.vector(i2020)[3])[[1]][[1]],
                 list(as.vector(i2020)[4])[[1]][[1]],
                 list(as.vector(i2020)[5])[[1]][[1]],
                 list(as.vector(j2020)[3])[[1]][[1]],
                 list(as.vector(j2020)[4])[[1]][[1]],
                 list(as.vector(j2020)[5])[[1]][[1]],
                 list(as.vector(j2020)[6])[[1]][[1]],
                 list(as.vector(j2020)[7])[[1]][[1]],
                 list(as.vector(j2020)[8])[[1]][[1]],
                 list(as.vector(j2020)[9])[[1]][[1]],
                 list(as.vector(j2020)[10])[[1]][[1]],
                 list(as.vector(j2020)[11])[[1]][[1]],
                 list(as.vector(j2020)[12])[[1]][[1]],
                 list(as.vector(j2020)[13])[[1]][[1]],
                 list(as.vector(k2020)[3])[[1]][[1]],
                 list(as.vector(k2020)[4])[[1]][[1]],
                 list(as.vector(k2020)[5])[[1]][[1]],
                 list(as.vector(k2020)[6])[[1]][[1]],
                 list(as.vector(k2020)[7])[[1]][[1]],
                 list(as.vector(k2020)[8])[[1]][[1]],
                 list(as.vector(k2020)[9])[[1]][[1]],
                 list(as.vector(k2020)[10])[[1]][[1]],
                 list(as.vector(k2020)[11])[[1]][[1]],
                 list(as.vector(k2020)[12])[[1]][[1]],
                 list(as.vector(l2020)[2])[[1]][[1]]
                 )


# OAXACA-BLINDER----

#without decomposition
 # diff_total <- joined_2019*modelo2019_adj-joined_2017*modelo2017_adj



#threefold
diff_endowmA <- (joined_2019-joined_2017)*modelo2017_adj
diff_coefsA <- joined_2017*(modelo2019_adj-modelo2017_adj)
diff_interA <- (joined_2019-joined_2017)*(modelo2019_adj-modelo2017_adj)

diff_endowmA_rj <- (joined_2019_rj-joined_2017_rj)*modelo2017_adj_rj
diff_coefsA_rj <- joined_2017_rj*(modelo2019_adj_rj-modelo2017_adj_rj)
diff_interA_rj <- (joined_2019_rj-joined_2017_rj)*(modelo2019_adj_rj-modelo2017_adj_rj)

diff_endowmA_sp <- (joined_2019_sp-joined_2017_sp)*modelo2017_adj_sp
diff_coefsA_sp <- joined_2017_sp*(modelo2019_adj_sp-modelo2017_adj_sp)
diff_interA_sp <- (joined_2019_sp-joined_2017_sp)*(modelo2019_adj_sp-modelo2017_adj_sp)

#--

diff_endowmB <- (joined_2020-joined_2019)*modelo2019_adj
diff_coefsB <- joined_2019*(modelo2020_adj-modelo2019_adj)
diff_interB <- (joined_2020-joined_2019)*(modelo2020_adj-modelo2019_adj)

diff_endowmB_rj <- (joined_2020_rj-joined_2019_rj)*modelo2019_adj_rj
diff_coefsB_rj <- joined_2019_rj*(modelo2020_adj_rj-modelo2019_adj_rj)
diff_interB_rj <- (joined_2020_rj-joined_2019_rj)*(modelo2020_adj_rj-modelo2019_adj_rj)

diff_endowmB_sp <- (joined_2020_sp-joined_2019_sp)*modelo2019_adj_sp
diff_coefsB_sp <- joined_2019_sp*(modelo2020_adj_sp-modelo2019_adj_sp)
diff_interB_sp <- (joined_2020_sp-joined_2019_sp)*(modelo2020_adj_sp-modelo2019_adj_sp)

# diff_endowmA_w <- (joined_2019[65]-joined_2017[65])*modelo2017_adj[65]

#twofold
diff_explicadoA <- joined_diff2019_17*media_coeficientesA
diff_explicadoB <- joined_diff2020_19*media_coeficientesB
diff_nexplicado2019A <- joined_2019*(as.vector(modelo2019_adj)-media_coeficientesA)
diff_nexplicado2019B <- joined_2019*(media_coeficientesB-as.vector(modelo2019_adj))
diff_nexplicado2017 <- joined_2017*(media_coeficientesA-as.vector(modelo2017_adj))
diff_nexplicado2020 <- joined_2020*(as.vector(modelo2020_adj)-media_coeficientesB)

# output_modelo2020 <- joined_2020*(as.vector(modelo2020$coefficients))
# output_modelo2019 <- joined_2019*(as.vector(modelo2019$coefficients))

svyplot(formula=log(VD4016_real)~VD4031, design=subset(dados2019, VD4031>0), style="bubble", xlab="grupamentos", ylab="Rendimento Hab.", basecol="darkgreen")

svyplot(formula=log(VD4016_real)~VD4031, design=dados2019, style="bubble", xlab="grupamentos", ylab="Rendimento Hab.", basecol="green")

svyplot(formula=log(VD4016_real)~VD4031, design=subset(dados2017, VD4031<100), style="bubble", xlab="grupamentos", ylab="Rendimento Hab.", basecol="darkred")

svyplot(formula=log(VD4016_real)~VD4031, design=dados2017, style="bubble", xlab="grupamentos", ylab="Rendimento Hab.", basecol="tomato")

svyplot(formula=log(VD4016_real)~VD4010_dummy, design=subset(dados1,log(VD4016_real)<14 & log(VD4016_real)>2), style="bubble", xlab="grupamentos", ylab="Rendimento Hab.", basecol="tomato")

svyplot(formula=log(VD4016_real)~VD4031, design=dados1, style="bubble", xlab="horas trabalhadas", ylab="Rendimento Hab.", basecol="tomato")

install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)

modelo_corrigido <- coeftest(modelo2019, vcov = vcovHC(modelo2019))
summary(modelo_corrigido)

#ATALHOS---------------------------------------

# Deflaciona variaveis de rendimento
dados1$variables <- transform(dados1$variables, VD4016_real=VD4016*Habitual)
svymean(x=~VD4016_real,design = dados4,na.rm=T)

# Formas de se obter totais -> estimacao pontual
svytotal(x= ~VD4003, design = dados, na.rm = TRUE)
svytotal(x=~VD4002,design = dados, na.rm=T)
svytotal(x=~VD4002,subset(dados,VD4017>=2500),na.rm=T)

# Estima??o de uma variavel a partir de outra
svyby(formula=~VD4016_real, by=~V2007, design=dados2, FUN=svymean, na.rm=T)

#Intervalos de confian?a
teste <- svymean(x=~VD4016,design = dados,na.rm=T)
confint(teste, level=0.99)
rm(teste)

#Histograma
svyhist(~as.numeric(VD4035), dados4, main = "Histograma", xlab="Rendimento h recebido")

#Gr?ficos de dispers?o
svyplot(formula=VD4016_real~VD4035, design=dados, style="bubble", xlab="Horas trabalhadas", ylab="Rendimento")

#Excluir
#Estima??o de uma variavel a partir de outra
dados1 <- get_pnadc(year=2021, quarter=3, vars = c("VD2006", "VD4002"), deflator=TRUE)

teste <- svyratio(numerator =~(VD4002=="Pessoas ocupadas"), denominator=~(VD2006=="20 a 24 anos"), design=dados1, na.rm=TRUE)
df <- as.data.frame(teste$var)

teste2 <- svytotal(x= ~VD2006, design = dados1, na.rm = TRUE)
df <- df + as.data.frame(teste2)

#Variaveis------
#VD5001 Renda domiciliar per capita
#VD4002 Condi??o de ocupa??o: ocupados-desocupadosx
#VD4001 Condi??o na forca de trab.: PEA-NPEA
#VD4016 Rendimento mensal habitual do principal
#RM_RIDE regi?o metropolitana
#VD4010 Setor de emprego (portanto, distribui??o de ocupados)x
#V2007 Sexo
#V2009 Idade
#VD3004 Instrucao
#VD4012 Previ
#V2010 Cor
#VD4017 Rendimento mensal efetivo do principalx
#VD4020 Rendimento mensal efetivo de todosx
#VD4035 Horas efetivamente trabalhadas
#VD2006 Faixas et?rias
#VD4001 PEA
#VD4009 Posiçao na ocupação e categoria

#----------------------








