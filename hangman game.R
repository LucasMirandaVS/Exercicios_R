# Fazendo um jogo da forca em R

library(ggplot2)

#########################
### Funções de auxilio ##
#########################
substituto <- function(palavra, controle, psq_palavra){
  if (regexpr(contole, palavra)[1] > 0){
    palavra <- base::tolower(palavra)
    controle <- base:: tolower(controle)
    
    posicao <- which(strsplit(palavra, "")[[1]]==palavra)
    psq_palavra[posicao] <- palavra
    
    message(paste(psq_palavra, collapse = " "))
    
    # converte de volta pra uma unica string para checar se há igualdade
    if(paste(psq_palavra, collapse = "") == palavra) {
      return(psq_palavra)
      message("Fim de Jogo!")
    }
    
    return(psq_palavra)
  }
}

# Agora para desenhar a cabeça
desenhoCabeca <- function(orig_position = c(0,0),
                          dia = 1,
                          nof_points = 10,
                          group = 5){
  vectT <- seq(0,2*pi, lenght.out = nof_points)
  r <- dia/2
  x_data <- orig_position[1] + r * cos(vectT)
  y_data <- orig_position[2] + r * sin(vectT)
  return(data.frame(x = x_data, y = y_data, group = group))
}

desenhoCorpo <- function(erros){ #, psq_palavras
  ggplot(levels[which(levels$group <= erros),],
         aes(x = x, y = y, group = group)) +
    geom_path(size = 2.5) +
    theme_void() +
    ggtitle('Jogo da forca')
  #ggtitle('Jogo da forca, a palavra é: ', psq_palavra)
  }

checkDuplicata <- function(controle, escolha){
  if(length(escolha) == 0){
    message('Sem espaço, adicionando...')
    escolha <- rbind(escolha, esco = controle)
    escolha[,1] <- as.character(escolha[,1])
  } else {
    if (grepl(controle, escolha) == TRUE) {
      escolha[,1] <- as.character(escolha[,1])
      message('existe')
    } else {
      escolha <- rbind(escolha, esco = controle)
      escolha[,1] <- as.character(escolha[,1])
      message("Adicionado...")
    }
  }
  return(escolha)
}

###########################
###### Dados pro gráfico ##
###########################
nivel1 <- data.frame(x = c(1,2,3,4,5,6,7,8),
                     y = c(1,1,1,1,1,1,1,1),
                     group = c(1,1,1,1,1,1,1,1))
nivel2 <- data.frame(x = c(4,4,4,4,4),
                     y = c(1,2,3,4,5),
                     group = c(2,2,2,2,2))
nivel3 <- data.frame(x = c(4, 5, 6),
                     y= c (5, 5, 5), 
                     group = c(3, 3, 3))
nive4 <- data.frame(x = c(6, 6),
                    y = c(5, 4),
                    group = c(4, 4))
nivel5 <- desenhoCabeca(c(6, 3.5), 1, 10, 5)
nivel6 <- data.frame(x = c(6, 6, 5.8, 6.2), 
                     y =c(3, 1.5, 1.5, 1.5),
                     group = c(6, 6, 6, 6))
nivel7 <- data.frame(x = c(5.5, 6, 6.5), 
                     y = c(2, 2.5, 2),
                     group = c(7, 7, 7))
niveis <- rbind(nivel1, nivel2, nivel3, nive4, nivel5, nivel6, nivel7)
rm(nivel1, nivel2, nivel3, nive4, nivel5, nivel6, nivel7)

###########################
### Variaveis de auxilio ##
###########################
suppressWarnings(rm(erros, escolha, controle, meta, meta_n, psq_palavra, palavra, i, active))
erros = 0
i = 0
escolha = data.frame(esco = c(NULL))
meta = 0
meta_n = data.frame(esco = c(NULL))
active = TRUE

#############
##  forca  ##
#############
novoJogo <- function(sensitive.flag = TRUE) {
  palavra <- readline(prompt = 'Palavra: ')
  
  cat('\f')
  graphics.off()
  if (sensitive.flag == FALSE) {
    palavra <- base::tolowe(palavra)
  }
  psq_palavra <- replicate(nchar(palavra), '_')
  
  while(active == TRUE){
    if ( i == 0 ){
      writeLines(paste(psq_palavra, collapse = ' '))
    }
    controle <- readline(prompt = 'Digite uma letra: ')
    if (nchar(controle)>1) message('Pegando a primeira letra')
    controle <- substr(controle, 1, 1)
    escolha <- checkDuplicata(controle, escolha)
    
    if (grepl(controle, palavra) == TRUE) {
      meta <- rbind(meta, controle)
      psq_palavra <- substituto(palavra, controle, psq_palavra)
      
      message(paste("Tu é brabao papai", "Tente novamente", i+1))
      
      if(as.character(paste(base::tolower(psq+palavra), collapse = "")) == base::tolower(palavra)){
      active == FALSE
        message("Carau tu é pica memo mané")
        breal
      }
    } else{
      meta_n <- checkDuplicata(controle = controle, escolha = meta_n)
      message(paste("Nope!", "Tente novamente", i + 1, "Letras erradas: {", 
                    (toString(paste0(meta_n[,1], sep = ","))),"}"))
      #Gráfico
      erros <- as.integer(nrow(meta_n))
      print(desenhoCorpo(erros = erros))
      
      if (as.integer(erros) == 7){
        active == FALSE
        break
        message('Fim de Jogo!')
      }
      
    }
    i = i + 1
    if(erros == 7){
      active == FALSE
      break
      message('Fim de Jogo!')
    }
  }
}
################
### Novo Jogo ##
################

novoJogo()
