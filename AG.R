#PARÂMETROS NECESSÁRIOS PARA O CÓDIGO
graph_nodes <- 10 #QUANTIA DE NÓS NO GRAFO

tam_pop <- 10 #TAMANHO DA POPULAÇÃO
tam_crom <- graph_nodes #TAMANHO DO CROMOSSOMO
max_ger <- 100 #NÚMERO MÁXIMO DE GERAÇÕES

graph_row <-
  graph_nodes #NÚMERO DE LINHAS NA MATRIZ QUE REPRESENTA O GRAFO
graph_col <-
  graph_nodes #NÚMERO DE COLUNAS NA MATRIZ QUE REPRESENTA O GRAFO

initial_select_prob <- 0.4
initial_mutate_prob <- 0.7

#FUNÇÃO ONDE O AG SERÁ REALIZADO
ag_maxclique <- function() {
  set.seed(sample(10^5,1)) #SEMENTE
  
  grafo <- gera_grafo()
  cat("grafo:\n")
  print(grafo)
  
  populacao <- gera_pop_ini(grafo)
  soma_fitness <- -100
  conta_estagnação <- 0
  
  for (i in 1:max_ger) {
    if (conta_estagnação == 20) {
      break
    }
    
    fitness <- calcula_fitness(populacao, grafo)
    parentes <- seleciona_pais(fitness, populacao)
    if (parentes[1] == -3 || parentes[2] == 3) {
      index_fit_max <- which(fitness == max(fitness))
      return(cat("\nclique máximo: ",populacao[index_fit_max[1],],
                 "\ntamanho: ", sum(populacao[index_fit_max[1],])))
    }
    populacao <- crossover(parentes, populacao, grafo, fitness)
    temp <- soma_fitness
    soma_fitness <- sum(fitness)
    
    if (temp == soma_fitness) {
      conta_estagnação <- conta_estagnação + 1
    }
  }
  
  index_fit_max <- which(fitness == max(fitness))
  return(cat("\nclique máximo: ",populacao[index_fit_max[1],],
             "\ntamanho: ", sum(populacao[index_fit_max[1],])))
}

#GERA O GRAFO USADO PARA O PROBLEMA
gera_grafo <- function() {
  #AQUI GERAMOS UMA MATRIZ BINARIA
  grafo <-
    matrix(
      data = sample(c(0, 1), size = graph_row * graph_col, replace = TRUE),
      nrow = graph_row,
      ncol = graph_col
    )
  
  #PRECISAMOS AJUSTAR A MATRIZ GERADA PARA UM GRAFO VÁLIDO
  for (i in 1:graph_col) {
    for (j in 1:graph_row) {
      if (grafo[i, j] == 1 && grafo[j, i] == 0) {
        grafo[j, i] <- 1
      }
    }
  }
  
  #POR NÃO SER UM GRAFO DIRECIONADO NÃO HÁ NECESSIDADE DE VERIFICAR SE HÁ CLIQUE
  #QUAISQUER DOIS VERTICES LIGADOS JÁ SÃO CONSIDERADOS UM CLIQUE, APESAR DE PEQUENO
  return(grafo)
}

#GERA A POPULAÇÃO INICIAL
gera_pop_ini <- function(g) {
  populacao_inicial <- matrix(nrow = 0, ncol = tam_crom)
  
  for (i in 1:tam_pop) {
    vertices_selecionados <-
      c() #VÉRTICES SELECIONADOS PARA COMPOR O CROMOSSOMO
    cromossomo <- rep(0, tam_crom)
    
    for (i in 1:tam_crom) {
      v_sort <-
        sample(x <-
                 c(1:graph_nodes),
               size = 1,
               replace = TRUE) #SORTEIA UM NÓ DO GRAFO
      #SE O ARRAY ESTIVER SEM VÉRTICES, ADICIONA O VÉRTICE SORTEADO
      if (length(vertices_selecionados) == 0) {
        vertices_selecionados <- c(vertices_selecionados, v_sort)
      }
      #SENÃO, TEMOS DE VERIFICAR SE ELE É ADJACENTE AOS OUTROS VÉRTICES DO ARRAY
      else {
        if (!(v_sort %in% vertices_selecionados)) {
          for (z in 1:length(vertices_selecionados)) {
            #ACHOU UM VÉRTICE O QUAL ELE NÃO É ADJACENTE, QUEBRA E DESISTE DELE
            if (g[vertices_selecionados[z], v_sort] == 0) {
              break
            }
            #NÃO ACHOU UM VÉRTICE QUE NÃO É ADJACENTE E ESTÁ NO FINAL DO FOR, OU SEJA,
            #TODOS SÃO ADJACENTES
            else {
              if (z == length(vertices_selecionados)) {
                vertices_selecionados <- c(vertices_selecionados, v_sort)
              }
            }
          }
        }
      }
    }
    
    #CONVERTENDO OS VÉRTICES SELECIONADOS PARA CROMOSSOMO BINÁRIO
    for (i in 1:length(vertices_selecionados)) {
      cromossomo[vertices_selecionados[i]] <- 1
    }
    
    #DEVEMOS AGORA MUTAR O CROMOSSOMO PARA DIFERENCIAR OS MEMBROS DA POPULAÇÃO INICIAL
    sorteio <-
      sample(
        x = c(0, 1),
        size = 1,
        replace = TRUE,
        prob = c(1 - initial_select_prob , initial_select_prob)
      )
    if (sorteio == 1) {
      for (i in 1:tam_crom) {
        sorteio <-
          sample(
            x = c(0, 1),
            size = 1,
            replace = TRUE,
            prob = c(1 - initial_mutate_prob, initial_mutate_prob)
          )
        if (sorteio == 1) {
          if (cromossomo[i] == 0) {
            cromossomo[i] <- 1
          } else {
            cromossomo[i] <- 0
          }
        }
      }
    }
    populacao_inicial <-
      rbind(populacao_inicial, as.vector(cromossomo))
  }
  return(populacao_inicial)
}

#VERIFICA SE DADO CROMOSSOMO REPRESENTA UM CLIQUE
is_clique <- function(g, cromossomo) {
  for (i in 1:(tam_crom - 1)) {
    for (j in (i+1):tam_crom) {
      if((cromossomo[i] == 1) && (cromossomo[j] == 1)) {
        if(g[i,j] == 0) {
          return (0)
        }
      }
    }
  }
  return(1)
}

#CALULA O FITNESS DOS MEMBROS DE UMA GERAÇÃO
calcula_fitness <- function(p, g) {
  fitness <- c()
  #CALCULAR FITNESS PARA CADA MEMBRO DA POPULAÇÃO
  for (i in 1:tam_pop) {
    clique <- is_clique(g, p[i,])
    if (clique == 0) {
      fitness <- c(fitness, -1)
    } else {
      fitness <- c(fitness, sum(p[i,]))
    }
  }
  return(fitness)
}

#SELECIONA OS PAIS DA GERAÇÃO USANDO ROLETA
seleciona_pais <- function(f, p) {
  if (length(f) == 0 || any(is.na(f))) {
    stop("Vetor de probabilidades inválido.")
  }
  
  #VAMOS NIVELAR O FITNESS A UM INTERVALO DE [0, 1]
  f <- (f - min(f)) / (max(f) - min(f))
  probabilidades <- f/sum(f)
  
  if (any(is.na(probabilidades))) {
    return(-3)
  }
  
  #SELECIONA OS PAIS
  pai1_index <- sample.int(nrow(p), size = 1, replace = TRUE, prob = probabilidades)
  pai2_index <- sample.int(nrow(p), size = 1, replace = TRUE, prob = probabilidades)
  
  return(c(pai1_index, pai2_index))
}


#GERA OS FILHOS A PARTIR DOS PAIS COM VARIOS PONTOS DE CORTE
crossover <- function(pais, pop, grafo, fitness_pop) {
  pai1 <- pop[pais[1],]
  pai2 <- pop[pais[2],]
  filho1 <- c()
  filho2 <- c()
  
  for(i in 1:tam_crom) {
    if (i%%2 == 0) {
      filho1 <- c(filho1, pai1[i])
      filho2 <- c(filho2, pai2[i])
    } else {
      filho1 <- c(filho1, pai2[i])
      filho2 <- c(filho2, pai1[i])
    }
  }
  
  if (is_clique(grafo, filho1) == 1) {
    fitness_filho1 <- sum(filho1)
  } else {
    fitness_filho1 <- -1
  }
  
  if (is_clique(grafo, filho2) == 1) {
    fitness_filho2 <- sum(filho2)
  } else {
    fitness_filho2 <- -1
  }
  
  for(i in 1:tam_pop) {
    if(fitness_filho1 > fitness_pop[i]) {
      temp <- pop[i,]
      pop[i,] <- filho1
      filho1 <- temp
    } else if(fitness_filho2 > fitness_pop[i]) {
      temp2 <- pop[i,]
      pop[i,] <- filho2
      filho2 <- temp2
    }
  }
  
  return(pop)
}
