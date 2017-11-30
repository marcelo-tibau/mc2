# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 4: Gerar a analise de tamanho de efeito: comparativo de qualidade para cada instancia e pares de abordagens NSGAII x MAR , NSGAII x SH e NSGAII x CPM (pagina 1065)
data_t4 <- read.table("data_t3-t4.txt", header = T)
result <- list()
DIGIT <- 4

## Loop para gerar o subset com mean e SD 
for (i in c('CPM', 'MAR', 'SH', 'nsga150k2x')) {
  dataQfour <- data_t4[ which(data_t4$config == i), ]
  result[[i]] <- cbind(
    best = c(round(mean(dataQfour$best),  digits=DIGIT), round(sd(dataQfour$best),  digits=DIGIT)),
    hv = c(round(mean(dataQfour$hv),  digits=DIGIT), round(sd(dataQfour$hv),  digits=DIGIT)),
    gd = c(round(mean(dataQfour$gd),  digits=DIGIT), round(sd(dataQfour$gd),  digits=DIGIT))
  )
}

# Construcao do Effect Size com base em https://www.leeds.ac.uk/educol/documents/00002182.htm
effectsize <- function (one, two) {
  effectsize_result <- cbind(
    best = (one[1,][1] - two[1,][1]) / one[2,][1],
    hv = (one[1,][2] - two[1,][2]) / one[2,][2],
    gd = (one[1,][3] - two[1,][3]) / one[2,][3]
  )
  
  return(effectsize_result)
}

# Print das respostas
print(effectsize(result$nsga150k2x, result$CPM))
print(effectsize(result$nsga150k2x, result$MAR))
print(effectsize(result$nsga150k2x, result$SH))


