# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 6: Gerar a análise de tamanho de efeito: comparativo de qualidade para cada instância e pares de abordagens NSGAII x NSGANE (página 1065).
data_t6 <- read.table("data_t5-t6.txt", header = T)
DIGIT <- 4

# Construcao do Effect Size com base em https://www.leeds.ac.uk/educol/documents/00002182.htm
effectsize <- function (one, two) {
  effectsize_result <- cbind(
    best = (one[1,][1] - two[1,][1]) / one[2,][1],
    hv = (one[1,][2] - two[1,][2]) / one[2,][2],
    gd = (one[1,][3] - two[1,][3]) / one[2,][3]
  )
  
  return(effectsize_result)
}

# Configuracao para as Instancias I0 a I5
for (i in c(0:5)) {
  instance_name <- paste('I', i, sep="")
  result <- list()
  
  for (config in c('nsga150k2x', 'nsga150k2xse')) {
    subdata <- data_t6[ which(data_t6$config == config & data_t6$inst == instance_name), ]
    result[[config]] <- cbind(
      best = c(round(mean(subdata$best), digits=DIGIT), round(sd(subdata$best), digits=DIGIT)),
      hv = c(round(mean(subdata$hv), digits=DIGIT), round(sd(subdata$hv), digits=DIGIT)),
      gd = c(round(mean(subdata$gd), digits=DIGIT), round(sd(subdata$gd), digits=DIGIT))
    )
  }
  
  print(effectsize(result$nsga150k2x, result$nsga150k2xse))
}
