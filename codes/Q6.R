# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 6: Gerar a análise de tamanho de efeito: comparativo de qualidade para cada instância e pares de abordagens NSGAII x NSGANE (página 1065).
rm(list=ls())
DIGIT <- 4
options(scipen=10)

data <- read.table("data_t5-t6.txt", header = TRUE)
instances = unique(data$inst)

AMeasure <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

instance_names <- list(c('Best', 'HV', 'GD'), instances)
result <- matrix(ncol = length(instances), nrow = 3, dimnames = instance_names)
i <- 1

for(instance in instances) {
  nsga150k2x_data = data[ which(data$config == 'nsga150k2x' & data$inst == instance), ]
  nsga150k2xse_data = data[ which(data$config == 'nsga150k2xse' & data$inst == instance), ]
  
  result[1, i] <- AMeasure(nsga150k2x_data$best, nsga150k2xse_data$best)
  result[2, i] <- AMeasure(nsga150k2x_data$hv, nsga150k2xse_data$hv)
  result[3, i] <- AMeasure(nsga150k2x_data$gd, nsga150k2xse_data$gd)
  
  i <- i + 1
}

print(result)