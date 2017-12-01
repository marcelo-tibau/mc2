# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 5: Gerar a an√°lise comparativa de qualidade para cada inst√¢ncia entre NSGAII x NSGASE (tabela 3).
data <- read.table("data_t5-t6.txt", header = T)
DIGIT <- 4

instances = unique(data$inst)

for (instance_name in instances) {
  subdata <- data[ which(data$config == 'nsga150k2x' & data$inst == instance_name), ]
  
  nsga150k2x <- cbind(
    best = paste(round(mean(subdata$best) * 100, digits=DIGIT), "±", round(sd(subdata$best) * 100, digits=DIGIT)),
    hv = paste(round(mean(subdata$hv), digits=DIGIT), "±",round(sd(subdata$hv), digits=DIGIT)),
    gd = paste(round(mean(subdata$gd), digits=DIGIT), "±", round(sd(subdata$gd), digits=DIGIT))
  )
  
  # 
  
  subdata <- data[ which(data$config == 'nsga150k2xse' & data$inst == instance_name), ]
  
  nsga150k2xse <- cbind(
    best = paste(round(mean(subdata$best) * 100, digits=DIGIT), "±", round(sd(subdata$best)* 100, digits=DIGIT)),
    hv = paste(round(mean(subdata$hv), digits=DIGIT), "±",round(sd(subdata$hv), digits=DIGIT)),
    gd = paste(round(mean(subdata$gd), digits=DIGIT), "±", round(sd(subdata$gd), digits=DIGIT))
  )
  
  # result <- matrix(data=c(nsga150k2x, nsga150k2xse), nrow=3, ncol=2)
  print("====================")
  print(instance_name)
  print("====================")
  print(nsga150k2x)
  print(nsga150k2xse)
}