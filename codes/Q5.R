# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 5: Gerar a análise comparativa de qualidade para cada instância entre NSGAII x NSGASE (tabela 3).
data_t5 <- read.table("data_t5-t6.txt", header = T)
DIGIT <- 4

## Códigos para I0
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(0)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one0 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI0 <- cbind(
    best = paste(round(mean(dataQ5one0$best), digits=DIGIT), "±", round(sd(dataQ5one0$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one0$hv), digits=DIGIT), "±",round(sd(dataQ5one0$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one0$gd), digits=DIGIT), "±", round(sd(dataQ5one0$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two0 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI0 <- cbind(
    best = paste(round(mean(dataQ5two0$best), digits=DIGIT), "±", round(sd(dataQ5two0$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two0$hv), digits=DIGIT), "±",round(sd(dataQ5two0$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two0$gd), digits=DIGIT), "±", round(sd(dataQ5two0$gd), digits=DIGIT))
  )
  
}


## Códigos para I1
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(1)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one1 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI1 <- cbind(
    best = paste(round(mean(dataQ5one1$best), digits=DIGIT), "±", round(sd(dataQ5one1$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one1$hv), digits=DIGIT), "±",round(sd(dataQ5one1$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one1$gd), digits=DIGIT), "±", round(sd(dataQ5one1$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two1 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI1 <- cbind(
    best = paste(round(mean(dataQ5two1$best), digits=DIGIT), "±", round(sd(dataQ5two1$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two1$hv), digits=DIGIT), "±",round(sd(dataQ5two1$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two1$gd), digits=DIGIT), "±", round(sd(dataQ5two1$gd), digits=DIGIT))
  )
  
}

## Códigos para I2
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(2)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one2 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI2 <- cbind(
    best = paste(round(mean(dataQ5one2$best), digits=DIGIT), "±", round(sd(dataQ5one2$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one2$hv), digits=DIGIT), "±",round(sd(dataQ5one2$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one2$gd), digits=DIGIT), "±", round(sd(dataQ5one2$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two2 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI2 <- cbind(
    best = paste(round(mean(dataQ5two2$best), digits=DIGIT), "±", round(sd(dataQ5two2$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two2$hv), digits=DIGIT), "±",round(sd(dataQ5two2$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two2$gd), digits=DIGIT), "±", round(sd(dataQ5two2$gd), digits=DIGIT))
  )
  
}

## Códigos para I3
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(3)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one3 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI3 <- cbind(
    best = paste(round(mean(dataQ5one3$best), digits=DIGIT), "±", round(sd(dataQ5one3$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one3$hv), digits=DIGIT), "±",round(sd(dataQ5one3$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one3$gd), digits=DIGIT), "±", round(sd(dataQ5one3$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two3 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI3 <- cbind(
    best = paste(round(mean(dataQ5two3$best), digits=DIGIT), "±", round(sd(dataQ5two3$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two3$hv), digits=DIGIT), "±",round(sd(dataQ5two3$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two3$gd), digits=DIGIT), "±", round(sd(dataQ5two3$gd), digits=DIGIT))
  )
  
}


## Códigos para I4
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(4)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one4 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI4 <- cbind(
    best = paste(round(mean(dataQ5one4$best), digits=DIGIT), "±", round(sd(dataQ5one4$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one4$hv), digits=DIGIT), "±",round(sd(dataQ5one4$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one4$gd), digits=DIGIT), "±", round(sd(dataQ5one4$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two4 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI4 <- cbind(
    best = paste(round(mean(dataQ5two4$best), digits=DIGIT), "±", round(sd(dataQ5two4$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two4$hv), digits=DIGIT), "±",round(sd(dataQ5two4$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two4$gd), digits=DIGIT), "±", round(sd(dataQ5two4$gd), digits=DIGIT))
  )

}


## Códigos para I5
# Loop para gerar o subset com a config nsga150k2x, mean e SD
for (i in c(5)) {
  instance_name <- paste('I', i, sep="")
  
  dataQ5one5 <- data_t5[ which(data_t5$config == 'nsga150k2x' & data_t5$inst == instance_name), ]
  
  nsga150k2xI5 <- cbind(
    best = paste(round(mean(dataQ5one5$best), digits=DIGIT), "±", round(sd(dataQ5one5$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5one5$hv), digits=DIGIT), "±",round(sd(dataQ5one5$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5one5$gd), digits=DIGIT), "±", round(sd(dataQ5one5$gd), digits=DIGIT))
  )
  
  # para gerar a config nsga150k2xse, mean e SD
  
  dataQ5two5 <- data_t5[ which(data_t5$config == 'nsga150k2xse' & data_t5$inst == instance_name), ]
  
  nsga150k2xseI5 <- cbind(
    best = paste(round(mean(dataQ5two5$best), digits=DIGIT), "±", round(sd(dataQ5two5$best), digits=DIGIT)),
    hv = paste(round(mean(dataQ5two5$hv), digits=DIGIT), "±",round(sd(dataQ5two5$hv), digits=DIGIT)),
    gd = paste(round(mean(dataQ5two5$gd), digits=DIGIT), "±", round(sd(dataQ5two5$gd), digits=DIGIT))
  )
  
}

## Resultados
# Print dos resultados I0
print(nsga150k2xI0)
print(nsga150k2xseI0)

# Print dos resultados I1
print(nsga150k2xI1)
print(nsga150k2xseI1)

# Print dos resultados I2
print(nsga150k2xI2)
print(nsga150k2xseI2)

# Print dos resultados I3
print(nsga150k2xI3)
print(nsga150k2xseI3)

# Print dos resultados I4
print(nsga150k2xI4)
print(nsga150k2xseI4)

# Print dos resultados I5
print(nsga150k2xI5)
print(nsga150k2xseI5)