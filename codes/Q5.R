# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 5: Gerar a análise comparativa de qualidade para cada instância entre NSGAII x NSGASE (tabela 3).
rm(list=ls())

DIGIT <- 4
options(scipen=10)

data <- read.table("data_t5-t6.txt", header = TRUE)
instances = unique(data$inst)
instance_names <-list(I0 = "ACAD", I5 = "WMET", I4 = "WAMS", I3 = "PSOA", I1 = "OMET", I2 = "PARM");

for (instance_name in instances) {
  subdata <- data[ which(data$config == 'nsga150k2x' & data$inst == instance_name), ]
  
  nsga150k2x <- cbind(
    best = paste(round(mean(subdata$best) * 100, digits=DIGIT), "�", round(sd(subdata$best) * 100, digits=DIGIT)),
    hv = paste(round(mean(subdata$hv), digits=DIGIT), "�",round(sd(subdata$hv), digits=DIGIT)),
    gd = paste(round(mean(subdata$gd) * 100, digits=DIGIT), "�", round(sd(subdata$gd) * 100, digits=DIGIT))
  )
  
  #
  
  subdata <- data[ which(data$config == 'nsga150k2xse' & data$inst == instance_name), ]
  
  nsga150k2xse <- cbind(
    best = paste(round(mean(subdata$best) * 100, digits=DIGIT), "�", round(sd(subdata$best)* 100, digits=DIGIT)),
    hv = paste(round(mean(subdata$hv), digits=DIGIT), "�",round(sd(subdata$hv), digits=DIGIT)),
    gd = paste(round(mean(subdata$gd) * 100, digits=DIGIT), "�", round(sd(subdata$gd) * 100, digits=DIGIT))
  )
  print("------------------------------------------")
  print(as.character(instance_names[instance_name]))
  tab2 <- data.frame(row.names= c('I_CV','I_HV','I_GD'),
                     NSGAII = c(nsga150k2x),
                     NSGA_NE = c(nsga150k2xse)
  )
  print(tab2)
  # result <- matrix(data=c(nsga150k2x, nsga150k2xse), nrow=3, ncol=2)
  
}