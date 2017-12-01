# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 4: Gerar a analise de tamanho de efeito: comparativo de qualidade para cada instancia e pares de abordagens NSGAII x MAR , NSGAII x SH e NSGAII x CPM (pagina 1065)

DIGIT <- 4

AMeasure <- function(r1, r2) 
{
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

data <- read.table("data_t3-t4.txt", header = TRUE)
instances <- unique(data$inst)
instance_names <- list(c("MAR", "SH", "CPM"), instances)
best_result <- matrix(ncol = length(instances), nrow = 3, dimnames = instance_names)
gd_result <- matrix(ncol = length(instances), nrow = 3, dimnames = instance_names)
hv_result <- matrix(ncol = length(instances), nrow = 3, dimnames = instance_names)
i <- 1

for (instance in instances) {
  mar_data = data[ which(data$config == 'MAR' & data$inst == instance), ]
  sh_data = data[ which(data$config == 'SH' & data$inst == instance), ]
  cpm_data = data[ which(data$config == 'CPM' & data$inst == instance), ]
  nsga_data = data[ which(data$config == 'nsga150k2x' & data$inst == instance), ]
  
  best_result[1, i] <- AMeasure(nsga_data$best, mar_data$best)
  best_result[2, i] <- AMeasure(nsga_data$best, sh_data$best)
  best_result[3, i] <- AMeasure(nsga_data$best, cpm_data$best)
  
  hv_result[1, i] <- AMeasure(nsga_data$hv, mar_data$hv)
  hv_result[2, i] <- AMeasure(nsga_data$hv, sh_data$hv)
  hv_result[3, i] <- AMeasure(nsga_data$hv, cpm_data$hv)
  
  gd_result[1, i] <- AMeasure(nsga_data$gd, mar_data$gd)
  gd_result[2, i] <- AMeasure(nsga_data$gd, sh_data$gd)
  gd_result[3, i] <- AMeasure(nsga_data$gd, cpm_data$gd)
  
  i <- i + 1
}

print("Best ===============")
print(best_result)

print("HV ===============")
print(hv_result)

print("GD ===============")
print(gd_result)

#### GD (quanto mais próximo de 1, pior o resultado)
#### HD (tem alguns 0 mesmo)