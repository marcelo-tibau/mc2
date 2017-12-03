# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 3: Gerar a analise comparativa de qualidade para cada instancia entre a NSGAII, MAR, SH, CPM (Tabela 2)
data_t3 <- read.table("dados/data_t3-t4.txt", header = T)

DIGIT = 4

instancias <- unique(data_t3$inst)

options(scipen=999)
options(digits=4)

for (instancia in instancias)
{
  print("-----------------------------------------------------")
  print(instancia)
  print("-----------------------------------------------------")
  
  mar_data = data_t3[ which(data_t3$config == 'MAR' & data_t3$inst == instancia), ]
  mar = cbind(
    best = mar_data$best,
    hv = mar_data$hv,
    gd = mar_data$gd
  )
  
  sh_data = data_t3[ which(data_t3$config == 'SH' & data_t3$inst == instancia), ]
  sh = cbind(
    best = sh_data$best,
    hv = sh_data$hv,
    gd = sh_data$gd
  )
  
  cpm_data = data_t3[ which(data_t3$config == 'CPM' & data_t3$inst == instancia), ]
  cpm = cbind(
    best = cpm_data$best,
    hv = cpm_data$hv,
    gd = cpm_data$gd
  )
  
  nsga_data = data_t3[ which(data_t3$config == 'nsga150k2x' & data_t3$inst == instancia), ]
  nsga = cbind(
    best = paste(formatC(round(mean(nsga_data$best), digits=DIGIT), digits = 4, format = "f"), "±", formatC(round(sd(nsga_data$best), digits=DIGIT), digits = 4, format = "f")),
    hv = paste(formatC(round(mean(nsga_data$hv), digits=DIGIT), digits = 4, format = "f"), "±", formatC(round(sd(nsga_data$hv), digits=DIGIT), digits = 4, format = "f")),
    gd = paste(formatC(round(mean(nsga_data$gd), digits=DIGIT), digits = 4, format = "f"), "±", formatC(round(sd(nsga_data$gd), digits=DIGIT), digits = 4, format = "f"))
  )
  
  tab2 <- data.frame(row.names= c('I_CV','I_HV','I_GD'),
                     NSGAII = c(nsga),
                     MAR = c(mar),
                     SH = c(sh),
                     CPM = c(cpm)
  )
  print(tab2)
}