# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 3: Gerar a analise comparativa de qualidade para cada instancia entre a NSGAII, MAR, SH, CPM (Tabela 2)
data_t3 <- read.table("data_t3-t4.txt", header = T, sep = "\t")

# Analise I0

instI0 <- subset(data_t3, inst == "I0" & config == "nsga150k2x"); 

meanI0Best <- mean(instI0$best); 
meanI0Best

sdI0Best <- sd(instI0$best);
sdI0Best

# Analise I1

instI1 <- subset(data_t3, inst == "I1" & config == "nsga150k2x"); 

meanI1Best <- mean(instI1$best); 
meanI1Best

sdI1Best <- sd(instI1$best);
sdI1Best

# Analise I2

instI2 <- subset(data_t3, inst == "I2" & config == "nsga150k2x"); 

meanI2Best <- mean(instI2$best); 
meanI2Best

sdI2Best <- sd(instI2$best);
sdI2Best

# Analise I3
instI3 <- subset(data_t3, inst == "I3" & config == "nsga150k2x"); 

meanI3Best <- mean(instI3$best); 
meanI3Best

sdI3Best <- sd(instI3$best);
sdI3Best

# Analise I4
instI4 <- subset(data_t3, inst == "I4" & config == "nsga150k2x"); 

meanI4Best <- mean(instI4$best); 
meanI4Best

sdI4Best <- sd(instI4$best);
sdI4Best

# Analise I5
instI5 <- subset(data_t3, inst == "I5" & config == "nsga150k2x"); 

meanI5Best <- mean(instI5$best); 
meanI5Best

sdI5Best <- sd(instI5$best);
sdI5Best