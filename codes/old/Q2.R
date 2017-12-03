# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 2: Gerar a analise do criterio de tamanho de populacao do algoritmo genetico (pg. 1064).
data_t2 <- read.table("data_t2.txt", header = T)

# Subset das instâncias
inst_I0_2 <- subset(data_t2, inst == "I0");
inst_I1_2 <- subset(data_t2, inst == "I1");
inst_I2_2 <- subset(data_t2, inst == "I2");
inst_I3_2 <- subset(data_t2, inst == "I3");
inst_I4_2 <- subset(data_t2, inst == "I4");
inst_I5_2 <- subset(data_t2, inst == "I5");

# Kruskal test

kruskal.test(gd~config, data=inst_I0_2);

kruskal.test(gd~config, data=inst_I1_2);

kruskal.test(gd~config, data=inst_I2_2);

kruskal.test(gd~config, data=inst_I3_2);

kruskal.test(gd~config, data=inst_I4_2);

kruskal.test(gd~config, data=inst_I5_2);

# Pairwise Wilcox

pairwise.wilcox.test(inst_I0_2$gd, inst_I0_2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I1_2$gd, inst_I1_2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I2_2$gd, inst_I2_2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I3_2$gd, inst_I3_2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I4_2$gd, inst_I4_2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I5_2$gd, inst_I5_2$config, p.adj="bonferroni", exact=F);
