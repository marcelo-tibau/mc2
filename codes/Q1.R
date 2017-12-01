# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 1: Gerar a analise do criterio de parada para o algoritmo genetico (pg. 1064)
data_t1 <- read.table("data_t1.txt", header = T)

# Subset das instâncias

inst_I0 <- subset(data_t1, inst == "I0");
inst_I1 <- subset(data_t1, inst == "I1");
inst_I2 <- subset(data_t1, inst == "I2");
inst_I3 <- subset(data_t1, inst == "I3");
inst_I4 <- subset(data_t1, inst == "I4");
inst_I5 <- subset(data_t1, inst == "I5");

# Kruskal test

kruskal.test(gd~config, data=inst_I0);

kruskal.test(gd~config, data=inst_I1);

kruskal.test(gd~config, data=inst_I2);

kruskal.test(gd~config, data=inst_I3);

kruskal.test(gd~config, data=inst_I4);

kruskal.test(gd~config, data=inst_I5);

# Pairwise Wilcox 

pairwise.wilcox.test(inst_I0$gd, inst_I0$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I1$gd, inst_I1$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I2$gd, inst_I2$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I3$gd, inst_I3$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I4$gd, inst_I4$config, p.adj="bonferroni", exact=F);

pairwise.wilcox.test(inst_I5$gd, inst_I5$config, p.adj="bonferroni", exact=F);
