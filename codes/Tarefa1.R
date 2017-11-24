# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")
# advanced grapics R - quick R charts

## Tarefa 1: Gerar a analise do criterio de parada para o algoritmo genetico (pg. 1064)
data_t1 <- read.table("data_t1.txt", header = T, sep = "\t")

table(data_t1$inst=='I0')

newdataI0 <- data_t1[which(data_t1$inst=='I0'),]
newdataI1 <- data_t1[which(data_t1$inst=='I1'),]
newdataI2 <- data_t1[which(data_t1$inst=='I2'),]
newdataI3 <- data_t1[which(data_t1$inst=='I3'),]
newdataI4 <- data_t1[which(data_t1$inst=='I4'),]
newdataI5 <- data_t1[which(data_t1$inst=='I5'),]

x <- newdataI0$count

# Separar sequencias de instances e config 
unique_config <- unique(as.character(data_t1$config));
unique_inst <- unique(data_t1$inst);

# Matrizes com mean e standard deviation 
mean_solution <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_hv <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_gd <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_spr <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));

sd_solution <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_hv <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_gd <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_spr <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));

for (config in unique_config )
{
  for (inst in unique_inst)
  {
    newdata_t1 <- subset(data_t1, inst == inst & config == config);
    
    mean_solution [inst, config] <- mean(newdata_t1$best);
    mean_hv [inst, config] <- mean(newdata_t1$hv);
    mean_gd [inst, config] <- mean(newdata_t1$gd);
    mean_spr [inst, config] <- mean(newdata_t1$spr);
    
    sd_solution [inst, config] <- sd(newdata_t1$best);
    sd_hv [inst, config] <- sd(newdata_t1$hv);
    sd_gd [inst, config] <- sd(newdata_t1$gd);
    sd_spr [inst, config] <- sd(newdata_t1$spr);
    
  }
}

# Testes de inferencia na instancias com base no IGD 
for (inst in unique_inst)
{
  instIGD <- subset(data_t1, inst == inst);
  
  pv <- kruskal.test(gd~config, data=instIGD)$p.value;
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instIGD$gd, instIGD$config, p.adj="bonferroni", exact=F)$p.value;
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  for (i in 1:5) 
  {
    for (j in 1:i)
    {
      if (wt[i,j] < 0.05)
      {
        print(paste("Diferencas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt);
}

# Teste de inferencia em instancias com HV

for (inst in unique_inst)
{
  instHV <- subset(data_t1, inst == inst);
  
  pv <- kruskal.test(hv~config, data=instHV)$p.value;
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instHV$hv, instHV$config, p.adj="bonferroni", exact=F)$p.value;
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  for (i in 1:2) 
  {
    for (j in 1:i)
    {
      if (wt[i,j] < 0.05)
      {
        print(paste("Existe diferenca entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt);
}


# Testes de inferencia na instancias com base no Spread (indicador de diversidade)

for (inst in unique_inst)
{
  instSP <- subset(data_t1, inst == inst);
  
  pv <- kruskal.test(spr~config, data=instSP)$p.value;
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instSP$spr, instSP$config, p.adj="bonferroni", exact=F)$p.value;
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  for (i in 1:5) 
  {
    for (j in 1:i)
    {
      if (wt[i,j] < 0.05)
      {
        print(paste("Diferencas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt);
  
}

# Melhor configuracao para cada instancia com HV 
names <- names(mean_hv[1,]);

for (inst in unique_inst)
{
  instance_index <- match(inst, unique_inst);
  instance_HV <- mean_hv[instance_index,];
  best <- match(max(instance_HV), instance_HV);
  print(paste("Melhor configuracao para instancia", inst, "e", names[best], sep=" "));
}

# Melhor configuracao para cada instancia com GD
names <- names(mean_gd[1,]);

for (inst in unique_inst)
{
  instance_index <- match(inst, unique_inst);
  instance_GD <- mean_gd[instance_index,];
  best <- match(min(instance_GD), instance_GD);
  print(paste("Melhor configuracao para instancia", inst, "e", names[best], sep=" "));
}

# Melhor configuracao para cada instancia com Spread 
names <- names(mean_spr[1,]);

for (inst in unique_inst)
{
  instance_index <- match(inst, unique_inst);
  instance_SPR <- mean_spr[instance_index,];
  best <- match(min(instance_SPR), instance_SPR);
  print(paste("Melhor configuracao para instancia", inst, "e", names[best], sep=" "));
}

# Print das solucoes
colname <- c( "nsga5k2x_m", "nsga5k2x_sd",   "nsga10k2x_m", "nsga10k2x_sd",  "nsga20k2x_m","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");

gd <- matrix(nrow=6, ncol=12, dimnames=list(unique_inst, colname));
gd [1:6,1:12] <- cbind(mean_gd[,1],sd_gd[,1] ,mean_gd[,2],sd_gd[,2],mean_gd[,3],sd_gd[,3],mean_gd[,4],sd_gd[,4],mean_gd[,5],sd_gd[,5],mean_gd[,6],sd_gd[,6]);

gd;

hv <- matrix(nrow=6, ncol=12, dimnames=list(unique_inst, colname));
hv [1:6,1:12] <- cbind(mean_hv[,1],sd_hv[,1] ,mean_hv[,2],sd_hv[,2],mean_hv[,3],sd_hv[,3],mean_hv[,4],sd_hv[,4],mean_hv[,5],sd_hv[,5],mean_hv[,6],sd_hv[,6]);

hv;

spr <- matrix(nrow=6, ncol=12, dimnames=list(unique_inst, colname));
spr [1:6,1:12] <- cbind(mean_spr[,1],sd_spr[,1] ,mean_spr[,2],sd_spr[,2],mean_spr[,3],sd_spr[,3],mean_spr[,4],sd_spr[,4],mean_spr[,5],sd_spr[,5],mean_spr[,6],sd_spr[,6]);

spr;

solution <- matrix(nrow=6, ncol=12, dimnames=list(unique_inst, colname));
solution [1:6,1:12] <- cbind(mean_solution[,1],sd_solution[,1] ,mean_solution[,2],sd_solution[,2],mean_solution[,3],sd_solution[,3],mean_solution[,4],sd_solution[,4],mean_solution[,5],sd_solution[,5],mean_solution[,6],sd_solution[,6]);
solution;


