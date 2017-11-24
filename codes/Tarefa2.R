# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")
# advanced grapics R - quick R charts


## Tarefa 2: Gerar a analise do criterio de tamanho de populacao do algoritmo genetico (pg. 1064).

data_t2 <- read.table("data_t2.txt", header = T, sep = "\t")

# Separar sequencia para configuracao e instancia
unique_config <- unique(as.character(data_t2$config));
unique_inst <- unique(data_t2$inst);

# Matrizes com mean e standard deviation 
mean_solution <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_hv <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_gd <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
mean_spr <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));

sd_solution <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_hv <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_gd <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));
sd_spr <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));

for (config in unique_config)
{
  for (inst in unique_inst)
  {
    newdata_t2 <- subset(data_t2, inst == inst & config == config);
    
    mean_solution [inst, config] <- mean(newdata_t2$best);
    mean_hv [inst, config] <- mean(newdata_t2$hv);
    mean_gd [inst, config] <- mean(newdata_t2$gd);
    mean_spr [inst, config] <- mean(newdata_t2$spr);
    
    sd_solution [inst, config] <- sd(newdata_t2$best);
    sd_hv [inst, config] <- sd(newdata_t2$hv);
    sd_gd [inst, config] <- sd(newdata_t2$gd);
    sd_spr [inst, config] <- sd(newdata_t2$spr);
    
  }
}

# Testes de inferencia em instancias com IGD

for (inst in unique_inst)
{
  instGD <- subset(data_t2, inst == inst);
  
  pv <- kruskal.test(gd~config, data=instGD)$p.value;
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instGD$gd, instGD$config, p.adj="bonferroni", exact=F)$p.value;
  
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

# Teste de inferencia em instancias com HV

for (inst in unique_inst)
{
  instHV <- subset(data_t2, inst == inst);
  
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

# Teste de inferencia para instancias com Spread 
for (inst in unique_inst)
{
  instSPR <- subset(data_t2, inst == inst);
  
  pv <- kruskal.test(spr~config, data=instSPR)$p.value;
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instSPR$spr, instSPR$config, p.adj="bonferroni", exact=F)$p.value;
  
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

# Melhor configuracao para cada instancia com GD
names <- names(mean_gd[1,]);

for (inst in unique_inst)
{
  instance_index <- match(inst, unique_inst);
  instance_GD <- mean_gd[instance_index,];
  best <- match(min(instance_GD), instance_GD);
  print(paste("Melhor configuracao para instancia", inst, "e", names[best], sep=" "));
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
colname <- c( "nsga2x_m", "nsga2x_sd",   "nsga4x_m", "nsga4x_sd", "nsga8x_m", "nsga8x_sd");

gd <- matrix(nrow=6, ncol=6, dimnames=list(unique_inst, colname));
gd [1:6,1:6] <- cbind(mean_gd[,1],sd_gd[,1] ,mean_gd[,2],sd_gd[,2],mean_gd[,3],sd_gd[,3]);
gd;

hv <- matrix(nrow=6, ncol=6, dimnames=list(unique_inst, colname));
hv [1:6,1:6] <- cbind(mean_hv[,1],sd_hv[,1] ,mean_hv[,2],sd_hv[,2],mean_hv[,3],sd_hv[,3]);
hv;

spr <- matrix(nrow=6, ncol=6, dimnames=list(unique_inst, colname));
spr [1:6,1:6] <- cbind(mean_spr[,1],sd_spr[,1] ,mean_spr[,2],sd_spr[,2],mean_spr[,3],sd_spr[,3]);
spr;

solution <- matrix(nrow=6, ncol=6, dimnames=list(unique_inst, colname));
solution [1:6,1:6] <- cbind(mean_solution[,1],sd_solution[,1] ,mean_solution[,2],sd_solution[,2],mean_solution[,3],sd_solution[,3]);
solution;
