# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

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

## Tarefa 3: Gerar a analise comparativa de qualidade para cada instancia entre a NSGAII, MAR, SH, CPM (Tabela 2)
data_t3 <- read.table("data_t3-t4.txt", header = T, sep = "\t")


# Separando sequencias de configuracao e instancias 
configs <- unique(as.character(data_t3$config));
insts <- unique(as.character(data_t3$inst));

# Teste de inferencia em instancia com base em contribuicao 
for (inst in insts)
{
  instance_Data <- subset(data_t3, inst == inst);
  
  NSGA <- subset(instance_Data, config == "nsga150k2x");
  mar <- subset(instance_Data, config == "MAR");
  sh <- subset(instance_Data, config == "SH");
  cpm <- subset(instance_Data, config == "CPM");
  
  pv <- wilcox.test(NSGA$best, mu=mar$best[1])$p.value;
  print(paste("IC: p-Value para o teste Wilcox:", inst, "(NSGA,MAR)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$best, mu=sh$best[1])$p.value;
  print(paste("IC: p-Value para o teste Wilcox:", inst, "(NSGA,SH)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$best, mu=cpm$best[1])$p.value;
  print(paste("IC: p-Value para o teste Wilcox:", inst, "(NSGA,CPM)=", pv, sep=" "));
  
  print("-------------------------------------------------------------------------------------------");
}

# Teste de inferencia em instancia com base em HV
for (inst in insts)
{
  instance_Data <- subset(data_t3, inst == inst);
  
  NSGA <- subset(instance_Data, config == "nsga150k2x");
  mar <- subset(instance_Data, config == "MAR");
  sh <- subset(instance_Data, config == "SH");
  cpm <- subset(instance_Data, config == "CPM");
  
  pv <- wilcox.test(NSGA$hv, mu=mar$hv[1])$p.value;
  print(paste("HV: p-Value para o teste Wilcox:", inst, "(NSGA,MAR)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$hv, mu=sh$hv[1])$p.value;
  print(paste("HV: p-Value para o teste Wilcox:", inst, "(NSGA,SH)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$hv, mu=cpm$hv[1])$p.value;
  print(paste("HV: p-Value para o teste Wilcox:", inst, "(NSGA,CPM)=", pv, sep=" "));
  
  print("-------------------------------------------------------------------------------------------");
}

# Teste de inferencia em instancia com base em GD 
for (inst in insts)
{
  instance_Data <- subset(data_t3, inst == inst);
  
  NSGA <- subset(instance_Data, config == "nsga150k2x");
  mar <- subset(instance_Data, config == "MAR");
  sh <- subset(instance_Data, config == "SH");
  cpm <- subset(instance_Data, config == "CPM");
  
  pv <- wilcox.test(NSGA$gd, mu=mar$gd[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,MAR)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$gd, mu=sh$gd[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,SH)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$gd, mu=cpm$gd[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,CPM)=", pv, sep=" "));
  
  print("-------------------------------------------------------------------------------------------");
}

# Teste de inferencia em instancia com base em Spread
for (inst in insts)
{
  instance_Data <- subset(data_t3, inst == inst);
  
  NSGA <- subset(instance_Data, config == "nsga150k2x");
  mar <- subset(instance_Data, config == "MAR");
  sh <- subset(instance_Data, config == "SH");
  cpm <- subset(instance_Data, config == "CPM");
  
  pv <- wilcox.test(NSGA$spr, mu=mar$spr[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,MAR)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$spr, mu=sh$spr[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,SH)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$spr, mu=cpm$spr[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,CPM)=", pv, sep=" "));
  
  print("-------------------------------------------------------------------------------------------");
}

# Teste de inferencia em instancia com base em Time 
for (inst in insts)
{
  instance_Data <- subset(data_t3, inst == inst);
  
  NSGA <- subset(instance_Data, config == "nsga150k2x");
  mar <- subset(instance_Data, config == "MAR");
  sh <- subset(instance_Data, config == "SH");
  cpm <- subset(instance_Data, config == "CPM");
  
  pv <- wilcox.test(NSGA$time, mu=mar$time[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,MAR)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$time, mu=sh$time[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,SH)=", pv, sep=" "));
  
  pv <- wilcox.test(NSGA$time, mu=cpm$time[1])$p.value;
  print(paste("GD: p-Value para o teste Wilcox:", inst, "(NSGA,CPM)=", pv, sep=" "));
  
  print("-------------------------------------------------------------------------------------------");
}

# Calculo da media (means)
mean_ic <- matrix(nrow=length(insts), ncol=length(configs), dimnames=list(insts, configs));
mean_hv <- matrix(nrow=length(insts), ncol=length(configs), dimnames=list(insts, configs));
mean_gd <- matrix(nrow=length(insts), ncol=length(configs), dimnames=list(insts, configs));
mean_sp <- matrix(nrow=length(insts), ncol=length(configs), dimnames=list(insts, configs));
mean_tm <- matrix(nrow=length(insts), ncol=length(configs), dimnames=list(insts, configs));

for (config in configs)
{
  for (inst in insts)
  {
    inst <- insts[which(insts==inst)];
    meandata <- subset(data_t3, inst == inst & config == config);
    mean_ic[inst, config] <- mean(meandata$best);
    mean_hv[inst, config] <- mean(meandata$hv);
    mean_gd[inst, config] <- mean(meandata$gd);
    mean_sp[inst, config] <- mean(meandata$spr);
    mean_tm[inst, config] <- mean(meandata$time);
  }
}

mean_ic;
mean_hv;
mean_gd;
mean_sp;
mean_tm;

# Calculo do standard deviation
sd_ic <- matrix(nrow=length(insts), ncol=1, dimnames=list(insts, c("nsga150k2x")));
sd_hv <- matrix(nrow=length(insts), ncol=1, dimnames=list(insts, c("nsga150k2x")));
sd_gd <- matrix(nrow=length(insts), ncol=1, dimnames=list(insts, c("nsga150k2x")));
sd_sp <- matrix(nrow=length(insts), ncol=1, dimnames=list(insts, c("nsga150k2x")));
sd_tm <- matrix(nrow=length(insts), ncol=1, dimnames=list(insts, c("nsga150k2x")));

for (inst in insts)
{
  inst <- insts[which(insts==inst)];
  sddata <- subset(data_t3, inst == inst & config == "nsga150k2x");
  sd_ic[inst, "nsga150k2x"] <- sd(sddata$best);
  sd_hv[inst, "nsga150k2x"] <- sd(sddata$hv);
  sd_gd[inst, "nsga150k2x"] <- sd(sddata$gd);
  sd_sp[inst, "nsga150k2x"] <- sd(sddata$spr);
  sd_tm[inst, "nsga150k2x"] <- sd(sddata$time);
}

sd_ic;
sd_hv;
sd_gd;
sd_sp;
sd_tm;

## Tarefa 4: Gerar a analise de tamanho de efeito: comparativo de qualidade para cada instancia e pares de abordagens NSGAII x MAR , NSGAII x SH e NSGAII x CPM (pagina 1065)
data_t4 <- read.table("data_t3-t4.txt", header = T, sep = "\t")

# Separando sequencias de configuracao e instancias 
configs <- unique(as.character(data_t4$config));
instances <- unique(as.character(data_t4$inst));

# Funcao para calcular o tamanho de efeito Vargha-Delaney (A12) 
varghaDelaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return((2*sum(rank(c(r1, r2))[seq_along(r1)]) - m*(m+1)) / (2*n*m));
}


# m = length(treatment)
# n = length(control)
# r = rank(c(treatment,control))
# r1 = sum(r[seq_len(m)])
# A = (2*r1 - m*(m+1)) / (2*n*m)
# A = (2*sum(rank(c(m, n))[seq_len(m)]) - m*(m+1)) / (2*n*m)
# Versao mtorchiano:http://mtorchiano.wordpress.com/2014/05/19/effect-size-of-r-precision/

# Testes de tamanho de efeito  
stat_Names <- c("NSGA/MAR", "NSGA/SH", "NSGA/CPM");
gdStats <- matrix(nrow=length(instances), ncol=length(stat_Names), dimnames=list(instances, stat_Names));
hvStats <- matrix(nrow=length(instances), ncol=length(stat_Names), dimnames=list(instances, stat_Names));
icStats <- matrix(nrow=length(instances), ncol=length(stat_Names), dimnames=list(instances, stat_Names));
spStats <- matrix(nrow=length(instances), ncol=length(stat_Names), dimnames=list(instances, stat_Names));
tmStats <- matrix(nrow=length(instances), ncol=length(stat_Names), dimnames=list(instances, stat_Names));

for (instance_ in instances)
{
  instance_ <- instances[which(instances==instance_)];
  
  margarine <- subset(data_t4, inst == instance_ & config == "MAR");
  CPM <- subset(data_t4, inst == instance_ & config == "CPM");
  SecondHalf <- subset(data_t4, inst == instance_ & config == "SH");
  nsNSGA <- subset(data_t4, inst == instance_ & config == "nsga150k2x");
  
  icStats[instance_, "NSGA/MAR"] <- varghaDelaney(nsNSGA$best, margarine$best);
  icStats[instance_, "NSGA/SH"] <- varghaDelaney(nsNSGA$best, SecondHalf$best);
  icStats[instance_, "NSGA/CPM"] <- varghaDelaney(nsNSGA$best, CPM$best);
  
  hvStats[instance_, "NSGA/MAR"] <- varghaDelaney(nsNSGA$hv, margarine$hv);
  hvStats[instance_, "NSGA/SH"] <- varghaDelaney(nsNSGA$hv, SecondHalf$hv);
  hvStats[instance_, "NSGA/CPM"] <- varghaDelaney(nsNSGA$hv, CPM$hv);
  
  gdStats[instance_, "NSGA/MAR"] <- varghaDelaney(nsNSGA$gd, margarine$gd);
  gdStats[instance_, "NSGA/SH"] <- varghaDelaney(nsNSGA$gd, SecondHalf$gd);
  gdStats[instance_, "NSGA/CPM"] <- varghaDelaney(nsNSGA$gd, CPM$gd);
  
  spStats[instance_, "NSGA/MAR"] <- varghaDelaney(nsNSGA$spr, margarine$spr);
  spStats[instance_, "NSGA/SH"] <- varghaDelaney(nsNSGA$spr, SecondHalf$spr);
  spStats[instance_, "NSGA/CPM"] <- varghaDelaney(nsNSGA$spr, CPM$spr);
  
  tmStats[instance_, "NSGA/MAR"] <- varghaDelaney(nsNSGA$time, margarine$spr);
  tmStats[instance_, "NSGA/SH"] <- varghaDelaney(nsNSGA$time, SecondHalf$spr);
  tmStats[instance_, "NSGA/CPM"] <- varghaDelaney(nsNSGA$time, CPM$spr);
}

icStats;
hvStats;
gdStats;
spStats;
tmStats;


### Tarefa 5: Gerar a análise comparativa de qualidade para cada instância entre NSGAII x NSGASE (tabela 3).
data_t5 <- read.table("data_t5-t6.txt", header = T, sep = "\t")

# Separate sequence configuration and instances
configs <- unique(as.character(data_t5$config));
instances <- c("I0", "I5", "I4", "I3", "I1", "I2");
instanceNames <- c("ACAD", "WMET", "WAMS", "PSOA", "OMET", "PARM");

# Inference tests on an instance basis (IC)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$best, nsga150k2xse$best)$p.value;
  print(paste("IC: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (HV)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$hv, nsga150k2xse$hv)$p.value;
  print(paste("HV: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (GD)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$gd, mu=nsga150k2xse$gd[1])$p.value;
  print(paste("GD: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (SP)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$gd, mu=nsga150k2xse$gd[1])$p.value;
  print(paste("SP: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Calculating means and standard deviation
## Create matrices to hold mean and stdev values
mean_count <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
mean_ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
mean_hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
mean_gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
mean_sp <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
sd_ic <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
sd_hv <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
sd_gd <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));
sd_sp <- matrix(nrow=length(instances), ncol=length(configs), dimnames=list(instanceNames, configs));

for (config_ in configs)
{
  for (instance_ in instances)
  {
    data_comp <- subset(data_t5, inst == instance_ & config == config_);
    instanceName <- instanceNames[which(instances==instance_)];
    
    mean_count[instanceName, config_] <- mean(data_comp$count);
    mean_ic[instanceName, config_] <- mean(data_comp$best);
    mean_hv[instanceName, config_] <- mean(data_comp$hv);
    mean_gd[instanceName, config_] <- mean(data_comp$gd);
    mean_sp[instanceName, config_] <- mean(data_comp$sp);
    
    sd_ic[instanceName, config_] <- sd(data_comp$best);
    sd_hv[instanceName, config_] <- sd(data_comp$hv);
    sd_gd[instanceName, config_] <- sd(data_comp$gd);
    sd_sp[instanceName, config_] <- sd(data_comp$sp);
  }
}

mean_ic;
mean_hv;
mean_gd;
mean_sp;

sd_ic;
sd_hv;
sd_gd;
sd_sp;

### Tarefa 6: Gerar a análise de tamanho de efeito: comparativo de qualidade para cada instância e pares de abordagens NSGAII x NSGANE (página 1065).

data_t6 <- read.table("data_t5-t6.txt", header = T, sep = "\t")

# Effect Size tests 
effectSizeNames <- c("ic", "hv", "gd", "sp");
effectSize <- matrix(nrow=length(instances), ncol=length(effectSizeNames), dimnames=list(instanceNames, effectSizeNames));

for (instance_ in instances)
{
  instanceName <- instanceNames[which(instances==instance_)];
  nsga150k2x <- subset(data, inst == instance_ & config == "nsga150k2x");
  nsga150k2xse <- subset(data, inst == instance_ & config == "nsga150k2xse");
  effectSize[instanceName, "ic"] <- vargha.delaney(nsga150k2x$best, nsga150k2xse$best);
  effectSize[instanceName, "hv"] <- vargha.delaney(nsga150k2x$hv, nsga150k2xse$hv);
  effectSize[instanceName, "gd"] <- 1.0 - vargha.delaney(nsga150k2x$gd, nsga150k2xse$gd);
  effectSize[instanceName, "sp"] <- 1.0 - vargha.delaney(nsga150k2x$sp, nsga150k2xse$sp);
}

effectSize;

