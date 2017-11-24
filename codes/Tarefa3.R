# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")
# advanced grapics R - quick R charts

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
