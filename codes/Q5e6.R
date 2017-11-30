# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

### Tarefa 5: Gerar a análise comparativa de qualidade para cada instância entre NSGAII x NSGASE (tabela 3).
### Tarefa 6: Gerar a análise de tamanho de efeito: comparativo de qualidade para cada instância e pares de abordagens NSGAII x NSGANE (página 1065).

data_t5_6 <- read.table("data_t5-t6.txt", header = T)

# Separate sequence configuration and instances
configs <- unique(as.character(data_t5_6$config));
instances <- c("I0", "I5", "I4", "I3", "I1", "I2");
instanceNames <- c("ACAD", "WMET", "WAMS", "PSOA", "OMET", "PARM");

# Inference tests on an instance basis (IC)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5_6, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5_6, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5_6, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$best, nsga150k2xse$best)$p.value;
  print(paste("IC: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (HV)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5_6, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5_6, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5_6, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$hv, nsga150k2xse$hv)$p.value;
  print(paste("HV: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (GD)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5_6, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5_6, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5_6, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$gd, mu=nsga150k2xse$gd[1])$p.value;
  print(paste("GD: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Inference tests on an instance basis (SP)
for (instance_ in instances)
{
  instanceData_ <- subset(data_t5_6, inst == instance_);
  instanceName <- instanceNames[which(instances==instance_)];
  
  nsga150k2xse <- subset(data_t5_6, inst == instance_ & config == "nsga150k2xse");
  nsga150k2x <- subset(data_t5_6, inst == instance_ & config == "nsga150k2x");
  
  pv <- wilcox.test(nsga150k2x$gd, mu=nsga150k2xse$gd[1])$p.value;
  print(paste("SP: p-Value for Wilcox:", instanceName, "(nsga150k2x,nsga150k2xse)=", pv, sep=" "));
}

# Calculating the Effect Size of Varga & Delaney (A12)
vargha.delaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

# Effect Size tests 
effectSizeNames <- c("ic", "hv", "gd", "sp");
effectSize <- matrix(nrow=length(instances), ncol=length(effectSizeNames), dimnames=list(instanceNames, effectSizeNames));

for (instance_ in instances)
{
  instanceName <- instanceNames[which(instances==instance_)];
  nsga150k2x <- subset(data_t5_6, inst == instance_ & config == "nsga150k2x");
  nsga150k2xse <- subset(data_t5_6, inst == instance_ & config == "nsga150k2xse");
  effectSize[instanceName, "ic"] <- vargha.delaney(nsga150k2x$best, nsga150k2xse$best);
  effectSize[instanceName, "hv"] <- vargha.delaney(nsga150k2x$hv, nsga150k2xse$hv);
  effectSize[instanceName, "gd"] <- 1.0 - vargha.delaney(nsga150k2x$gd, nsga150k2xse$gd);
  effectSize[instanceName, "sp"] <- 1.0 - vargha.delaney(nsga150k2x$sp, nsga150k2xse$sp);
}

effectSize;



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
    data_comp <- subset(data_t5_6, inst == instance_ & config == config_);
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



