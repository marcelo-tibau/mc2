# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")
# advanced grapics R - quick R charts

## Tarefa 1: Gerar a analise do criterio de parada para o algoritmo genetico (pg. 1064)
data_t1 <- read.table("data_t1.txt", header = T, sep = "\t")

# Separar sequencias de instances e config 
unique_config <- unique(data_t1$config);
unique_inst <- unique(data_t1$inst);

# Matrizes com mean e standard deviation 
mean_gd <- matrix(nrow=length(unique_inst), ncol=length(unique_config), dimnames=list(unique_inst, unique_config));


for (config in unique_config )
{
  for (inst in unique_inst)
  {
    newdata_t1 <- subset(data_t1, inst == inst & config == config);
    
    mean_gd [inst, config] <- mean(newdata_t1$gd);
    
  }
}
dfmean <- data.frame(mean_gd)

# Testes de inferencia na instancias com base no IGD 
for (inst in unique_inst)
{
  instIGD <- subset(data_t1, inst == inst);
  
  pv <- kruskal.test(gd~config, data=instIGD);
  print(paste("p-Value", inst, "=", pv, sep=" "));
  
  wt <- pairwise.wilcox.test(instIGD$gd, instIGD$config, p.adj="bonferroni", exact=F);
  
#  rownames <- names(wt[,1]);
  #colnames <- names(wt[1,]);
  
 # for (i in 1:(length(unique_config)-1)) 
#  {
#    for (j in 1:i)
  #  {
 #     if (wt[i,j] < 0.05)
#      {
    #    print(paste("As configuracoes", rownames[i], "e", colnames[j], "sao diferentes", sep=" "));
   #   } else
  #      print(paste("As configuracoes", rownames[i], "e", colnames[j], "sao iguais", sep=" "));
 #   }
#  }
  
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


}

# Print das solucoes
colname <- c( "nsga5k2x_m", "nsga5k2x_sd",   "nsga10k2x_m", "nsga10k2x_sd",  "nsga20k2x_m","nsga20k2x_sd",  "nsga50k2x_m","nsga50k2x_sd", "nsga100k2x_m","nsga100k2x_sd", "nsga150k2x_m","nsga150k2x_sd");

gd <- matrix(nrow=6, ncol=12, dimnames=list(unique_inst, colname));
gd [1:6,1:12] <- cbind(mean_gd[,1],sd_gd[,1] ,mean_gd[,2],sd_gd[,2],mean_gd[,3],sd_gd[,3],mean_gd[,4],sd_gd[,4],mean_gd[,5],sd_gd[,5],mean_gd[,6],sd_gd[,6]);

gd;



