# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")
# advanced grapics R - quick R charts

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
