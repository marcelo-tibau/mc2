# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 2: Gerar a analise do criterio de tamanho de populacao do algoritmo genetico (pg. 1064).
data_t2 <- read.table("dados/data_t2.txt", header = T)

data_t2$config <- gsub("nsga150k","",data_t2$config)
data_t2$config <- gsub("x","",data_t2$config)
data_t2$config <- as.numeric(data_t2$config)

grafico <- aggregate(gd ~ config + inst, data = data_t2, mean)

plot1 <- ggplot(grafico,aes(config,gd,color=inst)) + geom_line()

# Subset das instâncias

inst_I0 <- subset(data_t2, inst == "I0");
inst_I1 <- subset(data_t2, inst == "I1");
inst_I2 <- subset(data_t2, inst == "I2");
inst_I3 <- subset(data_t2, inst == "I3");
inst_I4 <- subset(data_t2, inst == "I4");
inst_I5 <- subset(data_t2, inst == "I5");

# Kruskal test

kt1 <- kruskal.test(gd~config, data=inst_I0);

kt2 <- kruskal.test(gd~config, data=inst_I1);

kt3 <- kruskal.test(gd~config, data=inst_I2);

kt4 <- kruskal.test(gd~config, data=inst_I3);

kt5 <- kruskal.test(gd~config, data=inst_I4);

kt6 <- kruskal.test(gd~config, data=inst_I5);

kt <- c(kt1$p.value,kt2$p.value,kt3$p.value,kt4$p.value,kt5$p.value,kt6$p.value)

ktd <-data.frame(list(Instancia = c("I0","I1","I2","I3","I4","I5")),kt)

library(ggplot2)
bp <- ggplot(data=ktd, aes(x=Instancia,y=kt*1e100)) +  geom_bar(stat="identity") + geom_hline(yintercept=0.05*1e100, color="red") + scale_y_log10() + labs(y = "P-value*1e100") 


#bp <- barplot(kt, log="y",names.arg=c("I0","I1","I2","I3","I4","I5"))
# Pairwise Wilcox 

pw1 <- pairwise.wilcox.test(inst_I0$gd, inst_I0$config, p.adj="bonferroni", exact=F);

pw2 <- pairwise.wilcox.test(inst_I1$gd, inst_I1$config, p.adj="bonferroni", exact=F);

pw3 <- pairwise.wilcox.test(inst_I2$gd, inst_I2$config, p.adj="bonferroni", exact=F);

pw4 <- pairwise.wilcox.test(inst_I3$gd, inst_I3$config, p.adj="bonferroni", exact=F);

pw5 <- pairwise.wilcox.test(inst_I4$gd, inst_I4$config, p.adj="bonferroni", exact=F);

pw6 <- pairwise.wilcox.test(inst_I5$gd, inst_I5$config, p.adj="bonferroni", exact=F);

library(gridExtra)
library(grid)

options(scipen = 0)
options(digits = 2)

d1 <- tableGrob(round(pw1$p.value,5))
d2 <- tableGrob(round(pw2$p.value,5))
d3 <- tableGrob(round(pw3$p.value,5))
d4 <- tableGrob(round(pw4$p.value,5))
d5 <- tableGrob(round(pw5$p.value,5))
d6 <- tableGrob(round(pw6$p.value,5))

edit_cell <- function(table, row, col, name="core-fg", ...){
  l <- table$layout
  ids <- which(l$t %in% row & l$l %in% col & l$name==name)
  for (id in ids){
    newgrob <- editGrob(table$grobs[id][[1]], ...)
    table$grobs[id][[1]] <- newgrob
  }
  table
}

for(i in 1:nrow(pw1$p.value)){
  for(j in 1:i){
    if(pw1$p.value[i,j] > 0.05){
      d1 <- edit_cell(d1, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
    if(pw2$p.value[i,j] > 0.05){
      d2 <- edit_cell(d2, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
    if(pw3$p.value[i,j] > 0.05){
      d3 <- edit_cell(d3, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
    if(pw4$p.value[i,j] > 0.05){
      d4 <- edit_cell(d4, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
    if(pw5$p.value[i,j] > 0.05){
      d5 <- edit_cell(d5, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
    if(pw6$p.value[i,j] > 0.05){
      d6 <- edit_cell(d6, i+1, j+1, "core-bg", 
                      gp=gpar(fill="red", 
                              col = "black", lwd=5))
    }
  }
}
library(ggplot2)
library(ggpubr)
grid.newpage()
ml <- ggarrange(plot1,bp,d1,d2,d3,d4,d5,d6,nrow=4,ncol=2, labels = c("Média GD por configuração","P-values do Kruskal Test","1ª Inst", "2ª Inst", "3ª Inst","4ª Inst", "5ª Inst", "6ª Inst"))
ggsave("ResultadosQ2.pdf", ml, width = 8, height = 10)