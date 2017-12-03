# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 7: Gerar projeção em 2D dos objetivos de custo, duração e horas extras para as instâncias ACAD e PARM
#(figura 2).

NSGA <- cbind(read.table(file="dados/data_t7_nsga_150k_c50_2x_error1_frontier_obj.txt", header=TRUE),list(metodo = "NSGA"));

NSGANoError <- cbind(read.table(file="dados/data_t7_nsga_150k_c50_2x_noerror_frontier_obj.txt", header=TRUE),list(metodo = "NSGANoError"));

mar <- cbind(read.table(file="dados/data_t7_Margarine_error1_frontier_obj.txt", header=TRUE),list(metodo = "mar"));

sh <- cbind(read.table(file="dados/data_t7_SecondHalf_error1_frontier_obj.txt", header=TRUE),list(metodo = "sh"));

cpm <- cbind(read.table(file="dados/data_t7_CPM_error1_frontier_obj.txt", header=TRUE),list(metodo = "cpm"));

dados <- rbind(NSGA,NSGANoError,mar,sh,cpm)

instanceNames <- unique(dados$inst)

InstACAD <- subset(dados, inst == "ACAD");

shapes <- c(1,1,1,2,3)
colors <- c("light gray","dark gray","black","black","black")

p1 <- ggplot(InstACAD,aes(mks,cst/1000,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Cost(1000$)", x = "Makespan (days)") + ggtitle("ACAD")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))
p2 <- ggplot(InstACAD,aes(noh,mks,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Makespan (days)", x = "Overtime (hours)") + ggtitle("ACAD")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))
p3 <- ggplot(InstACAD,aes(noh,cst/1000,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Cost(1000$)", x = "Overtime (hours)") + ggtitle("ACAD")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))

InstPARM <- subset(dados, inst == "PARM");

p4 <- ggplot(InstPARM,aes(mks,cst/1000,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Cost(1000$)", x = "Makespan (days)") + ggtitle("PARM")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))
p5 <- ggplot(InstPARM,aes(noh,mks,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Makespan (days)", x = "Overtime (hours)") + ggtitle("PARM")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))
p6 <- ggplot(InstPARM,aes(noh,cst/1000,color=metodo,shape=metodo)) + geom_point() + scale_shape_manual(values=shapes) + scale_color_manual(values=colors) + labs(y = "Cost(1000$)", x = "Overtime (hours)") + ggtitle("PARM")+ theme(plot.title = element_text(face="bold", hjust=0.98,margin = margin(b = -20)), legend.position="none", plot.margin=grid::unit(c(0.3,0.1,0.1,0.1), "in"))

library(ggplot2)

final <- ggarrange(p1, p2, p3, p4, p5, p6, nrow=2, ncol=3)
ggsave("ResultadosQ7.pdf", final, width = 10, height = 4)
