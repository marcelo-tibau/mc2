# setwd("C:/Users/Marcelo/iCloudDrive/Work/Casa - Pessoal/0_Project CP/Study/MESTRADO/UNIRIO/2017/MESTRADO/Disciplinas/Metodologia_Cientifica_II/finalAssignment")
# setwd("~/Documents/Research/unirio/MCII/finalAssignment")

## Tarefa 7: Gerar projeção em 2D dos objetivos de custo, duração e horas extras para as instâncias ACAD e PARM
#(figura 2).

gaOvertime <- read.table(file="caminho do arquivo", header=TRUE);

gaNoError <- read.table(file="caminho do arquivo", header=TRUE);

cpm <- read.table(file="caminho do arquivo", header=TRUE);

margarine <- read.table(file="caminho do arquivo", header=TRUE);

sh <- read.table(file="caminho do arquivo", header=TRUE);

instanceNames <- c("I0", "I5", "I4", "I3", "I1", "I2");

instanceNames <- c("ACAD", "WMET", "WAMS", "PSOA", "OMET", "PARM");


corrMatrixOT <- matrix(nrow=length(instanceNames), ncol=3, dimnames=list(instanceNames, c("NOHxMKS", "NOHxCST", "MKSxCST")));
corrMatrixNE <- matrix(nrow=length(instanceNames), ncol=3, dimnames=list(instanceNames, c("NOHxMKS", "NOHxCST", "MKSxCST")));

for (instance_ in instanceNames)
{
  instanceName <- instanceNames[which(instanceNames==instance_)];
  
  newdataOT <- subset(gaOvertime, inst == instance_);
  
  corrMatrixOT[instanceName, 1] <- cor(newdataOT$noh, newdataOT$mks, method="spearman");
  corrMatrixOT[instanceName, 2] <- cor(newdataOT$noh, newdataOT$cst, method="spearman");
  corrMatrixOT[instanceName, 3] <- cor(newdataOT$mks, newdataOT$cst, method="spearman");
  
  newdataNE <- subset(gaNoError, inst == instance_);
  
  corrMatrixNE[instanceName, 1] <- cor(newdataNE$noh, newdataNE$mks, method="spearman");
  corrMatrixNE[instanceName, 2] <- cor(newdataNE$noh, newdataNE$cst, method="spearman");
  corrMatrixNE[instanceName, 3] <- cor(newdataNE$mks, newdataNE$cst, method="spearman");
}

corrMatrixOT;
corrMatrixNE;


plot.my.chart <- function(xo, yo, xe, ye, xc, yc, xm, ym, xs, ys, xTitle, yTitle, instanceName) {
  xrange <- range(c(xo, xe, xc, xm, xe));
  yrange <- range(c(yo, ye, yc, ym, ye));
  
  plot(xrange, yrange, type="n", xlab=xTitle, ylab=yTitle);
  text(min(xrange) + 0.95 * (max(xrange) - min(xrange)), min(yrange) + 0.95 * (max(yrange) - min(yrange)), instanceName, font=2);
  
  lines(xo, yo, type="p", col="light gray", cex=1, pch="o"); # NSGA
  lines(xe, ye, type="p", col="dark gray", cex=0.5, pch=0);# NSGANE
  
  lines(xc, yc, type="p", col="black", pch=3);#cpm
  #text(xc, yc, "C");
  
  lines(xm, ym, type="p", col="black", pch=4);#mar
  #text(xm, ym, "M");
  
  lines(xs, ys, type="p", col="black", pch=8);#sh
  #text(xs, ys, "S");
}


pdf("caminho do arquivo", width=10, height=5)

par(mfrow=c(2, 3))
par(mar=c(2.7,2.7,1,1),mgp=c(1.4,0.5,0))	



for (instance_ in instanceNames)
{
  instanceName <- instanceNames[which(instanceNames==instance_)];
  
  dataOT <- subset(gaOvertime, inst == instance_);
  dataNE <- subset(gaNoError, inst == instance_);
  dataCPM <- subset(cpm, inst == instance_);
  dataMAR <- subset(margarine, inst == instance_);
  dataSH <- subset(sh, inst == instance_);
  
  plot.my.chart(dataOT$mks, dataOT$cst / 1000, 
                dataNE$mks, dataNE$cst / 1000, 
                dataCPM$mks, dataCPM$cst / 1000, 
                dataMAR$mks, dataMAR$cst / 1000, 
                dataSH$mks, dataSH$cst / 1000, 
                "Duração (days)", "Custo (1000R$)", instanceName);
  
  plot.my.chart(dataOT$noh * 8, dataOT$mks, 
                dataNE$noh * 8, dataNE$mks, 
                dataCPM$noh * 8, dataCPM$mks, 
                dataMAR$noh * 8, dataMAR$mks, 
                dataSH$noh * 8, dataSH$mks, 
                "Hora-extra (horas)", "Duração (dias)", instanceName);
  
  plot.my.chart(dataOT$noh * 8, dataOT$cst / 1000, 
                dataNE$noh * 8, dataNE$cst / 1000, 
                dataCPM$noh * 8, dataCPM$cst / 1000, 
                dataMAR$noh * 8, dataMAR$cst / 1000, 
                dataSH$noh * 8, dataSH$cst / 1000, 
                "Hora-extra (horas)", "Custo (1000R$)", instanceName);
}
