# R code 

str(data)
data$PN<- as.factor(data$PN)
data$Rep<- as.factor(data$Rep)
data$Block<- as.factor(data$Block)
data$Geno<- as.factor(data$Geno)
data1<-data[-10]
library(variability)
####genetic_parameters####
gen.var(data[8], data$Geno, data$Rep)
#####Correaltion_Analysis####
gencor<-geno.corr(data1[5:20], data1$Geno, data1$Rep)
gencor<-as.data.frame(gencor)
write_xlsx(gencor, "gencorrelation.xlsx")
phencor<-pheno.corr(data1[5:20], data1$Geno, data1$Rep)
phencor<-as.data.frame(phencor)
write_xlsx(phencor, "phencorrelation.xlsx")

#####Path_Analysis####
library(variability)
genopath<-geno.path(data1[20],data1[5:19],data1$Geno,data1$Rep)
phenopath<-pheno.path(data1[20],data1[5:19],data1$Geno,data1$Rep)
class(genopath)
genopath<-as.data.frame(genopath)
install.packages("writexl")
library(writexl)
write_xlsx(genopath, "Genopath.xlsx")
phenopath<-as.data.frame(phenopath)
class(phenopath)
write_xlsx(phenopath, "phenopath1.xlsx")
####ANOVA#######
library(agricolae)
attach(data)
ec<-PBIB.test(Block, Geno, Rep, HI, 5, method = "REML", test = "lsd", alpha = 0.05, console = TRUE, group=TRUE)
ec$ANOVA
ec$statistics
LSD.test($modl, "Geno", console = TRUE)
?LSD.test
mod1<- aov(HI~Rep+Geno+Rep:Block, data=data)
summary(mod1)
Out<-LSD.test(mod1,"Geno", console = TRUE)
Out$statistics$LSD
#######PCA_analysis#####
###Sacling the data###
attach(pca)
scale<-scale(pca[,-1],center = T)
options(max.print = 10000)
options(scipen = 100)
####visulation###
library(factoextra)
library(pca3d)
######built in#######
pca1<- prcomp(scale)
pca1
summary(pca1)
biplot(pca1)
###viualize
fviz_pca(pca1)
fviz_pca_var(pca1)
fviz_pca_ind(pca1)
fviz_screeplot(pca1)
fviz_screeplot(pca1, ncp=17)
fviz_screeplot(pca1,choice="eigenvalue" ncp=17, geom="line")
fviz_screeplot(pca1,choice="eigenvalue", ncp=17, geom="line")
install.packages("writexl")
library(writexl)
tableE<-fpca$eig
tableE<-as.data.frame(tableE)
write_xlsx(tableE, "eigenvalue.xlsx")
phenopath<-as.data.frame(phenopath)
#####ROtated Matrix CoMp####
library(psych)
library(GPArotation)
rpca<-principal(scale, nfactors=4, rotate = "Varimax", scores = TRUE)
rpca
#####communality####
rpca$communality
tablecom<-rpca$communality
tablecom<-as.data.frame(tablecom)
write_xlsx(tablecom, "communality1.xlsx")
rpca$loadings
load<-print(rpca$loadings, digits=3, cutoff=0)
load<-as.data.frame(load)
write_xlsx(load, "Rot_Comp_Mat_New.xlsx")
barplot(rpca$loadings)
library(pals)
barplot(rpca$loadings, beside = T, col = alphabet(17), 
                              main = "Rotated Component Matrix")
score<-rpca$scores
score<-as.data.frame(score)
write_xlsx(score, "PCscore_new.xlsx")
######FactoMineR######
library(FactoMineR)
fpca<-PCA(scale, ncp=17)
fpca
fpca$eig
fpca$ind$coord
