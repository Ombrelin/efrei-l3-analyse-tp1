# Title     : Projet Analuse de donnée
# Created by: arsen
# Created on: 18/02/2020

project_data <- read.table("data.csv", header = T,sep=";", row.names = 1);
message("Taille dimension x,y :",dim(project_data[1]),dim(project_data[2]));
project_data_rotated <- apply(t(project_data),2,rev);
print(summary(project_data_rotated));