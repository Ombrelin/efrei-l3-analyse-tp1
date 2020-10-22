#==================================#
#                                  #
#       Lecture des données        #
#                                  #
#==================================#

x_matrix <- read.csv("petrole.csv", header = T, sep = ";", row.names = 1); # parse csv file (with ; separator) into matrix

#==================================#
#                                  #
#       Informations basiques      #
#                                 #
#==================================#

message("Summary")
summary(x_matrix); #summary

message("Covariance")
print(cov(x_matrix)); # covariance

message("Correlation")
print(cor(x_matrix)); # correlation

message("Columns : "); # number of columns
ncol(x_matrix)
message("Rows : "); # number of rows
nrow(x_matrix);

#==================================#
#                                  #
#    Données centrées réduites     #
#                                  #
#==================================#

centree_reduite <- scale(x_matrix, center = T, scale = T);

#==================================#
#                                  #
# Analyse en composante principale #
#                                  #
#==================================#

# Valeurs propres
propres <- eigen(cor(centree_reduite));
valeursPropres <- propres$values;
vecteursPropres <- propres$vectors;

# Graphique des valeurs propres (éboulis et coude)
png(filename = "eboulis.png"); # start printing plot to png image
plot(1:length(valeursPropres), valeursPropres, type = "b");
dev.off(); # end printing

# Composantes principales
data_acp <- centree_reduite %*% vecteursPropres;

composante_principale_1 <- data_acp[, 1];
composante_principale_2 <- data_acp[, 2];

# Cercle de correlation
# Calcule de la correlation entre chaque variable et les composantes principales
cor1 <- cor(composante_principale_1,centree_reduite);
cor2 <- cor(composante_principale_2,centree_reduite);
png(filename = "cercle_de_correlation.png"); # start printing plot to png image
plot(cor1, cor2, xlim = c(-1, +1), ylim = c(-1, +1))
abline(h = 0, v = 0)
text(cor1, cor2, labels = colnames(data))
dev.off(); # end printing

# Graphe 2D
png(filename = "graphe2D.png"); # start printing plot to png image
plot(data_acp[, 1], data_acp[, 2])
text(data_acp[, 1], data_acp[, 2], labels = rownames(data))
abline(h = 0, v = 0)
dev.off(); # end printing