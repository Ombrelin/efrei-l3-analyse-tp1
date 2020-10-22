oil <- read.table("petrole.csv", header=T, sep = ";", row.names = 1); # parse csv file (with ; separator) into matrix
# header : labels for columns from 1st line of CSV file
# row.names : labels for row from the column 1
print(oil);
summary(oil);

print("Covariance")
print(cov(oil)); # covariance
print("Correlation")
print(cor(oil)); # correlation

ncol(oil); # number of columns
nrow(oil); # number of rows

# plot relationship between IMM and EXP variables (they have high correlation)
png(filename = "IMMEXP.png"); # start printing plot to png image
plot(oil$IMM,oil$EXP);
text(oil$IMM,oil$EXP, rownames(oil));
dev.off(); # end printing

# analyse variable pairs plot in table
pairs(oil);

# Center data
oilCenter <- scale(oil, center = T, scale = F);
print(oilCenter);
summary(oilCenter); # mean is 0 -> normal

print("Covariance Center")
print(cov(oilCenter)); # covariance
print("Correlation Center")
print(cor(oilCenter)); # correlation

# Center and reduce data
oilCenteredReduced <- scale(oil, center = T, scale = T);
print(oilCenteredReduced);
summary(oilCenter); # mean is 0 -> normal

print("Covariance Centered Reduced")
View(cov(oilCenteredReduced)); # covariance
print("Correlation Centered Reduced")
View(cor(oilCenteredReduced)); # correlation

# ======================== ACP ===================================

# Calcule des valeurs propres et vecteurs propres
propres <- eigen(cor(oilCenteredReduced));
valeursPropres <- propres$values;
vecteursPropres <- propres$vectors;

# Graphique des valeurs propres (éboulis et coude)
plot(1:8,valeursPropres, type="b");

# Composantes principales
