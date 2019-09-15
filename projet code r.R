data=read.csv("C:/Users/Caroline/Documents/M2/SVM/projet/creditcard.csv",header=T,sep=",")
head(data)
dim(data)
attach(data)
names(data)
summary(data)
#correlation
cor(data[,-31])
plot(class,V4)
#data
#on est dans le cas où la variable à expliquer Class est qualitative et prend 2 modalités 0 et 1.

#regression logistique
#la regression logistique attribut à la modalité 1 les élément dont la proba est supérieur à 0.5, et à la modalité 0 les élément dont la probabilité est inférieur à 0.5
dim(data)
train=(Time<150000)
data.150000=data[!train,]
#dim(data.150000)   = 45614 obs°
Class.150000=Class[!train]
glm.fit=glm(Class~.,data=data,family=binomial,subset=train)
summary(glm.fit)$coef
glm.fit=glm(Class~V4+V10+V14,data=data,family=binomial,subset=train)
summary(glm.fit)$coef
glm.probs=predict(glm.fit,type='response',data.150000)
glm.probs[1:10]
length(glm.probs)
glm.pred=rep(0,45614)
glm.pred[1:10]
glm.pred[glm.probs>.5]=1
glm.pred[1:10]
table(glm.pred,Class.150000)
#tableau de contingence
mean(glm.pred==Class.150000)
#renvoie le taux de bonne prédiction
mean(glm.pred!=Class.150000)
plot(Class)
plot(glm.fit)
qqplot
plot(glm.probs)


#analyse discriminante linéaire
#lorsque les classes sont bien séparées, les paramètres estimés pour le modèle de régression logistique sont instables
#si le nombre d'observation est petit le LDA est plus stable que la régression logistique
#plus populaire lorsque l'on à plus de 2 modalités de la variable de réponse
library(MASS)
#la fonction LDA se trouve dans ce packages
lda.fit=lda(Class~V4+V10+V14,data=data.150000,subset = train)
lda.fit
plot(lda.fit)
#proba d'appartenance : 99,82% dans la classe 0 et 00,17% dans 1
lda.pred=predict(lda.fit, data[!train,])
names(lda.pred)
table(lda.pred$class,Class.150000)
#création de la matrice de confusion
length(lda.pred$class)
#donne la longueur de la va class
#= au nb d'obs°
mean(lda.pred$class==Class.150000)
#taux d'exactitude = 55.95
mean(lda.pred$class!=Class.150000)
plot(lda.fit,residuals(lda.fit))



#qda
#on effectue une prédiction sur l'echantillon test
qda.fit=qda(Class~V4+V10+V14,data=data.150000,subset=train)
qda.pred=predict(qda.fit,data[!train,],type='vector')
#donne les proba d'appartenance -> avec > à 0.5 = Up
# il y a class et les proba posterieures pour les 2 classes
table(qda.pred$class,Class.150000)
#matrice de confusion
mean(qda.pred$class==Class.150000)
#taux d'exactitude
mean(qda.pred$class!=Class.150000)
#donne un taux d'erreur plus faible que les autres modèles
#avec un niveau de précision impressionnant par rapport aux autres de modèle





#arbre
library(tree)
tree.data=tree(Class~.,data)
summary(tree.data)
#selection des variables pertinentes
plot(tree.data)
text(tree.data,pretty=0)
set.seed(2)
train=sample(1:nrow(data),150000)
nrow(data)
data.test=data[-train,]
Class.test=Class[-train] 
length(Class.test)
tree.data=tree(Class~V17+V12+V14+V26+V27+V10+V16,data,subset=train,)
plot(tree.data)
text(tree.data,pretty=0)








tree.pred=predict(tree.data,data.test,type='class')
table(tree.pred,Class.test)
mean(tree.pred==Class.test)
mean(tree.pred!=Class.test)


















#fonctionne plus

#élagage de l'arbre
#on essaye de réduire l'arbre pour (peut-être) avoir de meilleurs résultats
#on réalise une validation croisée pour déterminer le niveau optimal de complexité
set.seed(3)
cv.data=cv.tree(tree.data,FUN=prune.misclass)
names(cv.data)
cv.data
plot(cv.data$size,cv.data$dev,type='b')
#taille -> nb de noeuds terminaux
#dev-> taux d'erreur obtenue par validation croisée
#l'arbre avec 9 noeud terminaux donne le taux d'erreur =50 le plus faible
#-> c'est donc le meilleur arbre
#k-> nb de prédicteur
which.min(cv.data$dev)
#renvoie la position de la valeur la plus petite de la liste
cv.data$size[which.min(cv.data$dev)]
#renvoie le nb de noeuds optimal
par(mfrow=c(1,2))
#affiche simultanément les 2 graph
#par(mfrow=c(2,1)) affiche les graphique horizontalement
plot(cv.data$size,cv.data$dev,type='b')
#représente le taux d'erreur selon le nb de noeud terminaux choisit
#correspond à 9
plot(cv.data$k,cv.data$dev,type='b')
#représente le taux d'erreur selon le nombre de prédicteurs choisit
#on veut obtenir l'arbre à 9 noeuds
#on veut vérifier que le taux d'erreur du sous arbre est plus faible
prune.data=prune.misclass(tree.data,best=9)
#utilise l'estimation du sous arbre
#ou la meilleur estimation est à 9 noeud terminaux (best=9)
par(mfrow=c(1,1))
plot(prune.data)
text(prune.data,pretty=0)
prune.pred=predict(prune.data,data.test,type='class')
#génére des prédictions avec le sous arbre à 9 noeuds
length(prune.pred)
#vérifie qu'il à généré les 200 prédiction
mean(prune.pred==Class.test)
#77% d'exactitude
mean(prune.pred!=Class.test)
#23% d'erreur -> plus faible donc préféré
#tous choix de best différent donne une taux d'erreur plus élevé
#donc le résultat sera plus mauvais
#on peut tenter de vérifier avec un best= différent 
#que l'arbre à 9 noeud à toujour le taux d'erreur le plus faible



#méthodes bagging  

#on rappelle que le bagging est un cas particulier du random forest où m=p   
#on peut donc utilisé la fonction random forest pour appliquer les 2 méthodes
#on commence par la méthode bagging
#install.packages('randomForest') 
library(randomForest)
set.seed(1)
bag.data=randomForest(Class~.,data=data,subset=train,mtry=31,importance=TRUE)
#mtry=13 -> 13 prédicteur dans le tableau de donnée
#ils doivent être considérés pour chaques division de l'arbre -> méthode bagging
bag.data
#compare l'importance de toutes les va candidates à chaque noeud de l'arbre
#bootstrap l'échantillon aléatoire à partir de l'échantillon initial par défaut
bag.data1=randomForest(Class~.,data=data,subset=train,mtry=13,ntree=200)
#ntree -> nb d'arbre bootstrappé
#que l'on à réduit à 200
bag.data1
#ntree=200-> on veut générer 200 arbre par le bagging
yhat.bag=predict(bag.data,newdata=data[-train,])
#prédiction avec les résultats du bagging sur l'échantillon test
plot(yhat.bag,data.test)
abline(0,1)
mean((data.test-yhat.bag)^2)
#RSE = 13.47
yhat.bag1=predict(bag.data1,newdata=data[-train,])
mean((data.test-yhat.bag1)^2)
#avec 200 arbres RSE= 
#préfère la régression avec 500 arbres où le taux est minimisé
#de même pour ntree=300, on trouve un RSE=13.32 -> plus petit
#progression à tâtons pour déterminer le nb d'arbre minimisant le taux d'erreur au carré
#le bagging à permit de réduire de presque de moitié le RSE

#random forest

#mais on utilise une valeur plus petite pour l'argumetnn mtry
#lors de la construct°d'un random forest, pour:
#un arbre de régression : divise par défaut par p/3 variables
#un arbre de classification : divise par défaut par sqrt(p)
#on utilise mtry=6
set.seed(1)
rf.data=randomForest(Class~.,data=data,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.data,newdata=data[-train,])
mean((yhat.rf-data.test)^2)
#RSE=11.48
# on à donc encore une amélioration par rapport au bagging
importance(rf.data)
#affiche l'importance de chaques va
#plusieurs estimation dans chaque noeud et élimine à chaque fois une va
#la 1ère colonne : se base sur la baisse moyenne de la précision des prédictions (baisse moyenne de l'impureté)
#sur l'échantillon test lorsqu'une va est exclue du modèle
#la 2ème colonne : mesure la baisse totale de l'impureté des noeuds résultant d'une division
#selon cette va, moyenné sur tous les arbres
#mesure de l'impureté des noeuds:
#arbre de régression : RSE d'app
#arbre de classification : déviance
varImpPlot(rf.data)
#représentations graphiques de ces mesures d'importance:
#gauche -> représente la baisse de la RSE -> arbre de classification
#rm & lstat sont les va les plus importantes
#droite ->baisse totale de l'impureté des noeuds -> arbre de régression
#les 2 critères de selection donne les 2 mêmes va les plus importantes

#application du boosting

#différence avec l'arbre de régression
#bernoulli pour l'arbre de classication
#gaussien pour l'arbre boosting (de régression)
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.data=gbm(Class~.,data=data[train,],distribution='gaussian',n.trees=5000,interaction=4)
#distribution='gaussian' -> car il s'agit d'un arbre de régression
#s'il s'agissait d'un pb de classification binaire, on utliserait une distribut° de bernoulli
#interaction=4(=interaction.depth=4) -> limite la profondeur de chaque arbre
#interaction=4 -> nb de division
summary(boost.data)
#donne l'importance relative des variables
#rangé de la va la plus importante à la moins importante
#avec une représentation de l'influence relative
#ainsi que leurs statitiques
#lstat & rm sont de loin les variables les plus importantes
par(mfrow=c(1,2))
plot(boost.data,i='rm')
plot(boost.data,i='lstat')
#donnent l'effet marginal des va sélectionnées sur la va de réponse
#après intégration des autres va
#soit les dépendances partielles pour ces 2 variables
#les prix médians des maisons augmentent avec rm et diminue avec lstat
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=5000)
#on utilise le modèle boosting pour prédire medv sur l'ensemble test
mean((data.test-yhat.boost)^2)
#RSE=11.84$
#= ou presque de celui du random forest
#supérieur à celui du bagging

#on peut appliquer un boosting en utilisant un shrinkage lambda différent

#la valeur par défaut=0.001
boost.data=gbm(Class~.,data=data[train,],distribution='gaussian',n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
#interaction.depth=4 -> nb de division
#shrinkage=0.2 -> taux seuil permattant de réduire le seuil de chaque sous arbres
#verbose=F -> enlève  le tableau des progressions & performances des paramètres
summary(boost.data)
#donne l'importance relative des variables
#rangé de la va la plus importante à la moins importante
par(mfrow=c(1,2))
plot(boost.data,i='rm')
plot(boost.data,i='lstat')
#donnent l'effet marginal des va sélectionnées sur la va de réponse
#les prix médians des maisons augmentent avec rm et diminue avec lstat
yhat.boost=predict(boost.data,newdata=data[-train,],n.trees=5000)
mean((data.test-yhat.boost)^2)
#l'utilisation du shrinkage=0.2 donne une légère réduction de la RSE par rapport au boosting par défaut









Class[Class==0]=-1
Class

#install.packages('e1071')
data1=sample(data[1:100,])
library(e1071)
svm.fit=svm(Class~V4+V10+V14,data=data1,subset=train,na.action = na.omit(V4, V10, V14),scale=T)
summary(svm.fit)
plot(svm.fit,data,Class~.)
svm.pred=predict(svm.fit, data[!train,])
names(svm.pred)
table(svm.pred$class,Class.150000)
length(svm.pred$class)
mean(svm.pred$class==Class.150000)
mean(svm.pred$class!=Class.150000)

svm(V1,V2,kernel='linear')






set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
svmfit$nSV
plot(svmfit, dat)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


set.seed(1)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 2, pch = 19)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 100, scale = FALSE)
print(svmfit)
plot(svmfit, dat)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


