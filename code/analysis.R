library(openxlsx)
library(sf)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(cluster)
library(factoextra)
library(dbscan)
library(fpc)
library(caret)

#wczytanie przygotowanych danych za 2016,2017,2018,2019 rok
Gus_2016 <- read.xlsx("GUS_2016_2019.xlsx", sheet = 1)
Gus_2017 <- read.xlsx("GUS_2016_2019.xlsx", sheet = 2)
Gus_2018 <- read.xlsx("GUS_2016_2019.xlsx", sheet = 3)
Gus_2019 <- read.xlsx("GUS_2016_2019.xlsx", sheet = 4)


sciagawka<- read.xlsx("GUS_2016_2019.xlsx", sheet = 5)

View(Gus_2016)
View(sciagawka)
#wykres gêstoœci zeby zobaczyc skosnosc wybranych zmiennych

Gus_2016 %>% keep(is.numeric) %>%           # tylko numeryczne
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +     
  geom_density (fill="red")


summary(Gus_2016)
shapiro.test(Gus_2016$KM_CO)
#dominuj¹ zmienne z rozk³adem skoœnym prowstronnie


# Konstrukcja miernika syntetycznego --------------------------------------
#unitaryzacja zerowana // min-max normalization, kwestia stymulanty i destymulanty
#stymulanta
minmaxS <- function(x, na.rm = FALSE) {
  return((x- min(x)) /(max(x)-min(x)))
}
#destymulanta
minmaxD <- function(x, na.rm = FALSE) {
  return((max(x)- x) /(max(x)-min(x)))
}
#wybieram i normalizuje stymulanty
gus2016_normS <- as.data.frame(lapply(select(Gus_2016, 4:6,8:12,14:15,17:26), minmaxS))

#wybieram i normalizuje destymulanty
gus2016_normD <- as.data.frame(lapply(select(Gus_2016, 3,7,13,16), minmaxD))

#lacze i dodaje TERYT, nazwa gminy
gus2016_norm <- cbind(gus2016_normS, gus2016_normD) 
rownames(gus2016_norm) <- Gus_2016$GMINA

#Tworzenie syntetycznego wskaznika rozwoju (Bray-Curtis) na podstawie Perda? // przedzial na stymulanty i destymulanty dzieki normalizacji nie jest potrzebny

# kod ls poczatek
wskaznik <- function(x) {
  x_abs <- as.data.frame(lapply(x, function(y) abs(y - max(y))))
  x_sum <- as.data.frame(lapply(x, function(y) y + max(y)))
  return(1 - rowSums(x_abs) / rowSums(x_sum))
}

gus2016_norm <- gus2016_norm %>%
  select(order(colnames(gus2016_norm))) %>%
  mutate(Rozwoj = wskaznik(.),
         K_Ludzki = wskaznik(select(., 9:14)),
         K_Spoleczny = wskaznik(select(., 20:24)),
         K_Materialny = wskaznik(select(., 15:19)),
         K_Finansowy = wskaznik(select(., 3:8)),
         Innowacje = wskaznik(select(., 1:2)))

gus2016_base <- select(gus2016_norm, Rozwoj:Innowacje)  #tworze tabele z wskaznikami syntetycznymi i nazw¹ gminy
gus2016_norm <- select(gus2016_norm, -(Rozwoj:Innowacje))

#tworze tabele z samymi znormalizowanymi wskaznikami, bez syntetycznych
gus2016_norm <- select(gus2016_norm, KL_PN:KM_WK)
rownames(gus2016_norm) <- Gus_2016$GMINA

# Analiza skupieñ ---------------------------------------------------------
#k-œrednich algorytm oparty na podziale
#szukanie optymalnej iloœci skupieñ Elbow Method
set.seed(123)
fviz_nbclust(gus2016_base, kmeans, method = "wss", k.max = 10)+geom_vline(xintercept = 4, linetype = 2)#4?
is.na(gus2016_base)
#szukanie optymalnej iloœci skupieñ Average Silhouette Method
fviz_nbclust(gus2016_base, kmeans, method = "silhouette") #idealnie 2, do 5 jest ok

#szukanie optymalnej iloœci skupieñ Gap Statistic Method
gap_stat <- clusGap(gus2016_base, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 100)
print(gap_stat, method = "firstmax")  #najwiêksza wartpsc gap dla 4, ale roznice sa niewielkie
fviz_gap_stat(gap_stat)
#lub? , daj¹ inne wyniki, nstart?
fviz_nbclust(gus2016_base, kmeans, method = "gap_stat")

#wybieram 4? niski, œrednioniski, œredniowysoki, wysoki?
k1 <- kmeans(gus2016_base$Rozwoj, centers = 3, nstart = 10)  #zmienic kolizje etykiet
fviz_cluster(k1, data = gus2016_base)  #  https://uc-r.github.io/kmeans_clustering

Poziom_Rozwoju <- data.frame(k1$cluster)
colnames(Poziom_Rozwoju) <- c("Poziom_Rozwoju")
#nadaje nazwy klastrom
Poziom_Rozwoju[Poziom_Rozwoju=="1"] <- "œredni"
Poziom_Rozwoju[Poziom_Rozwoju=="2"] <- "niski"
Poziom_Rozwoju[Poziom_Rozwoju=="3"] <- "wysoki"

#dodaje cluster do ramki danych
gus2016_base <- gus2016_base %>% 
  data.frame(Poziom_Rozwoju)          

gus2016_norm <-  gus2016_norm %>% 
  data.frame(Poziom_Rozwoju)

#as factor
gus2016_base$Poziom_Rozwoju <- as.factor(gus2016_base$Poziom_Rozwoju)
gus2016_norm$Poziom_Rozwoju <- as.factor(gus2016_norm$Poziom_Rozwoju)

#DBSCAN - algorytm oparty na gêstoœci 
kNNdistplot(gus2016_base, k =  3)
abline(h = 0.135, lty = 2)
set.seed(12345)
model.dbscan <- dbscan(gus2016_base, eps = 0.175, MinPts = 2)
fviz_cluster(model.dbscan, gus2016_base, geom = 'point') 

gus2016_norm <- select(gus2016_norm, -Poziom_Rozwoju)

#Optical  
model.optics <- optics(gus2016_base, eps = 0.175, minPts = 2)
model.optics$order
plot(model.optics) # Reachability plot
(result.optics <- extractDBSCAN(model.optics, eps_cl = 0.175))
fviz_cluster(list(data = gus2016_base, 
                  cluster = result.optics$cluster), 
             geom = 'point') # Red points are outliers.

#DBSCAN, OPTICAL 2 klasy, metody k-œrednich sugeruja ze wieksza liczba klas "ujdzie" - dylemat  
# "na oko" zrobi³bym 3 lub 4      //dopasowaæ nazwy klastrów i tytu³y wykresów/osi spojne



# Klasyfikacja  -----------------------------------------------------------
#lasy losowe
library(randomForest)
#model czynniki syntetyczne
model.forest <- randomForest(Poziom_Rozwoju ~ .,
                             data = gus2016_base,
                             ntree = 300,
                             importance = TRUE,
                             proximity = TRUE)
plot(model.forest)
varImpPlot(model.forest,
           sort = TRUE) # istotnosc czynników
importance(model.forest) #ciekawe roznice w isotnosci dla k.spolecznego, ludzikiego i finansowego ktory wykazuje swa istotnosc zwlaszcza w gminach o srednim poziomie rozwoju
varUsed(model.forest) #najwiêksz¹ istotnoœcia cechuje siê Rozwoj 

gus2016_norm_R <-  select(gus2016_norm_R, IT_JN:Rozwoj)
#model znormalizowane czynniki
gus2016_norm <- select(gus2016_norm, Poziom_Rozwoju) %>%
  data.frame(gus2016_normS) %>% 
  data.frame(gus2016_normD)
  
model.forestN <- randomForest(Poziom_Rozwoju ~ .,
                             data = gus2016_norm,
                             ntree = 600,
                             importance = TRUE,
                             proximity = TRUE) # OOB estimate of  error rate: 11.4%


plot(model.forestN)
varImpPlot(model.forestN,
           sort = TRUE) # istotnosc czynników
importance(model.forestN) #dostepnosc do gazu zdecydowanie najbardziej znaczace, potem ogrzewanie centralne, podmioty gosp w sekcjach J-N (informatyka, uslugi) // ciekawe ze przyrost naturalny i skolaryzacja tak nisko- mo¿e dlatego ¿e wskaŸniki s¹ podobne wszêdzie
varUsed(model.forestN) #stosunkowo ma³y rozrzut wartoœci istotnosci

library(randomForestExplainer)
plot_multi_way_importance(model.forestN, no_of_labels = 10)
#Wykres szans na zaklafikowanie jako wysoki rozwoj

partialPlot(model.forest, gus2016_base, Rozwoj, "niski")
partialPlot(model.forestN, gus2016_norm, Rozwoj, "œredni")
partialPlot(model.forestN, gus2016_norm, Rozwoj, "wysoki")

p1 <- predict(model.forest , gus2016_base)
gus2016_base$rm_cluster <-  p1
confusionMatrix(p1, gus2016_norm$Poziom_Rozwoju)

p2 <- predict(model.forestN, gus2016_norm)
accuracy_score(gus2016_norm, model.forestN)


gus2016_norm$rm_cluster <- p2

hist(treesize(model.forest)) #histogram 
hist(treesize(model.forestN))

print(classification_report(y_test, y_pred_test))

#xgboost
library(xgboost)
library(DiagrammeR)

dtrain <- list(as.matrix(gus2016_norm[, 1:24]), 
               label = as.numeric(gus2016_norm$Poziom_Rozwoju) - 1) # na znormalizowanych wskaznikach

(xgb.fit <- xgboost(data = dtrain[[1]],
                    label = dtrain[[2]],
                    eta = 0.1, # Step size shrinkage used in update to prevents overfitting. The range is 0 to 1. Low eta value means model is more robust to overfitting.
                    max_depth = 10, # The maximum depth of a tree
                    nround = 100, # The max number of iterations
                    subsample = 0.5, # The subsample ratio of the training instance
                    eval_metric = 'mlogloss', # The evaluation metrics for validation data
                    objective = 'multi:softprob', # The learning task
                    num_class = 3, # The number of classes
                    nthread = 4)) # The number of threads
xgb.plot.importance(xgb.importance(colnames(gus2016_norm),
                                   model = xgb.fit, rel_to_first = TRUE, xlab = "Relative importance")) # Variables importance // podobnie gaz i ogrzewanie centralne, JN + inaczej bo osoby fizyczne prowadzace dzia³anoœæ gospodarcz¹, ma³o istotne - dodatek mieszkaniowy, dochody z rolnictwa, przychodnie

xgb.plot.multi.trees(colnames(gus2016_norm), model = xgb.fit) # Ensemble tree
pxp1 <- predict(xgb.fit, dtrain[[1]], reshape = TRUE) # Posterior probabilities
rownames(pxp1) <- Gus_2016$GMINA
#wyznaczam clustry na podstawie predict (max.col)
xgb_cluster <- max.col(pxp1)
pxp1 <- cbind(pxp1,xgb_cluster)
pxp1 <- as.data.frame(pxp1)

pxp1$xgb_cluster[xgb_cluster=="1"] <- "niski"
pxp1$xgb_cluster[xgb_cluster=="2"] <- "œredni"
pxp1$xgb_cluster[xgb_cluster=="3"] <- "wysoki"
gus2016_base$xgb_cluster <-  pxp1$xgb_cluster


# Analiza wariancji -------------------------------------------------------
ggboxplot(gus2016_base, x = "Poziom_Rozwoju", y = "Rozwoj", 
          color = "Poziom_Rozwoju",
          xlab = "Poziom rozwoju", ylab = "Syntetyczny wskaŸnik rozwoju")

#test Kruskala-Wallis
kruskal.test(gus2016_base$Rozwoj~gus2016_base$Poziom_Rozwoju) #jest znacz¹ca ró¿nica miêdzy clastrami rozwoju

#wilcox
pairwise.wilcox.test(gus2016_base$Rozwoj, gus2016_base$Poziom_Rozwoju,
                     p.adjust.method = "BH") #wszystkie 3 klastry znacz¹co siê ró¿ni¹  //metoda Benjamini-Hoch 

#ANOVA
ANOVA <- aov(gus2016_base$Rozwoj~gus2016_base$Poziom_Rozwoju)
summary(ANOVA) #znacz¹ca ró¿nica miêdzy grupami 

#testy post-hoc
TukeyHSD(ANOVA) #najwiêksza ró¿nica miêdzy poziomami œredni-niski
pairwise.t.test(gus2016_base$Rozwoj, gus2016_base$Poziom_Rozwoju,
                p.adjust.method = "BH") #wszystkie znacz¹co siê ró¿ni¹ //metoda Benjamini-Hoch 

pairwise.t.test(gus2016_base$Rozwoj, gus2016_base$Poziom_Rozwoju,
                p.adjust.method = "holm") #testowa³em ró¿nymi metodami, ka¿da daje tak¹ sam¹ wartoœæ p

library(multcomp)
summary(glht(ANOVA, lincft = mcp(group = "Tukey"))) # znacz¹ca ró¿nica miêdzy klastrami/poziomami rozwoju

#wykres jednorodnoœci wariancji
plot(ANOVA, 1) # gminy 99, 2, 94 s¹ "odstaj¹ce"

library(car)
leveneTest(gus2016_base$Rozwoj~gus2016_base$Poziom_Rozwoju) #mo¿emy za³o¿yæ jednorodnoœæ wariancji 

#wykres normalnosci reszt  // sprawdzamy za³o¿enie ¿e reszty maj¹ rozk³ad normalny, 
plot(ANOVA, 2) # punkty le¿¹ w przybli¿eniu na lini prostej, kwantyle 2 zaczynaj¹ odbiegaæ od prostej

#test shapiro-wilka na resztach w celu sprawdzenia normalnoœci
ANOVA_reszty <- residuals(object = ANOVA)
shapiro.test(x = ANOVA_reszty ) #p wiêksze od 0.05 wiêc mo¿emy za³o¿yæ normalnoœæ


#usuwanie niepotrzebnych danych
rm(list=setdiff(ls(), "gus_2016", 'gus2016_base', 'gus2016_norm'))


