# Duma Vladislav, 601-I
trees               #открытие набора данных trees
View(trees)         #визуализация таблицы trees

mytrees<-as.data.frame(trees) #создание фрейма
summary(mytrees)

cor(mytrees)
cormtrx<-cor(mytrees)
cormtrx

# Task 1.1
t.test(mytrees$Girth,mu=mean(mytrees$Girth))

wilcox.test(mytrees$Volume,mu=median(mytrees$Volume),conf.int=TRUE)

shapiro.test(mytrees$Girth)

ks.test(mytrees$Girth,"pnorm")

# Task 1.1, Height
t.test(mytrees$Height,mu=mean(mytrees$Height))

wilcox.test(mytrees$Height,mu=median(mytrees$Height),conf.int=TRUE)

shapiro.test(mytrees$Height)

ks.test(mytrees$Height,"pnorm")
#---------------------------------------
dev.new() # newgraphicalwindow
qqnorm(mytrees$Girth, main="Girth")
qqline(mytrees$Girth, col=2)

dev.cur()# current graphical window
qqnorm(mytrees$Volume,main="Volume")
qqline(mytrees$Volume, col=2)

plot(mytrees$Girth)
dev.new() # новое графическое окн
plot(mytrees$Girth, xlab= "Cases",ylab= "Girth")

hist(mytrees$Girth)
hist(mytrees$Girth, freq=FALSE)

hist(mytrees$Girth, 
     breaks = 6,
     freq=FALSE,
     col="lightblue", 
     xlab ="Cases",
     ylab = "Girth",
     main="HistogrambyGirth")
lines(density(mytrees$Girth),col="red", lwd=2)

# Task 1.2, Height & Volume

hist(mytrees$Height, 
     breaks = 6,
     freq=FALSE,
     col="lightblue", 
     xlab ="Cases",
     ylab = "Height",
     main="HistogrambyHeight")
lines(density(mytrees$Height),col="red", lwd=2)

hist(mytrees$Volume, 
     breaks = 6,
     freq=FALSE,
     col="lightblue", 
     xlab ="Cases",
     ylab = "Volume",
     main="HistogrambyVolume")
lines(density(mytrees$Volume),col="red", lwd=2)

#---------------------------------------

boxplot(mytrees$Girth,   
        mytrees$Height,
        mytrees$Volume,
        main = "Box&Whisker Plot",
        xlab="Girth Height Volume")

boxplot.stats(mytrees$Girth, 
              coef= 1, 
              do.conf= TRUE, 
              do.out= TRUE)

boxplot.stats(mytrees$Volume, 
              coef = 1.5, 
              do.conf = TRUE, 
              do.out = TRUE)
# Task 1.3
boxplot(mytrees$Volume, range = 1.5, width = NULL, varwidth = FALSE,
        notch = FALSE, outline = TRUE, names, plot = TRUE,
        border = par("fg"), col = NULL, log = "",
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
        horizontal = FALSE, add = FALSE, at = NULL)
#--------------------------------

# Task 1.4
dev.new()#  вывод в текущее окно
plot(mytrees$Height,
     mytrees$Girth,
     type="p",
     col="red",
     cex=1,
     xlab = "Height",
     ylab = "Girth",
     main = "Height vs Girth")

plot(mytrees$Girth,
     mytrees$Volume,
     type="p",
     col="red",
     cex=1,
     xlab = "Girth",
     ylab = "Volume",
     main = " Girth vsVolume")
abline(lm(mytrees$Volume~mytrees$Girth),col="blue",lwd=1)

x<-cbind(mytrees$Girth,
         mytrees$Height,
         mytrees$Volume) #подготовка данных
pairs(x,      
      gap=0,
      diag.panel= function(x)
          {par(new = TRUE)
          hist(x, col = "light pink", probability = TRUE)
          lines(density(x), col = "red", lwd = 2)})


