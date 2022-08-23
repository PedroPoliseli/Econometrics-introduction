library(stats)
library(ggplot2)
library(lmtest)
library(skedastic)
library(plm)


#Regressão simples

View(mtcars)


#Milhas por galão e potencia.
reg_mpg.hp <- lm(data = mtcars, mpg~hp)
summary(reg_mpg.hp)

ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


#Potencia e cilindros


ggplot(mtcars, aes(cyl,hp))+
  geom_point()


reg_hp.cyl <- lm(data= mtcars, hp ~ cyl)
summary(reg_hp.cyl)


ggplot(mtcars, aes(cyl,hp))+
  geom_point()+
  geom_smooth(method = "lm", se = F)


reg_mpg.cyl <- lm(data = mtcars, mpg ~ cyl)
summary(reg_mpg.cyl)



#Regressão multipla

reg_mpg_hp.cyl <- lm(data = mtcars, mpg ~ hp + cyl + wt)
summary(reg_mpg_hp.cyl)


sqrt(sum(reg_mpg_hp.cyl$residuals^2)/ncol(mtcars))

#Variaveis Dummies

plantas <- iris

plantas$setosa <- ifelse(plantas$Species == "setosa",1,0)
plantas$versicolor <- ifelse(plantas$Species == "versicolor",1,0)
plantas$virginica <- ifelse(plantas$Species == "virginica", 1,0)


reg_lenght.width <- lm(data = plantas,  Sepal.Length ~ Sepal.Width + 
                         Petal.Length + setosa + virginica)
summary(reg_lenght.width)


#Heterodasticidade 

View(trees)

reg_volume.height <- lm(data = trees, Volume ~  Height)
summary(reg_volume.height)

ggplot(trees, aes(Height,Volume))+
  geom_point()+
  geom_smooth(method = "lm", se =T)

plot(reg_volume.height)


#Testes de heterocedasticidade:


#Breush-Pagan:
#H0: Homecedastico
#H1: Heterocedastico


bp.teste <- bptest(reg_volume.height)
bp.teste$p.value


#White test:
#H0: Homecedastico 
#H1: heterocedastico

white_lm(reg_volume.height)


#tratamentos para heterocedasticidade:


#feasiable generalised minimum squares
fgls <- lm(data = trees, Volume ~ Height, weights = 1/reg_volume.height$residuals^2)
summary(fgls)
























