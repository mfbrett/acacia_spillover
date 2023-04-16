library(ggplot2)
install.packages("ggpmisc")
library(ggpmisc)
library(readxl)

## Importing data (enter your file path) ----
acaciaflowers <- read_excel("~/path/acaciaflowers.xlsx")

## Plot A saligna and A longifolia together, with regression equations and R2 values ----

bothacacia<-ggplot(data=acaciaflowers, aes(x=circumference,y=noflowers, colour=species)) + 
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()+geom_smooth(method='lm',se=TRUE) +
  theme_cowplot() +
  scale_color_manual(labels = c("A. longifolia", "A. saligna"), values = c("blue", "darkgreen")) +
  ggtitle("Acacia flower counts and tree circumferences") +
  xlab("Circumference (cm)")+ylab("Inflorescence freq.") 
bothacacia

## Plotting species seperately ----
View(acaciaflowers)  
circumference<-acaciaflowers$circumference
aspecies<-acaciaflowers$species
flowers<-acaciaflowers$noflowers
alcirc<-circumference[aspecies=="A.longifolia"]
alflowers<-flowers[aspecies=="A.longifolia"]
ascirc<-circumference[aspecies=="A.saligna"]
asflowers<-flowers[aspecies=="A.saligna"]

#### A.saligna Plot and regression line ----
par(mfrow=c(2,1))
as<-ggplot(data=NULL, aes(ascirc,asflowers))+geom_point()+geom_smooth(method='lm',se=FALSE)+
        ggtitle("A.saligna") +
        xlab("Circumference (cm)")+ylab("Inflorescence freq.") +
        stat_poly_eq(formula = asflowers~ascirc,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE)
lm(asflowers~ascirc)

#### A.longifolia Plot and regression line ----
al<-ggplot(data=NULL, aes(alcirc,alflowers))+geom_point()+geom_smooth(method='lm',se=FALSE)+
       ggtitle("A.longifolia") +
      xlab("Circumference (cm)")+ylab("Inflorescence freq.") + 
      stat_poly_eq(formula = alflowers~alcirc,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
lm(alflowers~alcirc)

# Plot side by side
library("gridExtra") 
grid.arrange(as, al, ncol = 2) 


