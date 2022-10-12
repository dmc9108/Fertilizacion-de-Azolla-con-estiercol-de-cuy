
# Cargando la base de datos preexp 

library(readxl)
preexp <- read_excel("~/MEGA/Daniel Mancilla. Fertilización de Azolla con Curinaza/preexp.xlsx", 
                     range = "A1:G6")
View(preexp)

# Rendimiento promedio g/m2/día en BH

t.test(preexp$`Rendimiento/d?a BH`)  # Media e IC al 95% para la media

# Gráfico de líneas "Obtencion de Azolla en Base Húmeda"

datos <- data.frame("Azolla sembrada" = preexp$`Cantidad sembrada (g)`, 
                    "Azolla cosechada" = preexp$`Azolla obtenida (g)`, 
                    "Rendimiento obtenido en base humeda" = preexp$`Rendimiento BH (g)`)      
View(datos)


matplot(preexp$`D?a de cosecha`, datos, , type = "l", 
        main = "Obtenci?n de Azolla en Base H?meda", 
        xlab = "Cosecha (d?as)", ylab = "Cantidad (g)",
        col = c("blue","red", "green"),
        lty = 2, lwd = 2, xaxt = "n", las = 1)

  grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

axis(1, at = c(8, 16, 24, 32, 40))

  legend(x = "top",                            
       legend = c("Azolla sembrada", "Azolla obtenida", "Rendimiento"),     
       lty = 1,                                 
       col = c("blue","red", "green"),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n")                

# Rendimiento promedio g/m2/d?a en MS
  
t.test(preexp$`Rendimiento/d?a MS`)  # Media e IC al 95% para la media 
  

# Gr?fico de l?neas "Estimaci?n de Azolla en Materia seca"  

datos_seca <- data.frame("Rendimiento obtenido en base humeda" = preexp$`Rendimiento BH (g)`,
                         "Rendimiento obtenido en materia seca" = preexp$`Rendimiento MS (g)`) 
View(datos_seca)


matplot(preexp$`D?a de cosecha`, datos_seca, , type = "l", 
        main = "Estimaci?n del rendimiento de Azolla en Materia Seca",
        xlab = "Cosecha (d?as)", ylab = "Cantidad (g)",
        col = c("blue","red"),
        lty = 2, lwd = 2, xaxt = "n", yaxt = "n", ylim = c(0, 5000))

  grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

axis(1, at = c(8, 16, 24, 32, 40))
axis(2, at = c(50, 850, 1650, 2450, 3250, 4050, 4850), las = 1)

legend(x = "top",                            
       legend = c("Rendimiento obtenido en BH (93,36%)", 
                  "Rendimiento estimado en MS (6,64%)"),       
       lty = 1,                                 
       col = c("blue","red"),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n")                


# Comparacion de la dosificacion con esti?rcol de cuy

library(readxl)
exp <- read_excel("~/MEGA/Daniel Mancilla. Fertilización de Azolla con Curinaza/exp.xlsx", 
                     range = "A1:G6")
View(exp)


# Librer?as para el an?lisis
library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)


# cargando y comprobando los datos
str(exp)

# ANOVA
anova <- aov(Rendimiento ~ Tratamiento, data = exp)
summary(anova)

exp$Tratamiento <- as.factor(exp$Tratamiento)


# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(exp, Tratamiento) %>%
  summarise(mean=mean(Rendimiento), quant = quantile(Rendimiento, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Tratamiento)
Tk$cld <- cld$Letters

print(Tk)

# Gr?fico de cajas y bigotes


box_plot <- boxplot(exp$Rendimiento~exp$Tratamiento, main = "Rendimiento de Azolla por tratamiento",
                    xlab = "Esti?rcol de cuy", ylab = "Rendimiento (g)",
                    col = c("green","blue","red"), ylim=c(min(exp$Rendimiento) , 1.1*max(exp$Rendimiento)),
                    horizontal = F, names = c("T0", "T1", "T2"), las = 1)

grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

stripchart(exp$Rendimiento~exp$Tratamiento, vertical = TRUE, method = "jitter",
           pch = 7, add = TRUE, col = c(5,2,3))



legend(x = "top",                           
       legend = c("Repeticiones T0", "Repeticiones T1", "Repeticiones T2"),       
       lty = 0,                                
       col = c(5,2,3),          
       lwd = 2,
       inset = c(0, -0.15),
       xpd = TRUE,
       horiz = TRUE,
       bty = "n",
       pch = 7)  

over <- 0.1*max( box_plot$stats[nrow(box_plot$stats),] )
text( c(1:nlevels(exp$Tratamiento)) , box_plot$stats[nrow(box_plot$stats),]+over , LABELS[,1])

# Media e IC al 95% para la media por tratamiento (g)

Tratamiento_0 <- subset(exp$Rendimiento, exp$Tratamiento==0)
Tratamiento_1 <- subset(exp$Rendimiento, exp$Tratamiento==1)
Tratamiento_2 <- subset(exp$Rendimiento, exp$Tratamiento==2)

t.test(Tratamiento_0)
t.test(Tratamiento_1)
t.test(Tratamiento_2)

# Media e IC al 95% para la media por tratamiento (g)

Tratamiento_0 <- subset(exp$Rendimiento, exp$Tratamiento==0)
Tratamiento_1 <- subset(exp$Rendimiento, exp$Tratamiento==1)
Tratamiento_2 <- subset(exp$Rendimiento, exp$Tratamiento==2)

t.test(Tratamiento_0)
t.test(Tratamiento_1)
t.test(Tratamiento_2)

# Media e IC al 95% para la media por tratamiento (g/m2/d?a)

Tratamiento_0 <- subset((exp$Rendimiento/21/2), exp$Tratamiento==0)
Tratamiento_1 <- subset((exp$Rendimiento/21/2), exp$Tratamiento==1)
Tratamiento_2 <- subset((exp$Rendimiento/21/2), exp$Tratamiento==2)

t.test(Tratamiento_0)
t.test(Tratamiento_1)
t.test(Tratamiento_2)

