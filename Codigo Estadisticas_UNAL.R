#Respuestas parcial Estadistica 2
#UDEC Estadistica inferencial
#Punto 1
UN2023_2 <- read_csv("Estudiantes_matriculados_UNAL_20241015.csv")
install.packages("confintr")
library(confintr)
install.packages("pwr")
library(pwr)
#Población total 28006 matriculados en la sede Bogotá
#PUNTO 2
table(UN2023_2$SEXO)
table(UN2023_2$ESTRATO)
table(UN2023_2$TIPO_COL)
summary(UN2023_2$EDAD)
sd(UN2023_2$EDAD)
hist(UN2023_2$EDAD)
#PUNTO 3- TODAS LAS RELACIONES SON SIGNIFICATICAS
table(UN2023_2$SEXO,UN2023_2$ESTRATO)
table(UN2023_2$SEXO,UN2023_2$TIPO_COL)
table(UN2023_2$ESTRATO,UN2023_2$TIPO_COL)
A<-chisq.test(UN2023_2$SEXO,UN2023_2$ESTRATO)
B<-chisq.test(UN2023_2$SEXO,UN2023_2$TIPO_COL)
C<-chisq.test(UN2023_2$ESTRATO,UN2023_2$TIPO_COL)
print(A)
print(B)
print(C)
cramersv(A)
cramersv(B)
cramersv(C)
#Facultades Y muestra representativa
# Punto 4
table(UN2023_2$FACULTAD)
#Facultades
faculties <- c("Artes", "Ciencias", "Ciencias Agrarias", "Ciencias Económicas", 
               "Ciencias Humanas", "Derecho, Ciencias Políticas y Sociales", 
               "Enfermería", "Ingeniería", "Medicina", 
               "Medicina Veterinaria y de Zootecnia", "Odontología")

counts <- c(2654, 3627, 813, 2915, 4174, 1572, 829, 7075, 2585, 1095, 667)

# Genera una base de datos
results <- data.frame(Faculty = faculties, 
                      Count = counts, 
                      SampleSize_95 = NA, 
                      SampleSize_99 = NA)

# Niveles de confianza
confidence_levels <- c(0.95, 0.99)
z_values <- c(1.96, 2.576) 
margin_of_error <- 0.05    
p_estimate <- 0.5          

# Se calcula para cada facultad el tamaño de las muestras para ambos niveles de confianza.
for (i in 1:length(faculties)) {
  for (j in 1:length(confidence_levels)) {
        n <- (z_values[j]^2 * p_estimate * (1 - p_estimate)) / (margin_of_error^2)
        finite_population_correction <- n / (1 + (n - 1) / counts[i])
            if (j == 1) {
      results$SampleSize_95[i] <- ceiling(finite_population_correction) 
    } else {
      results$SampleSize_99[i] <- ceiling(finite_population_correction)  
    }
  }
}
# ver tabla final y es la respuesya del punto 4
print(results)
#punto 5
install.packages("tigerstats")
library(tigerstats)
ING<-subset(UN2023_2,UN2023_2$FACULTAD=="Ingeniería")
#población de ING 7075
#muestra representativa 365
MING<-popsamp(365,ING)
cor.test(MING$EDAD, MING$PBM)
cor.test(UN2023_2$EDAD,UN2023_2$PBM)
#Varia un monton y debe ser negativa pero significativa
install.packages("WebPower")
library(WebPower)
wp.correlation(n=365, r=-0.14)
#MINIMO .765
#punto 6
wp.correlation(n=NULL,r=0.3, power=0.85)
wp.correlation(n=NULL,r=0.5, power=0.85)
wp.correlation(n=NULL,r=0.7, power=0.85)
wp.correlation(n=NULL,r=0.3, power=0.9)
wp.correlation(n=NULL,r=0.5, power=0.9)
wp.correlation(n=NULL,r=0.7, power=0.9)

