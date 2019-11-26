aspiradoras<-as.data.frame(res)
#Procesado de peso

aspiradoras$`Peso del producto`<-as.character(aspiradoras$`Peso del producto`)
aspiradoras$`Peso del producto`<-gsub(" Kg", "", aspiradoras$`Peso del producto`)
aspiradoras$`Peso del producto`<-gsub(",", ".", aspiradoras$`Peso del producto`)
aspiradoras$`Peso del producto`<-gsub("-1",NA, aspiradoras$`Peso del producto`)
aspiradoras$`Peso del producto`<-as.numeric(aspiradoras$`Peso del producto`)
pesomedio<-mean(aspiradoras$`Peso del producto`, na.rm=TRUE)
pesomedio
aspiradoras$`Peso del producto`[is.na(aspiradoras$`Peso del producto`)]<-pesomedio
hist(aspiradoras$`Peso del producto`)
summary(aspiradoras$`Peso del producto`)

#Procesado de volumen
aspiradoras$Volumen<-as.character(aspiradoras$Volumen)
aspiradoras$Volumen<-gsub(" litros", "", aspiradoras$Volumen)
aspiradoras$Volumen<-gsub("-1",NA, aspiradoras$Volumen)
aspiradoras$Volumen<-as.numeric(aspiradoras$Volumen)
volumenmedio<-mean(aspiradoras$Volumen, na.rm=TRUE)
aspiradoras$Volumen[is.na(aspiradoras$Volumen)]<-volumenmedio
hist(aspiradoras$Volumen)



#Siguiente parametro numero de opiniiones
str(aspiradoras$Opiniones)
#Convertimos a char para procesar
aspiradoras$Opiniones<-as.character(aspiradoras$Opiniones)
aspiradoras$Opiniones<-gsub("opiniones de clientes", "", aspiradoras$Opiniones)
aspiradoras$Opiniones<-gsub("opinión de cliente", "", aspiradoras$Opiniones)
aspiradoras$Opiniones<-gsub(",", "", aspiradoras$Opiniones)
aspiradoras$Opiniones<-as.numeric(aspiradoras$Opiniones)
aspiradoras$Opiniones[is.na(aspiradoras$Opiniones)] <- mean(aspiradoras$Opiniones, na.rm = TRUE)
hist(aspiradoras$Opiniones)
boxplot(aspiradoras$Opiniones)


#Siguiente parametro numero de precio
str(aspiradoras$Precio)
#Convertimos a char para procesar
aspiradoras$Precio<-as.character(aspiradoras$Precio)
aspiradoras$Precio<-gsub("EUR", "", aspiradoras$Precio)
aspiradoras$Precio<-gsub("€", "", aspiradoras$Precio)
aspiradoras$Precio<-gsub(",", ".", aspiradoras$Precio)
#Eliminamos el ultimo character
aspiradoras$Precio<-str_sub(aspiradoras$Precio, 1, str_length(aspiradoras$Precio)-1)
aspiradoras$Precio<-as.numeric(aspiradoras$Precio)
aspiradoras$Precio[is.na(aspiradoras$Precio)] <- mean(aspiradoras$Precio, na.rm = TRUE)
hist(aspiradoras$Precio)
boxplot(aspiradoras$Precio)

str(aspiradoras$Potencia)
#Convertimos a char para procesar
aspiradoras$Potencia<-as.character(aspiradoras$Potencia)
aspiradoras$Potencia<-gsub("watt_hours", "", aspiradoras$Potencia)
aspiradoras$Potencia<-gsub("vatios", "", aspiradoras$Potencia)
aspiradoras$Potencia<-gsub("-1", NA, aspiradoras$Potencia)
aspiradoras$Potencia<-as.numeric(aspiradoras$Potencia)
pmean<-mean(aspiradoras$Potencia, na.rm = TRUE)
aspiradoras$Potencia[is.na(aspiradoras$Potencia)] <- pmean

library(stringr)
aspiradoras$`Dimensiones del producto`<-as.character(aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto`<-gsub("cm", "", aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto`<-gsub(",", ".", aspiradoras$`Dimensiones del producto`)
dimen<-str_split_fixed(aspiradoras$`Dimensiones del producto`,"x", ncol=3)


res_limpio<-cbind(aspiradoras, dimen)
res_limpio<-res_limpio[,-5]
