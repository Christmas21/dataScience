#* @param precio
#* @param opiniones
#* @param peso
#* @param volumen
#* @param potencia
#* @param alto
#* @param ancho
#* @param profunidad
#* @get /getCluster
function(precio, opiniones, peso,volumen,potencia,alto,ancho,profundidad){
  library(stringr)
  #campos<-str_split_fixed(values, ",", n=8)  
  campos<-c(precio,opiniones,peso,volumen,potencia,alto,ancho,profundidad)?
  campos<-as.numeric(campos)
  mycluster$centers
  #Vamos a crear una matriz donde( almacenar por atributo la distancia a cada cluster
  midist<-matrix(0, ncol=8, nrow=8)
  for(i in 1:8){
    a<-dist(x=c(campos[i],mycluster$centers[,i])) #Calculamos  la distancia del valor a cada centro
    b<-as.matrix(a) #Lo convertimos a matriz para poder acceder a los vlaores
    distancia<-b[-1,1] #Eliminamos la distancia consigo mismo
    midist[i,]<-distancia
  }
  rownames(midist)<-c("Precio" , "Opiniones","Peso del producto" ,"Volumen","Potencia", "alto", "ancho" ,"profundidad")
  dist_total<-apply(midist, 2, sum)#Sumamos las distancias
  clus<-which.min(dist_total) #Identificamos el cluster de menor distacncia
  val<-paste(as.character(mycluster$centers[clus,]), collapse = ",")
  paste0("Cluster asociado es ",clus," con valores: ", val )
  
}

#curl "http://localhost:8000/getCluster?precio=-0.2458767&opiniones=0.3095074&peso=1.0861028&volumen=0.32609&potencia=0.1184234&alto=0.322571&ancho=0.4440596&profundidad=-0.3254523"
