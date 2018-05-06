##Datos=====================================================
## Define el conjunto de datos en una data.frame cuya
## última columna corresponde a la clase
##==========================================================
color=c("gris","incoloro","gris","blanco","gris","verde")
dureza=c("elevada","elevada","baja","baja","media","elevada")
raya=c("blanca","gris","blanca","blanca","cafe","cafe")
mineral=c("diamante","diamante","yeso","yeso","zinc","zinc")
datos=data.frame(color,dureza,raya,mineral)

##==========================================================
## Función para convertir factor a caracteres
## tuve que agregar esta función ya que en R
## la función c, convierte factores a enteros y esto
## causaba problemas con la función esConsistente
##
## ENTRADA
## datos: data.frame
##
## SALIDA
## datos: data.frame con columnas que antes eran de tipo
## factor ahora con tipo char
##==========================================================
convierteFactores=function(datos){
  nCol=dim(datos)[2]
  
  for(i in 1:nCol){
    if(is.factor(datos[,i])){
      datos[,i]=as.character(datos[,i])
    }
  }
  return(datos)
}
datos=convierteFactores(datos)
##==========================================================
## Función para separar clases negativas y positivas
## ENTRADA
## datos: data.frame con toda la información
## clase: clase positiva
## 
## SALIDA
## resultado: lista cuya entrada 1 es un data.frame con
## las clases positivas y cuya entrada 2 es un data.frame
## con las clases negativas
##==========================================================
separa=function(datos,clase){
  resultado=list()
  
  #Número de columnas
  nCol=dim(datos)[2]
  
  #índices de la clase positiva
  indicesPositivos=which(datos[,nCol]==clase)
  
  #Observaciones de la clase positiva
  positivas=datos[indicesPositivos,]
  
  #observaciones de la clase negativa
  negativas=datos[-indicesPositivos,]
  
  resultado[[1]]=positivas
  resultado[[2]]=negativas
  
  resultado
}

##==================================================================
## Función para revisar si una regla es consistente
## es decir, no cubre ejemplos negativos
##
## ENTRADA
## reglaMaquina: vector con los valores de cada atributo
## negativos: data.frame creado con la función separa. Contiene
## todos los ejemplos negativos
## operador: string que representa el operador del selector ("==",">",..)
## indicesAtributo: entero, índices de los atributos
## 
## SALIDA
## TRUE si la regla es consistente, FALSE en otro caso
##
## La idea es ir filtrando de manera sucesiva
## el data.frame negativos de acuerdo al
## valor de cada atributo en la regla y si se encuentra un
## data.frame con 
##==================================================================
esConsistente=function(reglaMaquina,negativos,operador,indicesAtributo){
  
  #Número de selectores en la regla
  nSelectores=length(reglaMaquina)
  
  aux=1
  for(i in indicesAtributo){
    valor=reglaMaquina[aux]
    
    #Magia!!!
    #Determina si alguna observación tiene el valor de la regla en el atributo i
    indices=eval(parse(text=paste("which(",paste("negativos[,i]","==",quote(valor),sep=""),")",sep="")))
    if(length(indices)==0){
      return (TRUE)
    }
    #filtra la tabla
    negativos=negativos[indices,]
    aux=aux+1
    
  }
  return (FALSE)
  
}

##==================================================================
## Función para generar el conjunto potencia de un conjunto
##==================================================================
conjuntoPotencia <- function(set) {     
  n <- length(set)
  keepBool <- sapply(2^(1:n - 1), function(k) 
    rep(c(FALSE, TRUE), each=k, times=(2^n / (2*k))))
  lapply(1:2^n, function(j) set[keepBool[j, ]])
}


##==================================================================
## Función para encontrar la estrella de una semilla
##
## ENTRADA
## semilla: un renglón de un data.frame
## negativos:Observaciones negativas (se crea con la función separa)
##
## SALIDA
## resultado: una lista con [[1]] la estrella en lenguaje máquina y
## [[2]] la estrella en lenguaje humano (después de aplicar LEF)
##===================================================================
generaEstrella=function(semilla,negativos){
  #Obtiene el número de atributos
  #número de columnas menos 1
  numAtributos<-dim(semilla)[2]-1
  
  #Nombre de los atributos
  nombresAtributos<-names(semilla)[1:numAtributos]
  
  #auxiliar para agregar reglas a las estrellas
  aux=1
  
  estrellaMaquina=list()
  estrellaHumano=list()
  
  #Comienza a generar las reglas
  #utilizando el conjunto potencia
  potencia=getPowSet(1:numAtributos)
  
  for (i in 2:length(potencia)){
    
    #Aquí podría incluir otro for
    #para el operador del selector
    operador="=="
    reglaMaquina=c()
    reglaHumano=c()
    
    elementos=potencia[[i]]
    
    #Hace la conjunción de los selectores
    for (j in elementos){
      reglaMaquina=c(reglaMaquina,semilla[1,j]) #valor del atributo j en la semilla
      reglaHumano=c(reglaHumano,paste(nombresAtributos[j],operador,semilla[1,j],sep=""))
    }
    
    #Revisa consistencia
    if(esConsistente(reglaMaquina,negativos,operador,elementos)){
      estrellaMaquina[[aux]]=reglaMaquina
      estrellaHumano[[aux]]=reglaHumano
      
      aux=aux+1
    }
  }
  
  #Aplicar aquí LEF (pendiente)
  resultado=list()
  resultado[[1]]=estrellaMaquina
  resultado[[2]]=estrellaHumano
  return (resultado)
  
}
















