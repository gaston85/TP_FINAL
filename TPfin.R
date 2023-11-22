library(hdf5r)
library(ggplot2)
library(metR)
lista_variables<-function(ruta_archivo,lista_rutas){
  #Ingresan rutas con formato "FS/SLV/variable"
  
  #Transformo la ruta a formato [["FS"]][["SLV"]][["variable"]]
  
  #Obtengo una lista con la informacion de la variable
  
  archivo<-H5File$new(ruta_archivo,mode = "r+")
  lista_var<-list()
  for (i in 1:length(lista_rutas)) { 
  #Separo la ruta en funcion de /, y averiguo la cantidad de elementos
    
    if(length(strsplit(lista_rutas[[i]],"/")[[1]])==1){
  #En esta parte averiguo que variables son atributos o variables
      if(info$attr_exists(lista_rutas[[i]])){
        variable<-info$attr_open(attr_name = lista_rutas[[i]][1])
        lista_var[[eval(parse(text='strsplit(lista_rutas[[i]][1],"/")[[1]][1]'))]]<-variable
  #Genero un elemento de la lista con la informacion de la variable y la nombro como la variable
      }else{
        variable<-info[[lista_rutas[[i]]]]
        lista_var[[lista_rutas[[i]]]]<-variable
      }
  #Para generar las variables en este tipo de archivo obligatorimente tienen que tener un 
  #formato [["FS"]][["SLV"]][["variable"]], esto limita demasiado a la hora de querer transformar el formato
  #ya que hay que colocar comillas dentro de comillas.
    }else if(length(strsplit(lista_rutas[[i]],"/")[[1]])==2){
      variable<-eval(parse(text=paste('archivo[["',gsub("/",'"]][["',lista_rutas[[i]][1]),'"]]$read()',sep = "")))
  
  #A la ruta le cambio las barras que separan los terminos por los corchetes que lo separarian con la funcion gsub.
  #Al pegar los terminos queda "archivo[[\"FS\"]][[\"Longitude\"]]$read()", lo que genera un problema con las \.
  #parse es una funcion que genera una expresion sin evaluar, la cual me soluciona el problema de las \ y las comillas que se forman
  #por la funcion paste. Como quiero los datos en la variable, con la funcion eval, evaluo el termino y consigo esos datos.
  
      lista_var[[eval(parse(text='strsplit(lista_rutas[[i]][1],"/")[[1]][2]'))]]<-variable
    
    }else if(length(strsplit(lista_rutas[[i]],"/")[[1]])==3){
      variable<-eval(parse(text=paste('archivo[["',gsub("/",'"]][["',lista_rutas[[i]][1]),'"]]$read()',sep = "")))
      lista_var[[eval(parse(text='strsplit(lista_rutas[[i]][1],"/")[[1]][3]'))]]<-variable
    }
  }
  h5close(archivo)
  return(lista_var)
}

nombre_archivo<-"2A.GPM.DPR.V9-20211125.20231030-S074316-E091549.054943.V07B.HDF5"
ruta_del_archivo<-paste(getwd(),nombre_archivo,sep = "/")
#rutas:("FileHeader","InputRecord","AlgorithmRuntimeInfo","NavigationRecord","FileInfo","JAXAInfo","FS/Latitude","FS/Longitude","FS/SLV/precipRateESurface","FS/SLV/zFactorFinalESurface")

rutas_variables<-list("FS/Latitude","FS/Longitude","FS/SLV/precipRateESurface","FS/SLV/zFactorFinalESurface")
variables_necesarias<-lista_variables(ruta_del_archivo,rutas_variables)

tasa_pp<-as.data.frame(variables_necesarias$precipRateESurface)
factor_reflectividad<-as.data.frame(variables_necesarias$zFactorFinalESurface)
lat<-as.vector(variables_necesarias$Latitude)
lon<-as.vector(variables_necesarias$Longitude)

ggplot(tasa_pp) + geom_point(data=data.frame(lon=lon,lat=lat),aes(x=lon,y=lat))
  