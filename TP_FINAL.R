library(hdf5r)
library(ggplot2)
library(patchwork)
require(RColorBrewer)
require(scales)

nombre_archivo<-"2A.GPM.DPR.V9-20211125.20231030-S074316-E091549.054943.V07B.HDF5"
ruta_del_archivo<-paste(getwd(),nombre_archivo,sep = "/")
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

#Punto B ####
rutas_variables<-list("FS/Latitude","FS/Longitude","FS/SLV/precipRateESurface","FS/SLV/zFactorFinalESurface")
variables_necesarias<-lista_variables(ruta_del_archivo,rutas_variables)

datos<-data.frame(PP=as.vector(variables_necesarias$precipRateESurface),
                  Ref=as.vector(variables_necesarias$zFactorFinalESurface[1,,]),
                  lon=as.vector(variables_necesarias$Longitude),
                  lat=as.vector(variables_necesarias$Latitude))

datos$Ref[datos$Ref==min(datos$Ref)]<-NA
datos$PP[datos$Ref==min(datos$PP)]<-NA

my_scale <- scale_colour_gradientn(name="Reflectividad",colours=rev(brewer.pal(9,"Spectral")),
                                   limits=c(10,80),
                                   na.value = "white",breaks=pretty_breaks(n=9),aesthetics = c("colour","fill"))
graf_Ref <- ggplot(datos,aes(x=lon,y=lat)) + 
  geom_path(map_data("world"),mapping=aes(long,lat,group=group),linewidth=0.1)+ 
  coord_quickmap(xlim = range(-51,-56.5),ylim = range(-25.28,-30)) +
  geom_point(datos,mapping=aes(x=lon,y=lat,color=Ref),size=3)+my_scale

my_scale2 <- scale_colour_gradientn(name="Tasa de Precipitacion",colours=c("#FFFFFF","#66D1FF","#2588E4","#125DFF","#BB32EC"),
                                    limits=c(0,100),
                                    na.value = "white",breaks=pretty_breaks(n=9),aesthetics = c("colour","fill"))


graf_PP <- ggplot(datos,aes(x=lon,y=lat)) + 
  geom_path(map_data("world"),mapping=aes(long,lat,group=group),linewidth=0.1)+ 
  coord_quickmap(xlim = range(-51,-56.5),ylim = range(-25.28,-30)) +
  geom_point(datos,mapping=aes(x=lon,y=lat,color=PP),size=3)+my_scale2

correlacion<-cor.test(datos$PP,datos$Ref)

graf_correlacion<-ggplot(datos, aes(x = Ref, y = PP)) +
  geom_point(size = 0.5,color="blue") +
  labs(x = "Reflectividad",y = "Tasa de precipitacion",color="Reflectividad") +
  xlim(c(0,70))+
  ylim(c(0,300))+
  annotate("text", x = 10, y = 300,label = paste("r =", round(correlacion$estimate, 2)))


#Debido a que tuve problemas con utilizar la funcion layout, con el paquete patchwork encontre
#otra forma de poder combinar los graficos.

Graficos<-graf_Ref+graf_PP+graf_correlacion 

#Punto C ####
rutas_variables_Z<-list("FS/Latitude","FS/Longitude","FS/PRE/height","FS/SLV/precipRate","FS/SLV/zFactorFinal")

variables_necesarias_Z<-lista_variables(ruta_del_archivo,rutas_variables_Z)


lon<-variables_necesarias_Z$Longitude[,6622]
altura<-variables_necesarias_Z$height[,,6622]
lon<-matrix(lon,nrow = 176,ncol = 49,byrow = T)

datos_Z<-data.frame(altura=as.vector(altura),
                    lon=as.vector(lon),
                    pp=as.vector(variables_necesarias_Z$precipRate[,,6622]),
                    reflec=as.vector(variables_necesarias_Z$zFactorFinal[1,,,6622]))
datos_Z$pp[datos_Z$pp==min(datos_Z$pp,na.rm = T)]<-NA
datos_Z$reflec[datos_Z$reflec==min(datos_Z$reflec,na.rm = T)]<-NA


graf_ref_Z<-ggplot(datos_Z,aes(x=lon,y=altura,color=reflec))+
  geom_point()+
  my_scale


graf_pp_Z<-ggplot(datos_Z,aes(x=lon,y=altura,color=pp))+
  geom_point()+
  my_scale2

Graficos_Z<-graf_ref_Z+graf_pp_Z
