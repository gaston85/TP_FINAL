#library(hdf5r)

info<-H5File$new("2A.GPM.DPR.V9-20211125.20231030-S074316-E091549.054943.V07B.HDF5",mode = "r+",fi)

# Loque quiero: tasa_precip<-info[[2]][[8]][["precipRateESurface"]]$read()
paste(info,"[['FS']]","[['SLV']]","[['precipRateESurface']]","[['precipRateESurface']]","$read()",quote(F))

info[["FS"]][["SLV"]][["precipRateESurface"]][["precipRateESurface"]]$read()


A <- strsplit("FS/SLV/precipRateESurface",split = "/")

b<-eval(parse(text=(A[[1]][1]),keep.source = T))

a<- eval(parse(text = paste0("info[[",A[[1]][1],"]]$read()"),keep.source = T))

paste0("info","[[""",A[[1]][1],"""]]$read()")

#eval(paste0(info,[[mod1]],[[mod2]]))

list("FS/SLV/precipRateESurface")


lista_variables<-function(ruta_archivo,lista_ruta){
  #Formato de las rutas: "FS/SLV/precipRateESurface"
  archivo<-H5File$new(filename = ruta_archivo,mode = "r+")
  list_var<-list()
  for (i in 1:length(lista_ruta)) {
    variable<-paste(archivo,lista_ruta[[i]],sep="")
    list[strsplit()]
  }
}

A <-list()
A[strsplit("FS/SLV/precipRateESurface",split = "/")[[1]][3]]<-270

strsplit("FS/SLV/precipRateESurface",split = "/")[[1]][3]
variable<-cat("info[[",paste(A[[1]][1],A[[1]][2],A[[1]][3],sep = "']][['"),"']]$read()",sep = "")
