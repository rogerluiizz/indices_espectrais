

#Carregando os pacotes
install.packages("raster")
library(raster)
install.packages("rgdal")
library(rgdal)
install.packages("terra")
library("terra")
install.packages("RStoolbox")
librarby("RStoolbox")

#alterando pasta padrão
getwd()
setwd("C:/Users/roger/Documents/NDVI - Santana Do Araguaia/")


#carregando uma banda como exemplo
banda_exemplo = raster("CBERS_4A_WFI_20220803_211_124_L4_BAND13.tif")
plot(banda_exemplo, col = gray(0:100/100))



#criar um stack, que é basicamente a composição (composite bands no arcGIS)

stack_araguaia = stack("CBERS_4A_WFI_20220803_211_124_L4_BAND15.tif","CBERS_4A_WFI_20220803_211_124_L4_BAND14.tif",
                               "CBERS_4A_WFI_20220803_211_124_L4_BAND13.tif","CBERS_4A_WFI_20220803_211_124_L4_BAND16.tif")

plot(stack_araguaia)
stack_araguaia

#verificar a ordem

names(stack_araguaia)


#criando o plot da composição (cor verdadeira)
plotRGB(stack_araguaia, r=1, g=2, b=3, axes=T, stretch='lin', main="Cor verdadeira - Santana do Araguaia")


#carregando o arquivo shapefile de santana do araguaia
santana_araguaia=readOGR("Santana_Araguaia.shp")

plot(santana_araguaia)

#checar o sistema de coodenada dos arquivos
crs(santana_araguaia)

crs(stack_araguaia)


#recortar para a area do municipio - extract by mask
santana_mask=mask(x=stack_araguaia, mask = santana_araguaia)
santana_crop=crop(santana_mask, santana_araguaia)


#plotando o mask e o crop
par(mfrow=c(1,2))
plot(santana_mask$CBERS_4A_WFI_20220803_211_124_L4_BAND15,col=terrain.colors(100))
plot(santana_crop$B_RED_01,col=terrain.colors(100))

#MUDANDNO OS NOMES DAS BANDAS
santana_crop
names(santana_crop)= c("B_RED_01","B_GREEN_02","B_BLUE_03","B_NIR_04")


#CALCULANDO VÁRIOS INDICES DE VEGETAÇÃO DE UMA SÓ
indices = spectralIndices(santana_crop, red ="B_RED_01", green = "B_GREEN_02", blue = "B_BLUE_03", nir = "B_NIR_04" )
plot(indices)

#PLOTANDO OS PRINCIPAIS
plot_indices_principais= plot(indices, c("NDVI", "SR", "SAVI", "NDWI"))


#CALCULANDO OS INDICES DE VEGETAÇÃO
#NDVI
par(mfrow=c(1,1))
NDVI = (santana_crop$B_RED_01-santana_crop$B_NIR_04)/(santana_crop$B_RED_01+santana_crop$B_NIR_04)
plot(NDVI,col = gray(0:100/100))

#SAVI
SAVI = ((1+0.5)*(santana_crop$B_RED_01-santana_crop$B_NIR_04))/
  (santana_crop$B_RED_01+santana_crop$B_NIR_04+0.5)
plot(SAVI,col = gray(0:100/100))

#EVI
EVI = (2.5*((santana_crop$B_NIR_04-santana_crop$B_RED_01)/10000))/
  (santana_crop$B_NIR_04/10000+6*santana_crop$B_RED_01/10000-7.5*santana_crop$B_BLUE_03/10000+1)
plot(EVI)

#NDWI
NDWI = (santana_crop$B_GREEN_02-santana_crop$B_NIR_04)/(santana_crop$B_GREEN_02+santana_crop$B_NIR_04)
plot(NDWI,col = gray(0:100/100))
