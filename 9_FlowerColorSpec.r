#+ setup, error=TRUE
# Code for Streptanthus Flower Evolution Project
# Marjorie Weber - Summer 2017
# this code processes color spec data

rm(list=ls())
require(knitr)

tryCatch({
  setwd("~/Dropbox/SCIENCE_NOW/Streptanthus_Scents/R_analyses_NC")},
  error=function(e){
    setwd("/Volumes/Macintosh\ HD\ 2/Dropbox/SCIENCE_NOW/Streptanthus_Scents/R_analyses_NC")
  })


# devtools::install_github('thomased/colsci')
# require(SpecHelpers)
require(pavo)
require(colsci)
require(scales)

##########################################################################
#### Read in data ####
##########################################################################

spec.dat<-read.csv('00_Raw_data/1_FlowerSpecOutput_072115.csv')
head(spec.dat)


# names.samples<-unique(colnames(spec.dat))[2:ncol(spec.dat)]
# codes<-sapply(strsplit(as.character(names.samples), '_'), function(x) x[1])
# part<-sapply(strsplit(as.character(names.samples), '_'), function(x) x[2])
# write.csv(cbind(names.samples,codes,part),"00_Raw_data/color_codes.csv")

sample.codes<-read.csv("00_Raw_data/color_codes.csv",row.names = 1)
head(sample.codes)

species.codes<-read.csv("00_Raw_data/pop_metadata.csv",stringsAsFactors =FALSE)
head(species.codes)

sample.codes$codes <-tolower(sample.codes$codes)
species.codes$Code<-tolower(species.codes$Code)

unique(sample.codes$code[!sample.codes$codes %in% species.codes$Code])
species<-species.codes$Species[match(sample.codes$codes,species.codes$Code)]

sample.codes<-cbind(sample.codes,species)
identical(colnames(spec.dat)[2:ncol(spec.dat)],as.character(sample.codes$names.samples)) #this should still be true


##########################################################################
#### interpolate data ####
##########################################################################
spec.interpol<-as.rspec(spec.dat,lim=c(300,700))
plot(spec.interpol,col=spec2rgb(spec.interpol))

# spec.interpol<-as.rspec(spec.dat,interp=FALSE)
head(spec.interpol)
tail(spec.interpol)
wl<-spec.interpol[,1]
#scale from 0 to 1
# range(spec.interpol[,2])
# spec.interpol.scaled<-apply(spec.interpol[,2:ncol(spec.interpol)],2,FUN=rescale,to = c(0, 1))
# range(spec.interpol.scaled[,2])
# spec.interpol<-cbind(wl,spec.interpol.scaled)
# head(spec.interpol)
spec.interpol<-spec.interpol[,colnames(spec.interpol)!='wl']

#clean_datasets
identical(colnames(spec.interpol),as.character(sample.codes$names.samples))

flowers<-spec.interpol[,sample.codes$part.clean=="flower"]
head(flowers)
flowers<-cbind(wl,flowers)
flowers<-as.rspec(flowers,interp=FALSE)

temp<-t(flowers[,2:ncol(flowers)])
head(temp)
flowers.spp<-factor(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])
rm(temp)


buds<-spec.interpol[,sample.codes$part.clean=="bud"]
buds<-cbind(wl,buds)
buds<-as.rspec(buds,interp=FALSE)
temp<-t(buds[,2:ncol(buds)])
head(temp)
buds.spp<-factor(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])
rm(temp)



############################################
### clean data - remove crazy spectra, they are errors
############################################

flowers<-flowers[,!is.na(flowers.spp)]
flowers.spp<-flowers.spp[!is.na(flowers.spp)]


# for (i in length(levels(temp.spp))){
#   plot(flowers,flowers.spp==levels(flowers.spp)[i])
#   )
# }
plot(flowers,col=spec2rgb(flowers),type='o')
plot(buds,col=spec2rgb(buds),type='o')

# plot(procspec(flowers,opt="minimum"),col=spec2rgb(procspec(flowers,opt="minimum")),type='o')



# # Normalize to min of unity
flowers <- procspec(flowers, opt = c('min'), span = 0.25)
buds <- procspec(buds, opt = c('min'), span = 0.25)
plot(flowers,col=spec2rgb(flowers),type='o')
plot(buds,col=spec2rgb(buds),type='o')


flowers<-procspec(flowers,fixneg="addmin")
buds<-procspec(buds,fixneg="addmin")

plot(flowers,col=spec2rgb(flowers),type='o')
plot(buds,col=spec2rgb(buds),type='o')

# plot(procspec(flowers, opt = 'smooth', span = 0.5),col=spec2rgb(procspec(flowers, opt = 'smooth', span = 0.5)),type='o')

# # smooth to remove noise
# flowers <- procspec(flowers, opt = 'smooth', span = 0.15)
# buds <- procspec(buds, opt = 'smooth', span = 0.15)
# plot(flowers,col=spec2rgb(flowers),type='o')
# plot(buds,col=spec2rgb(buds),type='o')


write.csv(flowers,"9.1_flower_color_individuals.csv")
write.csv(buds,"9.1_bud_color_individuals.csv")



##########################################################################
#### species means ####
##########################################################################

#flowers
head(sample.codes)
species.codes<-read.csv("00_Raw_data/pop_metadata.csv",stringsAsFactors =FALSE)
head(species.codes)
sample.codes$codes <-tolower(sample.codes$codes)
species.codes$Code<-tolower(species.codes$Code)

unique(sample.codes$code[!sample.codes$codes %in% species.codes$Code])
species<-species.codes$Species[match(sample.codes$codes,species.codes$Code)]

sample.codes<-cbind(sample.codes,species)
identical(colnames(spec.interpol),as.character(sample.codes$names.samples)) #this should still be true


unique(sample.codes$species)
temp<-t(flowers[,2:ncol(flowers)])
head(temp)
temp.spp<-factor(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])

Species.flowers<-aggspec(flowers, by = temp.spp, FUN = mean, trim = TRUE)
Species.flowers<-Species.flowers[,colnames(Species.flowers)!="NA."]
write.csv(Species.flowers,"9.1_flower_color_species.csv")
plot(Species.flowers,col=spec2rgb(Species.flowers))

#buds
temp<-t(buds[,2:ncol(buds)])
head(temp)
temp.spp<-factor(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])
Species.buds<-aggspec(buds, by = temp.spp, FUN = mean, trim = TRUE)

Species.buds<-Species.buds[,colnames(Species.buds)!="NA."]
head(Species.buds)
Species.buds<-as.rspec(Species.buds[,!is.na(colnames(Species.buds))])

plot(Species.buds, col=spec2rgb(Species.buds))

write.csv(Species.buds,"9.1_bud_color_species.csv")

# 
# ##########################################################################
# #### pop means ####
# ##########################################################################
# pop<-species.codes$Pop[match(sample.codes$code,species.codes$Code)]
# 
# sample.codes<-cbind(sample.codes,pop)
# #flowers
# 
# temp<-t(flowers[,2:ncol(flowers)])
# head(temp)
# temp.spp<-as.character(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])
# temp.pop<-as.character(sample.codes$pop[match(rownames(temp),sample.codes$names.samples)])
# temp.spp.pop<-paste(temp.spp,temp.pop)
# 
# Pop.flowers<-aggspec(flowers, by = temp.spp.pop, FUN = mean, trim = TRUE)
# head(Pop.flowers)
# write.csv(Pop.flowers,"data_clean/flower_color_pop.csv")
# 
# #buds
# temp<-t(buds[,2:ncol(buds)])
# head(temp)
# temp.spp<-factor(sample.codes$species[match(rownames(temp),sample.codes$names.samples)])
# temp.pop<-as.character(sample.codes$pop[match(rownames(temp),sample.codes$names.samples)])
# temp.spp.pop<-paste(temp.spp,temp.pop)
# 
# pop.buds<-aggspec(buds, by = temp.spp.pop, FUN = mean, trim = TRUE)
# head(pop.buds)
# write.csv(pop.buds,"data_clean/bud_color_pops.csv")
# 
# 
# 
# 
# 
# ##########################################################################
# #### Maxwell triangle ####
# ##########################################################################
# # devtools::install_github('thomased/colsci')
# require(pavo)
# require(colsci)
# require(scales)
# 
# color.function<-function(flower.data){
#   #flower.data<-fl.col.ind
#   name<-deparse(substitute(flower.data))
#   print(name)
#   flowers.max <- maxwell(flower.data, visual = 'apis', illuminant = 'D65', background = 'green')#,transform='menton')
#   pdf(paste("data_clean/reduced_d/maxwell",name,".pdf",sep=""))
#   max_plot(flowers.max, bg = 'forestgreen', pch = 20)
#   title(main = name)
#   dev.off()
#   write.csv(flowers.max,paste("data_clean/reduced_d/maxwell",name,".csv",sep=""))
# }
# 
# color.function(fl.col.ind)
# color.function(fl.col.pop)
# color.function(fl.col.spp)
# 
# color.function(bd.col.ind)
# color.function(bd.col.pop)
# color.function(bd.col.spp)
# 
# ##########################################################################
# #### PCA floral####
# ##########################################################################
# 
# color.pca.function<-function(color.data){
#   #get every 10th nm
#   #color.data<-fl.col.ind
#   name<-deparse(substitute(color.data))
#   print(name)
#   color.data<- color.data[seq(1, nrow(color.data), 10),]
#   
#   flowers.pca <- prcomp(t(color.data[,2:ncol(color.data)]),
#                         center = TRUE,
#                         scale. = TRUE) 
#   
#   pdf(paste("data_clean/reduced_d/pca",name,".pdf",sep=""))
#   plot(flowers.pca,type="l")
#   dev.off()
#   
#   write.csv(flowers.pca$x[,1:4],paste("data_clean/reduced_d/pca",name,".csv",sep=""))
# }
# 
# 
# Species.buds.subset<- Species.buds[seq(1, nrow(Species.buds), 10),]
# head(Species.buds.subset)
# tail(Species.buds.subset)
# buds.pca <- prcomp(t(Species.buds.subset[,2:ncol(Species.buds.subset)]),
#                    center = TRUE,
#                    scale. = TRUE) 
# buds.pca$x
# save(buds.pca,file='1.1_buds.pca.RData')
# pdf("1.1_buds.pca.pdf",width=14)
# par(mfrow=c(1,2))
# plot(buds.pca,type="l")
# summary(buds.pca)
# biplot(buds.pca,var.axes = FALSE,col=c("black","n"))
# dev.off()
# write.csv(buds.pca$x,"data_clean/bud.species.pca.csv")
# }
# 
# color.pca.function(fl.col.ind)
# color.pca.function(fl.col.pop)
# color.pca.function(fl.col.spp)
# 
# color.pca.function(bd.col.ind)
# color.pca.function(bd.col.pop)
# color.pca.function(bd.col.spp)
# 
# 
# #Graveyard:
# ##########################################################################
# #### PCA raw and PCA Z-scores ####
# ##########################################################################
# # require(vegan)
# # si.mds<-metaMDS(t(spec.interpol[,2:ncol(spec.interpol)]),scale=TRUE)
# # plot(scores(si.pca))
# # text(scores(si.pca)[,1],scores(si.pca)[,2],rownames(scores(si.pca)))
# # 
# # ##########################################################################
# # #### Color Hexagon ####
# # ##########################################################################
# # 
# # hex<-hexagon(spec.interpol,visual='apis',illuminant='D65',background='green')
# # hex_plot(hex,sectors='fine')
# # 
# # ##########################################################################
# # #### tetraplot####
# # ##########################################################################
# # 
# # flowers.tetra <- tetra(spec.interpol, visual = 'apis', illuminant = 'bluesky')
# # tetra_plot(flowers.tetra, type = '3d')
# # 
# # pdf("test.pdf",height=28,width=28)
# # par(mfrow=c(17,17))
# # for (i in 1:269){
# #   print(plot(spec.interpol[,1],spec.interpol[,i],main=as.character(colnames(spec.interpol)[i]),ylim=c(min(spec.interpol[,2:ncol(spec.interpol)]),max(spec.interpol[,2:ncol(spec.interpol)]))))
# # }
# # dev.off()
# # 
# # 
# # plot(spec.interpol[,1],spec.interpol[,236],main=as.character(colnames(spec.interpol)[236]))
# # 
# # par(mfrow=c(1,1))
# # plot(spec.interpol[,1],spec.interpol[,248],main=as.character(colnames(spec.interpol)[248]))
# # plot(spec.interpol[,1],spec.interpol[,249],main=as.character(colnames(spec.interpol)[249]))
# # plot(spec.interpol[,1],spec.interpol[,250],main=as.character(colnames(spec.interpol)[250]))
# # 
# # plot(spec.interpol[,1],spec.interpol[,101],main=as.character(colnames(spec.interpol)[101]))
# # plot(spec.interpol[,1],spec.interpol[,102],main=as.character(colnames(spec.interpol)[102]))
# # plot(spec.interpol[,1],spec.interpol[,103],main=as.character(colnames(spec.interpol)[103]))
# # 
# # 
# 
# 
