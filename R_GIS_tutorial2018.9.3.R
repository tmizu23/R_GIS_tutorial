#' ---
#' title: "R��p����GIS"
#' author: "������ЃG�R���X ���J�M�s"
#' date: "2018�N9��3��"
#' output:
#'    html_document:
#'      keep_md: true
#'      toc: true
#'      toc_float:
#'         collapsed: false
#'         smooth_scroll: true
#'      toc_depth: 3
#'      number_sections: true
#'      md_extensions: -ascii_identifiers
#'      highlight: kate
#'      css: styles.css
#' ---

#+ echo=F
options(max.print="75")
knitr::opts_chunk$set(echo=TRUE,
               cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
knitr::opts_knit$set(width=75)

#++++++++++++++++++++++++++++++++++++++
#' #������ 
#++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++
#' ##�C���X�g�[�� 
#++++++++++++++++++++++++++++
#' �ȉ��̊���p�ӂ��Ă��������B
#' 
#' * Windows OS >=7
#' * R >= 3.5.1
#' * RStudio >= 1.0
#'
#' �g�p����f�[�^���_�E�����[�h���Ă��������B
#' 
#' * [�g�p�f�[�^�_�E�����[�h]()
#'
#' �p�b�P�[�W���C���X�g�[�����܂��B�i�ŏ���1�񂾂��j
#+ eval=F
install.packages(c("sf","geos","dplyr","tidyr","raster","ggplot2","ggrepel","mapview","jpndistrict","rpart","randomForest","kernlab"),dependencies=T,type="win.binary")

#' ���C�u������ǂݍ��݂܂��B
library(sf) #�x�N�^����
library(raster) #���X�^����
library(dplyr) #��������
library(purrr) #��������
library(tidyr) #��������
library(ggplot2) #�n�}�\��
library(ggrepel) #���x���\������
library(mapview) #�n�}�\���i�C���^���N�e�B�u�j
library(jpndistrict) #���{�̍s���E�f�[�^
library(rpart) #�����
library(randomForest) #�����_���t�H���X�g
library(kernlab) #SVM

#++++++++++++++++++++++++++++
#' ##��b�m�� 
#++++++++++++++++++++++++++++

#++++++++++++++++++
#' ###�p�C�v
#++++++++++++++++++
#' �p�C�v�����̊֐��̏o�͌��ʂ��p�C�v�E���̊֐��̑������ɂ���
#head(1:5,3)���p�C�v�ŏ�����
1:5 %>% head(3)
#�p�C�v�̏o�͂����̃p�C�v�̓��͂ɂł���
c(1,10,100) %>% tan %>% sin %>% cos
#.���g���Ƒ�1�����ȊO�ɂł���
3 %>% head(1:5,.)

#++++++++++++++++++
#' ###map�֐� 
#++++++++++++++++++
#' �x�N�g�������X�g����͂��A�e�v�f�Ɋ֐���K�p���A���͂Ɠ��������̃��X�g��Ԃ�
#x�𕽋ϒl�Ƃ������K������5�쐬����֐����`
myfunc<-function(x){
  rnorm(5,x)
}
#�e�l�������Ƃ��āAmyfunc�֐���K�p����B�߂�l�́A�e5�̗�����������3�v�f�̃��X�g�B
c(1,10,100) %>% map(myfunc)
#+ results='hide'
#�ȉ��̂悤�Ɋȗ������ď�����B~��function�A.x�͈�����\���B
c(1,10,100) %>% map(~rnorm(5,.x))
#+ results='markup'
#���X�g�̊e�v�f���Ƃɕ��ϒl���v�Z����ꍇ�B�߂�l�́A�g���̕��ϒl�A�̏d�̕��ϒl��2�v�f�̃��X�g
mylist<-list(height=130:140,weight=40:50)
mylist %>% map(mean)

#++++++++++++++++++
#' ###reduce�֐� 
#++++++++++++++++++
#' �x�N�g�������X�g����͂��A��ϐ��֐������X�ɓK�p����1�̒l��Ԃ��B
c(1,10,100) %>% reduce(sum)
x <- list(c(0, 1), c(2, 3), c(4, 5))
x %>% reduce(c)

#++++++++++++++++++
#' ###�f�[�^�̕ό`
#++++++++++++++++++
#�����ȃf�[�^���쐬
data <- tibble(
  Point = c("�n�_A","�n�_B"),
  Red = c(0.1,0.3),
  Green = c(0.3,0.2),
  Blue = c(0.5,0.6)
)
print(data)

#�c���̃f�[�^�ɕϊ�
data2<-data %>% gather(key = Band, value="Refrectance",-Point)
#�����̃f�[�^�ɕϊ�
data2 %>% spread(key=Band,value=Refrectance)

#++++++++++++++++++++++++++++++++++++++
#' #�x�N�^��{��
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##��{����
#++++++++++++++++++++++++++++

#' ### �V�F�[�v�t�@�C����ǂݍ��݂���
landcover<-read_sf("data/landcover.shp",options=c("ENCODING=CP932"))

#' ### �f�[�^�̊T�v���m�F������
head(landcover)

#' ### �n�}��\��������
plot(landcover)

#' ### �f�[�^�N���X���m�F������
class(landcover)

#' ### �f�[�^�̍\�����m�F������
str(landcover) 

#' ### ���������m�F������
names(landcover)

#' ### �����l�̃T�}���[���m�F������
summary(landcover)

#' ### �f�[�^�͈̔͂��m�F������
st_bbox(landcover)

#' ### Rstudio�ő����\���m�F������
#+ eval=F
View(landcover)

#' ### �C���^���N�e�B�u�Ȓn�}��\��������
#+ eval=T
mapview(landcover)

#' ### ���{�̍s���E��\��������
library(jpndistrict)
ibaraki<-jpn_pref(admin_name="��錧")
plot(ibaraki)

#' ### �V�F�[�v�t�@�C���������o������
st_write(landcover, "mydata/landcover2.shp",layer_options = "ENCODING=UTF-8",delete_layer = TRUE)

#++++++++++++++++++++++++++++
#' ##���W�n�̑��� 
#++++++++++++++++++++++++++++

#' ### �f�[�^�̋�ԎQ�ƌn���m�F������
st_crs(landcover) 

#' ### ��ԎQ�ƌn��EPSG�R�[�h�Ŏw�肵����
landcover_epsg<-st_set_crs(landcover,2451)

#' ### �ʂ̍��W�n�ɕϊ�������
landcover_latlon<-st_transform(landcover,4612)

#' ### �f�[�^�̃W�I���g�����m�F������
st_geometry(landcover)

#++++++++++++++++++++++++++++
#' ##�f�[�^�̍쐬�E�ҏW 
#++++++++++++++++++++++++++++

#' ### �|�C���g�f�[�^���쐬������
p1<-st_point(c(1,2)) #sfg�N���X
p2<-st_point(c(2,1))
p1
p2
mygeom<-st_sfc(p1,p2) #sfc�N���X
mygeom
myname <- c("point1","point2") #�����l�����
point_f<-st_sf(name=myname,geom=mygeom) #sf�N���X
point_f

#' ### ���C���f�[�^���쐬������
pts1 = rbind(c(-1,-1), c(1,1), c(2,0), c(3,2))
pts2 = rbind(c(1,-1), c(2,2), c(3,1), c(2,4))
ls1 = st_linestring(pts1)
ls2 = st_linestring(pts2)
mygeom<-st_sfc(ls1,ls2) #sfc�N���X
myname <- c("line1","line2") #�����l�����
line_f<-st_sf(myname,mygeom) #sf�N���X

#' ### �|���S���f�[�^���쐬������
pts = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
b0 = st_polygon(list(pts)) #sfg�N���X
b1 = b0 + c(2,0) #b0�̃|���S����X���W��2���炵���|���S�����쐬
b2 = b0 * 0.5 + c(0,1.5) #b0�̃|���S�����k������Y���W��1.5���炵���|���S�����쐬
mygeom<-st_sfc(b0,b1,b2) #sfc�N���X
myname<-data.frame(number=c(1,2,3),species=c("dog","cat","cat")) #�����f�[�^���쐬
#+ eval=F
poly_f<-st_sf(myname,geom) #sf�N���X #Bug ref.#782

#' ### �����_���|�C���g���쐬������
pnt<-st_sample(landcover,1000) #landcover�͈̔͂�1000�_
plot(pnt,pch=".",cex=2)

#' ### ���b�V�����쐬������
mesh <- st_make_grid(n=c(10,5), offset = c(24920,2740), cellsize = c(6,6),crs = 2451)
mesh <- st_sf(mesh)
plot(mesh,axes=T)

#' ### �|���S�������C���ɕϊ�������
landcover_l<-landcover %>% st_cast("LINESTRING")

#' ### sf�N���X����sp�N���X�ɕϊ�������
landcover_sp<-as(landcover,"Spatial")

#' ### sp�N���X����sf�N���X�ɕϊ������� 
landcover_sf<-st_as_sf(landcover_sp)

#++++++++++++++++++++++++++++
#' ##�n�}�\�� 
#++++++++++++++++++++++++++++

#' ### �w�肵���f�[�^��\��������
plot(landcover["subclass"])

#' ### ���W����\��������
plot(landcover["subclass"],axes=T)

#' ### �}��̕\�������w�肵����
plot(landcover["subclass"],key.width = lcm(3))

#' ### ���E������\��������
plot(st_geometry(landcover))

#' ### ggplot2�ŕ\��������
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass))

#' ### ���W���̍��W�n���w�肵����
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  coord_sf(datum = 2451)

#' ### ���W���Ɣw�i����������
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  coord_sf(datum = NA) +
  theme_void()

#' ### �}��̐F���w�肵����
mycol <- c("gray", "black", "white", "darkgreen","brown","lightgreen","lightblue", "blue","orange","green","yellow")
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  scale_fill_manual(values = mycol) + 
  coord_sf(datum = NA) +
  theme_void()

#' ### �f�[�^���d�˂���
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  geom_sf(data = pnt,size=0.5) +
  geom_sf(data = mesh,fill = "transparent",color="white",size=1)

#' ### ���x����\��������
#' [�Q��:�����Ƀ|�C���g�̍��W��ǉ�������](#�����Ƀ|�C���g�̍��W��ǉ�������)
cent_xy<-landcover %>% st_centroid() %>% st_coordinates()
landcover_xy<-cbind(landcover,cent_xy)
ggplot(data=landcover_xy) + 
  geom_sf(aes(fill = subclass))+
  geom_text_repel(aes(x=X,y=Y,label=subclass),size=2)
  

#++++++++++++++++++++++++++++
#' ##�f�[�^�̑I���E���� 
#++++++++++++++++++++++++++++

#' ### ����̍s�̃f�[�^�𒊏o������
landcover[1,]

#' ### ����̗�̃f�[�^�𒊏o������
landcover[,"class"]

#' ### ����̗�̃f�[�^���x�N�g���Œ��o������
landcover$class

#' ### �����l�Ńf�[�^�𒊏o������
landcover_A<-landcover %>% dplyr::filter(class=="�\����")
plot(landcover_A["class"])

#' ### �����̑����l�Ńf�[�^�𒊏o������
landcover_B<-landcover %>% dplyr::filter(subclass %in% c("����","��"))
plot(landcover_B["subclass"])

#' ### �e�|���S���ɏd�Ȃ�|�C���g�̃C���f�b�N�X��m�肽��
st_intersects(mesh,pnt)

#' ### �e�|���S���ɏd�Ȃ�|�C���g�̐���m�肽��
st_intersects(mesh,pnt) %>% lengths

#' ### ��������I�u�W�F�N�g�𒊏o������
inter<-landcover %>% filter(lengths(st_intersects(.,mesh))>0)
plot(inter)

#++++++++++++++++++++++++++++
#' ##��ԉ��Z 
#++++++++++++++++++++++++++++

#' ### ���������I�u�W�F�N�g��؂蔲������
inter<-st_intersection(landcover,mesh)
plot(inter)

#' ### �|���S���̏d�S�����߂���
cent<-st_centroid(mesh)
plot(cent)

#' ### �o�b�t�@�[���쐬������
st_buffer(cent,2) %>% plot


#++++++++++++++++++++++++++++
#' ##�f�[�^�̌v�� 
#++++++++++++++++++++++++++++

#' ### �ʐς��v��������
st_area(landcover)

#' ### �������v��������
st_length(landcover_l)

#' ### �n���̋������v��������
st_distance(cent[1,],cent[2:4,])

#' ### �n���̍��W��m�肽��
st_coordinates(cent)

#++++++++++++++++++++++++++++
#' ##�������̑���
#++++++++++++++++++++++++++++

#' ### ������ID��U�肽��
mesh<-mesh %>% mutate(ID=1:nrow(.))
mesh

#' ### �����Ƀ|�C���g�̍��W��ǉ�������
#' [�Q��:�|���S���̏d�S�����߂���](#�|���S���̏d�S�����߂���)
cent_xy<-cbind(cent,st_coordinates(cent))

#' ### �ʐς𑮐��ɒǉ�������
landcover_area<-landcover %>% mutate(AREA=st_area(.))

#' ### �ʐς̒P�ʂ�ύX������
#�w�N�^�[��ha�A�A�[��a�A�����L��,km^2
landcover_area %>% mutate(AREA=units::set_units(.$AREA,ha))

#' ### ���b�V�����̃|�C���g���𑮐��ɒǉ�������
cnt<-lengths(st_intersects(mesh,pnt))
mesh2<-mesh %>% mutate(count=cnt)

#' ### �����l�Ōv�Z������
mesh2<-mesh2 %>% mutate(AREA=st_area(.))
mesh2 %>% mutate(density=count/AREA)

#' ### ��������폜������
landcover %>% dplyr::select(-subclass)

#' ### �������������o������
landcover %>% st_set_geometry(NULL)

#' ### ���������̃|���S�����}���`�|���S���ɂ�����
landcover %>% group_by(class) %>% summarise()

#' ### ���������̖ʐς��W�v������
landcover_area %>% group_by(class) %>% summarise(TOTALAREA=sum(AREA))

#' ### ���b�V�����̓��������̖ʐς��W�v������
landcover_mesh<- st_intersection(landcover,mesh) %>% mutate(ID=1:nrow(.),AREA=st_area(.))
landcover_mesh<-landcover_mesh %>% group_by(ID,subclass) %>% summarise(AREA=sum(AREA)) 

#' ### ���b�V�����̍ő�ʐς̑��������b�V���̑����ɂ�����
df<-landcover_mesh %>% group_by(ID) %>% filter(AREA==max(AREA)) %>% st_set_geometry(NULL)
mesh %>% left_join(df,by="ID")

#++++++++++++++++++++++++++++++++++++++
#' #���X�^��{�� 
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##��{����
#++++++++++++++++++++++++++++

#' ### ���X�^�f�[�^��ǂݍ��݂���
ortho<-stack('data/rededge.tif')

#' ### ���X�^�f�[�^���o���h���w�肵�ēǂݍ��݂���
ortho1<-raster('data/rededge.tif',layer=1)

#' ### �f�[�^�̊T�v���m�F������
ortho

#' ### �n�}��\��������
plot(ortho1)

#' ### ���Z�������m�F������
ncell(ortho)

#' ### X�Z�����AY�Z�����A�o���h�����m�F������
dim(ortho)

#' ### �𑜓x���m�F������
res(ortho)

#' ### �o���h�����m�F������
nlayers(ortho)

#' ### �o���h�����m�F������
names(ortho)

#' ### �Z���l�̃T�}���[���m�F������
summary(ortho1)

#' ### �f�[�^�͈̔͂��m�F������
extent(ortho)

#' ### ���X�^�f�[�^�������o������
writeRaster(ortho1,filename = "mydata/ortho1.tif", format = "GTiff", overwrite = TRUE)


#++++++++++++++++++++++++++++
#' ##���W�n�̑���
#++++++++++++++++++++++++++++

#' ### �f�[�^�̋�ԎQ�ƌn���m�F������
crs(ortho)

#' ### ��ԎQ�ƌn��EPSG�R�[�h�Ŏw�肵����
crs(ortho)<-CRS("+init=epsg:2451")

#' ### �ʂ̍��W�n�ɕϊ�������
ortho_utm54<-projectRaster(ortho1, crs=CRS("+init=epsg:3100"))


#++++++++++++++++++++++++++++
#' ##���X�^�̕ҏW
#++++++++++++++++++++++++++++

#' ###���X�^�[�X�^�b�N���烌�C�������o������
#subset��
ortho1<-subset(ortho,1)
#raster��
ortho1<-raster(ortho,1)
#���X�g��
ortho1<-ortho[[1]]


#' ###�w��͈͂Ő؂蔲������
e<-extent(24905,24992,2711,2824)
orthocrop<-crop(ortho,e)

#' ###�}�E�X�Ŏw��͈͂�؂蔲������
#+ eval=F
plot(ortho1)
orthocrop<-raster::select(ortho)
plot(orthocrop)

#+ eval=T
#' ###�o���h�����w�肵����
names(ortho) <- c('blue','green','red','nir','rededge','alpha')
ortho

#' ###�𑜓x��ύX������
ortho_res5 <- ortho[[1]]
res(ortho_res5)<-5
ortho_res5 <- resample(ortho[[1]], ortho_res5, method='bilinear')
plot(ortho_res5)

#++++++++++++++++++++++++++++
#' ##�l�̎擾�E�W�v
#++++++++++++++++++++++++++++

#' �q�X�g�O������\��������
hist(ortho1,breaks = 30)

#' ###���X�^�l���擾������
getValues(ortho1)

#' ###���X�^�̒l�����W�t���Ŏ擾������
df<-as.data.frame(ortho,xy=TRUE)
head(df)

#' ###���X�^�̒l���|�C���g�Œ��o������
#' [�Q��:�����_���|�C���g���쐬������](#�����_���|�C���g���쐬������)
df<-raster::extract(ortho1,as(pnt,"Spatial"),df=TRUE)

#++++++++++++++++++++++++++++
#' ##���X�^���Z 
#++++++++++++++++++++++++++++

#' ###���X�^�l��ύX������
ortho2<-ortho1/65536

#' ###���X�^�l�Ɋ֐���K�p������
myfunc<-function(x){
  y=x/65536
  return(y)
}
ortho2 <- calc(ortho1, myfunc)

#' ���X�^���ĕ��ނ�����
ortho3 <- reclassify(ortho2, c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.4,3, 0.4,0.5,4, 0.5,Inf, 5))

#++++++++++++++++++++++++++++
#' ##�n�}�\�� 
#++++++++++++++++++++++++++++

#' ###�o���h�ƐF���w�肵�ĕ\��������
plot(ortho,1,col=gray.colors(30))

#' ###�o���h��RGB�Ɏw�肵�ĕ\��������
plotRGB(ortho, r = 3, g = 2, b = 1, axes = TRUE, stretch = "hist", main = "True Color Composite")

#' ###�Y�[�����ĕ\��������
#�Y�[���͈͂̍���ƉE�����N���b�N
#+ eval=F
zoom(ortho)

#+ eval=T
#' ###ggplot2�ŕ\��������
df<-as.data.frame(ortho1,xy=TRUE)
colnames(df)<-c("x","y","value")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = value))+
  scale_fill_gradientn(colors=gray.colors(30))

#++++++++++++++++++++++++++++++++++++++ 
#' #���p�� 
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##�}���`�X�y�N�g���摜���琅��̊����󋵂�c������ 
#++++++++++++++++++++++++++++
#'
#++++++++++++++++++
#' ###�f�[�^��ǂݍ��݁A�����m�F����
#++++++++++++++++++
#'
#�ǂݍ���
sequoia <- stack('data/sequoia.tif')
#���m�F
crs(sequoia)
ncell(sequoia)
dim(sequoia)
res(sequoia)
nlayers(sequoia)
sequoia
#�\��
plot(sequoia,col=gray.colors(30))

#++++++++++++++++++
#' ###�f�[�^��O��������
#++++++++++++++++++
#'
#��������o���h�𒊏o
sequoia <- subset(sequoia,1:4)
#�o���h�O��t����
names(sequoia) <- c('green','red','nir','rededge')
#��ԎQ�ƌn���w��
crs(sequoia)<-CRS('+init=EPSG:2451')
#�m�F
sequoia

#�����͈͂�shp��ǂݍ���
tanbo<-read_sf("data/tanbo.shp",options=c("ENCODING=CP932"))
plot(tanbo)
#�����͈͂Ńf�[�^��؂蔲��
croped<-crop(sequoia,tanbo)
plot(croped)
#�����͈͂��}�X�N����
masked<-mask(croped,tanbo)
#�\�����Ċm�F
plot(masked)

#++++++++++++++++++
#' ###�A���w�W(NDVI)���v�Z����
#++++++++++++++++++
#'
#NDVI���v�Z����֐�
calc_NDVI <- function(img, r, n) {
  red <- img[[r]]
  nir <- img[[n]]
  return ((red - nir) / (red + nir))
}

#NDVI���v�Z����BSEQUOIA�o���hNIR = 3, red = 2.
ndvi <- calc_NDVI(masked,3, 2)
plot(ndvi, col = rev(terrain.colors(10)), main = 'SEQUOIA-NDVI')

#�Y�[�����Ċm�F����
#+ eval=F
zoom(ndvi)
#+ eval=T

#++++++++++++++++++
#' ###�����󋵂�F�X�ȕ��@�Ŕc������
#++++++++++++++++++
#'
#' ####NDVI�̃q�X�g�O������\�����Ă݂�
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab="Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))


#' ####�����󋵂��ǂ��Ȃ��ꏊ�𒊏o���Ă݂�
veg <- calc(ndvi, function(x){x[x > 0.6] <- NA; return(x)})
plot(veg, main = 'Veg cover')

#' ####�����󋵂�5�i�K�ɕ��ނ��Ă݂�
vegc <- reclassify(ndvi, c(-Inf,0.4,1, 0.4,0.5,2, 0.5,0.6,3, 0.6,0.7,4, 0.7,Inf, 5))
plot(vegc,col = rev(terrain.colors(5)),breaks=0:5, main = 'NDVI based thresholding')

#' ####���b�V����NDVI���W�v���Ă݂�

#���b�V�����쐬���ĉ�]������
#cp�𒆐S�Ƀ��W�A��r����sfobj����]����֐�
rotate_sf<-function(sfobj,cp,r){
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  rot_sf<-(sfobj-cp)*rot(-r)+cp
  return(rot_sf)
}

mesh <- st_make_grid(n=c(38,12), offset = c(24787.8,2755.4), cellsize = c(0.5,0.5))
cp<-st_sfc(st_point(c(24787.8,2755.4)))
mesh<-rotate_sf(mesh,cp,pi*50/180)
plot(mesh,axes=T)

#���b�V�����Ƃ�NDVI�̕��ς��f�[�^�t���[���Ŏ擾
mesh_sp<-as(mesh,"Spatial") #sp�N���X�ɕϊ�
df<-raster::extract(ndvi,mesh_sp,fun=mean,df=TRUE)

#���b�V���̑����Ɍ���
mesh<-st_sf(mesh) #�������t������悤��sf�N���X�ɕϊ�
mesh<-mesh %>% mutate(ndvi=df$layer)
plot(mesh)

#���b�V����ndvi�l��\��
mesh<-mesh %>% mutate(id=1:nrow(.)) #���b�V����id��ǉ�
mesh_df<-mesh %>% st_set_geometry(NULL) #�f�[�^�t���[���ɕϊ�
hist(mesh_df$ndvi) #�q�X�g�O�����\��
mesh_df %>% filter(ndvi<0.5) #�������ǂ��Ȃ����b�V��ID��\��
mesh %>% filter(ndvi<0.5) %>% dplyr::select(ndvi) %>% plot()

#++++++++++++++++++
#' ###���ʂ������o��
#++++++++++++++++++
#'
#ggplot2�Ŕw�i�摜��ndvi�̃��b�V�����d�˂�
sequoia_df<-as.data.frame(croped[[4]],xy=TRUE) #���X�^���f�[�^�t���[���ɕϊ�
head(sequoia_df)
#���b�V���̒��S���W�𑮐��ɒǉ�
cent_xy<-mesh %>% st_centroid() %>% st_coordinates()
mesh<-cbind(mesh,cent_xy)
g<-ggplot() +
  geom_raster(data=sequoia_df, aes(x = x, y = y,alpha = rededge))+
  scale_alpha(name = "", range = c(0.6, 0), guide = F)+
  geom_sf(data = mesh %>% filter(ndvi>0.5),aes(fill = ndvi,alpha=0.3))+
  scale_fill_gradientn(colors=c("white","yellow","red"))+
  geom_text_repel(data = mesh %>% filter(id %in% c(1,38,191,343,456)),aes(x=X,y=Y,label = id),nudge_y=10)
plot(g)

#�摜��jpg�ɕۑ�����
ggsave("mydata/ndvi.jpg",g)
#�}�X�N�����������X�^�f�[�^�������o��
writeRaster(masked,filename = "mydata/masked_sequoia.tif", format = "GTiff", overwrite = TRUE)
#ndvi���b�V���|���S���������o��
st_write(mesh, "mydata/ndvi_mesh.shp",layer_options = "ENCODING=UTF-8",delete_layer = TRUE)

#++++++++++++++++++++++++++++
#' ##�}���`�X�y�N�g���摜����y�n�핢�𕪗ނ��� 
#++++++++++++++++++++++++++++
#'
#++++++++++++++++++
#' ###�O�������āA�����m�F����
#++++++++++++++++++
#'
rededge <- stack('data/rededge.tif')
rededge <- subset(rededge,1:5) #�A���t�@�o���h�����O
names(rededge) <- c('blue','green','red','NIR','rededge')
crs(rededge)<-CRS("+init=epsg:2451") #CRS��ݒ�

#�؂���
e<-extent(24905,24992,2711,2824)
rededgecrop<-crop(rededge,e)

#�𑜓x�ύX
rededge <- rededgecrop
res(rededge)<-0.5
rededge <- resample(rededgecrop, rededge, method='bilinear')

#�����o��
writeRaster(rededge,filename = "mydata/cropped-rededge.tif", format = "GTiff", overwrite = TRUE)

#���m�F
crs(rededge)
ncell(rededge)
dim(rededge)
res(rededge)
nlayers(rededge)

#++++++++++++++++++
#' ###�F�X�ȕ��@�ŕ\������
#++++++++++++++++++
#'
#' blue�o���h
plot(rededge,1,col=gray.colors(30))
#' TrueColor
plotRGB(rededge, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
#' FalseColor
plotRGB(rededge, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge False Color Composite")
#' NaturalColor
plotRGB(rededge, r = 3, g = 4, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge Natural Color Composite")

#++++++++++++++++++
#' ###�f�[�^�𕪐͂���
#++++++++++++++++++
#'
#' #### �o���h�Ԃ̑��ւ����Ă݂�
#�e�o���h�𒊏o���A16bit�̒l����0�`1�̔��˗��ɕϊ�
b_red<-rededge[[3]]/65536
b_nir<-rededge[[4]]/65536
b_rededge<-rededge[[5]]/65536

#���փO���t
# �Ԉ�ƋߐԊO��
red_nir<-stack(b_red,b_nir)
pairs(red_nir, main = "Scatterplot between Red and NIR")
# �Ԉ�ƃ��b�h�G�b�W��
red_rededge<-stack(b_red,b_rededge)
pairs(red_rededge, main = "Scatterplot between Red and Rededge")


#' #### �o���h�̔��˗���y�n�핢���ƂɃ����_���_�Ŏ擾���Ă݂�
#�y�n�핢�̓ǂݍ���
landcover<-read_sf("data/landcover.shp",options=c("ENCODING=CP932"))
plot(landcover)

#�T�u�N���X�̈ꗗ���x�N�g���Ŏ擾
subcl<-landcover %>% 
  st_set_geometry(NULL) %>% #sf����f�[�^�t���[���ɕϊ�
  distinct(subclass) #�d�����폜
subcl

#�T�u�N���X���ƂɃ����_���_��500�|�C���g�쐬
subclass_sampling<-function(x){
  filter(landcover,subclass==x) %>% 
    st_sample(500)
}
pntlist<-subcl$subclass %>% map(subclass_sampling) 
pnt<-pntlist %>% reduce(c)

#�|�C���g�̑����ɓy�n�핢��ǉ�
pnt<-st_intersection(landcover,pnt)
plot(pnt,pch=".",cex=3)

#�e�|�C���g�Ńo���h�l�𒊏o
band_df <- raster::extract(rededge, as(pnt,"Spatial"),df=TRUE)
#�|�C���g�̑����Ƀo���h�l��ǉ�
pnt<-pnt %>% bind_cols(band_df)


#' #### �n���ƃo���h�̔��˗��̊֌W�����Ă݂�
#�y�n�핢(��敪)���Ƃ̃o���h�̕��ϒl���v�Z
df_class_mean<-pnt %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(-ID,-subclass) %>% 
  group_by(class) %>% 
  summarise_all(mean) %>% 
  print

#�c���ȃf�[�^�ɕϊ�
df_class_mean<-df_class_mean %>% 
  gather(key=Bands,value=Reflectance,-class) %>% 
  print

#���˗��ɕϊ�
df_class_mean <- df_class_mean %>% mutate(Reflectance=Reflectance/65536)
#Bands��factor�ɕϊ��i�O���t�\���̍ۂ�Bands�̏��Ԃ���ׂ邽�߁j
df_class_mean <- df_class_mean %>% mutate(Bands= factor(Bands, levels = c("blue", "green", "red", "rededge", "nir")))

# �eclass�̃o���h�̕��ϔ��˗����O���t�\��
# ���b�h�G�b�W�̍Z����������������
df_class_mean %>% 
  ggplot(aes(x=Bands ,y=Reflectance,col=class))+
  geom_line(aes(group = class),size=1)+
  geom_point()+
  labs(title = "Spectral Profile")

#++++++++++++++++++
#' ###���t�Ȃ����ނ�����(5�o���h�g�p)
#++++++++++++++++++
#'
set.seed(99)
km<-kmeans(values(rededge), centers = 11)
knr<-rededge[[1]]
knr[] <- km$cluster
par(mfrow = c(1,2))
plotRGB(rededge, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
plot(knr, main = 'Unsupervised classification',breaks=0:11,col=rainbow(11))
par(mfrow = c(1,1))

#++++++++++++++++++
#' ###���t���蕪�ނ�����
#++++++++++++++++++
#'
#' �g���[�j���O�f�[�^���쐬����
area1 = st_polygon(list(rbind(c(24910,2800), c(24930,2800), c(24930,2820), c(24910,2820), c(24910,2800))))
area2 = st_polygon(list(rbind(c(24950,2800), c(24970,2800), c(24970,2820), c(24950,2820), c(24950,2800))))
area3 = st_polygon(list(rbind(c(24950,2770), c(24970,2770), c(24970,2790), c(24950,2790), c(24950,2770))))
area4 = st_polygon(list(rbind(c(24910,2770), c(24930,2770), c(24930,2790), c(24910,2790), c(24910,2770))))

training_area<-st_sfc(area1,area2,area3,area4,crs=2451)

#�g���[�j���O�G���A�̊m�F
plot(landcover %>% st_geometry(),axes=T)
plot(training_area,add=T)

#�g���[�j���O�G���A�̃f�[�^�𒊏o
training_pnt<-st_intersection(pnt,training_area)
# �f�[�^�t���[���ɕϊ�
training_df<-training_pnt %>% st_set_geometry(NULL) %>% dplyr::select(-class,-ID)
head(training_df)

#' ####����؂ŕ��ނ���
#���f���̍쐬
model <- rpart(as.factor(subclass)~., data=training_df, method="class")
plot(model, uniform=TRUE, main="Classification Tree")
text(model, cex = 0.8)

#���f������S���\��
pr <- predict(rededge, model, type="class")

#�\�����ʂ��v���b�g
df<-as.data.frame(pr,xy=TRUE)

classcolor <- c("�R���N���[�g"="gray", "���̑�"="black","����" = "white","����"="darkgreen","���n"="brown","��"="lightgreen","���c"="lightblue", "��"="blue","���n"="orange","���"="green","���n"="yellow")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)

#�\�����ʂ������o��
writeRaster(pr,filename = "mydata/RP_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)


#' ####�����_���t�H���X�g�ŕ��ނ���
#���f���̍쐬
model <- randomForest(as.factor(subclass)~.,data=training_df)

#���f������S���\��
pr <- predict(rededge, model)

#�\�����ʂ��v���b�g
df<-as.data.frame(pr,xy=TRUE)
classcolor <- c("�R���N���[�g"="gray", "���̑�"="black","����" = "white","����"="darkgreen","���n"="brown","��"="lightgreen","���c"="lightblue", "��"="blue","���n"="orange","���"="green","���n"="yellow")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)

#�\�����ʂ������o��
writeRaster(pr,filename = "mydata/RF_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)


#' ####SVM�ŕ��ނ���
#���f���̍쐬
model <- ksvm(as.factor(subclass)~.,data=training_df)

#���f������S���\��
pr <- predict(rededge, model)

#�\�����ʂ��v���b�g
classcolor <- c("�R���N���[�g"="gray", "���̑�"="black","����" = "white","����"="darkgreen","���n"="brown","��"="lightgreen","���c"="lightblue", "��"="blue","���n"="orange","���"="green","���n"="yellow")
df<-as.data.frame(pr,xy=TRUE)
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)

#�\�����ʂ������o��
writeRaster(pr,filename = "mydata/SVM_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)
