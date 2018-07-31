## --------------------------------------------------------------------
# title: "Rを用いたGIS"
# author: "株式会社エコリス 水谷貴行"
# date: "2018年4月5日"
## --------------------------------------------------------------------

install.packages(c("sf", "dplyr", "tidyr","ggplot2", "raster", "mapview", "jpndistrict", "maptools","igraph"),dependencies=T)
install.packages(c("curl", "png", "jpeg", "spatialEco","igraph"),dependencies=T)

library(sf)
library(dplyr)
library(tidyr)
library(raster)
library(mapview)
library(jpndistrict)
library(maptools)
library(ggplot2)

## --------------------------------------------------------------------
# 基本編
## --------------------------------------------------------------------
## ベクタの処理
## --------------------------------------------------------------------
### ベクタデータの読み込み、確認、表示１
data(wrld_simpl,package="maptools")　#データの読み込み
wld<-st_as_sf(wrld_simpl) #sp形式からsf形式に変換
class(wld) #クラスの確認
head(wld) #データの中身
str(wld) #データの情報
names(wld) #データの属性名
summary(wld) #属性値のサマリー
st_crs(wld) #データの空間参照系
st_bbox(wld) #データの範囲
plot(wld) #地図表示(各属性を主題として)
plot(wld["NAME"]) #地図表示(NAME属性のみ)
mapview(wld) #インタラクティブな地図表示


## --------------------------------------------------------------------
### ベクタデータの読み込み、確認、表示２

library(jpndistrict) #国土数値情報の行政界データ（R用にデータ変換されたもの）
ibaraki<-jpn_pref(admin_name="茨城県") #茨城県を読み込み
class(ibaraki) #クラスを確認
head(ibaraki,n=3) #データの中身
str(ibaraki) #データの情報
names(ibaraki) #データの属性名
summary(ibaraki) #属性値のサマリー
st_crs(ibaraki) #データの空間参照系
st_bbox(ibaraki) #データの範囲
plot(ibaraki) #地図表示(各属性を主題として)
plot(ibaraki["city"]) #地図表示(city_name属性のみ)
mapview(ibaraki) #インタラクティブな地図表示


## --------------------------------------------------------------------
### ベクタデータの読み込み、確認、表示3
#ファイルのダウンロードと解凍
f <- tempfile(fileext = ".zip")
url<-"http://soil-inventory.dc.affrc.go.jp/download/Shape20/%E8%8C%A8%E5%9F%8E_20.zip"
download.file(url,f)
mydir<-"./soil"
unzip(f, exdir = mydir)

#日本語ファイル名をアルファベットに変化
for(fi in list.files(mydir,full.names=TRUE)){
  file.rename(fi,gsub("茨城", "ibaraki", fi))
}

ibaraki_soil<-read_sf(mydir,options=c("ENCODING=CP932")) #データの読み込み。日本語エンコーディングを指定
str(ibaraki_soil) #データの情報
st_crs(ibaraki_soil) #データの空間参照系
st_bbox(ibaraki_soil) #データの範囲
plot(ibaraki_soil["SoilName"],key.size = lcm(7)) #データの読み込み


## --------------------------------------------------------------------
### ひといき
View(ibaraki_soil)

## --------------------------------------------------------------------
### ベクタデータの作成
#ポイント
p1<-st_point(c(1,2)) #ポイントデータを作成(sfgクラス)
head(p1) #データの確認
class(p1) #クラスの確認
plot(p1, pch = 1, col = 'red') #地図表示
p2<-st_point(c(2,1)) #ポイントデータを作成(sfgクラス)
geom<-st_sfc(p1,p2) #p1,p2からなるジオメトリデータを作成(sfcクラス)
head(geom) #データの確認
class(geom) #クラスの確認
plot(p2, pch = 2, col = 'blue') #地図表示
name <- c("point1","point2") #属性値を入力
point_f<-st_sf(name=name,geom) #ジオメトリと属性を設定してデータを作成(sfクラス)
class(point_f) #クラスを確認
head(point_f) #データを確認
plot(point_f,key.size = lcm(4)) #地図表示

#ポリゴン
b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1)))) #ポリゴンデータを作成(sfgクラス)
plot(b0,col="red") #地図表示
b1 = b0 + c(2,0) #b0のポリゴンのX座標を2ずらしたポリゴンを作成
plot(b1,col="blue") #地図表示
b2 = b0 * 0.5 + c(0,1.5) #b0のポリゴンを縮小してY座標を1.5ずらしたポリゴンを作成
geom<-st_sfc(b0,b1,b2) #b0,b1,b2からなるジオメトリデータを作成(sfcクラス)
class(geom) #クラスの確認
a<-data.frame(number=c(1,2,3),species=c("dog","cat","cat")) #属性データを作成
poly_f<-st_sf(a,geom) #ジオメトリと属性を設定してデータを作成(sfクラス)
class(poly_f) #クラスの確認
head(poly_f) #データの確認
plot(poly_f) #地図表示


## --------------------------------------------------------------------
### 座標系の確認、設定、変換
st_crs(ibaraki_soil) #空間参照系の確認
st_set_crs(ibaraki_soil,4612) #空間参照系の設定(EPSGコード)
soil_latlon<-st_set_crs(ibaraki_soil,4612) #空間参照系をJGD2000緯度経度に設定して変数に代入
soil_zone9<-st_transform(soil_latlon,2451) #緯度経度から平面直角座標系9系に投影変換
soil_UTM54<-st_transform(soil_latlon,3100) #緯度経度からUTM座標系ゾーン54に投影変換

st_crs(soil_latlon) #空間参照系の確認
st_crs(soil_zone9) #空間参照系の確認
st_crs(soil_UTM54) #空間参照系の確認

st_geometry(soil_latlon) #ジオメトリの座標値確認
st_geometry(soil_zone9) #ジオメトリの座標値確認
st_geometry(soil_UTM54) #ジオメトリの座標値確認

#パイプを利用した場合

library(dplyr)
soil_latlon<-ibaraki_soil %>% st_set_crs(4612) #行政界データの空間参照系をEPSGコード4612に設定
soil_zone9<-ibaraki_soil %>% st_set_crs(4612) %>% st_transform(2451) #EPSG4612に設定した後、EPSG2451に変換
soil_UTM54<-ibaraki_soil %>% st_set_crs(4612) %>% st_transform(3100) #EPSG4612に設定した後、EPSG3100に変換

soil_latlon %>% st_crs() #空間参照系の確認
soil_zone9 %>% st_crs() #空間参照系の確認
soil_UTM54 %>% st_crs() #空間参照系の確認

soil_latlon %>% st_geometry() #ジオメトリの座標値確認
soil_zone9 %>% st_geometry() #ジオメトリの座標値確認
soil_UTM54 %>% st_geometry() #ジオメトリの座標値確認


#helpのExamplesを張り付けて実行してみましょう！





# --------------------------------------------------------------------
## ラスタの処理
## --------------------------------------------------------------------
### ラスタデータの読み込み、確認、表示１
climate<-getData('worldclim', var='bio', res=10) #データのダウンロード
class(climate) #クラスの確認
climate #データの情報
names(climate) #スタックレイヤ名
bio1<-raster(climate,layer=1) #ラスタレイヤを取得（年平均気温*10）
plot(bio1) #地図表示
bio1_deg<-bio1/10 #ラスタの値を年平均気温に変換
zoom(bio1_deg) #ズーム（範囲の始点と終点をクリック）
click(bio1_deg) #値の取得（クリック、Escで終了）
s<-raster::select(bio1_deg) #範囲を選択して変数に代入（範囲の始点と終点をクリック）
plot(s) #地図表示
hist(s) #値の分布のヒストグラム表示
density(s) #値の分布の密度表示

## --------------------------------------------------------------------
### ラスタデータの読み込み、確認、表示２
#データのダウンロード
f <- tempfile(fileext = ".zip")
url<-"http://www1.gsi.go.jp/geowww/globalmap-gsi/download/data/gm-japan/gm-jpn-el_u_1_1.zip"
download.file(url,f)
unzip(f, exdir = ".")

f<-"gm-jpn-el_u_1_1/jpn/el.tif"
dem<-raster(f) #データの読み込み
dem #データの情報
plot(dem) #地図表示
crs(dem) #空間参照系の確認
hist(dem) #値の分布のヒストグラム表示
density(dem) #値の分布の密度表示

## --------------------------------------------------------------------
### ラスタデータの作成
(r<-raster()) #ラスタデータを作成（範囲と解像度はデフォルト値）
class(r) #クラスを確認
(r<-raster(ncol=10,nrow=10)) #列数10、行数10のラスタデータを作成（範囲はデフォルト値）
(r<-raster(ncol=36,nrow=18,xmn=-1000,xmx=1000,ymn=-100,ymx=900)) #列数、行数、範囲を指定してラスタデータ作成
res(r)<-100 #解像度を100に変更
r #データの情報を表示
hasValues(r) #ラスタ値があるか確認
set.seed(0)
values(r)<-runif(ncell(r)) #乱数をラスタの値に代入
plot(r) #地図表示
r1<-r2<-r3<- raster(nrow=10,ncol=10) #列数10、行数10のラスタデータを作成
values(r1) <- 1:ncell(r1) #ラスタ値に1～セル数の値を代入
values(r2) <- runif(ncell(r2)) #乱数をラスタの値に代入
r3 <- r1*r2 #ラスタ演算（セルの値同士を掛け算）
(s<- stack(r1,r2,r3)) #RasterStackを作成
plot(s) #地図表示
(b<-brick(r1,r2,r3)) #RasterBrickを作成
r<-raster(b,layer=3) #ラスタレイヤを取り出し
plot(r) #地図表示

## --------------------------------------------------------------------
### 座標系の確認、設定、変換
bio1<-raster(climate,layer=1) #気候データからRasterLayerを読み込み
crs(bio1) #空間参照系を確認
crs(bio1)<-CRS("+init=epsg:4326") #空間参照系を設定
bio1 #データを確認
plot(bio1) #地図表示
bio1_crop<-crop(bio1,extent(138,144,33,45)) #範囲を指定して切り抜き
plot(bio1_crop) #地図表示
bio1_UTM54 <- projectRaster(bio1_crop, crs=CRS("+init=epsg:3100")) #UTM54に投影変換
bio1_UTM54 #データを確認
options(scipen = 10) #軸の指数表記をやめる
plot(bio1_UTM54) #地図表示


#helpのExamplesを張り付けて実行してみましょう！




## --------------------------------------------------------------------
# 応用編
## --------------------------------------------------------------------
## 陰影段彩図を作成してみる
alt <- getData('alt', country='JP',mask=TRUE) #標高データの読み込み
class(alt) #クラスの確認
alt #データの情報
alt <- crop(alt,extent(137,141,34.5,38)) #範囲を指定して切り抜き
slope = terrain(alt, opt='slope') #傾斜の作成（ラジアン）
aspect = terrain(alt, opt='aspect') #斜面方位の作成（ラジアン　北が０で時計回り）
hill = hillShade(slope, aspect) #陰影起伏の作成
plot(hill, col=grey(0:100/100), legend=FALSE, main='陰影段彩図') #陰影起伏を表示
plot(alt,col=terrain.colors(10, alpha=0.35),zlim=c(0,1500), add=TRUE) #標高を重ね合わせ
x <- terrain(alt, opt=c('slope', 'aspect'), unit='degrees') #傾斜、斜面方位をRasterStackで作成（度数）
hist(x) #分布表示
density(x) #分布表示
writeRaster(slope,"slope.tif",format="GTiff",overwrite=TRUE) #GeoTIFFに書き出し

## --------------------------------------------------------------------
## 正距方位図法を作成してみる
data(wrld_simpl,package="maptools") #世界地図の読み込み
wld<-wrld_simpl %>% st_as_sf() %>% st_geometry() #SpatialPolygonクラスを一旦sfクラスに変換して、sfcクラスに変換
aeqd_proj <- '+proj=aeqd +lat_0=39.036694 +lon_0=125.764559' #中心座標を指定して正距方位図法の定義を指定
wld_aeqd <- wld %>% st_transform(aeqd_proj) #投影変換
#表示範囲を指定し地図表示。経緯度図郭線も表示
plot(wld_aeqd, col = 'grey80',graticule = st_crs(4326),axes = TRUE,xlim=c(-2000000,2000000),ylim=c(-3000000,3000000),lon = seq(80,180,by=10),main="正距方位図法で表示")
#中心位置のポイントを緯経度で作成し投影変換
p<-st_sf(name="平壌",geom=st_sfc(st_point(c(125.764559,39.036694))), crs=4326) %>%
  st_transform(aeqd_proj)
#ポイントを赤色で重ね合わせて表示
plot(p,col="red",pch=20,cex=2,add=T)

p_geom<-p %>% st_transform(aeqd_proj) %>% st_geometry() #sfcに変換（st_transformはしなくて良い）
cent<-p_geom[[1]] #中心点のsfgを取得
dist.sfc <- lapply(10^6 * 1:4, function(x) st_buffer(cent,x)) %>% st_sfc() #多重のバッファーsfcを作成
dist.sf <- st_sf(name=c("1000km","2000km","3000km","4000km"),geom=dist.sfc,crs=aeqd_proj) #sfクラスに変換
plot(dist.sf,border="red",col=NA,add=T)　#バッファーを重ねて表示
text(data.frame(x=10^6*1:4,y=0*1:4), labels=dist.sf$name, cex = 0.8) #ラベルを表示
p_guam<-st_sf(name="グアム",geom=st_sfc(st_point(c(144.7631,13.4521))), crs=4326) %>%
  st_transform(aeqd_proj) #別のポイントを作成
plot(p_guam,col="blue",pch=20,cex=2,add=T) #ポイントを青色で重ねて表示
#正距円筒図法で表示（横方向の等距離は歪む）
plot(wld, col = 'grey80',graticule = st_crs(4326),axes = TRUE,xlim=c(100,160),ylim=c(10,60),lon = seq(80,180,by=10),main="正距円筒図法で表示")
plot(st_transform(p,4326),col="red",pch=20,cex=2,add=T)
plot(st_transform(p_guam,4326),col="blue",pch=20,cex=2,add=T)
plot(st_transform(dist.sf,4326),border="red",col=NA,add=T)


## --------------------------------------------------------------------
### 土壌図を編集して情報をみる。
#読み込み、表示
mydir<-"./soil" #ファイル名を指定してもOK
ibaraki_soil<-read_sf(mydir,options=c("ENCODING=CP932")) #読み込み
plot(ibaraki_soil["SoilName"],key.size = lcm(7)) #属性を指定して表示
#全域を統合（統合して、簡素化）
dojyou_area<-
  st_union(ibaraki_soil) %>%
  st_simplify(preserveTopology = FALSE,dTolerance = 0.003)
plot(dojyou_area)
#同じ土壌を融合
soil_union<-ibaraki_soil %>% group_by(SoilName) %>% summarise(do_union=T)
#マルチポリゴンをポリゴンにして、投影変換して、面積を計算
soil_UTM54<-soil_union %>% st_cast() %>% st_cast("POLYGON") %>% st_transform(3100) %>% mutate(AREA=st_area(.))
#融合した土壌図を表示
plot(soil_UTM54["SoilName"],border=NA,col=rainbow(17,alpha=0.8),key.size = lcm(7))

#抽出 SoilName列を抽出し、特定の土壌のみを表示
soil_UTM54 %>% dplyr::select("SoilName") %>% filter(SoilName=="褐色森林土") %>% plot(main="褐色森林土")
soil_UTM54 %>% dplyr::select("SoilName") %>% filter(grepl("アロフェン",SoilName)) %>% plot(col=rainbow(4,alpha=0.5),border=NA,main="アロフェン")　#アロフェンとつくものすべてを表示
soil_UTM54 %>% transmute(AREA_NO_UNIT=as.numeric(AREA)) %>% filter(AREA_NO_UNIT<10000000) %>% plot(border=NA,main="SMALL AREA") #面積が小さいものを表示 filterでUNITに対応していない

#データフレーム
library(ggplot2)
df<-soil_UTM54 %>% st_set_geometry(NULL) #データフレームに変換
df2<-df %>% group_by(SoilName) %>% summarise(TOTALAREA=as.numeric(sum(AREA))) %>% arrange(desc(TOTALAREA)) %>% mutate(SoilName = factor(SoilName, SoilName)) #土壌ごとに面積を集計したものを、降順に並び替え、factorもその順番にする
df2<-df2 %>% mutate(TOTALAREA=TOTALAREA/1000000) #面積を平方キロに変換
ggplot(df2,aes(x=SoilName,y=TOTALAREA))+geom_bar(stat = "identity",fill="lightblue") +
  labs(title="茨城県の土壌面積", x="", y="面積(km^2)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #グラフ表示

#ファイルに書き出し
write_sf(soil_UTM54,"soil_UTM54.shp",layer_options="ENCODING=CP932")


## --------------------------------------------------------------------
### 地理院タイルを取得してみる
source("gsitiles.R")

#標準地図の取得
stdmap<-getTile(137.665215,36.294582,137.7,36.314582,15,type="std")
stdmap
plotRGB(stdmap)

#航空写真の取得
photo<-getTile(137.665215,36.294582,137.7,36.314582,15,type="photo")
plotRGB(photo)

#投影変換と書き出し
stdmapUTM<-projectRaster(stdmap, crs=CRS("+init=epsg:3099"),method='ngb')
writeRaster(stdmapUTM,"stdmap.tif",datatype="INT1U") #datatypeを指定

#標高データを取得・表示・投影変換・書き出し
dem<-getTile(137.665215,36.294582,137.7,36.314582,15,type="dem5a")
plot(dem)
demUTM<-projectRaster(dem, crs=CRS("+init=epsg:3099"),method='bilinear')
writeRaster(demUTM,"dem.tif")

#CS立体図の作成
cs<-makeCS(dem,shaded=TRUE)
plotRGB(cs)

#標準地図とCS立体図を乗算合成
stdmap_cs<-overlayColor(stdmap,cs,type="multiply")
plotRGB(stdmap_cs)


## --------------------------------------------------------------------
## メッシュでデータを集計してみる。

#茨城県を読み込み
ibaraki<-jpn_pref(admin_name="茨城県")
ibaraki
plot(ibaraki)

#投影変換
ibaraki_UTM <- ibaraki %>% st_transform(3100)

#つくば市を抽出
tsukuba<-ibaraki_UTM %>% filter(city=="つくば市")

#範囲の確認と設定
tsukuba %>% st_bbox()
bbox<-c(409000,3978000,426000,4011000)

#メッシュ作成
mesh<-matrix(bbox,2,byrow=T) %>% st_multipoint %>% st_sfc(crs=3100) %>% st_make_grid(cellsize=c(1000,1000))%>% st_sf(ID=1:length(.))
plot(mesh)

#つくば市に重なるメッシュだけ抽出
inter<-mesh %>% st_intersects(tsukuba,sparse = F)
mesh<-mesh %>% filter(inter)
mesh<-mesh %>% mutate(ID=1:length(.))
plot(mesh)

#ここの場所
here<-st_point(c(140.096739,36.204601)) %>% st_sfc(crs=4612) %>% st_transform(3100)

#表示
plot(st_geometry(tsukuba))
plot(mesh,col=NA,add=T)
plot(here,col="blue",pch=20,cex=2,add=T)

#メッシュ中心とこの場所との距離
dist<-st_distance(here,mesh %>% st_centroid()) %>% as.vector()

#メッシュの属性にdistを追加
mesh<-mesh %>% mutate(distance=dist)
plot(mesh)

# #標高を読み込み

bbox<-ibaraki %>% filter(city=="つくば市") %>% st_bbox() %>% as.vector()
dem<-getTile(bbox[1],bbox[2],bbox[3],bbox[4],11,type="dem10b")

#投影変換
dem_UTM <- projectRaster(dem, crs=CRS("+init=epsg:3100"),method="bilinear")

#解像度の変更
dem100 <- dem_UTM
res(dem100)<-100
dem100 <- resample(dem_UTM, dem100, method='bilinear')


#メッシュの矩形範囲でdemを切り取る
bbox<-c(409000,3978000,426000,4011000)
dem_crop<-crop(dem100,extent(bbox[1],bbox[3],bbox[2],bbox[4]))
plot(dem_crop)
plot(st_geometry(tsukuba),col=NA,add=T)
plot(st_geometry(mesh),col=NA,add=T)


#起伏と傾斜の作成
slope = terrain(dem_crop, opt='slope', unit='degrees')
aspect = terrain(dem_crop, opt='aspect', unit='degrees')
plot(slope)
plot(aspect,col=rainbow(8))

#メッシュごとの平均傾斜
slope_df<-raster::extract(slope,as(mesh,"Spatial"),fun=mean,df=TRUE,na.rm=TRUE)

#メッシュの属性に追加
mesh<-mesh %>% mutate(slope=slope_df$slope)

#斜面方位の最頻値

#東西南北にリクラスする関数
asp_reclass<-function(x){
  #北から時計回り。0はフラット
  x[x==0]<-0#FLAT
  x[0<x&x<45]<-1#N
  x[45<=x&x<135]<-2#E
  x[135<=x&x<225]<-3#S
  x[225<=x&x<315]<-4#W
  x[x>=315]<-1#N
  
  return(x)
}

asp<-calc(aspect,fun=asp_reclass)

plot(asp,col=rainbow(5))

#最頻値を返す関数
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#メッシュで斜面方位を抽出し、メッシュIDごとの最頻値を算出
aspect_df<-raster::extract(asp,as(mesh,"Spatial"),df=TRUE) %>% group_by(ID) %>% summarise(aspect=getmode(layer))
#方位を表す数値をNEWS+Fの文字列に変換
aspect_df<-aspect_df %>% mutate(aspect=plyr::mapvalues(aspect_df$aspect, from = c(0,1,2,3,4), to = c("F","N","E","S","W")))
#メッシュの属性にfactorとして追加
mesh<-mesh %>% mutate(aspect=factor(aspect_df$aspect))
plot(mesh)


##土壌データの読み込み
ibaraki_soil<-read_sf(mydir,options=c("ENCODING=CP932"))
#投影変換
soil_UTM<-ibaraki_soil %>% st_transform(3100)
#土壌データをメッシュで切る
mesh_soil<-soil_UTM %>% st_intersection(mesh)
#メッシュごとの同じ土壌をマルチポリゴンにして面積計算
mesh_soil<-mesh_soil%>% group_by(ID,SoilName) %>% st_cast("MULTIPOLYGON") %>% mutate(area=as.numeric(st_area(.))) 
#メッシュ内で面積が大きい土壌をメッシュの土壌とする
mesh_soil_df<-mesh_soil %>% group_by(ID) %>% filter(area==max(area))
#メッシュの属性にfactorとして追加
mesh<-mesh %>% mutate(soil=factor(mesh_soil_df$SoilName))
plot(mesh)

#データフレームに変換
mesh_df <- mesh %>% st_set_geometry(NULL)
#ペアプロット
mesh_df %>% pairs


#メッシュの隣接関係（空間自己相関の解析に利用）
library(igraph)

#自分以外の縦横の隣接行列を作成。
st_rook <- function(a, b = a){
  st_relate(a, b, pattern = "F***1****",sparse = F)
}

#斜めも隣接として入れる場合
#st_queen <- function(a, b = a){
#  st_relate(a, b, pattern = "F***T****")
#}

#隣接行列から片方向のgraphオブジェクトを作成して、リストにする
nblist<-mesh %>% st_rook() %>% graph_from_adjacency_matrix(mode=c("undirected")) %>% get.edgelist()
nblist

## --------------------------------------------------------------------
## 衛星データをダウンロードして表示してみる
url<-"http://landsat-pds.s3.amazonaws.com/L8/107/035/LC81070352015218LGN00/"
flist<-c("LC81070352015218LGN00_B2.TIF","LC81070352015218LGN00_B3.TIF","LC81070352015218LGN00_B4.TIF","LC81070352015218LGN00_B5.TIF")

mydir<-"./landsat"
dir.create(mydir, showWarnings = FALSE)

for(fi in flist){
  f<-paste(url,fi,sep="")
  dest<-paste(mydir,fi,sep="/")
  if(!file.exists(dest)) download.file(f,dest,mode="wb")
}

files<-list.files(mydir,pattern="TIF$",full.names=TRUE)
landsat_org<-stack(files)

class(landsat_org)
landsat_org
names(landsat_org)
nlayers(landsat_org)
dim(landsat_org)
crs(landsat_org)
plot(landsat_org,1,col=gray.colors(30))

## --------------------------------------------------------------------
## 衛星データを切り抜いてカラーで表示してみる
ibaraki<-jpn_pref(admin_name="茨城県")
ibaraki_UTM54<-ibaraki %>% st_transform(32654)
ibaraki_cities<-ibaraki_UTM54 %>% filter(grepl("つくば市|つくばみらい市|常総市",city))
#plot(ibaraki_cities)
bbox<-ibaraki_cities %>% st_bbox() %>% as.vector()
landsat<-crop(landsat_org,extent(bbox[1],bbox[3],bbox[2],bbox[4]))

#landsat<-crop(landsat,extent(400000,430000,3980000,4000000))

plotRGB(landsat, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin",
        main = "Landsat8 True Color Composite")

plotRGB(landsat, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin",
        main = "Landsat8 False Color Composite")

plot(ibaraki_cities,border="white",lwd=2,col=NA,add=T)

## --------------------------------------------------------------------
## 衛星データから植生地域と水域を抽出してみる
NIR<-raster(landsat,4)
Red<-raster(landsat,3)
ndvi <- (NIR-Red)/(NIR + Red)
plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from landsat8')

veg <- ndvi >= 0.4
#値を残したい場合はこちら
#veg <- calc(ndvi, function(x){x >= 0.4})
plot(veg,legend=FALSE,main = '植生地域を抽出')

km <- kmeans(values(landsat), centers=5, iter.max=500, nstart=3, algorithm="Lloyd")
kmr <- setValues(ndvi, km$cluster)
plot(kmr,main='ランドサット画像の教師なし分類')

water<-kmr==1
plot(water,col=c("white","blue"),main = '水域を抽出')

## --------------------------------------------------------------------
## 土壌と地形の関係を調べてみる
#標高を読み込み
f<-"gm-jpn-el_u_1_1/jpn/el.tif"
dem<-raster(f)
dem
crs(dem)<-CRS("+init=epsg:4612")

#土壌エリアでdemを切り取る
bbox<-dojyou_area %>% st_bbox() %>% as.vector()
dem_crop<-crop(dem,extent(bbox[1],bbox[3],bbox[2],bbox[4]))
plot(dem_crop)
plot(dojyou_area,col=NA,add=T)

#起伏と傾斜の作成
dem_UTM54 <- projectRaster(dem_crop, crs=CRS("+init=epsg:3100"),method="bilinear") #標高データの投影変換
slope = terrain(dem_UTM54, opt='slope') #傾斜の作成
aspect = terrain(dem_UTM54, opt='aspect') #斜面方位の作成
hill = hillShade(slope, aspect) #陰影起伏の作成
plot(hill, col=grey(0:100/100), legend=FALSE, main='陰影段彩図') #陰影起伏の作成（タイトル間違い）
plot(slope*180/pi) #傾斜をラジアンから度に変換して表示
plot(aspect*180/pi,col=rainbow(8)) #斜面方位をラジアンから度に変換して表示

##土壌データのラスタ化
plot(soil_UTM54) #ベクタデータの表示
soil.sp<-as(soil_UTM54,"Spatial") #sfクラスからSpatialPolygonsDataFrameクラスに変換(spパッケージ)
soil.sp$SoilName<-as.factor(soil.sp$SoilName) #SoilNameをfactorに変換
levels(soil.sp$SoilName) #factorを確認
soil<-rasterize(soil.sp,dem_UTM54,"SoilName",fun="last") #SoilNameのfactor値をラスタ化。dem_UTM54の範囲と解像度を使用。1セルに複数のポリゴンがある場合は最後のものを使用。

plot(soil,breaks=1:17,col=rainbow(17)) #ラスタ化した土壌図を表示
plot(soil_UTM54,col=NA,add=T) #土壌の境界ポリゴンを表示

#ランダムポイント作成
dojyou_area_UTM54<-dojyou_area %>% st_transform(3100) #土壌範囲のベクタデータを投影変換
pnt<-st_sample(dojyou_area_UTM54,10000) #範囲内に1万点をランダムに作成
plot(dojyou_area_UTM54) #範囲の地図表示
plot(pnt,pch=".",add=T) #ランダム点の表示


#ラスタから値を抽出
envs<-stack(dem_UTM54,slope,aspect,soil) #標高、傾斜、斜面方位、土壌のRasterStackを作成
names(envs)<-c("elev","slope","aspect","soil") #RasterStackのレイヤ名を変更
envs #データの確認
pnt.sp<-as(pnt,"Spatial") #SpatialPointsクラスに変換
ex<-raster::extract(envs,pnt.sp,df=TRUE) #ランダムポイントの位置のRasterStackの値を抽出
head(ex) #抽出した値の確認

#モデル作成と予測
ex$soil<-as.factor(ex$soil) #土壌データはfactorに変換
pairs(ex[,2:5],cex=0.1) #ペアプロット

m<-glm(slope~elev+aspect+soil,data=ex) #傾斜の決定要因をモデル化
summary(m) #モデルの概要（モデルは出鱈目です）

result<-predict(envs,m) #モデルを全域に適用
par(mfrow=c(1,2)) #追加（プロットを2列表示）
plot(result) #予測した傾斜の結果を地図表示
slope_mask<-mask(slope,soil) #土壌の範囲で傾斜データをマスク（周辺部分を消す）
plot(slope_mask) #実際の傾斜を表示