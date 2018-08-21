#' ---
#' title: "Rを用いたGIS"
#' author: "株式会社エコリス 水谷貴行"
#' date: "2018年4月5日"
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
#' #準備編 
#++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++
#' ##インストール 
#++++++++++++++++++++++++++++
#' 以下の環境を用意してください。
#' 
#' * Windows OS >=7
#' * R >= 3.4.1
#' * RStudio >= 1.0
#'
#' 使用するデータをダウンロードしてください。
#' 
#' * [使用データダウンロード]()
#'
#' パッケージをインストールします。
#+ eval=F
install.packages(c("sf","geos","dplyr","tidyr","raster","ggplot2","mapview","jpndistrict","maptools","igraph","dismo"),dependencies=T,type="win.binary")

#' ライブラリを読み込みます。
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(raster)
library(mapview)
library(jpndistrict)
#library(maptools)
library(ggplot2)
library(dismo)

#++++++++++++++++++++++++++++
#' ##基礎知識 
#++++++++++++++++++++++++++++

#++++++++++++++++++
#' ###パイプ
#++++++++++++++++++
#' パイプ左側の関数の出力結果をパイプ右側の関数の第一引数にする
#head(1:5,3)をパイプで書くと
1:5 %>% head(3)
#パイプの出力を次のパイプの入力にできる
c(1,10,100) %>% tan %>% sin %>% cos
#.を使うと第1引数以外にできる
3 %>% head(1:5,.)

#++++++++++++++++++
#' ###map関数 
#++++++++++++++++++
#' ベクトルかリストを入力し、各要素に関数を適用し、入力と同じ長さのリストを返す
#xを平均値とした正規乱数を5個作成する関数を定義
myfunc<-function(x){
  rnorm(5,x)
}
#各値を引数として、myfunc関数を適用する。戻り値は、各5個の乱数を持った3要素のリスト。
c(1,10,100) %>% map(myfunc)
#+ results='hide'
#以下のように簡略化して書ける。~はfunction、.xは引数を表す。
c(1,10,100) %>% map(~rnorm(5,.x))
#+ results='markup'
#リストの各要素ごとに平均値を計算する場合。戻り値は、身長の平均値、体重の平均値の2要素のリスト
mylist<-list(height=130:140,weight=40:50)
mylist %>% map(mean)

#++++++++++++++++++
#' ###データの変形
#++++++++++++++++++
#横長なデータを作成
data <- tibble(
  Point = c("地点A","地点B"),
  Red = c(0.1,0.3),
  Green = c(0.3,0.2),
  Blue = c(0.5,0.6)
)
print(data)

#縦長のデータに変換
data2<-data %>% gather(key = Band, value="Refrectance",-Point)
#横長のデータに変換
data2 %>% spread(key=Band,value=Refrectance)

#++++++++++++++++++++++++++++++++++++++
#' #ベクタ基本編
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##基本操作
#++++++++++++++++++++++++++++

#' ### シェープファイルを読み込みたい
landcover<-read_sf("data/landcover.shp",options=c("ENCODING=CP932"))

#' ### データの概要を確認したい
head(landcover)

#' ### 地図を表示したい
plot(landcover)

#' ### データクラスを確認したい
class(landcover)

#' ### データの構造を確認したい
str(landcover) 

#' ### 属性名を確認したい
names(landcover)

#' ### 属性値のサマリーを確認したい
summary(landcover)

#' ### データの範囲を確認したい
st_bbox(landcover)

#' ### Rstudioで属性表を確認したい
View(landcover)

#' ### インタラクティブな地図を表示したい
mapview(landcover)

#' ### シェープファイルを書き出したい
st_write(landcover, "mydata/landcover2.shp",layer_options = "ENCODING=UTF-8",delete_layer = TRUE)

#++++++++++++++++++++++++++++
#' ##座標系の操作 
#++++++++++++++++++++++++++++

#' ### データの空間参照系を確認したい
st_crs(landcover) 

#' ### 空間参照系をEPSGコードで指定したい
landcover_epsg<-st_set_crs(landcover,2451)

#' ### 別の座標系に変換したい
landcover_latlon<-st_transform(landcover,4612)

#' ### データのジオメトリを確認したい
st_geometry(landcover)

#++++++++++++++++++++++++++++
#' ##データの作成・編集 
#++++++++++++++++++++++++++++

#' ### ポイントデータを作成したい
p1<-st_point(c(1,2)) #sfgクラス
p2<-st_point(c(2,1))
p1
p2
mygeom<-st_sfc(p1,p2) #sfcクラス
mygeom
myname <- c("point1","point2") #属性値を入力
point_f<-st_sf(name=myname,geom=mygeom) #sfクラス
point_f

#' ### ラインデータを作成したい
pts1 = rbind(c(-1,-1), c(1,1), c(2,0), c(3,2))
pts2 = rbind(c(1,-1), c(2,2), c(3,1), c(2,4))
ls1 = st_linestring(pts1)
ls2 = st_linestring(pts2)
mygeom<-st_sfc(ls1,ls2) #sfcクラス
myname <- c("line1","line2") #属性値を入力
line_f<-st_sf(myname,mygeom) #sfクラス

#' ### ポリゴンデータを作成したい
pts = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
b0 = st_polygon(list(pts)) #sfgクラス
b1 = b0 + c(2,0) #b0のポリゴンのX座標を2ずらしたポリゴンを作成
b2 = b0 * 0.5 + c(0,1.5) #b0のポリゴンを縮小してY座標を1.5ずらしたポリゴンを作成
mygeom<-st_sfc(b0,b1,b2) #sfcクラス
myname<-data.frame(number=c(1,2,3),species=c("dog","cat","cat")) #属性データを作成
#+ eval=F
poly_f<-st_sf(myname,geom) #sfクラス #Bug ref.#782

#' ### ランダムポイントを作成したい
pnt<-st_sample(landcover,1000) #landcoverの範囲に1000点
plot(pnt,pch=".",cex=2)

#' ### メッシュを作成したい
mesh <- st_make_grid(n=c(10,5), offset = c(24920,2740), cellsize = c(6,6),crs = 2451)
mesh <- st_sf(mesh)
plot(mesh,axes=T)

#' ### ポリゴンをラインに変換したい
landcover_l<-landcover %>% st_cast("LINESTRING")

#' ### sfクラスからspクラスに変換したい

#' ### spクラスからsfクラスに変換したい 

#++++++++++++++++++++++++++++
#' ##地図表示 
#++++++++++++++++++++++++++++

#' ### 指定したデータを表示したい
plot(landcover["subclass"])

#' ### 座標軸を表示したい
plot(landcover["subclass"],axes=T)

#' ### 凡例の表示幅を指定したい
plot(landcover["subclass"],key.width = lcm(3))

#' ### ggplot2で表示したい
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass))

#' ### 座標軸の座標系を指定したい
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  coord_sf(datum = 2451)

#' ### 座標軸と背景を消したい
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  coord_sf(datum = NA) +
  theme_void()

#' ### 凡例の色を指定したい
mycol <- c("gray", "black", "white", "darkgreen","brown","lightgreen","lightblue", "blue","orange","green","yellow")
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  scale_fill_manual(values = mycol) + 
  coord_sf(datum = NA) +
  theme_void()

#' ### データを重ねたい
ggplot() + 
  geom_sf(data = landcover,aes(fill = subclass)) +
  geom_sf(data = pnt,size=0.5) +
  geom_sf(data = mesh,fill = "transparent",color="white",size=1)

#++++++++++++++++++++++++++++
#' ##データの選択・検索 
#++++++++++++++++++++++++++++

#' ### 特定の行のデータを抽出したい
landcover[1,]

#' ### 特定の列のデータを抽出したい
landcover[,"class"]

#' ### 特定の列のデータをベクトルで抽出したい
landcover$class

#' ### 属性値でデータを抽出したい
landcover_A<-landcover %>% dplyr::filter(class=="構造物")
plot(landcover_A["class"])

#' ### 複数の属性値でデータを抽出したい
landcover_B<-landcover %>% dplyr::filter(subclass %in% c("高木","川"))
plot(landcover_B["subclass"])

#' ### 各ポリゴンに重なるポイントのインデックスを知りたい
st_intersects(mesh,pnt)

#' ### 各ポリゴンに重なるポイントの数を知りたい
st_intersects(mesh,pnt) %>% lengths

#' ### 交差するオブジェクトを抽出したい
inter<-landcover %>% filter(lengths(st_intersects(.,mesh))>0)
plot(inter)

#++++++++++++++++++++++++++++
#' ##空間演算 
#++++++++++++++++++++++++++++

#' ### 交差したオブジェクトを切り抜きたい
inter<-st_intersection(landcover,mesh)
plot(inter)

#' ### ポリゴンの重心を求めたい
cent<-st_centroid(mesh)
plot(cent)

#' ### バッファーを作成したい
st_buffer(cent,2) %>% plot

#++++++++++++++++++++++++++++
#' ##データの計測 
#++++++++++++++++++++++++++++

#' ### 面積を計測したい
st_area(landcover)

#' ### 長さを計測したい
st_length(landcover_l)

#' ### 地物の距離を計測したい
st_distance(cent[1,],cent[2:4,])

#++++++++++++++++++++++++++++
#' ##属性情報の操作
#++++++++++++++++++++++++++++

#' ### 属性にIDを振りたい
mesh<-mesh %>% mutate(ID=1:nrow(.))
mesh

#' ### 面積を属性に追加したい
landcover_area<-landcover %>% mutate(AREA=st_area(.))

#' ### 面積の単位を変更したい
#ヘクタールha、アールa、平方キロ,km^2
landcover_area %>% mutate(AREA=units::set_units(.$AREA,ha))

#' ### メッシュ内のポイント数を属性に追加したい
cnt<-lengths(st_intersects(mesh,pnt))
mesh2<-mesh %>% mutate(count=cnt)

#' ### 属性値で計算したい
mesh2<-mesh2 %>% mutate(AREA=st_area(.))
mesh2 %>% mutate(density=count/AREA)

#' ### 属性列を削除したい
landcover %>% dplyr::select(-subclass)

#' ### 属性だけを取り出したい
landcover %>% st_set_geometry(NULL)

#' ### 同じ属性のポリゴンをマルチポリゴンにしたい
landcover %>% group_by(class) %>% summarise()

#' ### 同じ属性の面積を集計したい
landcover_area %>% group_by(class) %>% summarise(TOTALAREA=sum(AREA))

#' ### メッシュ内の同じ属性の面積を集計したい
landcover_mesh<- st_intersection(landcover,mesh) %>% mutate(ID=1:nrow(.),AREA=st_area(.))
landcover_mesh<-landcover_mesh %>% group_by(ID,subclass) %>% summarise(AREA=sum(AREA)) 

#' ### メッシュ内の最大面積の属性をメッシュの属性にしたい
df<-landcover_mesh %>% group_by(ID) %>% filter(AREA==max(AREA)) %>% st_set_geometry(NULL)
mesh %>% left_join(df,by="ID")

#++++++++++++++++++++++++++++++++++++++
#' #ラスタ基本編 
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##基本操作
#++++++++++++++++++++++++++++

#' ### ラスタデータを読み込みたい
ortho<-stack('data/cropped-rededge.tif')

#' ### ラスタデータをバンドを指定して読み込みたい
ortho1<-raster('data/cropped-rededge.tif',layer=1)

#' ### データの概要を確認したい
ortho

#' ### 地図を表示したい
plot(ortho1)

#' ### 総セル数を確認したい
ncell(ortho)

#' ### Xセル数、Yセル数、バンド数を確認したい
dim(ortho)

#' ### 解像度を確認したい
res(ortho)

#' ### バンド数を確認したい
nlayers(ortho)

#' ### バンド名を確認したい
names(ortho)

#' ### セル値のサマリーを確認したい
summary(ortho1)

#' ### データの範囲を確認したい
extent(ortho)

#' ### ラスタデータを書き出したい
writeRaster(ortho1,filename = "mydata/ortho1.tif", format = "GTiff", overwrite = TRUE)


#++++++++++++++++++++++++++++
#' ##座標系の操作
#++++++++++++++++++++++++++++

#' ### データの空間参照系を確認したい
crs(ortho)

#' ### 空間参照系をEPSGコードで指定したい
crs(ortho)<-CRS("+init=epsg:2451")

#' ### 別の座標系に変換したい
ortho_utm54<-projectRaster(ortho1, crs=CRS("+init=epsg:3100"))

#++++++++++++++++++++++++++++
#' ##地図表示 
#++++++++++++++++++++++++++++

#' ###バンドと色を指定して表示したい
plot(ortho,1,col=gray.colors(30))

#' ###バンドをRGBに指定して表示したい
plotRGB(ortho, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "True Color Composite")

#' ###ggplot2で表示したい


#++++++++++++++++++++++++++++
#' ##データの選択・検索
#++++++++++++++++++++++++++++

#' ###ラスタ値を取得したい
getValues(ortho1)

#' ###ラスタ値を変更したい
ortho1<-ortho1/65536

#++++++++++++++++++++++++++++
#' ##データの作成・編集
#++++++++++++++++++++++++++++

#' ###指定範囲で切り抜きたい
e<-extent(24905,24992,2711,2824)
orthocrop<-crop(ortho,e)

#' ###マウスで範囲を指定したい

#++++++++++++++++++++++++++++
#' ##空間演算 
#++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++ 
#' #応用編 
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##マルチスペクトル画像による土地被覆の自動分類 
#++++++++++++++++++++++++++++

#' ###画像の読み込み、下処理、書き出し

#データを読み込みバンドを名を設定
rededge <- stack('data/flight20180703_rededge.tif')
rededge <- subset(rededge,1:5) #アルファバンドを除外
names(rededge) <- c('blue','green','red','nir','rededge')
rededge

#切り取り(最初は手動で実施して値を取得)
#rededgecrop<-raster::select(rededge) #手動で選択する場合
e<-extent(24905,24992,2711,2824)
rededgecrop<-crop(rededge,e)

#書き出し
crs(rededgecrop)<-CRS("+init=epsg:2451")
writeRaster(rededgecrop,filename = "mydata/cropped-rededge.tif", format = "GTiff", overwrite = TRUE)


# ###画像の読み込み、情報確認、表示

#データを読み込み、バンド名を指定、情報確認
rededge <- stack('mydata/cropped-rededge.tif')
names(rededge) <- c('blue','green','red','nir','rededge')
crs(rededge)
ncell(rededge)
dim(rededge)
res(rededge)
nlayers(rededge)

#表示（blueバンド、TrueColor、FalseColor、NaturalColor）
plot(rededge,1,col=gray.colors(30))
plotRGB(rededge, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
plotRGB(rededge, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge False Color Composite")
plotRGB(rededge, r = 3, g = 4, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge Natural Color Composite")

# ### バンド間の相関

#各バンドを抽出し、16bitの値から0～1の反射率に変換
b_red<-subset(rededge,3)/65536
b_nir<-subset(rededge,4)/65536
b_rededge<-rededge[[5]]/65536
red_nir<-stack(b_red,b_nir)
red_rededge<-stack(b_red,b_rededge)

#相関グラフ
pairs(red_nir, main = "Scatterplot between Red and NIR")
pairs(red_rededge, main = "Scatterplot between Red and Rededge")

# ### スペクトルプロファイルの作成

#土地被覆の読み込み
landcover<-read_sf("data/landcover.shp",options=c("ENCODING=CP932"))
plot(landcover)
plot(landcover["class"],key.width = lcm(3))
plot(landcover["subclass"],key.width = lcm(3))

#サブクラスの一覧をベクトルで取得
subcl<-landcover %>% 
  st_set_geometry(NULL) %>% #sfからデータフレームに変換
  dplyr::distinct(subclass,.keep_all=FALSE) %>% #重複を削除 
  .$subclass #ベクトルを返す

#サブクラスごとにランダム点を300ポイント作成
subclass_sampling<-function(x){
  filter(landcover,subclass==x) %>% 
    st_sample(300)
}
pntlist<-subcl %>% map(subclass_sampling) 
pnt<-pntlist %>% reduce(c)

#ポイントの属性にclassとsubclassを追加
pnt<-st_intersection(landcover,pnt)
plot(pnt,pch=".",cex=3)

#各ポイントでのバンド値を抽出
band_df <- raster::extract(rededge, as(pnt,"Spatial"),df=TRUE)
#ポイントの属性にバンド値（反射率に変換）を追加
pnt<-pnt %>% bind_cols(band_df/65536)

#土地被覆(大区分)ごとのバンドの平均値を計算
df_class_mean<-pnt %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(-ID,-subclass) %>% 
  group_by(class) %>% 
  summarise_all(mean) %>% 
  print

#tidyなデータに変換
df_class_mean<-df_class_mean %>% 
  gather(key=Bands,value=Reflectance,-class) %>% 
  print

#Bandsをfactorに変換（グラフ表示の際にBandsの順番を並べるため）
df_class_mean <- transform(df_class_mean, Bands= factor(Bands, levels = c("blue", "green", "red", "rededge", "nir")))

#各classのバンドの平均反射率をグラフ表示
df_class_mean %>% 
  ggplot(aes(x=Bands ,y=Reflectance,col=class))+
  geom_line(aes(group = class),size=1)+
  geom_point()+
  labs(title = "Spectral Profile")


# ### NDVIの作成

#NDVIの計算
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# nir = 4, red = 3
ndvi <- VI(rededge,4, 3)
plot(ndvi, col = rev(terrain.colors(10)), main = 'RedEdge-NDVI')

# nir = 4, rededge = 5
ndre <- VI(rededge,4, 5)
plot(ndre, col = rev(terrain.colors(10)), main = 'RedEdge-NDRE')

# ndviのヒストグラム表示
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab="Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))

# 植生の抽出
veg <- calc(ndvi, function(x){x[x < 0.4] <- NA; return(x)})
plot(veg, main = 'Veg cover')
# ndviを閾値としてカテゴリ化
vegc <- reclassify(veg, c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.4,3, 0.4,0.5,4, 0.5,Inf, 5))
plot(vegc,col = rev(terrain.colors(5)),breaks=0:5, main = 'NDVI based thresholding')


# ###教師なし分類(5バンド使用)
# set.seed(99)
# km <- kmeans(values(rededgecrop), centers = 6, iter.max = 100, nstart = 3, algorithm="Lloyd")
# knr <-raster(rededgecrop,1)
# knr[] <- km$cluster
# par(mfrow = c(1,2))
# plotRGB(rededgecrop, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
# plot(knr, main = 'Unsupervised classification',breaks=0:6,col=c("gray","orange","white","darkgreen","lightgreen","brown"))


# ###教師なし分類(ndvi使用)
# ndvi[is.na(ndvi)]<-0
# set.seed(99)
# km <- kmeans(values(ndvi), centers = 5, iter.max = 100, nstart = 3, algorithm="Lloyd")
# knr <- ndvi
# knr[] <- km$cluster
# par(mfrow = c(1,2))
# plotRGB(rededgecrop, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
# plot(knr, main = 'Unsupervised classification',breaks=0:5,col=c("gray","green","brown","darkgreen","lightgreen"))

# ###教師あり分類
#カテゴリごとにランダム点を作成
cl<-landcover %>% st_set_geometry(NULL) %>% dplyr::distinct(subclass,.keep_all=FALSE) %>% .$subclass
pnt<-cl %>% map(~filter(landcover,subclass==.) %>% st_sample(300)) %>% reduce(c)
#土地被覆を属性に追加
ptsamp<-st_intersection(landcover,pnt)
plot(ptsamp["subclass"])

#バンドの値をランダム点で抽出
df <- raster::extract(rededgecrop, as(ptsamp,"Spatial"),df=TRUE)
#反射率には変換しない
#df <- df/65536
df <- df %>% dplyr::select(-ID)
#ランダム点とバンドの値を結合
sampdata<-bind_cols(ptsamp,df)
sampdata2<-sampdata %>% st_set_geometry(NULL) %>% dplyr::select(-class)
#トレーニングデータの作成
j <- kfold(sampdata2, k = 5, by = sampdata2$subclass)
training2011 <- sampdata2[j!= 1, ] # selected the rows where j equals 1
validation2011 <- sampdata2[j == 1, ] # selected the rows where j equals [2:k]

#training2011 <- sampdata2

#決定木で学習
library('rpart')
# Train the model
cart <- rpart(as.factor(subclass)~., data = training2011, method = 'class', minsplit = 5)
plot(cart, uniform=TRUE, main="Classification Tree")
text(cart, cex = 0.8)
#学習結果でラスタから予測
pr2011 <- predict(rededgecrop, cart, type='class', progress = 'text')
#予測結果をプロット
library(rasterVis)
classcolor <- c("gray", "black", "white", "darkgreen","brown","lightgreen","lightblue", "blue","orange","green","yellow")
#classcolor <- rainbow(11)
levelplot(pr2011, maxpixels = 1e6,
          col.regions = classcolor,
          scales=list(draw=FALSE),
          main = "Decision Tree classification of RedEdge camera")

###精度の評価
prclass <- predict(cart, validation2011[,2:ncol(validation2011)], type='class')
conmat <- data.frame(prediction = prclass, reference = validation2011$subclass)
conmat <- table(conmat)
print(conmat)
n <- sum(conmat)
print(n)
nc <- nrow(conmat)
print(nc)
diag <- diag(conmat)
rowsums <- apply(conmat, 1, sum)
colsums <- apply(conmat, 2, sum)
p <- rowsums / n
q <- colsums / n
OA <- sum(diag) / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
print(OA)
print(kappa)
PA <- diag / colsums
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
print(outAcc)

#++++++++++++++++++++++++++++
#' ##マルチスペクトル画像による水稲の活性状況の把握 
#++++++++++++++++++++++++++++
#+ chunka3-2,cache=TRUE

sequoia <- stack('data/flight20180702_ortho.tif')
sequoia <- subset(sequoia,1:4)
names(sequoia) <- c('green','red','nir','rededge')

#情報確認
crs(sequoia)
ncell(sequoia)
dim(sequoia)
res(sequoia)
nlayers(sequoia)
crs(sequoia)<-CRS('+init=EPSG:2451')

#表示
plot(sequoia,3,col=gray.colors(30))
tanbo<-read_sf("data/tanbo.shp",options=c("ENCODING=CP932"))
croped<-crop(sequoia,tanbo)
sequoia<-mask(croped,tanbo)
plot(sequoia)

#表示（切り取り範囲）
par(mfrow=c(1,2))
plotRGB(sequoia, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
plotRGB(sequoia, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge False Color Composite")
plotRGB(sequoia, r = 3, g = 4, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge Natural Color Composite")

#書き出し
writeRaster(sequoia,filename = "mydata/cropped-sequoia.tif", format = "GTiff", overwrite = TRUE)

#ノイズ除去

nir<-raster(sequoia,3)
nir<- focal(nir, w=matrix(1/(3*3),nrow=3,ncol=3))
#ext<-select(nir)
ext<-extent(24790, 24792, 2761, 2763)
plot(nir,col=gray.colors(30),ext=ext)

gf <- focalWeight(nir, 0.1, "circle")
gf <- ifelse(gf == 0, 0, 1)
rg <- focal(nir, w=gf,fun=max)
plot(rg,ext=ext)
#rg<-crop(rg,ext)
pol <- rasterToPolygons(rg,n=4,dissolve=TRUE,fun=function(x){x>11000})
sfpol<-st_as_sf(pol) %>% st_cast("POLYGON")
plot(sfpol)
sfpol<-sfpol %>% mutate(AREA=st_area(.))
sfpol
fpol<-sfpol %>% filter(AREA>units::as_units(80,"cm^2"))
plot(fpol %>% st_geometry())
cent<-st_centroid(fpol)

plot(nir,col=gray.colors(30))
plot(fpol,add=T,col=NA)
plot(cent,add=T,pch=".",cex=2)


rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
p<-c(24787.8,2755.4)
g = st_make_grid(n=c(38,12), offset = p, cellsize = c(0.5,0.5))#
cp<-st_sfc(st_point(p))


g<-(g-cp)*rot(-pi*50/180)+cp
plot(nir,col=gray.colors(30))
plot(g,axes=T,add=T)
plot(cent,add=T,pch=".",cex=2)

#メッシュで集計
g<-st_set_crs(g,2451)
cnt<-g %>% st_intersects(cent,sparse=T) %>% map_int(length)
st_sf(g) %>% mutate(ct=cnt) %>% plot


#writeRaster(nir,filename = "sequoia-nir.tif", format = "GTiff", overwrite = TRUE)
#st_write(cent, "cent.shp", delete_layer = TRUE)

plot(nir,ext=ext,col=gray.colors(30))
plot(nir,ext=ext,col=gray.colors(30))

#NDVIの計算
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

#par(mfrow=c(1,2))

#NDVIの平均メッシュ表示
# For SEQUOIA NIR = 3, red = 2.
ndvi <- VI(sequoia,3, 2)
plot(ndvi, col = rev(terrain.colors(10)), main = 'SEQUOIA-NDVI')
plot(g,axes=T,add=T)
df<-raster::extract(ndvi,as(g,"Spatial"),fun=mean,df=TRUE)
st_sf(g) %>% mutate(ndvi=df$layer) %>% filter(ndvi>0.55) %>% plot(pal = rev(heat.colors(10)))

# For RedEdge NIR = 3, rededge = 4.
ndre <- VI(sequoia,3, 4)
plot(ndre, col = rev(terrain.colors(10)), main = 'SEQUOIA-NDRE')
