#' ---
#' title: "Rを用いたGIS"
#' author: "株式会社エコリス 水谷貴行"
#' date: "2018年9月3日"
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
#' 以下の環境を用意します。
#' 
#' * Windows OS >=7
#' * R >= 3.5.1
#' * RStudio >= 1.0
#'
#' 使用するデータのダウンロードとテキストを用意します。
#' 
#' * [使用データ](https://goo.gl/5zXQYk)
#' * [スライドのテキスト](https://docs.google.com/presentation/d/1-URRoB1MCcqvXsdt70Po98-USkWems_GkqzFZdslvx4/edit?usp=sharing)
#' * [スクリプトの実行結果（このhtml）](https://tmizu23.github.io/R_GIS_tutorial/R_GIS_tutorial2018.9.3.html)
#'
#' パッケージをインストールします。
#' RStudioを起動して、コンソールに以下を貼り付けて実行します。
#+ eval=F
install.packages(c("sf","geos","dplyr","tidyr","raster","ggplot2","ggrepel","mapview","jpndistrict","randomForest","kernlab"),dependencies=T,type="win.binary")

#' ライブラリが読み込めるか確認します。  
#' RStudioのコンソールに以下を貼り付けて実行します。エラーメッセージが出なければOKです。  
#' 「～マスクされています：」というメッセージは問題ありません。  

library(sf) #ベクタ処理
library(raster) #ラスタ処理
library(dplyr) #属性処理
library(purrr) #属性処理
library(tidyr) #属性処理
library(ggplot2) #地図表示
library(ggrepel) #ラベル表示処理
library(mapview) #地図表示（インタラクティブ）
library(jpndistrict) #日本の行政界データ
library(rpart) #決定木
library(randomForest) #ランダムフォレスト
library(kernlab) #SVM

#++++++++++++++++++++++++++++
#' ##プロジェクトの作成
#++++++++++++++++++++++++++++
#' RStudioで新規プロジェクトを作成します。  
#' ダウンロードしたデータを解凍し「R_GIS_2018」フォルダを”日本語パス名ではない”場所に置きます。  
#' File > New Projext... > Existing Directory > 「R_GIS_data2018」を指定し「Create Project」を押します。

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
#' ###reduce関数 
#++++++++++++++++++
#' ベクトルかリストを入力し、二変数関数を順々に適用して1つの値を返す。
c(1,10,100) %>% reduce(sum)
x <- list(c(0, 1), c(2, 3), c(4, 5))
x %>% reduce(c)

#++++++++++++++++++
#' ###データの変形
#++++++++++++++++++
#'横長なデータを作成
data <- tibble(
  Point = c("地点A","地点B"),
  Red = c(0.1,0.3),
  Green = c(0.3,0.2),
  Blue = c(0.5,0.6)
)
print(data)

#'縦長のデータに変換
data2<-data %>% gather(key = Band, value="Refrectance",-Point)
#'横長のデータに変換
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
#+ eval=F
View(landcover)

#' ### インタラクティブな地図を表示したい
#+ eval=T
mapview(landcover)

#' ### 日本の行政界を表示したい
library(jpndistrict)
ibaraki<-jpn_pref(admin_name="茨城県")
plot(ibaraki)

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
landcover_sp<-as(landcover,"Spatial")

#' ### spクラスからsfクラスに変換したい 
landcover_sf<-st_as_sf(landcover_sp)

#++++++++++++++++++++++++++++
#' ##地図表示 
#++++++++++++++++++++++++++++

#' ### 指定したデータを表示したい
plot(landcover["subclass"])

#' ### 座標軸を表示したい
plot(landcover["subclass"],axes=T)

#' ### 凡例の表示幅を指定したい
plot(landcover["subclass"],key.width = lcm(3))

#' ### 境界だけを表示したい
plot(st_geometry(landcover))

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

#' ### ラベルを表示したい
#' [参照:属性にポイントの座標を追加したい](#属性にポイントの座標を追加したい)
cent_xy<-landcover %>% st_centroid() %>% st_coordinates()
landcover_xy<-cbind(landcover,cent_xy)
ggplot(data=landcover_xy) + 
  geom_sf(aes(fill = subclass))+
  geom_text_repel(aes(x=X,y=Y,label=subclass),size=2)
  

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

#' ### 地物の座標を知りたい
st_coordinates(cent)

#++++++++++++++++++++++++++++
#' ##属性情報の操作
#++++++++++++++++++++++++++++

#' ### 属性にIDを振りたい
mesh<-mesh %>% mutate(ID=1:nrow(.))
mesh

#' ### 属性にポイントの座標を追加したい
#' [参照:ポリゴンの重心を求めたい](#ポリゴンの重心を求めたい)
cent_xy<-cbind(cent,st_coordinates(cent))

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
ortho<-stack('data/rededge.tif')

#' ### ラスタデータをバンドを指定して読み込みたい
ortho1<-raster('data/rededge.tif',layer=1)

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
#' ##ラスタの編集
#++++++++++++++++++++++++++++

#' ###ラスタースタックからレイヤを取り出したい
#subsetで
ortho1<-subset(ortho,1)
#rasterで
ortho1<-raster(ortho,1)
#リストで
ortho1<-ortho[[1]]


#' ###指定範囲で切り抜きたい
e<-extent(24905,24992,2711,2824)
orthocrop<-crop(ortho,e)

#' ###マウスで指定範囲を切り抜きたい
#+ eval=F
plot(ortho1)
orthocrop<-raster::select(ortho)
plot(orthocrop)

#+ eval=T
#' ###バンド名を指定したい
names(ortho) <- c('blue','green','red','nir','rededge','alpha')
ortho

#' ###解像度を変更したい
ortho_res5 <- ortho[[1]]
res(ortho_res5)<-5
ortho_res5 <- resample(ortho[[1]], ortho_res5, method='bilinear')
plot(ortho_res5)

#++++++++++++++++++++++++++++
#' ##値の取得・集計
#++++++++++++++++++++++++++++

#' ヒストグラムを表示したい
hist(ortho1,breaks = 30)

#' ###ラスタ値を取得したい
getValues(ortho1)

#' ###ラスタの値を座標付きで取得したい
df<-as.data.frame(ortho,xy=TRUE)
head(df)

#' ###ラスタの値をポイントで抽出したい
#' [参照:ランダムポイントを作成したい](#ランダムポイントを作成したい)
df<-raster::extract(ortho1,as(pnt,"Spatial"),df=TRUE)

#++++++++++++++++++++++++++++
#' ##ラスタ演算 
#++++++++++++++++++++++++++++

#' ###ラスタ値を変更したい
ortho2<-ortho1/65536

#' ###ラスタ値に関数を適用したい
myfunc<-function(x){
  y=x/65536
  return(y)
}
ortho2 <- calc(ortho1, myfunc)

#' ラスタを再分類したい
ortho3 <- reclassify(ortho2, c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.4,3, 0.4,0.5,4, 0.5,Inf, 5))

#++++++++++++++++++++++++++++
#' ##地図表示 
#++++++++++++++++++++++++++++

#' ###バンドと色を指定して表示したい
plot(ortho,1,col=gray.colors(30))

#' ###バンドをRGBに指定して表示したい
plotRGB(ortho, r = 3, g = 2, b = 1, axes = TRUE, stretch = "hist", main = "True Color Composite")

#' ###ズームして表示したい
#ズーム範囲の左上と右下をクリック
#+ eval=F
zoom(ortho)

#+ eval=T
#' ###ggplot2で表示したい
df<-as.data.frame(ortho1,xy=TRUE)
colnames(df)<-c("x","y","value")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = value))+
  scale_fill_gradientn(colors=gray.colors(30))

#++++++++++++++++++++++++++++++++++++++ 
#' #応用編 
#++++++++++++++++++++++++++++++++++++++
#'
#++++++++++++++++++++++++++++
#' ##マルチスペクトル画像から水稲の活性状況を把握する 
#++++++++++++++++++++++++++++
#'
#++++++++++++++++++
#' ###データを読み込み、情報を確認する
#++++++++++++++++++
#'
#読み込み
sequoia <- stack('data/sequoia.tif')
#情報確認
crs(sequoia)
ncell(sequoia)
dim(sequoia)
res(sequoia)
nlayers(sequoia)
sequoia
#表示
plot(sequoia,col=gray.colors(30))

#++++++++++++++++++
#' ###データを前処理する
#++++++++++++++++++
#'
#処理するバンドを抽出
sequoia <- subset(sequoia,1:4)
#バンド前を付ける
names(sequoia) <- c('green','red','nir','rededge')
#空間参照系を指定
crs(sequoia)<-CRS('+init=EPSG:2451')
#確認
sequoia

#処理範囲のshpを読み込む
tanbo<-read_sf("data/tanbo.shp",options=c("ENCODING=CP932"))
plot(tanbo)
#処理範囲でデータを切り抜く
croped<-crop(sequoia,tanbo)
plot(croped)
#処理範囲をマスクする
masked<-mask(croped,tanbo)
#表示して確認
plot(masked)

#++++++++++++++++++
#' ###植生指標(NDVI)を計算する
#++++++++++++++++++
#'
#NDVIを計算する関数
calc_NDVI <- function(img, r, n) {
  red <- img[[r]]
  nir <- img[[n]]
  return ((red - nir) / (red + nir))
}

#NDVIを計算する。SEQUOIAバンドNIR = 3, red = 2.
ndvi <- calc_NDVI(masked,3, 2)
plot(ndvi, col = rev(terrain.colors(10)), main = 'SEQUOIA-NDVI')

#ズームして確認する
#+ eval=F
zoom(ndvi)
#+ eval=T

#++++++++++++++++++
#' ###活性状況を色々な方法で把握する
#++++++++++++++++++
#'
#' ####NDVIのヒストグラムを表示してみる
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab="Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))


#' ####活性状況が良くない場所を抽出してみる
veg <- calc(ndvi, function(x){x[x > 0.6] <- NA; return(x)})
plot(veg, main = 'Veg cover')

#' ####活性状況を5段階に分類してみる
vegc <- reclassify(ndvi, c(-Inf,0.4,1, 0.4,0.5,2, 0.5,0.6,3, 0.6,0.7,4, 0.7,Inf, 5))
plot(vegc,col = rev(terrain.colors(5)),breaks=0:5, main = 'NDVI based thresholding')

#' ####メッシュでNDVIを集計してみる

#メッシュを作成して回転させる
#cpを中心にラジアンrだけsfobjを回転する関数
rotate_sf<-function(sfobj,cp,r){
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  rot_sf<-(sfobj-cp)*rot(-r)+cp
  return(rot_sf)
}

mesh <- st_make_grid(n=c(38,12), offset = c(24787.8,2755.4), cellsize = c(0.5,0.5))
cp<-st_sfc(st_point(c(24787.8,2755.4)))
mesh<-rotate_sf(mesh,cp,pi*50/180)
plot(mesh,axes=T)

#メッシュごとのNDVIの平均をデータフレームで取得
mesh_sp<-as(mesh,"Spatial") #spクラスに変換
df<-raster::extract(ndvi,mesh_sp,fun=mean,df=TRUE)

#メッシュの属性に結合
mesh<-st_sf(mesh) #属性が付けられるようにsfクラスに変換
mesh<-mesh %>% mutate(ndvi=df$layer)
plot(mesh)

#メッシュのndvi値を表示
mesh<-mesh %>% mutate(id=1:nrow(.)) #メッシュにidを追加
mesh_df<-mesh %>% st_set_geometry(NULL) #データフレームに変換
hist(mesh_df$ndvi) #ヒストグラム表示
mesh_df %>% filter(ndvi<0.5) #活性が良くないメッシュIDを表示
mesh %>% filter(ndvi<0.5) %>% dplyr::select(ndvi) %>% plot()

#++++++++++++++++++
#' ###結果を書き出す
#++++++++++++++++++
#'
#ggplot2で背景画像とndviのメッシュを重ねる
sequoia_df<-as.data.frame(croped[[4]],xy=TRUE) #ラスタをデータフレームに変換
head(sequoia_df)
#メッシュの中心座標を属性に追加
cent_xy<-mesh %>% st_centroid() %>% st_coordinates()
mesh<-cbind(mesh,cent_xy)
g<-ggplot() +
  geom_raster(data=sequoia_df, aes(x = x, y = y,alpha = rededge))+
  scale_alpha(name = "", range = c(0.6, 0), guide = F)+
  geom_sf(data = mesh %>% filter(ndvi>0.5),aes(fill = ndvi,alpha=0.3))+
  scale_fill_gradientn(colors=c("white","yellow","red"))+
  geom_text_repel(data = mesh %>% filter(id %in% c(1,38,191,343,456)),aes(x=X,y=Y,label = id),nudge_y=10)+
  annotate("text",x=24798,y=2756,label="数字は区画ID")
  
plot(g)

#画像をjpgに保存する
ggsave("mydata/ndvi.jpg",g)
#マスク処理したラスタデータを書き出す
writeRaster(masked,filename = "mydata/masked_sequoia.tif", format = "GTiff", overwrite = TRUE)
#ndviメッシュポリゴンを書き出す
st_write(mesh, "mydata/ndvi_mesh.shp",layer_options = "ENCODING=UTF-8",delete_layer = TRUE)

#++++++++++++++++++++++++++++
#' ##マルチスペクトル画像から土地被覆を分類する 
#++++++++++++++++++++++++++++
#'
#++++++++++++++++++
#' ###前処理して、情報を確認する
#++++++++++++++++++
#'
rededge <- stack('data/rededge.tif')
rededge <- subset(rededge,1:5) #アルファバンドを除外
names(rededge) <- c('blue','green','red','nir','rededge')
crs(rededge)<-CRS("+init=epsg:2451") #CRSを設定

#解像度変更
rededge0.5<- rededge
res(rededge0.5)<-0.5
rededge <- resample(rededge, rededge0.5, method='bilinear')

#切り取り
e<-extent(24900,24995,2710,2825)
rededge<-crop(rededge,e)

#書き出し
writeRaster(rededge,filename = "mydata/cropped-rededge.tif", format = "GTiff", overwrite = TRUE)

#情報確認
crs(rededge)
ncell(rededge)
dim(rededge)
res(rededge)
nlayers(rededge)

#++++++++++++++++++
#' ###色々な方法で表示する
#++++++++++++++++++
#'
#' blueバンド
plot(rededge,1,col=gray.colors(30))
#' TrueColor
plotRGB(rededge, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "RedEdge True Color Composite")
#' FalseColor
plotRGB(rededge, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge False Color Composite")
#' NaturalColor
plotRGB(rededge, r = 3, g = 4, b = 2, axes = TRUE, stretch = "lin", main = "RedEdge Natural Color Composite")

#++++++++++++++++++
#' ###データを分析する
#++++++++++++++++++
#'
#' #### バンド間の相関を見てみる
#各バンドを抽出し、16bitの値から0～1の反射率に変換
b_red<-rededge[[3]]/65536
b_nir<-rededge[[4]]/65536
b_rededge<-rededge[[5]]/65536

#相関グラフ
# 赤域と近赤外域
red_nir<-stack(b_red,b_nir)
pairs(red_nir, main = "Scatterplot between Red and NIR")
# 赤域とレッドエッジ域
red_rededge<-stack(b_red,b_rededge)
pairs(red_rededge, main = "Scatterplot between Red and Rededge")


#' #### バンドの反射率を土地被覆ごとにランダム点で取得してみる
#土地被覆の読み込み
landcover<-read_sf("data/landcover.shp",options=c("ENCODING=CP932"))
plot(landcover)

#サブクラスの一覧をベクトルで取得
subcl<-landcover %>% 
  st_set_geometry(NULL) %>% #sfからデータフレームに変換
  distinct(subclass) #重複を削除
subcl

#サブクラスごとにランダム点を500ポイント作成
subclass_sampling<-function(x){
  filter(landcover,subclass==x) %>% 
    st_sample(500)
}
pntlist<-subcl$subclass %>% map(subclass_sampling) 
pnt<-pntlist %>% reduce(c)

#ポイントの属性に土地被覆を追加
pnt<-st_intersection(landcover,pnt)
plot(pnt,pch=".",cex=3)

#各ポイントでバンド値を抽出
band_df <- raster::extract(rededge, as(pnt,"Spatial"),df=TRUE)
#ポイントの属性にバンド値を追加
pnt<-pnt %>% bind_cols(band_df)

#' #### 地物とバンドの反射率の関係を見てみる
#土地被覆(subclass)ごとのバンドの平均値を計算
df_class_mean<-pnt %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(-ID,-class) %>% 
  group_by(subclass) %>% 
  summarise_all(mean) %>% 
  print

#縦長なデータに変換
df_class_mean<-df_class_mean %>% 
  gather(key=Bands,value=Reflectance,-subclass) %>% 
  print

#反射率に変換
df_class_mean <- df_class_mean %>% mutate(Reflectance=Reflectance/65536)

#Bandsをfactorに変換（グラフ表示の際にBandsの順番を並べるため）
df_class_mean <- df_class_mean %>% mutate(Bands= factor(Bands, levels = c("blue", "green", "red", "rededge", "nir")))

# 各subclassのバンドの平均反射率をグラフ表示
# 結果を見るとレッドエッジの校正がおかしいかも（？）
classcolor <- c("コンクリート"="gray", "その他"="black","建物" = "white","高木"="darkgreen","砂地"="brown","芝"="lightgreen","水田"="lightblue", "川"="blue","草地"="orange","低木"="green","裸地"="yellow")
df_class_mean %>% 
  ggplot(aes(x=Bands ,y=Reflectance,col=subclass))+
  geom_line(aes(group = subclass),size=1)+
  geom_point()+
  labs(title = "Spectral Profile")+
  scale_color_manual(values=classcolor)
  

#++++++++++++++++++
#' ###教師なし分類をする(5バンド使用)
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
#' ###教師あり分類をする
#++++++++++++++++++
#'
#' トレーニングデータを作成する
area1 = st_polygon(list(rbind(c(24910,2800), c(24930,2800), c(24930,2820), c(24910,2820), c(24910,2800))))
area2 = st_polygon(list(rbind(c(24950,2800), c(24970,2800), c(24970,2820), c(24950,2820), c(24950,2800))))
area3 = st_polygon(list(rbind(c(24950,2770), c(24970,2770), c(24970,2790), c(24950,2790), c(24950,2770))))
area4 = st_polygon(list(rbind(c(24910,2770), c(24930,2770), c(24930,2790), c(24910,2790), c(24910,2770))))

training_area<-st_sfc(area1,area2,area3,area4,crs=2451)

#トレーニングエリアの確認
plot(landcover %>% st_geometry(),axes=T)
plot(training_area,add=T)

#トレーニングエリアのデータを抽出
training_pnt<-st_intersection(pnt,training_area)
# データフレームに変換
training_df<-training_pnt %>% st_set_geometry(NULL) %>% dplyr::select(-class,-ID)
head(training_df)

#' ####決定木で分類する
#モデルの作成
model <- rpart(as.factor(subclass)~., data=training_df, method="class")
plot(model, uniform=TRUE, main="Classification Tree",margin=0.05)
text(model, cex = 0.7)

#モデルから全域を予測
pr <- predict(rededge, model, type="class")

#予測結果をプロット
df<-as.data.frame(pr,xy=TRUE)

classcolor <- c("コンクリート"="gray", "その他"="black","建物" = "white","高木"="darkgreen","砂地"="brown","芝"="lightgreen","水田"="lightblue", "川"="blue","草地"="orange","低木"="green","裸地"="yellow")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)+ coord_equal(ratio = 1) 

#予測結果を書き出す
writeRaster(pr,filename = "mydata/RP_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)


#' ####ランダムフォレストで分類する
#モデルの作成
model <- randomForest(as.factor(subclass)~.,data=training_df)

#モデルから全域を予測
pr <- predict(rededge, model)

#予測結果をプロット
df<-as.data.frame(pr,xy=TRUE)
classcolor <- c("コンクリート"="gray", "その他"="black","建物" = "white","高木"="darkgreen","砂地"="brown","芝"="lightgreen","水田"="lightblue", "川"="blue","草地"="orange","低木"="green","裸地"="yellow")
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)+ coord_equal(ratio = 1) 

#予測結果を書き出す
writeRaster(pr,filename = "mydata/RF_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)


#' ####SVMで分類する
#モデルの作成
model <- ksvm(as.factor(subclass)~.,data=training_df)

#モデルから全域を予測
pr <- predict(rededge, model)

#予測結果をプロット
classcolor <- c("コンクリート"="gray", "その他"="black","建物" = "white","高木"="darkgreen","砂地"="brown","芝"="lightgreen","水田"="lightblue", "川"="blue","草地"="orange","低木"="green","裸地"="yellow")
df<-as.data.frame(pr,xy=TRUE)
ggplot()+
  geom_raster(data=df, aes(x = x, y = y,fill = layer_value))+
  scale_fill_manual(values=classcolor)+ coord_equal(ratio = 1) 

#予測結果を書き出す
writeRaster(pr,filename = "mydata/SVM_predicted_landcover.tif", format = "GTiff", overwrite = TRUE)

