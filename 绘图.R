####1 文氏图
library("VennDiagram")
A = 50
B = 35
C = 5
AB = 3
BC = 1
AC = 2
ABC = 0.5
require(VennDiagram)
draw.triple.venn(area1=A, area2=B, area3=C
                 ,n12=AB, n23=BC, n13=AC, n123=ABC
                 ,category = c('A','B','C')
                 ,col=c('white','white','white'),fill=c('gray','black','lightblue')
                 ,cat.col=c('gray','black','lightblue')
                 ,cex = c(2.5,2,2.5,1,1,1,2))

####2 饼图
x = c(50,35,5) 
piepercent = paste(round(100*x/sum(x), 2), "%")
pie(x, labels = piepercent, main = "饼图", col = c("gray","lightblue","white"))
legend("topright", c("A","B","C"), cex = 1, fill = c("gray","lightblue","white"))


####3 直方图
x = rnorm(30)
hist(x, col = "lightblue", main = "直方图")

####4 条形图
par()
x = rexp(8)
barplot(axes = TRUE, sort(x))

####5 箱线图
A = c(20,34,21,34,39,34,32,23,25,27)
B = c(12,35,52,35,36,32,33,38,57,47,44,33,35,39,41)
boxplot(A, B, names = c("A","B"), col = c("gray", "lightblue"),width = c(1,2))

####6 折线图，散点图，平滑曲线图
x = rnorm(10)
fx = dnorm(x)
plot(x, fx, ylab = "频率", main = "散点图")
plot(sort(x), fx[order(x)], type = "b", main = "折线图")

sp = spline(x, fx, n = 1000)
plot(sp, type = "l", lwd = "3", main = "平滑曲线")

####7 qq图
x1 = rnorm(60)
qqnorm(x1)
x2 = runif(60)
qqnorm(x2)
x2 = rexp(60)
qqnorm(x3)

#------------------------一些改进------------------------------------------------

####指数分布图(分割绘图区域)
split.screen(c(1,2))                                            #分割绘图区域
split.screen(c(2,1),2)
rx=rexp(1000)
screen(1)                                                       #选定绘图区域
hist(rx,main = "指数分布直方图")
sx=seq(0,5,0.01)
dx=dexp(sx)
screen(3)
plot(sx,dx,type = "l",main="指数分布密度曲线")
text(x=4,y=0.5,expression(f(x)==1/e^x))                         #添加文本
screen(4)
px=pexp(sx)
plot(sx,px,type = "l",main = "指数分布分布曲线")
text(x=3.8,y=0.5,expression(F(x)==1-1/e^x))

####1 双坐标的折线图
plot(trees$Girth,trees$Height, axes = FALSE,                    #树子的数据             
     type = "l", col = "lightblue", 
     xlab = "", ylab = "", lwd = "2")
axis(1)                                                         #自己添加坐标轴
axis(2, col = "lightblue")
par(new = TRUE)
plot(trees$Girth,trees$Volume, axes = FALSE, 
     type = "l", col = "pink", 
     xlab = "", ylab = "", lwd = "2")
axis(4,col="pink")
mtext(c("Girth","Height","Volume"),side=c(1,2,4),line=3)
title("Trees")

####条形图
fdeaths                                                                     #男女吸烟死亡人数数据
mdeaths
md=c(mdeaths[1],mdeaths[2],mdeaths[3],mdeaths[4],mdeaths[5])
barplot(md)
fd=c(fdeaths[1],fdeaths[2],fdeaths[3],fdeaths[4],fdeaths[5])
data=data.frame(md,fd)
data=t(as.matrix(data))
month=c("Jan","Feb","March","April","May")
barplot(data,names.arg = month,axes=FALSE,col = c("lightblue","gray"))      #复合的条形图
axis(2)
title("吸烟死亡人数")
legend(5,3000,c("男性","女性"),col=c("yellow","lightblue"),fill=c("lightblue","gray"))

####3D图
x=seq(-5,5,0.1)
y=seq(-5,5,0.1)
f=function(x,y){
  (sin(pi*x)/(pi*x))*(sin(pi*y)/(pi*y))
}
z=outer(x,y,f)
persp(x,y,z,theta=30,phi = 30,expand = 0.5,col="lightgray")    #3D函数图像
title(main = expression(z == sinc(x,y)))                       #添加标题
contour(x, y, z, main = "Coutour Plot")                        #等高线图绘制
filled.contour(x,y,z)     
image(x, y, z)                                                #绘制热力图

#### 交互图的绘制
library("rgl")
demo(rgl)
?rgl
?plot3d
plot3d(x,y,z)

