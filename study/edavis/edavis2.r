# R Code
# Explorative Datenanalyse und Visualisierung
# Uebung 2, Stefan Theuﬂl 0352689

# R Objekte erzeugen

h2<-c(1592,533,229,71,26,11,7,1)
h3<-c(2398,685,242,82,24,13,9,1,2,1,1,1,1,1,1)
h4<-c(3696,958,328,138,39,20,6,2)
h5<-c(532,234,100,53,33,17,6,8,4,1)
h6<-c(397,69,26,2)

gen.x<-function(vec){
x<-c(NULL)
for(i in 0:(length(vec)-1))
x<-c(x,rep(i,vec[i+1]))
x
}

x2<-gen.x(h2)
x3<-gen.x(h3)
x4<-gen.x(h4)
x5<-gen.x(h5)
x6<-gen.x(h6)

boxplot(x2,x3,x4,x5,x6)

median(x2)
median(x3)
median(x4)
median(x5)
median(x6)

IQR(x2)
IQR(x3)
IQR(x4)
IQR(x5)
IQR(x6)


plot()
