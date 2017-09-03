rm(list=ls())
Bo = read.csv("~/Dropbox/R-work/ChinaIO/B.csv",header=FALSE)
Fo = read.csv("~/Dropbox/R-work/ChinaIO/F.csv",header=FALSE)
Xo = read.csv("~/Dropbox/R-work/ChinaIO/X.csv",header=FALSE)
Vo = read.csv("~/Dropbox/R-work/ChinaIO/V.csv",header=FALSE)
BRo = read.csv("~/Dropbox/R-work/ChinaIO/BR.csv",header=FALSE)
FRo = read.csv("~/Dropbox/R-work/ChinaIO/FR.csv",header=FALSE)
XXXo = read.csv("~/Dropbox/R-work/ChinaIO/XXX.csv",header=FALSE)

B = as.matrix(Bo,nrow=60,ncol=60)
F = as.matrix(Fo,nrow=60,ncol=18)
V = as.matrix(Vo,nrow=5,ncol=60)
X = as.matrix(Xo,nrow=60,ncol=1)
BR = as.matrix(BRo,nrow=30,ncol=60)
FR = as.matrix(FRo,nrow=30,ncol=18)
XXX = as.matrix(XXXo,nrow=1,ncol=60)

FF = F[,-c(5,8,13,17,18)]; VV = V[-5,]; # e=rep(1,60)
print( rowSums(B) + rowSums(FF) - X)
# print( B %*% e + FF %*% e[1:13] - X )
print( colSums(B) + colSums(BR) + colSums(VV) + XXX - X )
# print( e %*% B + e[1:30] %*% BR + e[1:4] %*% VV + t(XXX) - t(X) )

JJ = B[1:30,1:30]; CJ = B[1:30,31:60]; JC = B[31:60,1:30]; CC = B[31:60,31:60]
FJJ = F[1:30,1:3]; FJC = F[31:60,1:3]; FRJ = F[1:30,6]
FCJ = F[1:30,9:11]; FCC = F[31:60,9:11]; FRC = F[31:60,14]
XJ = X[1:30]; XC = X[31:60]
BJR = BR[1:30,1:30]; BCR = BR[1:30,31:60]
FJR = FR[1:30,1:3]; FCR = FR[1:30,9:11]
VJ = V[1:4,1:30]; VC = V[1:4,31:60]

par(font.axis=2,family="sans",ps=10)

# METI report 2/Mar/2012 (1)
barplot(rbind(XJ,XC),beside=T,names=1:30,legend=c("Japan","China"),args.legend=list(x=10,y=2e+09),main="産業別生産額")
JIP = XJ/sum(XJ); CIP = XC/sum(XC) # 構成比の計算
barplot(rbind(JIP,CIP),beside=T,names=1:30,legend=c("Japan","China"),args.legend=list(x=10,y=0.2),main="産業別生産額構成比")
# JIP1=sum(JIP[1:2]);JIP2=sum(JIP[3:24]);JIP3=sum(JIP[25:30])
# CIP1=sum(CIP[1:2]);CIP2=sum(CIP[3:24]);CIP3=sum(CIP[25:30])
# rect(1,-0.1,6,0,density=0,col="red")
# rect(6,-0.1,72,0,density=0,col="red")
# rect(72,-0.1,90,0,density=0,col="red")


# METI report 2/Mar/2012 (2)
JVR = colSums(VJ)/XJ  # 粗付加価値率の計算
CVR = colSums(VC)/XC
barplot(rbind(JVR,CVR),beside=T,names=1:30,legend=c("Japan","China"),main="産業別付加価値率")

JB = colSums(JJ)+colSums(JC)+colSums(BJR)
JR1 = colSums(JJ)/JB
JR2 = colSums(JC)/JB
JR3 = colSums(BJR)/JB
CB = colSums(CJ)+colSums(CC)+colSums(BCR)
CR1 = colSums(CC)/CB
CR2 = colSums(CJ)/CB
CR3 = colSums(BCR)/CB
# barplot(rbind(JR1,JR2,JR3),names=1:30,legend=c("Domestic","from China","from ROW"),main="日本の中間投入に占める自国財・中国財・他国財の比率",ylim=c(0,1.0))
# barplot(rbind(CR1,CR2,CR3),names=1:30,legend=c("Domestic","from Japan","from ROW"),main="中国の中間投入に占める自国財・日本財・他国財の比率",ylim=c(0,1.0))
barplot(rbind(JR1,CR1),beside=T,names=1:30,legend=c("日本","中国"),main="中間投入における自国財への依存度")
barplot(rbind(JR2,CR2),beside=T,names=1:30,legend=c("日本の中国への依存","中国の日本への依存"),main="日中の相互依存（中間投入）")


# METI report 2/Mar/2012 (3)
JWR = VJ[1,]/colSums(VJ)
CWR = VC[1,]/colSums(VC)
barplot(rbind(JWR,CWR),beside=T,names=1:30,legend=c("Japan","China"),main="祖付加価値総額に占める雇用者所得の比率",ylim=c(0,1.0))
JWR = VJ[2,]/colSums(VJ)
CWR = VC[2,]/colSums(VC)
barplot(rbind(JWR,CWR),beside=T,names=1:30,legend=c("Japan","China"),main="祖付加価値総額に占める営業余剰の比率",ylim=c(0,1.0))


# METI report 2/Mar/2012 (4)
barplot(t(FJJ),legend=c("民間消費","政府消費","投資"),names=1:30,col=c("cyan","blue","orange"),main="日本から日本への最終需要")
barplot(t(FJC),legend=c("民間消費","政府消費","投資"),names=1:30,col=c("cyan","blue","orange"),main="日本から中国への最終需要")
barplot(t(FCJ),legend=c("民間消費","政府消費","投資"),names=1:30,col=c("cyan","blue","orange"),main="中国から日本への最終需要")
barplot(t(FCC),legend=c("民間消費","政府消費","投資"),names=1:30,col=c("cyan","blue","orange"),main="中国から中国への最終需要")


# METI report 2/Mar/2012 (5)
barplot(rbind(rowSums(CJ),rowSums(FCJ)),beside=T,legend=c("中間需要","最終需要"),names=1:30,col=c("cyan","blue"),main="中国から日本の各産業への需要")
barplot(rbind(rowSums(JC),rowSums(FJC)),beside=T,legend=c("中間需要","最終需要"),names=1:30,col=c("cyan","blue"),main="日本から中国の各産業への需要")


# METI report 2/Mar/2012 (6)
A = matrix(0,nrow=60,ncol=60)     # 投入係数行列
for(i in 1:60) A[i,]=B[i,]/X; A[,60]=0     
IA = solve(diag(60)-A)            # diag() 単位行列を作る,  solve() 逆行列を求める

XFJ = IA %*% rbind(FJJ,FJC)       # 日本の国内最終需要がもたらす生産誘発額
XFRJ = IA %*% c(FRJ,rep(0,30))       #  ROWから日本への最終需要がもたらす生産誘発額
XFJ = cbind(XFJ,XFRJ)
XFC = IA %*% rbind(FCJ,FCC)       # 中国の国内最終需要がもたらす生産誘発額
XFRC = IA %*% c(rep(0,30),FRC)       #  ROWから中国への最終需要がもたらす生産誘発額
XFC = cbind(XFC,XFRC)

colnames(XFJ) = c("民間消費","政府消費","固定資本形成","ROWからの最終需要")
colnames(XFC) = colnames(XFJ)
TJ = rbind( colSums(XFJ),colSums(XFJ[1:30,]),colSums(XFJ[31:60,]) )
TC = rbind( colSums(XFC),colSums(XFC[1:30,]),colSums(XFC[31:60,]) )
rownames(TJ) = c("生産誘発額（合計）","日本内","中国内")
rownames(TC) = rownames(TJ)
print( t(TJ) ); print( t(TC) )
TJR=TJ/sum(XJ); print( t(TJR) ); print( rowSums(TJR[,1:3]) )
TCR=TC/sum(XC); print( t(TCR) ); print( rowSums(TCR[,1:3]) )

barplot(t(XFJ[1:30,]),names=1:30,legend=colnames(XFJ),main="日本の最終需要項目がもたらす日本国内産業の生産誘発額",args.legend=list(x=30,y=1.5e+09))
barplot(t(XFJ[31:60,]),names=1:30,legend=colnames(XFJ),main="日本の最終需要項目がもたらす中国国内産業の生産誘発額")
barplot(t(XFC[1:30,]),names=1:30,legend=colnames(XFC),main="中国の最終需要項目がもたらす日本国内産業の生産誘発額",args.legend=list(x=32,y=3.3e+07))
barplot(t(XFC[31:60,]),names=1:30,legend=colnames(XFC),main="中国の最終需要項目がもたらす中国国内産業の生産誘発額",args.legend=list(x=25,y=1e+09))

VR = c(JVR,CVR)
VFJ = XFJ * VR
VFC = XFC * VR

barplot(t(VFJ[1:30,]),names=1:30,legend=colnames(XFJ),main="日本の最終需要項目がもたらす日本国内産業の粗付加価値誘発額",args.legend=list(x=20,y=9e+08))
barplot(t(VFJ[31:60,]),names=1:30,legend=colnames(XFJ),main="日本の最終需要項目がもたらす中国国内産業の粗付加価値誘発額",args.legend=list(x=30,y=9.5e+06))
barplot(t(VFC[1:30,]),names=1:30,legend=colnames(XFC),main="中国の最終需要項目がもたらす日本国内産業の粗付加価値誘発額",args.legend=list(x=10,y=1.3e+07))
barplot(t(VFC[31:60,]),names=1:30,legend=colnames(XFC),main="中国の最終需要項目がもたらす中国国内産業の粗付加価値誘発額",args.legend=list(x=25,y=4.5e+08))

# write.csv(XFJ,file="~/Desktop/XFJ.csv")
# write.csv(VFJ,file="~/Desktop/VFJ.csv")
# write.csv(XFC,file="~/Desktop/XFC.csv")
# write.csv(VFC,file="~/Desktop/VFC.csv")
# write.csv(X,file="~/Desktop/Xt.csv")
# write.csv(t(V),file="~/Desktop/Vt.csv")
