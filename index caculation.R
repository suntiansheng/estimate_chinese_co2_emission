a1<- c(30,20,30)
a2<- c(0,20,60)
a3<- c(60,40,30)
q<- rbind(a1,a2,a3)
a4<- c(150,200,300)
Q<- diag(a4)
A<- q%*%solve(Q)
I<- diag(rep(1,3))
B1<- solve(I-A)
B<- B1-I


#1995
q1995<- read.csv('china2007q.csv',header = F)
Q1995<- read.csv('china2007daq.csv',header = F)
q<- as.matrix(q1995)
Q1995<- as.vector(t(Q1995))
Q<- diag(Q1995)
A<- q%*%solve(Q)
I<- diag(rep(1,42))
B1<- solve(I-A)
B<- B1-I
write.csv(B,file = 'complete consumption coefficient2007.csv')
real<- read.csv(file = 'real.csv',header = F)
real<- as.matrix(real)
max(B-real)
write.csv(B-real,file = 'Îó²îÏî.csv')
