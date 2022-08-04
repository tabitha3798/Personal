
#######################################################################
########## 능동적 차원의 행동 의도 검사 응답 데이터 요인분석 ##########
#######################################################################



#######################################################################
########## 검사데이터 불러오기 & 전처리 ###############################
#######################################################################

##데이터 읽기 
library(readxl)
data.all = read_excel("C:/Users/kk/Dropbox/Lecture/MathEM(2021f)/FinalProject/ResponseData.xlsx",sheet ="설문지 응답 시트1") 
data.all$그룹=ifelse(data.all$그룹=='고등학교 2학년',1,0) #그룹변수 : 1(고등학교2학년), 0(중학교1학년)
data=data.all[,4:34] #그룹var & 30문항 응답만 남김 

#무의미 응답 제거 
obs.out1=which(apply(data[2:31],1,var)==0) #한 번호로 찍은 학생들  
obs.out2=which((data[,'A1']-(6-data[,'G1'])>3) | (data[,'A1']-(6-data[,'G1']) < (-3))) #A1, G1 유사문항 응답 
obs.out3=which((data[,'B1']-data[,'E2']>3) | (data[,'B1']-data[,'E2'] < (-3))) #B1, E2 유사문항 응답 
obs.out=c(obs.out1, obs.out2, obs.out3)
data=data[-obs.out,]

#reverse문항 응답 뒤집기(데이터를 읽어온 후 반드시 홀수번 시행!) 
reverse.item = c('A2','B4','C1','D2','D3','F1','F2','G1','G3','G4','G5','G6')
for(item in reverse.item){data[,item] = 6-data[,item]}


########################################################################
########## 문항별 기본 통계량 ##########################################
########################################################################

#평균 
item.mean=apply(data[2:31],2,mean)
item.mean
#분산 
item.var=apply(data[2:31],2,var)
item.var
#분포 
k=1  #k=1, 2,3대해서 한번씩 그려주면 전체 그래프 
par(mfrow=c(2,5))
for(i in (10*k-8):(10*k+1)){
  hist(data[[i]], breaks=c(0,1,2,3,4,5), main=paste("Histogram of",colnames(data)[i]), xlab="student response")
}

par(mfrow=c(1,1))

########################################################################
########## EFA 수행  ###################################################
########################################################################

library(psych)
data1=data[,2:31] #FA를 위해 문항응답부분만 추출(그룹변수 제외)

##EFA 적절성 검증
#KMO test 
KMO(cor(data1, use = "complete.obs")) # 0.83 : 적절성 만족 
#Bartlett's test of sphericity
library(EFAtools)
BARTLETT(cor(data1, use = "complete.obs"), N=182) #p<0.001 : significant


##요인 개수 결정 
#Scree Method 
pout=fa.parallel(data1) #제안 요인수 = 4 : 기존 검사 구성과 일치 
#VSS and MAP
vout=VSS(data1) 
vout  #제안 요인수 = 1,4(VSS), 3(MAP : 기존 검사 구성과 일치)

##요인 추출 및 회전 
library(GPArotation)
#4개 요인 / ML with Cov-Mat / Orthogonal 회전 
MLout_4fac_var<-fa(r=cov(data1, use = "complete.obs"),nfactors=4,n.obs = 182,fm="ml",rotate="varimax", covar=TRUE)
fa.diagram(MLout_4fac_var, cut=0.2, digits=2)
#4개 요인 / PA with Cov-Mat / Orthogonal 회전 
PAout_4fac_var<-fa(r=cor(data1, use = "complete.obs"),nfactors=4,n.obs = 182,fm="pa",rotate="varimax", covar=TRUE)
fa.diagram(PAout_4fac_var, cut=0.2, digits=2)
#4개 요인 / ML with Cov-Mat / Oblique 회전 
MLout_4fac_obl<-fa(r=cov(data1, use = "complete.obs"),nfactors=4,n.obs = 182,fm="ml",rotate="oblimin", covar=TRUE)
fa.diagram(MLout_4fac_obl, cut=0.2, digits=2)
#4개 요인 / PA with Cov-Mat / Oblique 회전 
PAout_4fac_obl<-fa(r=cov(data1, use = "complete.obs"),nfactors=4,n.obs = 182,fm="pa",rotate="oblimin", covar=TRUE)
fa.diagram(PAout_4fac_obl, cut=0.2, digits=2)


#3개 요인 / ML with Cov-Mat / Orthogonal 회전 
MLout_3fac_var<-fa(r=cov(data1, use = "complete.obs"),nfactors=3,n.obs = 182,fm="ml",rotate="varimax", covar=TRUE)
fa.diagram(MLout_3fac_var, cut=0.2, digits=2)
#3개 요인 / PA with Cov-Mat / Orthogonal 회전 
PAout_3fac_var<-fa(r=cor(data1, use = "complete.obs"),nfactors=3,n.obs = 182,fm="pa",rotate="varimax", covar=TRUE)
fa.diagram(PAout_3fac_var, cut=0.2, digits=2)
#3개 요인 / ML with Cov-Mat / Oblique 회전 
MLout_3fac_obl<-fa(r=cov(data1, use = "complete.obs"),nfactors=3,n.obs = 182,fm="ml",rotate="oblimin", covar=TRUE)
fa.diagram(MLout_3fac_obl, cut=0.2, digits=2)
#3개 요인 / PA with Cov-Mat / Oblique 회전 
PAout_3fac_obl<-fa(r=cov(data1, use = "complete.obs"),nfactors=3,n.obs = 182,fm="pa",rotate="oblimin", covar=TRUE)
fa.diagram(PAout_3fac_obl, cut=0.2, digits=2)


##모델 제안 : 문서 참조 



########################################################################
########## 신뢰도 분석 수행 ############################################
########################################################################

## 전체 검사 Cronbach's alpha 
library(psych)
alpha(data1) # C-alpha = 0.84
alpha(data1[,-c(7,13,29)]) #B4, D3, G6 삭제 후  C-alpha = 0.87

##요인 별 문항데이터 Cronbach's alpha 
#기존 4요인 모델 
f1.item = c('C1','C2','D4','E1','E3','F5')
f2.item = c('A1','B3','D1','F1','G1','G2','G3')
f3.item = c('A2','B2','C3','D2','E4','F4','G4')
f4.item = c('A3','B1','E2','F2','F3','G5','G7')
alpha(data1[,f1.item])  # 0.52 : 각 문항 제거했을 때 c.alpha값 감소 
alpha(data1[,f2.item])  # 0.66 : 각 문항 제거했을 때 c.alpha값 감소
alpha(data1[,f3.item])  # 0.72 : 'B2'를 제거했을 때 c.alpha값 증가 
alpha(data1[,f4.item])  # 0.72 : 'F2'를 제거했을 때 c.alpha 증가 

#수정된 4요인 모델 요인1 문항 Cronbach's alpha
f1.item = c('C1','C2','D4','E1','E3','F5') 
alpha(data1[,f1.item])  # 0.55

#3요인 모델 
f1.item = c('B1', 'G7', 'B2', 'E4', 'E1', 'E2','A3', 'B3', 'F3', 'G5')
f2.item = c('C3', 'F4', 'F5', 'G4', 'D2', 'D1')
f3.item = c('G3', 'F1', 'F2', 'G1', 'D4', 'A1', 'A2', 'C1')
alpha(data[,f1.item])  # 0.84 : 'G5'를 제거했을 때 c.alpha값 증가
alpha(data[,f2.item])  # 0.74 : 각 문항 제거했을 때 c.alpha값 감소
alpha(data[,f3.item])  # 0.68 : 'C1'을 제거했을 때 C.alpha값 증가



########################################################################
########## CFA 수행 ####################################################
########################################################################

library(lavaan)
library(lavaanPlot)

##4요인 모델 기본형 분석 
model1.0 <- ' yt =~ C1+C2+D4+E1+E3+F5
              mi =~ A1+B3+D1+F1+G1+G2+G3
              tg =~ A2+B2+C3+D2+E4+F4+G4
              dc =~ A3+B1+E2+F2+F3+G5+G7'

fit1.0 <- cfa(model1.0, data = data1)
summary(fit1.0, fit.measures = TRUE) #모델 핏 확인 
standardizedSolution(fit1.0)

labels = list( yt= "융통성", mi = "몰입성", tg = "탐구성", dc="독창성")
lavaanPlot(model = fit1.0, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)


##3요인 모델 기본형 분석 
model2.0 <- ' sh =~ B1+G7+B2+E4+E1+E2+A3+B3+F3+G5
              sw =~ C3+F4+F5+G4+D2+D1
              zz =~ G3+F1+F2+G1+D4+A1+A2+C1'

fit2.0 <- cfa(model2.0, data = data1)
summary(fit2.0, fit.measures = TRUE) #모델 핏 확인 
standardizedSolution(fit2.0)

labels = list( sh= "수학적확장성", sw = "수학적완결성", zz = "자발적지속성")
lavaanPlot(model = fit2.0, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)


##3요인 개선 모델1 분석 
model2.1 <- ' sh =~ B1+G7+B2+E4+E1+E2+A3+B3+F3+G5
              sw =~ C3+F4+F5+G4+D2+D1+A2+F2
              zz =~ G3+F1+F2+G1+A1+A2+D1+G4'

fit2.1 <- cfa(model2.1, data = data1)
summary(fit2.1, fit.measures = TRUE) #모델 핏 확인 -> 기본형보다 좋아짐!
standardizedSolution(fit2.1)
lavaanPlot(model = fit2.1, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs=TRUE)

##3요인 개선 모델2 분석 
model2.2 <- ' sh =~ B1+G7+B2+E4+E1+E2+A3+B3+F3+G5
              sw =~ C3+F4+F5+G4+D2+D1+A2+F2
              zz =~ G3+F1+F2+G1+A1+A2+D1+G4
              G5~~D2'

fit2.2 <- cfa(model2.2, data = data1)
summary(fit2.2, fit.measures = TRUE) #모델 핏 확인 -> 개선 모델1이 더 좋음
standardizedSolution(fit2.2)
lavaanPlot(model = fit2.2, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs=TRUE)


## 최종 모델 제안 : 3요인 개선 모델1



########################################################################
########## 그룹간 비교 #################################################
########################################################################

#그룹간 비교 분석: 
#1. 중학교vs고등학교 (data에는 학교그룹변수가 포함되어있으므로 data를 활용하여 분석)
#2. D4문항 응답을 바탕으로 발전가능성에 대해 매우긍정적/긍정적/보통/부정적/매우부정적 그룹간 비교 

## 문항 응답 분석 
#요건 간단하게 그룹 간 총점 분석정도만 하믄 되지 않을까요? 아니면 궁금한 문항 평균 1개정도하면 될 것 같아요^^ 

## factor score 분석 
#lavPredict(fit2.1) 
#위 함수 사용하면 observation순서대로 factor score 나온답니닷! 
#요기에 그룹변수 열을 붙혀서 그룹 간 세 factor score 평균으로 비교하면 될듯 합니다 

## 그룹변수(원인) 추가해서 sem? 
#요거는 교수님 주신 R파일 처럼 최종 모델에 인과관계 추가해서 보시면 됩니다! 
#그룹변수 -> 세 요인 뻗어나가는 화살표 추가 
