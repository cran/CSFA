## ----load,echo=FALSE,warning=FALSE,message=FALSE,results='hide', eval=FALSE----
#  require(viridis)
#  library(CSFA)
#  data("dataSIM",package="CSFA")

## ----data_heatmap,fig.keep="high", eval=FALSE, echo=FALSE, warning="FALSE",message=FALSE,fig.pos='H',dev='png',fig.cap="Heatmap of Reference and Query Matrix"----
#  
#  par(fig=c(0,0.25,0,1),xpd=TRUE)
#  d <- dataSIM[,c(1:6)]
#  image(c(1:dim(d)[2]),c(1:dim(d)[1]),t(d),xlab="condition",ylab="genes",col=viridis(75))
#  par(fig=c(0.25,1,0,1), new=TRUE,xpd=TRUE)
#  d <- dataSIM[,-c(1:6)]
#  #d <- dataMFA
#  image(c(1:dim(d)[2]),c(1:dim(d)[1]),t(d),xlab="condition",ylab="genes",col=viridis(75),axes=FALSE)

## ----packageload,echo=TRUE,eval=FALSE,warning=FALSE,message=FALSE--------
#  library(CSFA)
#  data("dataSIM",package="CSFA")
#  	
#  refMat <- dataSIM[,c(1:6)]
#  querMat <- dataSIM[,-c(1:6)]

## ----ZG,echo=TRUE,eval=FALSE,cache=FALSE,fig.keep="high",fig.pos='H',out.width="11cm",fig.align='center',fig.cap="CSanalysis Graphs for CSzhang"----
#  out_ZG <- CSanalysis(refMat,querMat,"CSzhang",plot.type="sweave")

## ----MFA_hidden,echo=FALSE,eval=FALSE,cache=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSmfa"----
#  out_MFA <- CSanalysis(refMat,querMat,"CSmfa",plot.type="sweave",which=c(),
#  		profile.type="cmpd",gene.thresP=2.3,gene.thresN=-2.3,component.plot=1,
#  		column.interest=c(21,22,23))

## ----MFA,echo=TRUE,eval=FALSE,cache=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSmfa"----
#  out_MFA <- CSanalysis(refMat,querMat,"CSmfa",plot.type="sweave",which=c(2,3,4,7),
#  		profile.type="cmpd",gene.thresP=2.3,gene.thresN=-2.3,component.plot=1,
#  		column.interest=c(21,22,23))

## ----MFA2,echo=TRUE,eval=FALSE,cache=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSmfa"----
#  out_MFA <- CSanalysis(refMat,querMat,"CSmfa",plot.type="sweave",which=c(5),
#  		component.plot=1,column.interest=c(1,2,3),result.available=out_MFA)

## ----MFA3,echo=TRUE,eval=FALSE,cache=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSfma"----
#  out_MFA <- CSanalysis(refMat,querMat,"CSmfa",plot.type="sweave",which=c(7),
#  		profile.type="gene",component.plot=1,column.interest=c(1,2,3),
#  		row.interest=c(846,871,4,6),result.available=out_MFA)

## ----FABIA_hidden,echo=FALSE,eval=FALSE,cache=FALSE,message=FALSE,results='hide',fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSfabia"----
#  color.columns <- rep("black",dim(dataSIM)[2])
#  color.columns[1:6] <- "blue"
#  color.columns[c(29,30,31)] <- "red"
#  	
#  set.seed(8956)
#  out_FABIA <- CSanalysis(refMat,querMat,"CSfabia",plot.type="sweave",which=c(),
#  		color.columns=color.columns,
#  		legend.names=c("References","SP Connected"),
#  		legend.cols=c("blue","red"), component.plot=c(1,2),
#  		gene.thresP=2,gene.thresN=-2)

## ----FABIA,echo=TRUE,eval=FALSE,cache=FALSE,message=FALSE,results='hide',fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSanalysis Graphs for CSfabia"----
#  color.columns <- rep("black",dim(dataSIM)[2])
#  color.columns[1:6] <- "blue"
#  color.columns[c(29,30,31)] <- "red"
#  	
#  set.seed(8956)
#  out_FABIA <- CSanalysis(refMat,querMat,"CSfabia",plot.type="sweave",which=c(2,5),
#  		color.columns=color.columns,
#  		legend.names=c("References","SP Connected"),
#  		legend.cols=c("blue","red"), component.plot=c(1,2),
#  		gene.thresP=2,gene.thresN=-2)

## ----CSpermute_compute,eval=FALSE,echo=TRUE,cache=FALSE------------------
#  out_MFA <- CSpermute(refMat,querMat,CSresult=out_MFA,B=250,method.adjust="BH",
#  		which=c(),verbose=FALSE,MultiCores=TRUE)
#  out_ZG <- CSpermute(refMat,querMat,CSresult=out_ZG,B=250,method.adjust="BH",
#  		which=c(),verbose=FALSE,MultiCores=TRUE)
#  

## ----cheat1,echo=FALSE, eval=FALSE---------------------------------------
#  	
#  # out_MFA@extra <- list()
#  # save(list="out_MFA",file="vignettes/perm_data/out_MFA.RData")
#  #save(list="out_ZG",file="vignettes/perm_data/out_ZG.RData")
#  
#  	
#  extra_temp <- out_MFA@extra	
#  	
#  load("perm_data/out_MFA.RData")
#  load("perm_data/out_ZG.RData")
#  
#  out_MFA@extra <- extra_temp
#  

## ----CSpermute_compute2,echo=FALSE,cache=FALSE, eval=FALSE---------------
#  head(out_MFA@CS[[1]]$CS.query)
#  
#  ##       CLoadings   CLpvalues CLpvalues.adjusted CRankScores   CRpvalues
#  ## cWP-1 0.5858096 0.003984064         0.03813318   0.6800244 0.003984064
#  ## cWP-2 0.5771857 0.003984064         0.03813318   0.6690638 0.003984064
#  ## cWP-3 0.5739775 0.003984064         0.03813318   0.6649862 0.003984064
#  ## cWP-4 0.5867910 0.003984064         0.03813318   0.6812717 0.003984064
#  ## cWP-5 0.5770335 0.003984064         0.03813318   0.6688703 0.003984064
#  ## cWP-6 0.5852554 0.003984064         0.03813318   0.6793200 0.003984064
#  ##       CRpvalues.adjusted CLrank CLabsrank CRrank CRabsrank
#  ## cWP-1         0.03813318     14        24     14        24
#  ## cWP-2         0.03813318     17        27     17        27
#  ## cWP-3         0.03813318     19        29     19        29
#  ## cWP-4         0.03813318     13        23     13        23
#  ## cWP-5         0.03813318     18        28     18        28
#  ## cWP-6         0.03813318     15        25     15        25
#  
#  head(out_ZG@CS$CS.query)
#  
#  ##         ZGscore     pvalues pvalues.adjusted ZGrank ZGabsrank
#  ## cWP-1 0.5590061 0.003984064       0.03707393     14        24
#  ## cWP-2 0.5473340 0.003984064       0.03707393     20        30
#  ## cWP-3 0.5621351 0.003984064       0.03707393     13        23
#  ## cWP-4 0.5649765 0.003984064       0.03707393     11        21
#  ## cWP-5 0.5578107 0.003984064       0.03707393     16        26
#  ## cWP-6 0.5468158 0.003984064       0.03707393     21        31

## ----CSpermuteplots,echo=TRUE,eval=FALSE,cache=FALSE,message=FALSE,results='hide',fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSpermute graphs for MFA result"----
#  out_MFA <- CSpermute(refMat,querMat,out_MFA,B=250,method.adjust="BH",
#  		which=c(1,2),cmpd.hist=c(23,99),plot.type="sweave")

## ----CSpermuteplots2,echo=TRUE,eval=FALSE,cache=FALSE,message=FALSE,results='hide',fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="CSpermute graphs for MFA result"----
#  out_MFA <- CSpermute(refMat,querMat,out_MFA,B=250,method.adjust="BH",
#  		which=c(3,4),cmpd.hist=c(23,99),plot.type="sweave")

## ----CScompare_hidden,echo=FALSE,eval=FALSE,cache=FALSE,message=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="Compare CSresults"----
#  	comp_ZG_MFA <- CScompare(out_ZG,out_MFA,component2.plot=1,plot.type="sweave",which=c())
#  	
#  	comp_MFA_FABIA <- CScompare(out_MFA,out_FABIA,component1.plot=1,component2.plot=1,
#  			gene.thresP=c(2,2),gene.thresN=c(-2,-2),plot.type="sweave",which=c())
#  	
#  	comp_ZG_MFA[[1]]	
#  	comp_MFA_FABIA[[1]]

## ----CScompare,echo=FALSE,eval=FALSE,cache=FALSE,message=FALSE,fig.keep="high",fig.show='hold',fig.pos='H',out.width='9cm',out.height='10cm',fig.align='left',fig.cap="Compare CSresults"----
#  comp_ZG_MFA <- CScompare(out_ZG,out_MFA,component2.plot=1,plot.type="sweave")
#  	
#  comp_MFA_FABIA <- CScompare(out_MFA,out_FABIA,component1.plot=1,component2.plot=1,
#  			gene.thresP=c(2,2),gene.thresN=c(-2,-2),plot.type="sweave")
#  	
#  comp_ZG_MFA[[1]]	
#  
#  ## $scores
#  ##                      CLoadings CRankScores GeneScores
#  ## Correlation_Pearson  0.9965602   0.9982107         NA
#  ## Correlation_Spearman 0.9627536   0.7273489         NA
#  ##
#  ## $pvalues
#  ##                      CLoadings CRankScores
#  ## Correlation_Pearson  0.9532248   0.9113716
#  ## Correlation_Spearman 0.8737479   0.7128987
#  ##
#  ## $adj.pvalues
#  ##                      CLoadings CRankScores
#  ## Correlation_Pearson  0.9509475    0.946321
#  ## Correlation_Spearman 0.5533918    0.542450	
#  		
#  comp_MFA_FABIA[[1]]
#  
#  ## $scores
#  ##                       CLoadings CRankScores GeneScores
#  ## Correlation_Pearson  -0.9573119   0.9801271 -0.7752930
#  ## Correlation_Spearman -0.5693159   0.5550448 -0.7996527
#  ##
#  ## $pvalues
#  ## NULL
#  ##
#  ## $adj.p	

## ----CScompare2,echo=FALSE, eval=FALSE-----------------------------------
#  comp_ZG_MFA[[2]]	
#  
#  ## $pvalues
#  ##                 Result1.Sign Result1.NotSign
#  ## Result2.Sign              35               0
#  ## Result2.NotSign            9             291
#  ##
#  ## $adj.pvalues
#  ##                 Result1.Sign Result1.NotSign
#  ## Result2.Sign              35               0
#  ## Result2.NotSign            1             299		

