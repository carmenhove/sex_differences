
#library(MatchIt) # Exact matching via propensity score
#cbc.out <- matchit(pregYN ~ Age, data = NHdata.cbc)
#cbc.matched <- match.data(cbc.out)

#crp.out <- matchit(pregYN ~ Age, data = NHdata.crp)
#crp.matched <- match.data(crp.out)

# Subsetting for CBC and CRP, where CBC = individuals who have truly complete blood count and CRP = individuals who have CRP value
#TSdata.wbc.1<-TSdata.1[!is.na(TSdata.1$WBC),-c(1,3:7)]
#TSdata.neu<- TSdata.1[!is.na(TSdata.1$NEU),-c(1:2,4:7)] 
#TSdata.lym<-TSdata.1[!is.na(TSdata.1$LYM),-c(1:3,5:7)]
#TSdata.eos<-TSdata.1[!is.na(TSdata.1$EOS),-c(1:6)]
#TSdata.mon<-TSdata.1[!is.na(TSdata.1$MON),-c(1:4,6:7)]
#TSdata.bas<-TSdata.1[!is.na(TSdata.1$BAS),-c(1:5,7)]

# Age-match samples
#TSwbc.out <- matchit(pregYN ~ Age, data = TSdata.wbc)
#TSwbc.matched <- match.data(TSwbc.out)
#TSneu.out <- matchit(pregYN ~ Age, data = TSdata.neu)
#TSneu.matched <- match.data(TSneu.out)
#TSlym.out <- matchit(pregYN ~ Age, data = TSdata.lym)
#TSlym.matched <- match.data(TSlym.out)
#TSmon.out <- matchit(pregYN ~ Age, data = TSdata.mon)
#TSmon.matched <- match.data(TSmon.out)
#TSeos.out <- matchit(pregYN ~ Age, data = TSdata.eos)
#TSeos.matched <- match.data(TSeos.out)
#TSbas.out <- matchit(pregYN ~ Age, data = TSdata.bas)
#TSbas.matched <- match.data(TSbas.out)
#TScrp.out <- matchit(pregYN ~ Age, data = TSdata.crp)
#TScrp.matched <- match.data(TScrp.out)

# pull back together in final age-matched dataset
#df1<-full_join(TScrp.matched, TSwbc.matched)
#df2<-full_join(TSneu.matched, TSlym.matched)
#df3<-full_join(TSmon.matched, TSbas.matched)
