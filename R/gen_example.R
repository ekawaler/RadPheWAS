gen_example <- function(n=50,phenotypes.per=10,hit=c("335","764")) {
  #print(hit)
  phewas_code=unique(phemap$phewas_code)
  #Exclude the code to add
  #phewas_code=phewas_code[phewas_code!=hit]
  phewas_code=phewas_code[!(phewas_code %in% hit)]
  #Assign individuals random phenotypes
  random=data.frame(id=rep.int(1:n,phenotypes.per),phewas_code="",count=0)
  random$phewas_code=sample(phewas_code,nrow(random),replace=TRUE)

  for (item in hit){
    #Create the signal
    #signal=as.data.frame(MASS::mvrnorm(n=n,mu=c(0,0),Sigma=rbind(c(.5,.1),c(.1,1)))) # original
    #signal=as.data.frame(MASS::mvrnorm(n=n,mu=c(0,0),Sigma=rbind(c(.25,.1),c(.1,1)))) # stronger signal
    signal=as.data.frame(MASS::mvrnorm(n=n,mu=c(0,0),Sigma=rbind(c(.25,.1),c(.1,0.75)))) # even stronger, but not enough
    signal=as.data.frame(MASS::mvrnorm(n=n,mu=c(0,0),Sigma=rbind(c((.5/length(hit)),.1),c(.1,(1/length(hit))))))
    names(signal)=c("phenotype","rsEXAMPLE")
    #Normalize the phenotype to logical and the genotype to 0,1,2.
    signal$phenotype=signal$phenotype>.25
    #View(signal)
    signal$rsEXAMPLE=signal$rsEXAMPLE+abs(min(signal$rsEXAMPLE))
    signal$rsEXAMPLE=floor(signal$rsEXAMPLE*2.99/max(signal$rsEXAMPLE))
    signal$id=1:n
    signal$count=0
    signal$phewas_code=item
    if (!exists("sig")){
      sig <- signal
    } else {
      sig = rbind(sig, signal)
    }
    #print(signal)
    #print(cor(signal$phenotype,signal$rsEXAMPLE))
    #View(signal)
    random=rbind(random,signal[signal$phenotype,c("id","phewas_code","count")])
  }

  random=merge(random,phemap)
  random$count= rpois(nrow(random),4)
  #random[random$phewas_code==hit,]$count=random[random$phewas_code==hit,]$count+2
  random[random$phewas_code %in% hit,]$count=random[random$phewas_code %in% hit,]$count+2
  #print(random[random$phewas_code %in% hit,])
  return(list(id.icd9.count=random[,c("id","icd9","count")],genotypes=sig[,c("id","rsEXAMPLE")]))
}
