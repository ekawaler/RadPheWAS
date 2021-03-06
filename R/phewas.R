#phewas <-
#  function(phenotypes,genotypes,data,covariates=c(NA),adjustments=list(NA),
#outcomes, predictors, cores=1, additive.genotypes=T,
#           significance.threshold, alpha=0.05, unadjusted=F,
#return.models=F, min.records=20, MASS.confint.level=NA,quick.confint.level) {
#  }

# What abstractions do I want to make here? Probably covariates, adjustments,
# Significance threshold: significance.threshold=c("bonferroni")
#thresh=match(c("p-value","bonferroni","fdr","simplem-genotype",
#"simplem-phenotype","simplem-product")

#S3/S4 objects. Like, a<-phewas(phenotypes,genotypes) then p_w_b(a)
#chaining with magrittr?

phewas_with_bonferroni<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE){
    res <- phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("bonferroni"))
    if (explain) {explain_phewas_bon(res,verbose)}
    return(res)
  }

phewas_with_fdr<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE){
    res <- phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("fdr"))
    if (explain) {explain_phewas_fdr(res,verbose)}
    return(res)  }

phewas_with_both<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE){
    phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("fdr","bonferroni"))
  }

explain_phewas_bon <- function(res,verbose){
  # Which parts of this table are we interested in explaining?
  results_des<-addPhewasDescription(res)
  signif_res <- results_des[results_des$bonferroni&!is.na(results_des$p),]
  #print(dim(bonfer))
  for (i in seq(1,dim(signif_res)[1])) {
    print(paste("The SNP ",signif_res$snp[i]," is significantly associated with ",
                signif_res$phewas_description[i]," (PheWAS code ",signif_res$phewas_code[i],
                ") with a p-value of ",signif(signif_res$p[i], digits=3)," after Bonferroni correction.",
                sep=''))
      if (verbose) {
        print((paste("PheWAS code ",signif_res$phewas_code[i]," includes the following ICD-9 code(s): ",
                     paste(phemap$icd9[phemap$phewas_code==signif_res$phewas_code[i]],collapse=', '),sep='')))
    }
  }

  print(signif_res)
}

explain_phewas_fdr <- function(res,verbose){
  # Which parts of this table are we interested in explaining?
  results_des<-addPhewasDescription(res)
  signif_res <- results_des[results_des$fdr&!is.na(results_des$p),]
  for (i in seq(1,dim(signif_res)[1])) {
    print(paste("The SNP ",signif_res$snp[i]," is significantly associated with ",
                signif_res$phewas_description[i]," (PheWAS code ",signif_res$phewas_code[i],
                ") with a p-value of ",signif(signif_res$p[i], digits=3)," after FDR correction.",sep=''))
      if (verbose) {
        print((paste("PheWAS code ",signif_res$phewas_code[i]," includes the following ICD-9 code(s): ",
                     paste(phemap$icd9[phemap$phewas_code==signif_res$phewas_code[i]],collapse=', '),sep='')))
      }
    }
  print(signif_res)
}
