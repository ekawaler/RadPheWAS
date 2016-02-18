# There's a way to do this so you can just type example(Something)
# Can't quite figure it out

ex_function <- function(){
  #Load the PheWAS package
  library(RadPheWAS)
  #Set the random seed so it is replicable
  set.seed(2)
  #Generate some example data
  #ex=generate_example(number_of_patients=3000, phenotypes_per_patient=10, code_to_enhance=c("335","764"))
  ex=generate_example(number_of_patients=2000, phenotypes_per_patient=10, code_to_enhance=c("764","335","327.71"))
  #print(slice(ex$genotypes,1:10))
  #Extract the two parts from the returned list
  id.icd9.count=ex$id.icd9.count
  #print(slice(id.icd9.count,1:10))
  genotypes=ex$genotypes
  #Create the PheWAS code table- translates the icd9s, adds
  #exclusions, and reshapes to a wide format
  phenotypes=create_phewas_table(id.icd9.count)
  #Run the PheWAS
  results=phewas_with_bonferroni(phenotypes,genotypes,alpha=0.10)
  print(results[results$phenotype %in% c("335","764","327.71"),])
  #print(slice(results,1:10))
  #print(results[order(results$p)[1:10],])
  #Plot the results
  print(phewas_manhattan(results))
  #Add PheWAS descriptions
  results_d=addPhewasDescription(results)
  #List the significant results
  results_d[results_d$bonferroni&!is.na(results_d$p),]
  #List the top 10 results
  results_d[order(results_d$p)[1:10],]
}
