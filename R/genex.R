# generateExample <- function(n=50,phenotypes.per=10,hit="335")

# Can I not overload functions in R? I'm not sure I can change the parameter names without
# changing the function name, otherwise
generate_example <- function(number_of_patients=5, phenotypes_per_patient=5, code_to_enhance=c("427.8")){
  gen_example(n=number_of_patients, phenotypes.per=phenotypes_per_patient,
                          hit=code_to_enhance)
}
