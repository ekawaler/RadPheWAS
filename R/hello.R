# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function(n="bad") {
  print("Hello, world!")
}

hello <- function(n=50){
  print("Oh, my bad")
}

#hello <- function()

gen_ex <- function() {
  generateExample(n=50)
}
