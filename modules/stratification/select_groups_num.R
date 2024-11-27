
#' Creates new numeric groups for the stratified sample
#'
#' @name select_groups
#' @param mydata Der Datensatz mit der Spalte, die neu gruppiert werden soll
#' @param mycol Der Name der Spalte, die Sie ändern möchten
#' @param mylist Eine Variable mit den Grenzwerten der neuen numerischen Gruppen;
#'              der erster Wert der Variable ist der min. Wert und der letzte, der max. Wert,
#'              die Werte dazwischen sind mit dem Kleiner-Gleich-Zeichen definiert.
#'              Hierfür kann die base::summary() eingesehen werden.
#' @param newname Der neue Name der Spalte, die die neuen Gruppen enthalten soll
#' @returns Der neue Datensatz mit einer zusätzlichen Spalte
#' @export

select_groups_num <- function(mydata, mycol, mylist, newname){

  #create a new column, with the selected name "newname"
  mydata[newname] <- NA

  #loop over the list of list
  for (i in 2:length(mylist)){

    #variable that holds the indices for each group
    my_index <- c()

    my_var <- mylist[i]
    my_var_b <- mylist[i-1]

    for (v in 1:length(mydata[,mycol])){

      if(is.na(mydata[v,mycol])){
        #ignore if it is an NA

      } else if(mydata[v,mycol] >= my_var_b & mydata[v,mycol] <= my_var){

        #collect the indices for each group
        my_index <- c(my_index, v)
      }
    }
    #add the new group names to the associated indices
    mydata[my_index, newname] <- paste(my_var_b, "-", my_var)

  }
  return(mydata)
}