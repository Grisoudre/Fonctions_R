# multitab : croiser des séries de variables nominales deux à deux ----

multitab <- function(table, lignes, colonnes, 
                     weights = "NULL", prop = "n", 
                     digits=1, na= T, normwt = F){
 # Préparation des arguments
  if(weights == "NULL"){
  t<- table %>% select(lignes, colonnes)
  w.log <- F
  w <- "NULL"
  }else{
  t<- table %>% select(lignes, colonnes, weights)
  w.log <- T
  w <-which(as.list(names(t)) == weights)
  }
  x <-which(as.list(names(t)) %in% lignes)
  y <-which(as.list(names(t)) %in% colonnes)
# %lignes :
  if(prop == "r")
{ for (k in x){
    for (i in y){
      cat(names(t)[k], "*", names(t)[i], ":", "\n")
      print(round(questionr::rprop(questionr::wtd.table(t[,k], t[,i],
                            weights = switch (w.log,
                              T = t[,w],
                              F = w),na.show = na, digits= digits,
                             normwt =normwt )), digits=digits))
    }
   }
  }
#%colonnes :
  else if(prop == "c")
  {   for (k in x){
    for (i in y){
      cat(names(t)[k], "*", names(t)[i], ":", "\n")
      print(round(questionr::cprop(questionr::wtd.table(t[,k], t[,i],
                            weights = switch (w.log,
                                              T = t[,w],
                                              F = w),
                            na.show = na, digits= digits,
                            normwt =normwt)), digits=digits))
    }
  }
  }
# %totaux :
 else if(prop == "t")
  {   for (k in x){
    for (i in y){
      cat(names(t)[k], "*", names(t)[i], ":", "\n")
      print(round(questionr::prop(questionr::wtd.table(t[,k], t[,i],
                            weights = switch (w.log,
                                              T = t[,w],
                                              F = w),
                            na.show = na, digits= digits,
                            normwt =normwt)), digits=digits))
    }
  }
 }
  
# effectifs :
  else if(prop == "n")
  {   for (k in x){
    for (i in y){
      cat(names(t)[k], "*", names(t)[i], ":", "\n")
      print(round(addmargins(questionr::wtd.table(t[,k], t[,i], 
                                 weights = switch (w.log,
                                                   T = t[,w],
                                                   F = w), 
                                 digits= digits, na.show = na,
                                 normwt =normwt)), digits=digits))
    }
  }
  }
  rm(t)
}

# multitab.knitr : croiser des séries de variables deux à deux / sortie tableaux knitr ----

multitab.knitr <- function(table, lignes, colonnes, 
                     weights = "NULL", prop = "n", 
                     digits=1, na= T, normwt = F){
# Préparation des arguments
  if(weights == "NULL"){
    t<- table %>% select(lignes, colonnes)
    w.log <- F
    w <- "NULL"
  }else{
    t<- table %>% select(lignes, colonnes, weights)
    w.log <- T
    w <-which(as.list(names(t)) == weights)
  }
  x <-which(as.list(names(t)) %in% lignes)
  y <-which(as.list(names(t)) %in% colonnes)
  # %lignes :
  if(prop == "r")
  {   for (k in x){
    for (i in y){
      cat("\n",names(t)[k], "*", names(t)[i], ":")
      print(knitr::kable(round(questionr::rprop(questionr::wtd.table(t[,k], t[,i],
                            weights = switch (w.log,
                                              T = t[,w],
                                              F = w),
                            na.show = na, digits= digits,
                            normwt =normwt)), digits=digits)))
    }
  }
  }
  #%colonnes :
  else if(prop == "c")
  {   for (k in x){
    for (i in y){
      cat("\n",names(t)[k], "*", names(t)[i], ":")
      print(knitr::kable(round(questionr::cprop(questionr::wtd.table(t[,k], t[,i],
                                         weights = switch (w.log,
                                                           T = t[,w],
                                                           F = w),
                                         na.show = na, digits= digits,
                                         normwt =normwt)), digits = digits)))
    }
  }
  }
  # %totaux :
  else if(prop == "t")
  {   for (k in x){
    for (i in y){
      cat("\n",names(t)[k], "*", names(t)[i], ":")
      print(knitr::kable(round(questionr::prop(questionr::wtd.table(t[,k], t[,i],
                                         weights = switch (w.log,
                                                           T = t[,w],
                                                           F = w),
                                         na.show = na, digits= digits,
                                         normwt =normwt)), digits=digits)))
    }
  }
  }
  
  # effectifs :
  else if(prop == "n")
  {   for (k in x){
    for (i in y){
      cat("\n",names(t)[k], "*", names(t)[i], ":")
      print(knitr::kable(round(addmargins(questionr::wtd.table(t[,k], t[,i],
                                         weights = switch (w.log,
                                                           T = t[,w],
                                                           F = w),
                                         na.show = na, digits= digits,
                                         normwt =normwt)), digits=digits)))
    }
  }
  }
  rm(t)
}

# Arguments :
#' table : table
#' lignes : colonnes représentées en lignes
#' colonnes : colonnes représentées en colonnes
#' weights : variable de pondération, "NULL" par défaut
#' prop : "n" effectifs, "r" lignes, "c" colonnes, "t" totaux, "n" par défaut
#' digits : décimales, 1 par défaut
#' na T/F : représentation des NA, T par défaut
#' normwt T/F : poids normalisés (total égal au total sans pondération), F par défaut
#' packages questionr et knitr


# Exemples : 
library(questionr)
data(hdv2003)
multitab(hdv2003, c("sexe","nivetud"), c("cinema","sport"), na=T, digits=0,
         prop="n",weights = "poids",  normwt = T)
multitab.knitr(hdv2003, c("sexe","nivetud"), c("cinema","sport"), na=T, digits=1,
               prop="r")
