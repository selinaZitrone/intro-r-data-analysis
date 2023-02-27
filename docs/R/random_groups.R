names <- c(
  "Jule",
  "Mohamed",
  "Khairy",
  "Fabian",
  "Tejal",
  "Amit",
  "Mohammed",
  "Xiangfei",
  "Nadra",
  "Anna Carolina" ,
  "Ann-Kathrin",
  "Ozan",
  "Joana-Lysiane",
  "Shaoyi",
  "Emanoela",
  "Alina")
ngroups <- 4
groupsize <- 4

assign_groups(ngroups = ngroups, groupsize = groupsize, names = names)

assign_groups <- function(ngroups, groupsize, names) {
  groups <- list()
  for (i in 1:ngroups) {
    groups[[i]] <- sample(names, size = groupsize)
    names <- names[!(names %in% groups[[i]])]
  }
  names(groups) <- paste0("group_",1:i)
  groups[["not_assigned_yet"]] <- names
  return(groups)
}


