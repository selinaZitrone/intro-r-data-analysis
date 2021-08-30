names <- c(
  "Francisco",
  "Deborah",
  "Dhanur", "Jeane", "Ginevra",
  "Hongyu", "Nadja", "Suchetana",
  "Fabian", "Ahmad Rashad",
  "Safiya", "Julia", "Subhakankha",
  "Aya", "Lili", "Mueena")
ngroups <- 5
groupsize <- 3

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
