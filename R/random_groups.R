names <- c(
  "Fabian", "Sara",
  "Vajiheh", "Kinga",
  "Haicheng", "Xiaohui",
  "zineb", "Keqing",
  "Malkeet", "Johannes",
  "Xuchao", "Desjana",
  "Henriette ", "Saidaqa ",
  "Mohammad Reza", "Malavika"
)

# Function to assign people to groups
assign_groups <- function(groupsize, names) {
  groups <- list()
  # How many groups can there be?
  ngroups <- length(names) %/% groupsize
  for (i in 1:ngroups) {
    groups[[i]] <- sample(names, size = groupsize)
    names <- names[!(names %in% groups[[i]])]
    # convert groups to a nice string
    groups[[i]] <- paste(groups[[i]], collapse = ", ")
  }
  names(groups) <- paste0("group_", 1:i)
  groups[["not_assigned_yet"]] <- names
  return(groups)
}

assign_groups(groupsize = 4, names = names)
