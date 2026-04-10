# konstruktory

make_LD <- function(data) {
  required_cols <- c("id", "visit", "room", "value")
  
  if (!is.data.frame(data)) {
    stop("`data` musi byc obiektem typu data.frame.")
  }
  
  if (!all(required_cols %in% names(data))) {
    stop("Ramka danych musi zawierac kolumny: id, visit, room, value.")
  }
  
  x <- list(
    data = data[, required_cols]
  )
  
  class(x) <- "LongitudinalData"
  x
}

new_subject <- function(data, id) {
  x <- list(
    data = data,
    id = id
  )
  class(x) <- "LDSubject"
  x
}

new_visit <- function(data, id, visit) {
  x <- list(
    data = data,
    id = id,
    visit = visit
  )
  class(x) <- "LDVisit"
  x
}

new_room <- function(data, id, visit, room) {
  x <- list(
    data = data,
    id = id,
    visit = visit,
    room = room
  )
  class(x) <- "LDRoom"
  x
}

# funkcje generyczne

subject <- function(x, id, ...) {
  UseMethod("subject")
}

visit <- function(x, visit, ...) {
  UseMethod("visit")
}

room <- function(x, room, ...) {
  UseMethod("room")
}

# metody subject

subject.LongitudinalData <- function(x, id, ...) {
  df <- x$data
  out <- df[df$id == id, , drop = FALSE]
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  new_subject(out, id)
}

# metody visit

visit.LDSubject <- function(x, visit, ...) {
  df <- x$data
  out <- df[df$visit == visit, , drop = FALSE]
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  new_visit(out, x$id, visit)
}

# metody room

room.LDVisit <- function(x, room, ...) {
  df <- x$data
  out <- df[df$room == room, , drop = FALSE]
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  new_room(out, x$id, x$visit, room)
}

# wyświetlanie

print.LongitudinalData <- function(x, ...) {
  n_subjects <- length(unique(x$data$id))
  cat("Longitudinal dataset with", n_subjects, "subjects", "\n")
  invisible(x)
}

print.LDSubject <- function(x, ...) {
  cat("Subject ID:", x$id, "\n")
  invisible(x)
}

print.LDVisit <- function(x, ...) {
  cat("ID:", x$id, "\n")
  cat("Visit:", x$visit, "\n")
  invisible(x)
}

print.LDRoom <- function(x, ...) {
  cat("ID:", x$id, "\n")
  cat("Visit:", x$visit, "\n")
  cat("Room:", x$room, "\n")
  invisible(x)
}

# podsumowania

summary.LDSubject <- function(object, ...) {
  df <- object$data
  
  visit_levels <- sort(unique(df$visit))
  room_levels  <- sort(unique(df$room))
  
  mat <- matrix(
    NA_real_,
    nrow = length(visit_levels),
    ncol = length(room_levels),
    dimnames = list(NULL, room_levels)
  )
  
  for (i in seq_along(visit_levels)) {
    for (j in seq_along(room_levels)) {
      vals <- df$value[df$visit == visit_levels[i] & df$room == room_levels[j]]
      if (length(vals) > 0) {
        mat[i, j] <- mean(vals, na.rm = TRUE)
      }
    }
  }
  
  tab <- data.frame(
    visit = visit_levels,
    mat,
    check.names = FALSE,
    row.names = NULL
  )
  
  out <- list(
    id = object$id,
    table = tab
  )
  class(out) <- "summary.LDSubject"
  out
}

print.summary.LDSubject <- function(x, ...) {
  cat("ID:", x$id, "\n")
  print(x$table)
  invisible(x)
}

summary.LDRoom <- function(object, ...) {
  stats <- summary(object$data$value)
  
  out <- list(
    id = object$id,
    stats = stats
  )
  class(out) <- "summary.LDRoom"
  out
}

print.summary.LDRoom <- function(x, ...) {
  cat("ID:", x$id, "\n")
  print(x$stats)
  invisible(x)
}