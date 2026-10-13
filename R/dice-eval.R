#  Parser and evaluator for dice expressions, following the syntax of Avrae's
#  d20 library (https://github.com/avrae/d20).
#
#  Grammar (whitespace is ignored, case is ignored):
#
#    expr     := term (("+" | "-") term)*
#    term     := power (("*" | "/") power)*
#    power    := unary (("^" | "**") power)?
#    unary    := ("-" | "+") unary | atom
#    atom     := "(" expr ")" | dice | number
#    dice     := [count] "d" (sides | "%") modifier*
#    modifier := ("k" | "p" | "rr" | "ro" | "ra" | "e" | "mi" | "ma") selector+
#    selector := ["h" | "l" | "<" | ">"] number
#
#  Consecutive identical modifiers are merged (e5e6 is e(5,6)), as in d20.
#
#  A parsed expression is a tree of lists with a `type` of "num", "neg",
#  "binop" or "dice".

# --- Parser ------------------------------------------------------------------

#' Parse a dice expression
#'
#' @param dice_formula A string containing a dice formula, e.g. 4d6e6kh3+2
#' @param require_dice If TRUE, the formula must contain at least one dice
#' @param validate If TRUE, check that the modifiers can be rolled (e.g. no 
#'   reroll of a number that is not on the dice). Otherwise only the syntax is checked.
#' @return A tree (nested lists) describing the expression. Number and dice 
#'   nodes also keep their source text, for parse_dice_formula()

parse_dice_expression <- function(dice_formula, require_dice = TRUE, validate = TRUE) {

  st <- new.env()
  st$orig <- gsub("\\s", "", dice_formula) # same length as st$s: keeps the case
  st$s <- tolower(st$orig)
  st$pos <- 1L
  st$n_dice <- 0L

  fail <- function(msg) {
    stop(msg, call. = FALSE)
  }

  # Consume `pattern` at the current position, return the capture groups or NULL
  take <- function(pattern) {
    rest <- substring(st$s, st$pos)
    m <- regmatches(rest, regexec(paste0("^(?:", pattern, ")"), rest, perl = TRUE))[[1]]
    if (length(m) == 0) {
      return(NULL)
    }
    st$pos <- st$pos + nchar(m[1])
    m
  }

  parse_expr <- function() {
    node <- parse_term()
    while (!is.null(op <- take("[+-]"))) {
      node <- list(type = "binop", op = op[1], op_text = op[1], lhs = node, rhs = parse_term())
    }
    node
  }

  parse_term <- function() {
    node <- parse_unary()
    while (!is.null(op <- take("\\*(?!\\*)|/"))) {
      node <- list(type = "binop", op = op[1], op_text = op[1], lhs = node, rhs = parse_unary())
    }
    node
  }

  # unary signs are weaker than ^: -2^2 is -(2^2), like in R
  parse_unary <- function() {
    if (!is.null(op <- take("[+-]"))) {
      node <- parse_unary()
      if (op[1] == "-") {
        node <- list(type = "neg", x = node)
      }
      return(node)
    }
    parse_power()
  }

  # right associative: 2^3^2 is 2^(3^2); the exponent can be negative: 2^-1
  parse_power <- function() {
    node <- parse_atom()
    if (!is.null(op <- take("\\^|\\*\\*"))) {
      node <- list(type = "binop", op = "^", op_text = op[1], lhs = node, 
                   rhs = parse_unary())
    }
    node
  }

  parse_atom <- function() {
    start <- st$pos
    source_text <- function() substring(st$orig, start, st$pos - 1)
    if (!is.null(take("\\("))) {
      node <- parse_expr()
      if (is.null(take("\\)"))) {
        fail("invalid dice_formula, missing closing parenthesis")
      }
      return(node)
    }
    if (!is.null(m <- take("(\\d*)d(\\d+|%)"))) {
      node <- parse_dice(m[2], m[3])
      node$base_text <- source_text() # before the modifiers
      return(parse_dice_modifiers(node, start))
    }
    if (!is.null(m <- take("\\d+(?:\\.\\d+)?"))) {
      return(list(type = "num", value = as.numeric(m[1]), text = source_text()))
    }
    fail(paste0("invalid dice_formula, cannot parse '", substring(st$s, st$pos), "'"))
  }

  parse_dice <- function(count, sides) {
    count <- if (count == "") 1 else as.numeric(count)
    sides <- if (sides == "%") 100 else as.numeric(sides)
    assertthat::assert_that(count >= 1, msg = "cannot roll 0 dice!")
    assertthat::assert_that(sides >= 1, msg = "cannot roll a d0!")
    st$n_dice <- st$n_dice + 1L
    list(type = "dice", id = st$n_dice, count = count, sides = sides, 
         mods = list(), raw_mods = list())
  }

  # Add the modifiers that follow the dice
  parse_dice_modifiers <- function(node, start) {
    count <- node$count
    sides <- node$sides
    
    # Like d20, consecutive identical modifiers are merged: e5e6 is e(5,6)
    mods <- list()
    raw_mods <- list() # as written, for parse_dice_formula()
    while (!is.null(m <- take("rr|ro|ra|mi|ma|k|p|e"))) {
      op <- m[1]
      sels <- parse_selectors(op)
      raw_mods <- c(raw_mods, lapply(sels, function(sel) {
        list(op = op, cat = sel$cat, num = sel$num)
      }))
      last <- length(mods)
      if (last > 0 && mods[[last]]$op == op && !op %in% c("mi", "ma")) {
        mods[[last]]$sels <- c(mods[[last]]$sels, sels)
      } else {
        mods[[last + 1]] <- list(op = op, sels = sels)
      }
    }
    
    can_grow <- FALSE # can the set contain more dice than `count`?
    values_changed <- FALSE # mi/ma can create values outside the original faces
    for (mod in mods) {
      if (validate) validate_modifier(mod, count, sides, can_grow, values_changed)
      if (mod$op %in% c("mi", "ma")) values_changed <- TRUE
      if (mod$op %in% c("e", "ra")) {
        can_grow <- TRUE
      }
    }
    node$mods <- mods
    node$raw_mods <- raw_mods
    node$text <- substring(st$orig, start, st$pos - 1)
    node
  }

  parse_selectors <- function(op) {
    sels <- list()
    while (!is.null(m <- take("([hl<>]?)(\\d+)"))) {
      sels[[length(sels) + 1]] <- list(cat = m[2], num = as.numeric(m[3]))
    }
    if (length(sels) == 0) {
      fail(paste0("invalid dice_formula, modifier '", op, "' needs a selector, e.g. ",
                  op, "6"))
    }
    sels
  }

  node <- parse_expr()
  if (st$pos <= nchar(st$s)) {
    fail(paste0("invalid dice_formula, cannot parse '", substring(st$s, st$pos), "'"))
  }
  if (require_dice) {
    assertthat::assert_that(st$n_dice > 0,
                            msg = "dice_formula need to contain at least one d statement")
  }
  node
}

# Does a selector match a die with value `x`? Only for selectors that do not
# depend on the other dice of the set (not h/l).
selector_matches <- function(sel, x) {
  switch(sel$cat,
         "<" = x < sel$num,
         ">" = x > sel$num,
         x == sel$num)
}

# Sides (between 1 and `sides`) matched by a selector that does not depend on 
# the other dice, as an interval c(first, last). Empty if first > last.
selector_sides <- function(sel, sides) {
  switch(sel$cat,
         "<" = c(1, min(sides, sel$num - 1)),
         ">" = c(max(1, sel$num + 1), sides),
         c(max(1, sel$num), min(sides, sel$num)))
}

# Do the selectors match some sides, or all sides, of a die? 
# Works on intervals: sides can be large, so they are never listed.
sides_matched <- function(sels, sides) {
  intervals <- lapply(sels, selector_sides, sides = sides)
  intervals <- Filter(function(x) x[1] <= x[2], intervals)
  if (length(intervals) == 0) {
    return(c(any = FALSE, all = FALSE))
  }
  intervals <- intervals[order(vapply(intervals, `[`, numeric(1), 1))]
  covered_to <- 0 # all the sides up to this one are matched
  for (x in intervals) {
    if (x[1] > covered_to + 1) break
    covered_to <- max(covered_to, x[2])
  }
  c(any = TRUE, all = covered_to >= sides)
}

# Check that a modifier makes sense for a die with `sides` sides
validate_modifier <- function(mod, count, sides, can_grow, values_changed = FALSE) {
  op <- mod$op
  
  if (op %in% c("mi", "ma")) {
    assertthat::assert_that(
      length(mod$sels) == 1 && mod$sels[[1]]$cat == "",
      msg = paste0("invalid dice_formula, '", op, "' needs a plain number, e.g. ", op, "2"))
  }
  
  if (op == "k") {
    for (sel in mod$sels) {
      if (sel$cat %in% c("h", "l")) {
        assertthat::assert_that(sel$num > 0, 
                                msg = "invalid kh/kl formula, can't keep less than 1 die")
        assertthat::assert_that(can_grow || sel$num <= count, 
                                msg = "invalid kh/kl formula, can't keep more dice than rolled")
      }
    }
  }
  
  if (op == "rr") {
    # the highest (or lowest) dice are always there to be rerolled
    assertthat::assert_that(
      !any(vapply(mod$sels, function(sel) sel$cat %in% c("h", "l"), logical(1))),
      msg = "invalid reroll specification, rr with h or l would never end (use ro)")
  }
  
  if (op %in% c("rr", "ro", "ra", "e")) {
    # selectors that do not depend on the other dice: can be checked now
    fixed <- Filter(function(sel) !sel$cat %in% c("h", "l"), mod$sels)
    matched <- sides_matched(fixed, sides)
    what <- if (op == "e") "exploding dice" else "reroll"
    # h or l always select a die, so "no side matches" only without them
    if (length(fixed) == length(mod$sels) && !values_changed) {
      assertthat::assert_that(
        matched[["any"]], 
        msg = paste0("invalid ", what, " specification, no side of the dice matches"))
    }
    # but if the other selectors match every side, h or l can't stop it (e1h1 on a d1)
    if (op %in% c("e", "rr")) {
      assertthat::assert_that(
        !matched[["all"]], 
        msg = paste0("invalid ", what, " specification, every side would ", 
                     if (op == "e") "explode" else "be rerolled"))
    }
  }
  invisible(TRUE)
}

# --- Evaluator ---------------------------------------------------------------

# Dice nodes of an expression, from left to right
collect_dice_nodes <- function(node) {
  switch(node$type,
         dice = list(node),
         neg = collect_dice_nodes(node$x),
         binop = c(collect_dice_nodes(node$lhs), collect_dice_nodes(node$rhs)),
         list())
}

#' Probabilities of the sides, for each group of dice of an expression
#'
#' @param node A tree returned by parse_dice_expression()
#' @param prob NULL (fair dice), a vector of probabilities that is used for 
#'   every group of dice, or a list with a vector (or NULL) for each group of
#'   dice, from left to right
#' @return A list with one element for each group of dice

resolve_prob <- function(node, prob = NULL) {
  groups <- collect_dice_nodes(node)
  
  if (is.null(prob)) {
    return(rep(list(NULL), length(groups)))
  }
  if (is.list(prob)) {
    assertthat::assert_that(
      length(prob) == length(groups), 
      msg = paste0("prob is a list with ", length(prob), " elements, but the formula has ", 
                   length(groups), " groups of dice"))
    probs <- prob
  } else {
    probs <- rep(list(prob), length(groups))
  }
  
  for (i in seq_along(groups)) {
    if (is.null(probs[[i]])) next
    assertthat::assert_that(
      is.numeric(probs[[i]]), 
      msg = "prob must be numeric")
    assertthat::assert_that(
      length(probs[[i]]) == groups[[i]]$sides, 
      msg = paste0("prob has ", length(probs[[i]]), " values, but dice group ", i, 
                   " has ", groups[[i]]$sides, " sides. Use a list with one vector ", 
                   "of probabilities for each group of dice to mix dice with different sides"))
    check_prob_terminates(groups[[i]], probs[[i]])
  }
  probs
}

# With unfair dice, rerolling or exploding on sides that have all the probability 
# never ends: every die would roll one of them. 
check_prob_terminates <- function(group, prob) {
  for (mod in group$mods) {
    if (!mod$op %in% c("rr", "e")) next
    # only the selectors that do not depend on the other dice: h or l can't stop it
    fixed <- Filter(function(sel) !sel$cat %in% c("h", "l"), mod$sels)
    if (length(fixed) == 0) next
    sides_hit <- selector_matches_any(fixed, seq_len(group$sides))
    assertthat::assert_that(
      sum(prob[!sides_hit]) > 0,
      msg = paste0("invalid ", if (mod$op == "e") "exploding dice" else "reroll", 
                   " specification, with this prob every die would ", 
                   if (mod$op == "e") "explode" else "be rerolled", " (it never ends)"))
  }
}

#' Evaluate a parsed dice expression
#'
#' @param node A tree returned by parse_dice_expression()
#' @param n Number of times the expression is evaluated
#' @param prob Probabilities of the sides of the dice, see resolve_prob()
#' @param detail If TRUE, keep the dice that were rolled (in the attribute `dice`)
#' @return Numeric vector of length n. With `detail`, it has an attribute `dice`: 
#'   a list of n tibbles (group, sides, value, kept), one row for each die. 
#'   Dice that are not `kept` were dropped by a modifier.

eval_dice_expression <- function(node, n, prob = NULL, detail = FALSE) {
  # state shared by the groups of dice: the details (if wanted)
  ctx <- new.env()
  ctx$detail <- detail
  ctx$groups <- list()
  
  total <- eval_dice_node(node, n, resolve_prob(node, prob), ctx)
  
  if (detail) {
    groups <- Filter(Negate(is.null), ctx$groups)
    attr(total, "dice") <- lapply(seq_len(n), function(i) {
      tibble::as_tibble(do.call(rbind, lapply(groups, `[[`, i)))
    })
  }
  total
}

eval_dice_node <- function(node, n, probs, ctx) {
  switch(node$type,
    num = rep(node$value, n),
    neg = -eval_dice_node(node$x, n, probs, ctx),
    binop = {
      lhs <- eval_dice_node(node$lhs, n, probs, ctx)
      rhs <- eval_dice_node(node$rhs, n, probs, ctx)
      switch(node$op, "+" = lhs + rhs, "-" = lhs - rhs, "*" = lhs * rhs,
             "/" = lhs / rhs, "^" = lhs ^ rhs)
    },
    dice = eval_dice_group(node, n, probs[[node$id]], ctx)
  )
}

# The dice of each set (row of the matrices) as data frames
dice_rows <- function(node, values, present, kept) {
  lapply(seq_len(nrow(values)), function(i) {
    ok <- present[i, ]
    data.frame(group = node$id, sides = node$sides, 
               value = as.numeric(values[i, ok]), kept = kept[i, ok])
  })
}

# Roll a group of dice (e.g. 4d6e6kh3) n times, return the n totals
eval_dice_group <- function(node, n, prob, ctx) {
  
  roll <- function(k) {
    sample(x = seq_len(node$sides), size = k, replace = TRUE, prob = prob)
  }
  detail <- ctx$detail
  
  # The sets of dice are handled together as matrices, in chunks to limit the 
  # memory used (many dice, or dice that explode)
  chunk_size <- if (length(node$mods) == 0) {
    max(1, floor(5e6 / node$count))
  } else {
    50000
  }
  starts <- seq(1, n, by = chunk_size)
  rows <- list()
  totals <- lapply(starts, function(start) {
    n_chunk <- min(chunk_size, n - start + 1)
    if (length(node$mods) == 0) {
      # without modifiers all dice are summed
      values <- matrix(roll(n_chunk * node$count), nrow = n_chunk, ncol = node$count)
      if (detail) {
        all_dice <- matrix(TRUE, nrow = n_chunk, ncol = node$count)
        rows <<- c(rows, dice_rows(node, values, all_dice, all_dice))
      }
      rowSums(values)
    } else {
      res <- roll_dice_sets(n_chunk, node$count, node$mods, roll, detail)
      if (detail) {
        rows <<- c(rows, dice_rows(node, res$values, res$present, res$kept))
      }
      res$total
    }
  })
  
  if (detail) {
    ctx$groups[[node$id]] <- rows
  }
  unlist(totals, use.names = FALSE)
}

# Does a value match any of the selectors (that do not depend on other dice)?
selector_matches_any <- function(sels, x) {
  Reduce(`|`, lapply(sels, selector_matches, x = x))
}

# Select dice in every set (row) at once, returns a logical matrix. 
# A selector is either relative to the other kept dice (h = highest n, l = lowest n)
# or compares the value (<, >, ==). Like in d20, only kept dice can be selected.
select_dice <- function(sels, values, kept) {
  selected <- matrix(FALSE, nrow(values), ncol(values))
  for (sel in sels) {
    selected <- selected | switch(sel$cat,
      h = , l = {
        # rank of each kept die inside its row (ties: leftmost die first)
        key <- if (sel$cat == "h") -values else values
        key[!kept] <- Inf
        ord <- order(row(key), key, col(key))
        rank <- matrix(0L, nrow(key), ncol(key))
        rank[ord] <- rep(seq_len(ncol(key)), times = nrow(key))
        kept & rank <= sel$num
      },
      kept & selector_matches(sel, values))
  }
  selected
}

# Roll n sets of dice at once and apply the modifiers, in the order they 
# are written. A set is a row of the matrix `values`: `kept` tells which dice
# still count (not dropped, and present: sets can have different sizes).
roll_dice_sets <- function(n, count, mods, roll, detail = FALSE) {
  
  values <- matrix(roll(n * count), nrow = n, ncol = count)
  kept <- matrix(TRUE, nrow = n, ncol = count)
  exploded <- matrix(FALSE, nrow = n, ncol = count)
  
  # add dice to the sets: `n_new` is the number of new dice for each set
  add_dice <- function(n_new) {
    width <- max(n_new)
    new_kept <- matrix(rep(seq_len(width), each = n), nrow = n) <= n_new
    new_values <- matrix(NA_real_, nrow = n, ncol = width)
    new_values[new_kept] <- roll(sum(new_kept))
    values <<- cbind(values, new_values)
    kept <<- cbind(kept, new_kept)
    exploded <<- cbind(exploded, matrix(FALSE, nrow = n, ncol = width))
  }
  
  for (mod in mods) {
    sels <- mod$sels
    switch(mod$op,
      k = {
        kept <- kept & select_dice(sels, values, kept)
      },
      p = {
        kept <- kept & !select_dice(sels, values, kept)
      },
      rr = {
        # reroll until no die matches the selector
        while (any(sel <- select_dice(sels, values, kept))) {
          values[sel] <- roll(sum(sel))
        }
      },
      ro = {
        sel <- select_dice(sels, values, kept)
        values[sel] <- roll(sum(sel))
      },
      ra = {
        # like d20: at most one die per set is rolled again, the new die is added
        sel <- select_dice(sels, values, kept)
        has_sel <- rowSums(sel) > 0
        if (any(has_sel)) {
          first <- matrix(FALSE, nrow = n, ncol = ncol(sel))
          first[cbind(which(has_sel), max.col(sel[has_sel, , drop = FALSE], 
                                              ties.method = "first"))] <- TRUE
          exploded <- exploded | first
          add_dice(as.integer(has_sel))
        }
      },
      e = {
        # Each explosion operation starts a new history, including after ra.
        exploded <- matrix(FALSE, nrow(values), ncol(values))
        # new dice can explode too, but a die explodes only once per operation
        sel <- select_dice(sels, values, kept) & !exploded
        exploded <- exploded | sel
        if (any(vapply(sels, function(x) x$cat %in% c("h", "l"), 
                                        logical(1)))) {
          # selectors that depend on the other dice: look at the whole sets
          while (any(sel)) {
            add_dice(rowSums(sel))
            sel <- select_dice(sels, values, kept) & !exploded
            exploded <- exploded | sel
          }
        } else {
          # otherwise only the new dice can explode: work on blocks of new dice
          blocks <- list()
          n_new <- rowSums(sel)
          while (any(n_new > 0)) {
            width <- max(n_new)
            block_kept <- matrix(rep(seq_len(width), each = n), nrow = n) <= n_new
            block_values <- matrix(NA_real_, nrow = n, ncol = width)
            block_values[block_kept] <- roll(sum(block_kept))
            sel <- block_kept & selector_matches_any(sels, block_values)
            blocks[[length(blocks) + 1]] <- list(values = block_values, 
                                                 kept = block_kept, exploded = sel)
            n_new <- rowSums(sel)
          }
          if (length(blocks) > 0) {
            values <- do.call(cbind, c(list(values), lapply(blocks, `[[`, "values")))
            kept <- do.call(cbind, c(list(kept), lapply(blocks, `[[`, "kept")))
            exploded <- do.call(cbind, c(list(exploded), lapply(blocks, `[[`, "exploded")))
          }
        }
      },
      mi = {
        values[kept & values < sels[[1]]$num] <- sels[[1]]$num
      },
      ma = {
        values[kept & values > sels[[1]]$num] <- sels[[1]]$num
      }
    )
  }
  
  counted <- values
  counted[!kept] <- 0
  total <- rowSums(counted)
  if (!detail) {
    return(list(total = total))
  }
  # absent dice (sets can have different sizes) have no value
  list(total = total, values = values, present = !is.na(values), 
       kept = kept)
}
