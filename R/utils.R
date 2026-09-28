.aliases <- list(
  element = c("element", "элемент", "группа", "статья", "name", "часть", "категория"),
  base    = c("base", "d0", "v0", "базис", "база", "план", "прошлый", "было"),
  actual  = c("actual", "d1", "v1", "vt", "отчет", "отчёт", "факт", "текущий", "стало")
)

.find_col <- function(nms, key) {
  hit <- which(tolower(trimws(nms)) %in% .aliases[[key]])
  if (length(hit)) hit[1L] else NA_integer_
}

.check_values <- function(x, what) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` должен быть числовым вектором.", what), call. = FALSE)
  }
  if (anyNA(x)) stop(sprintf("В `%s` есть пропуски (NA).", what), call. = FALSE)
  if (any(!is.finite(x))) stop(sprintf("В `%s` есть бесконечные значения.", what), call. = FALSE)
  if (any(x < 0)) {
    stop(sprintf("В `%s` есть отрицательные значения: структура состоит из неотрицательных частей.",
                 what), call. = FALSE)
  }
  if (sum(x) == 0) stop(sprintf("Сумма `%s` равна нулю, доли не определены.", what), call. = FALSE)
  x
}

# Приводит вход к двум векторам долей одинаковой длины с общими названиями.
.parse_structures <- function(base, actual) {
  if (is.null(actual)) {
    if (!is.data.frame(base)) {
      stop("Передайте `actual` или таблицу с колонками element / base / actual.", call. = FALSE)
    }
    ec <- .find_col(names(base), "element")
    bc <- .find_col(names(base), "base")
    ac <- .find_col(names(base), "actual")
    num <- which(vapply(base, is.numeric, logical(1)))
    if (is.na(bc) || is.na(ac)) {
      if (length(num) != 2L) {
        stop("Не понял формат таблицы. Нужны колонки base / actual (или d0 / d1, ",
             "базис / отчёт) либо ровно две числовые колонки.", call. = FALSE)
      }
      bc <- num[1L]
      ac <- num[2L]
    }
    if (is.na(ec)) {
      chr <- which(!vapply(base, is.numeric, logical(1)))
      ec <- if (length(chr)) chr[1L] else NA_integer_
    }
    labels <- if (is.na(ec)) NULL else as.character(base[[ec]])
    actual <- stats::setNames(base[[ac]], labels)
    base <- stats::setNames(base[[bc]], labels)
  }
  if (is.list(base)) base <- unlist(base)
  if (is.list(actual)) actual <- unlist(actual)
  base <- .check_values(base, "base")
  actual <- .check_values(actual, "actual")

  nb <- names(base)
  na <- names(actual)
  if (!is.null(nb) && !is.null(na) && all(nzchar(nb)) && all(nzchar(na))) {
    if (anyDuplicated(nb) || anyDuplicated(na)) {
      stop("Названия элементов структуры не должны повторяться.", call. = FALSE)
    }
    if (!setequal(nb, na)) {
      stop("Состав элементов в `base` и `actual` различается: ",
           paste(union(setdiff(nb, na), setdiff(na, nb)), collapse = ", "),
           ". Добавьте недостающие элементы с нулевым значением.", call. = FALSE)
    }
    actual <- actual[nb]
  } else if (length(base) != length(actual)) {
    stop("В `base` и `actual` разное число элементов структуры.", call. = FALSE)
  }
  labels <- if (!is.null(nb) && all(nzchar(nb))) nb
            else if (!is.null(na) && all(nzchar(na))) na
            else paste0("Элемент ", seq_along(base))
  if (length(base) < 2L) {
    stop("Структура должна состоять хотя бы из двух элементов.", call. = FALSE)
  }
  list(
    labels = labels,
    raw0 = unname(base), raw1 = unname(actual),
    d0 = unname(base) / sum(base), d1 = unname(actual) / sum(actual)
  )
}

.fmt <- function(x, digits = 4) {
  out <- formatC(x, format = "f", digits = digits, big.mark = " ")
  out[is.na(x)] <- "—"
  out
}

.fmt_signed <- function(x, digits = 4) {
  out <- .fmt(x, digits)
  pos <- !is.na(x) & x > 0
  out[pos] <- paste0("+", out[pos])
  out
}
