#' Полный анализ структурных сдвигов
#'
#' Считает сразу все показатели пакета и строит расчётную таблицу, как в
#' учебнике: доли, их разности, квадраты разностей и квадраты долей.
#' Показатели:
#'
#' | Показатель | Формула | Границы |
#' |---|---|---|
#' | Индекс различия \eqn{I_S} | \eqn{\sum \lvert d_1 - d_0 \rvert} | 0 – 2 |
#' | Нормированный индекс различия \eqn{I_S^*} | \eqn{\frac12 \sum \lvert d_1 - d_0 \rvert} | 0 – 1 |
#' | Линейный коэф. абсолютных сдвигов (Казинец) | \eqn{\sum \lvert d_1 - d_0 \rvert / k} | — |
#' | Квадратический коэф. абсолютных сдвигов (Казинец) | \eqn{\sqrt{\sum (d_1 - d_0)^2 / k}} | — |
#' | Линейный коэф. относительных сдвигов (Казинец) | \eqn{\sum \lvert d_1/d_0 - 1 \rvert d_0} | 0 – 2 |
#' | Квадратический коэф. относительных сдвигов (Казинец) | \eqn{\sqrt{\sum (d_1/d_0 - 1)^2 d_0}} | — |
#' | Нормированный квадратический коэф. (Гатев) \eqn{\sigma^*} | \eqn{\sqrt{\sum (d_1 - d_0)^2 / 2}} | 0 – 1 |
#' | Интегральный коэф. Гатева \eqn{K_S} | \eqn{\sqrt{\frac{\sum (d_1 - d_0)^2}{\sum d_0^2 + \sum d_1^2}}} | 0 – 1 |
#' | Индекс Рябцева \eqn{I_R} | \eqn{\sqrt{\frac{\sum (d_1 - d_0)^2}{\sum (d_1 + d_0)^2}}} | 0 – 1 |
#' | Индекс Салаи \eqn{I_{Sz}} | \eqn{\sqrt{\frac1k \sum \left(\frac{d_1 - d_0}{d_1 + d_0}\right)^2}} | 0 – 1 |
#'
#' Колонка «Вклад, %» в расчётной таблице показывает долю каждого элемента
#' в сумме \eqn{\sum (d_1 - d_0)^2} — числителе коэффициентов Гатева и
#' Рябцева. Так видно, какие элементы дали основной сдвиг.
#'
#' @inheritParams gatev_index
#' @param type `"shift"` — сдвиги во времени (базисный и отчётный период),
#'   `"difference"` — различия между двумя структурами на один момент
#'   (регионы, предприятия, факт и норматив). Влияет только на подписи.
#' @param labels Подписи двух структур в выводе. По умолчанию
#'   «Базис»/«Отчёт» или «Структура 1»/«Структура 2».
#' @return Объект класса `struct_shift`:
#'   * `$table` — расчётная таблица по элементам;
#'   * `$indices` — таблица всех показателей;
#'   * `$gatev`, `$ryabtsev` — основные коэффициенты;
#'   * `$interpretation` — оценка по шкале Рябцева.
#'
#'   У объекта есть методы `print()`, `summary()`, `plot()` и
#'   `as.data.frame()`.
#' @export
#' @examples
#' # Пример из учебника Гатевых (табл. 9.1)
#' res <- struct_shift(export_firm_n)
#' res
#' res$gatev
#' if (interactive()) plot(res)
#'
#' # Структура затрат ТЭЦ (Афанасьев, 2019): 2008 и 2018 гг.
#' struct_shift(tec_costs[c("element", "stec_2008", "stec_2018")],
#'              labels = c("2008", "2018"))
struct_shift <- function(base, actual = NULL, type = c("shift", "difference"),
                         labels = NULL) {
  type <- match.arg(type)
  s <- .parse_structures(base, actual)
  if (is.null(labels)) {
    labels <- if (type == "shift") c("Базис", "Отчёт") else c("Структура 1", "Структура 2")
  }
  if (length(labels) != 2L) stop("`labels` — это две подписи.", call. = FALSE)
  d0 <- s$d0
  d1 <- s$d1
  k <- length(d0)
  sq <- (d1 - d0)^2

  table <- data.frame(
    element = s$labels, d0 = d0, d1 = d1, diff = d1 - d0,
    growth = ifelse(d0 > 0, d1 / d0, NA_real_),
    diff_sq = sq, d0_sq = d0^2, d1_sq = d1^2,
    contribution = if (sum(sq) > 0) sq / sum(sq) * 100 else rep(0, k),
    stringsAsFactors = FALSE
  )

  kaz <- .kazinets(d0, d1)
  szalai <- tryCatch(.szalai(d0, d1, s$labels), error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    NA_real_
  })
  gatev <- .gatev(d0, d1)
  ryabtsev <- .ryabtsev(d0, d1)
  indices <- data.frame(
    id = c("I_S", "I_S_norm", "kazinets_linear_abs", "kazinets_quadratic_abs",
           "kazinets_linear_rel", "kazinets_quadratic_rel", "gatev_sigma_norm",
           "gatev_K", "ryabtsev", "szalai"),
    name = c(
      "Индекс различия I_S",
      "Нормированный индекс различия I*_S",
      "Линейный коэф. абсолютных сдвигов (Казинец)",
      "Квадратический коэф. абсолютных сдвигов (Казинец)",
      "Линейный коэф. относительных сдвигов (Казинец)",
      "Квадратический коэф. относительных сдвигов (Казинец)",
      "Нормированный квадратический коэф. σ* (Гатев)",
      if (type == "shift") "Интегральный коэф. структурных сдвигов K_S (Гатев)"
      else "Интегральный коэф. структурных различий K_D (Гатев)",
      "Индекс Рябцева I_R",
      "Индекс Салаи I_Sz"
    ),
    value = c(sum(abs(d1 - d0)), sum(abs(d1 - d0)) / 2, kaz[["linear_abs"]],
              kaz[["quadratic_abs"]], kaz[["linear_rel"]], kaz[["quadratic_rel"]],
              sqrt(sum(sq) / 2), gatev, ryabtsev, szalai),
    range = c("0–2", "0–1", "—", "—", "0–2", "—", "0–1", "0–1", "0–1", "0–1"),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      table = table, indices = indices, k = k, type = type, labels = labels,
      raw = list(base = s$raw0, actual = s$raw1),
      gatev = gatev, ryabtsev = ryabtsev,
      interpretation = ryabtsev_scale(ryabtsev)
    ),
    class = "struct_shift"
  )
}

#' @export
print.struct_shift <- function(x, digits = 4, ...) {
  title <- if (x$type == "shift") "Анализ структурных сдвигов" else "Анализ структурных различий"
  cat(title, ": ", x$labels[1L], " → ", x$labels[2L], " (элементов: ", x$k, ")\n\n", sep = "")
  t <- x$table
  shown <- data.frame(
    t$element, .fmt(t$d0, digits), .fmt(t$d1, digits), .fmt_signed(t$diff, digits),
    .fmt(t$diff_sq, digits), .fmt(t$d0_sq, digits), .fmt(t$d1_sq, digits),
    .fmt(t$contribution, 1),
    stringsAsFactors = FALSE
  )
  total_diff <- sum(t$diff)
  if (abs(total_diff) < 1e-12) total_diff <- 0
  shown <- rbind(shown, c(
    "Сумма", .fmt(sum(t$d0), digits), .fmt(sum(t$d1), digits), .fmt_signed(total_diff, digits),
    .fmt(sum(t$diff_sq), digits), .fmt(sum(t$d0_sq), digits), .fmt(sum(t$d1_sq), digits),
    .fmt(sum(t$contribution), 1)
  ))
  names(shown) <- c("Элемент", "d0", "d1", "d1 − d0", "(d1 − d0)²", "d0²", "d1²", "Вклад, %")
  print(shown, row.names = FALSE, right = TRUE)

  cat("\nПоказатели:\n")
  ind <- x$indices
  w <- max(nchar(ind$name))
  for (i in seq_len(nrow(ind))) {
    cat("  ", formatC(ind$name[i], width = -w), "  ", .fmt(ind$value[i], digits),
        "  [", ind$range[i], "]\n", sep = "")
  }
  cat("\nОценка по шкале Рябцева (I_R = ", .fmt(x$ryabtsev, 3), "): ",
      x$interpretation, ".\n", sep = "")
  top <- t$element[which.max(t$contribution)]
  noun <- if (x$type == "shift") "сдвиг" else "различие"
  if (sum(t$diff_sq) > 0) {
    cat("Наибольший вклад в ", noun, ": ", top, " (",
        .fmt(max(t$contribution), 1), " % суммы квадратов разностей).\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.struct_shift <- function(object, ...) {
  out <- object$indices[c("name", "value", "range")]
  names(out) <- c("Показатель", "Значение", "Границы")
  out
}

#' @export
as.data.frame.struct_shift <- function(x, ...) x$table

#' График структур
#'
#' Горизонтальная столбчатая диаграмма долей элементов в двух структурах.
#' Чтобы сохранить график с русскими подписями в PDF, используйте
#' [grDevices::cairo_pdf()] вместо `pdf()`: стандартное PDF-устройство
#' не поддерживает кириллицу.
#'
#' @param x Результат [struct_shift()].
#' @param main Заголовок; по умолчанию — значения K и I_R.
#' @param col Цвета столбцов для двух структур.
#' @param ... Передаётся в [graphics::barplot()].
#' @return `x` (невидимо).
#' @export
#' @examples
#' res <- struct_shift(export_firm_n)
#' if (interactive()) plot(res)
plot.struct_shift <- function(x, main = NULL, col = c("#9DB4C0", "#1F5F8B"), ...) {
  m <- rbind(x$table$d0, x$table$d1) * 100
  colnames(m) <- x$table$element
  if (is.null(main)) {
    main <- sprintf("K = %.3f (Гатев),  I_R = %.3f (Рябцев)", x$gatev, x$ryabtsev)
  }
  old <- graphics::par(mar = c(5, max(4, 0.5 * max(nchar(colnames(m)))), 3, 1))
  on.exit(graphics::par(old))
  graphics::barplot(m, beside = TRUE, horiz = TRUE, las = 1, col = col,
                    xlab = "Доля, %", main = main, legend.text = x$labels,
                    args.legend = list(x = "right", bty = "n", inset = 0.02), ...)
  invisible(x)
}

#' Матрица попарных структурных различий
#'
#' Сравнивает каждую структуру с каждой (периоды, регионы, предприятия)
#' выбранным показателем. Удобно для рядов динамики структуры и для
#' сравнения нескольких объектов.
#'
#' @param data `data.frame` или матрица: строки — элементы структуры,
#'   колонки — сравниваемые структуры. Нечисловые колонки считаются
#'   названиями элементов и пропускаются.
#' @param index Показатель: `"gatev"`, `"ryabtsev"`, `"szalai"` или
#'   `"dissimilarity"` (нормированный индекс различия \eqn{I_S^*}).
#' @param chain Если `TRUE`, вместо матрицы возвращаются цепные сравнения
#'   соседних колонок (2 с 1, 3 с 2, …) — сдвиги за каждый период.
#' @return Симметричная матрица или `data.frame` цепных сдвигов.
#' @export
#' @examples
#' shift_matrix(tec_costs)
#' shift_matrix(tec_costs[c("element", "stec_2008", "stec_2018")], chain = TRUE)
shift_matrix <- function(data, index = c("gatev", "ryabtsev", "szalai", "dissimilarity"),
                         chain = FALSE) {
  index <- match.arg(index)
  data <- as.data.frame(data, check.names = FALSE)
  num <- vapply(data, is.numeric, logical(1))
  if (sum(num) < 2L) stop("Нужны хотя бы две числовые колонки (структуры).", call. = FALSE)
  structs <- data[num]
  fun <- switch(index,
    gatev = .gatev,
    ryabtsev = .ryabtsev,
    szalai = function(d0, d1) .szalai(d0, d1, seq_along(d0)),
    dissimilarity = function(d0, d1) sum(abs(d1 - d0)) / 2
  )
  shares <- lapply(names(structs), function(nm) {
    x <- .check_values(structs[[nm]], nm)
    x / sum(x)
  })
  names(shares) <- names(structs)
  n <- length(shares)
  if (chain) {
    return(data.frame(
      from = names(shares)[-n], to = names(shares)[-1L],
      value = vapply(seq_len(n - 1L), function(i) fun(shares[[i]], shares[[i + 1L]]), numeric(1)),
      stringsAsFactors = FALSE
    ))
  }
  m <- matrix(0, n, n, dimnames = list(names(shares), names(shares)))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i < j) m[i, j] <- m[j, i] <- fun(shares[[i]], shares[[j]])
  }
  m
}
