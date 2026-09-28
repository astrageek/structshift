d0 <- c(0.30, 0.40, 0.20, 0.10)
d1 <- c(0.60, 0.20, 0.15, 0.05)

test_that("пример Гатевых (табл. 9.1) совпадает с учебником", {
  res <- struct_shift(export_firm_n)
  val <- stats::setNames(res$indices$value, res$indices$id)
  expect_equal(round(val[["I_S_norm"]], 2), 0.30)
  expect_equal(round(val[["gatev_sigma_norm"]], 2), 0.26)
  expect_equal(round(val[["gatev_K"]], 2), 0.43)
  expect_equal(sum(res$table$diff_sq), 0.135)
  expect_equal(sum(res$table$d0_sq), 0.300)
  expect_equal(sum(res$table$d1_sq), 0.425)
})

test_that("формулы Гатева 9.12 и 9.13 эквивалентны", {
  alt <- sqrt(1 - 2 * sum(d0 * d1) / (sum(d0^2) + sum(d1^2)))
  expect_equal(gatev_index(d0, d1), alt)
})

test_that("индекс Рябцева считается по формуле", {
  expect_equal(ryabtsev_index(d0, d1), sqrt(0.135 / sum((d0 + d1)^2)))
  expect_equal(attr(ryabtsev_index(d0, d1, interpret = TRUE), "interpretation"),
               "значительный уровень различия структур")
})

test_that("одинаковые структуры дают ноль, противоположные — единицу", {
  expect_equal(gatev_index(d0, d0), 0)
  expect_equal(ryabtsev_index(d0, d0), 0)
  expect_equal(szalai_index(d0, d0), 0)
  expect_equal(gatev_index(c(0, 1), c(1, 0)), 1)
  expect_equal(ryabtsev_index(c(0, 1), c(1, 0)), 1)
  expect_equal(szalai_index(c(0, 1), c(1, 0)), 1)
})

test_that("показатели симметричны и лежат в [0, 1]", {
  set.seed(1)
  for (i in 1:50) {
    a <- runif(6); b <- runif(6)
    for (f in list(gatev_index, ryabtsev_index, szalai_index)) {
      v <- f(a, b)
      expect_true(v >= 0 && v <= 1)
      expect_equal(v, f(b, a))
    }
  }
})

test_that("проценты, доли и абсолютные значения дают одно и то же", {
  expect_equal(gatev_index(d0 * 100, d1 * 100), gatev_index(d0, d1))
  expect_equal(gatev_index(d0 * 537, d1 * 12), gatev_index(d0, d1))
})

test_that("элементы сопоставляются по названиям", {
  a <- c(x = 0.5, y = 0.3, z = 0.2)
  b <- c(z = 0.1, x = 0.6, y = 0.3)
  expect_equal(gatev_index(a, b), gatev_index(c(0.5, 0.3, 0.2), c(0.6, 0.3, 0.1)))
  expect_error(gatev_index(a, c(x = 1, y = 2, w = 3)), "различается")
})

test_that("коэффициенты Казинца совпадают с формулами 9.6–9.9", {
  k <- kazinets(d0, d1)
  expect_equal(k[["linear_abs"]], 0.60 / 4)
  expect_equal(k[["quadratic_abs"]], sqrt(0.135 / 4))
  expect_equal(k[["linear_rel"]], sum(abs(d1 / d0 - 1) * d0))
  expect_equal(k[["quadratic_rel"]], sqrt(sum(d1^2 / d0) - 1))
})

test_that("нулевая базисная доля обрабатывается", {
  expect_warning(k <- kazinets(c(0, 0.5, 0.5), c(0.2, 0.4, 0.4)), "не определён")
  expect_true(is.na(k[["quadratic_rel"]]))
  expect_error(szalai_index(c(0, 0.5, 0.5), c(0, 0.6, 0.4)), "не определён")
})

test_that("шкала Рябцева", {
  expect_equal(nrow(ryabtsev_scale()), 8L)
  expect_equal(ryabtsev_scale(c(0, 0.03, 0.0305, 0.07, 0.2, 0.95, 1)), c(
    "тождественность структур",
    "тождественность структур",
    "весьма низкий уровень различия структур",
    "весьма низкий уровень различия структур",
    "существенный уровень различия структур",
    "полная противоположность структур",
    "полная противоположность структур"
  ))
  expect_error(ryabtsev_scale(1.2))
})

test_that("разные форматы входных данных", {
  df <- data.frame(статья = c("a", "b", "c"), базис = c(10, 20, 70), отчёт = c(20, 20, 60))
  expect_equal(gatev_index(df), gatev_index(c(10, 20, 70), c(20, 20, 60)))
  df2 <- data.frame(name = c("a", "b", "c"), y2020 = c(10, 20, 70), y2021 = c(20, 20, 60))
  expect_equal(gatev_index(df2), gatev_index(df))
  expect_error(gatev_index(c(1, 2), c(1, 2, 3)), "разное число")
  expect_error(gatev_index(c(-1, 2), c(1, 2)), "отрицательные")
  expect_error(gatev_index(c(NA, 2), c(1, 2)), "NA")
  expect_error(gatev_index(c(0, 0), c(1, 2)), "равна нулю")
})

test_that("struct_shift печатается и рисуется", {
  res <- struct_shift(tec_costs$stec_2008, tec_costs$stec_2018, labels = c("2008", "2018"))
  expect_s3_class(res, "struct_shift")
  expect_output(print(res), "Индекс Рябцева")
  expect_output(print(res), "шкале Рябцева")
  expect_equal(nrow(summary(res)), 10L)
  expect_equal(sum(res$table$contribution), 100)
  expect_output(print(struct_shift(export_firm_n, type = "difference")), "K_D")
})

test_that("plot рисует график", {
  # На CI-машинах графические устройства часто работают без поддержки кириллицы.
  skip_on_ci()
  skip_on_cran()
  skip_if_not(capabilities("cairo"))
  res <- struct_shift(export_firm_n)
  grDevices::png(tempfile(fileext = ".png"), type = "cairo")
  on.exit(grDevices::dev.off())
  expect_no_error(suppressWarnings(plot(res)))
})

test_that("shift_matrix", {
  m <- shift_matrix(tec_costs)
  expect_equal(dim(m), c(6L, 6L))
  expect_equal(unname(diag(m)), rep(0, 6))
  expect_equal(m, t(m))
  expect_equal(m["stec_2008", "stec_2018"],
               gatev_index(tec_costs$stec_2008, tec_costs$stec_2018))
  ch <- shift_matrix(tec_costs[c("element", "stec_2008", "stec_2018")],
                     index = "ryabtsev", chain = TRUE)
  expect_equal(ch$value, ryabtsev_index(tec_costs$stec_2008, tec_costs$stec_2018))
})
