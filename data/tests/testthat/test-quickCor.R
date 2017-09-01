context("quickCor")
  test_that("quickCor exports", {
    ex <- quickCor(x = "mpg", y = "hp", dat = mtc_bis,
                   nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE)
    expect_that(ex$coeff, is_a("summary.lm"))
    expect_that(ex$result, is_a("matrix"))
  })


  # test_that("quickCor results", {
  #   ex <- quickCor(x = "mpg", y = "am", dat = mtc_bis,
  #                  nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE)
  #   expect_error(ex)
  #
  # })
