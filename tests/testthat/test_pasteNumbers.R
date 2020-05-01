library(pasteNumbers)
context("test all")

test_that("null clipboard", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(NULL), expected="")
})

test_that("NA clipboard", {
    expect_true(object=is.na(pasteNumbers::splitIntoNumbers(NA)))
})

test_that("empty string set", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(character()), expected="")
})

test_that("Multi strings", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(c("1.", "2", "3.5")), expected="1 2 3.5")
})

test_that("empty string", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(""), expected="")
})

test_that("zero", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("0"), expected="0")
})

test_that("integers", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("3"), expected="3")
    expect_equal(object=pasteNumbers::splitIntoNumbers("2147483647"), expected="2147483647")
})

test_that("floating numbers", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("3."), expected="3")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-3."), expected="-3")
    expect_equal(object=pasteNumbers::splitIntoNumbers("3.25"), expected="3.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-3.25"), expected="-3.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("0.25"), expected="0.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-0.25"), expected="-0.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers(".25"), expected=".25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-.25"), expected="-.25")
})

test_that("commas", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("1,234"), expected="1234")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1,234"), expected="-1234")
    expect_equal(object=pasteNumbers::splitIntoNumbers("1,234.5"), expected="1234.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1,234.5"), expected="-1234.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-123,456,789.25"), expected="-123456789.25")
})

test_that("multile numbers", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("1 -2.5"), expected="1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("1 -2.5"), expected="1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1 -2.5"), expected="-1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1*-2.5"), expected="-1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1=-2.5"), expected="-1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-1  -2.5"), expected="-1 -2.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers("1,234.5-6.75"), expected="1234.5 -6.75")
    expect_equal(object=pasteNumbers::splitIntoNumbers("1,234.5- 6.75"), expected="1234.5 6.75")
})

test_that("strip strings", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(" 1 2 "), expected="1 2")
    expect_equal(object=pasteNumbers::splitIntoNumbers(" -1.5 2. "), expected="-1.5 2")
})

test_that("non-number characters", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(">1x2..3$"), expected="1 2 .3")
    expect_equal(object=pasteNumbers::splitIntoNumbers("---3---4---5+"), expected="-3 -4 -5")
})

test_that("cut strings", {
    input <- "1,234.5"
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=0), expected="")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=1), expected="1")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=2), expected="1")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=3), expected="12")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=4), expected="123")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=5), expected="1234")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=6), expected="1234")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=7), expected="1234.5")
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=-1), expected="1234.5")
})

test_that("long strings", {
    pattern <- "123"
    seperator <- " "
    unit_length <- stringr::str_length(stringr::str_c(c(pattern, seperator), collapse=""))

    n_unit <- 48
    input <- stringr::str_c(rep(pattern, n_unit), collapse=seperator)
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input), expected=input)

    n_unit <- 7
    expected <- stringr::str_c(rep(pattern, n_unit), collapse=seperator)
    expected_len <- stringr::str_length(expected)
    expect_equal(object=pasteNumbers::splitIntoNumbers(x=input, end_pos=expected_len),
                 expected=expected)
})
