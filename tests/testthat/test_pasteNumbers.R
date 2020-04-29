library(pasteNumbers)
context("test all")

test_that("null clipboard", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(NULL), expected="")
})

test_that("empty string", {
    expect_equal(object=pasteNumbers::splitIntoNumbers(""), expected="")
})

test_that("zero", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("0"), expected="0")
})

test_that("integers", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("1"), expected="1")
    expect_equal(object=pasteNumbers::splitIntoNumbers("2147483647"), expected="2147483647")
})

test_that("floating numbers", {
    expect_equal(object=pasteNumbers::splitIntoNumbers("3.25"), expected="3.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-3.25"), expected="-3.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("0.25"), expected="0.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-0.25"), expected="-0.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers(".25"), expected=".25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-.25"), expected="-.25")
    expect_equal(object=pasteNumbers::splitIntoNumbers("3."), expected="3")
    expect_equal(object=pasteNumbers::splitIntoNumbers("-3."), expected="-3")
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

test_that("long strings", {
    input <- stringr::str_c(rep("123", 1024), collapse=" ")
    expected <- stringr::str_c(rep("123", 1024 / 4), collapse=" ")
    expect_equal(object=pasteNumbers::splitIntoNumbers(input), expected=expected)
})
