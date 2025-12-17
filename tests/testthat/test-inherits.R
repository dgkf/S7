test_that("it works", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", parent = foo1)

  expect_true(S7_inherits(foo1(), NULL))
  expect_true(S7_inherits(foo1(), foo1))
  expect_true(S7_inherits(foo2(), foo1))
  expect_false(S7_inherits(foo1(), foo2))
  expect_false(S7_inherits(1, NULL))
})

test_that("checks that input is a class", {
  expect_snapshot(S7_inherits(1:10, "x"), error = TRUE)
})

test_that("throws informative error", {
  expect_snapshot(error = TRUE, {
    foo1 <- new_class("foo1", package = NULL)
    foo2 <- new_class("foo2", package = NULL)
    check_is_S7(foo1(), foo2)
  })
  expect_snapshot(check_is_S7("a"), error = TRUE)
})

test_that("property defaults and setters can be masked", {
  parent <- new_class(
    "parent",
    properties = list(
      "x" = class_integer,
      "y" = new_property(
        class_any,
        setter = function(self, value) {
          self@y <- paste("parent", format(value))
          self
        }
      )
    )
  )

  child <- new_class(
    "child",
    parent = parent,
    properties = list(
      "y" = new_property(
        class_double,
        default = 10,
        setter = function(self, value) {
          self@y <- value * 2
          self
        }
      )
    )
  )

  expect_equal(child()@y, 20)
  expect_equal(child(y = 2)@y, 4)
  expect_equal(parent()@y, "parent NULL")
  expect_equal(parent(y = 2)@y, "parent 2")
})
