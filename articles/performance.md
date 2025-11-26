# Performance

``` r
library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r
Text <- new_class("Text", parent = class_character)
Number <- new_class("Number", parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 <- new_generic("foo_S7", "x")
method(foo_S7, Text) <- function(x, ...) paste0(x, "-foo")

foo_S3 <- function(x, ...) {
  UseMethod("foo_S3")
}

foo_S3.Text <- function(x, ...) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("Number", "numeric", "S7_object"))
setOldClass(c("Text", "character", "S7_object"))

setGeneric("foo_S4", function(x, ...) standardGeneric("foo_S4"))
#> [1] "foo_S4"
setMethod("foo_S4", c("Text"), function(x, ...) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_S7(x), foo_S3(x), foo_S4(x))
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_S7(x)     7.2µs   8.42µs   110713.    18.2KB     22.1
#> 2 foo_S3(x)    2.56µs   2.85µs   317239.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.13µs   306840.        0B     30.7

bar_S7 <- new_generic("bar_S7", c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)   13.5µs  14.92µs    64428.        0B     25.8
#> 2 bar_S4(x, y)    7.2µs   8.06µs   120997.        0B     24.2
```

A potential optimization is caching based on the class names, but lookup
should be fast without this.

The following benchmark generates a class hierarchy of different levels
and lengths of class names and compares the time to dispatch on the
first class in the hierarchy vs the time to dispatch on the last class.

We find that even in very extreme cases (e.g. 100 deep hierarchy 100 of
character class names) the overhead is reasonable, and for more
reasonable cases (e.g. 10 deep hierarchy of 15 character class names)
the overhead is basically negligible.

``` r
library(S7)

gen_character <- function (n, min = 5, max = 25, values = c(letters, LETTERS, 0:9)) {
  lengths <- sample(min:max, replace = TRUE, size = n)
  values <- sample(values, sum(lengths), replace = TRUE)
  starts <- c(1, cumsum(lengths)[-n] + 1)
  ends <- cumsum(lengths)
  mapply(function(start, end) paste0(values[start:end], collapse=""), starts, ends)
}

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 <- new_generic("foo_S7", "x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", "x")
    method(foo2_S7, S7_object) <- function(x, ...) paste0(x, "-foo")

    bench::mark(
      best = foo_S7(x),
      worst = foo2_S7(x)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   7.58µs   8.79µs   110160.        0B     33.1
#>  2 worst                3          15   7.67µs   8.96µs   108751.        0B     21.8
#>  3 best                 5          15   7.46µs   8.79µs   110243.        0B     22.1
#>  4 worst                5          15   7.71µs   9.04µs   107526.        0B     32.3
#>  5 best                10          15   7.55µs   8.81µs   109717.        0B     32.9
#>  6 worst               10          15   7.97µs   9.25µs   104373.        0B     20.9
#>  7 best                50          15   8.15µs    9.4µs   103272.        0B     31.0
#>  8 worst               50          15   9.56µs  10.98µs    88495.        0B     17.7
#>  9 best               100          15   8.63µs  10.03µs    96593.        0B     29.0
#> 10 worst              100          15  11.89µs   13.4µs    72355.        0B     21.7
#> 11 best                 3         100   7.64µs   8.97µs   107035.        0B     32.1
#> 12 worst                3         100   7.88µs   9.28µs   104311.        0B     20.9
#> 13 best                 5         100   7.71µs   9.08µs   106749.        0B     21.4
#> 14 worst                5         100   8.12µs    9.5µs   101760.        0B     30.5
#> 15 best                10         100   7.59µs   9.03µs   106319.        0B     21.3
#> 16 worst               10         100    8.4µs   9.75µs    97727.        0B     29.3
#> 17 best                50         100      8µs   9.39µs   102374.        0B     20.5
#> 18 worst               50         100   13.1µs  14.52µs    66192.        0B     19.9
#> 19 best               100         100   8.91µs  10.41µs    91694.        0B     27.5
#> 20 worst              100         100  19.33µs  20.87µs    46485.        0B     13.9
```

And the same benchmark using double-dispatch

``` r
bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))
    y <- do.call(cls, list("ho"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 <- new_generic("foo_S7", c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", c("x", "y"))
    method(foo2_S7, list(S7_object, S7_object)) <- function(x, y, ...) paste0(x, y, "-foo")

    bench::mark(
      best = foo_S7(x, y),
      worst = foo2_S7(x, y)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15    9.3µs   10.9µs    87753.        0B     26.3
#>  2 worst                3          15   9.49µs   11.1µs    85100.        0B     34.1
#>  3 best                 5          15   9.49µs     11µs    86639.        0B     26.0
#>  4 worst                5          15   9.95µs   11.3µs    84025.        0B     25.2
#>  5 best                10          15   9.59µs   11.3µs    84652.        0B     25.4
#>  6 worst               10          15  10.49µs   12.1µs    79106.        0B     23.7
#>  7 best                50          15  10.34µs   11.3µs    84577.        0B     25.4
#>  8 worst               50          15  13.38µs   14.3µs    68491.        0B     20.6
#>  9 best               100          15   11.7µs   12.5µs    77838.        0B     31.1
#> 10 worst              100          15  17.94µs   18.9µs    51900.        0B     20.8
#> 11 best                 3         100   9.53µs   10.5µs    92041.        0B     36.8
#> 12 worst                3         100  10.52µs   11.7µs    82017.        0B     32.8
#> 13 best                 5         100   9.62µs   10.8µs    89274.        0B     26.8
#> 14 worst                5         100   10.4µs   11.7µs    82477.        0B     24.8
#> 15 best                10         100   9.57µs   10.8µs    88442.        0B     35.4
#> 16 worst               10         100  11.73µs     13µs    74289.        0B     22.3
#> 17 best                50         100  11.02µs   12.3µs    77447.        0B     31.0
#> 18 worst               50         100  19.68µs   21.2µs    45769.        0B     13.7
#> 19 best               100         100  12.08µs   13.4µs    71410.        0B     28.6
#> 20 worst              100         100  30.67µs   32.1µs    30335.        0B     12.1
```
