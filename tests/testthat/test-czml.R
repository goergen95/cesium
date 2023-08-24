test_that("set_interval() works", {
  # check wrong inputs
  expect_error(set_interval(1, simt(1)), "length <2")
  expect_error(set_interval(1, simt(3)), "length <2")
  expect_error(set_interval(1:3, simt(1)), "length <2")
  expect_error(set_interval(1:3, simt(4)), "equal lengths")

  # vector input
  n <- 4
  timesteps <- simt(n)
  timesteps_ps <- as.POSIXct(timesteps, format = .pkgenv$time_format)
  values <- 1:n
  out <- set_interval(values, timesteps, name = "double")
  expect_equal(class(out), "list")
  expect_equal(length(out), n - 1)
  names <- c(sapply(out, names))
  expect_equal(names, rep(c("interval", "double"), n - 1))
  # list input
  values <- as.list(values)
  out <- set_interval(values, timesteps, name = "double")
  expect_equal(class(out), "list")
  expect_equal(length(out), n - 1)
  names <- c(sapply(out, names))
  expect_equal(names, rep(c("interval", "double"), n - 1))
  # check types and values
  interval <- sapply(out, \(x) x$interval)
  values_out <- sapply(out, \(x) x$double)
  expect_true(inherits(interval, "character"))
  expect_true(inherits(values_out, "integer"))
  expect_equal(values_out, c(4,3,2))
  out <- set_interval(values, timesteps_ps, name = "double")
  interval <- sapply(out, \(x) x$interval)
  expect_true(inherits(interval, "character"))
  expect_equal(set_interval(NULL), NULL)
})


test_that("set_temporal_property() works", {
  n <- 4
  values <- 1:n
  timesteps <- simt(n)
  timesteps_ps <- as.POSIXct(timesteps, format = .pkgenv$time_format)
  expect_equal(set_temporal_property(NULL), NULL)
  expect_error(set_temporal_property(x[1:2], timesteps))
  out1 <- set_temporal_property(values, timesteps)
  out2 <- set_temporal_property(values, timesteps_ps)
  expect_equal(length(out1), n * 2)
  expect_equal(sapply(out1, class), rep(c("character", "integer"), n))
  expect_equal(sapply(out2, class), rep(c("character", "integer"), n))
})


test_that("czml_color() works", {
  n <- 4
  expect_equal(czml_color(NULL), NULL)
  expect_equal(czml_color("white"), list(rgba = c(255, 255, 255, 255)))
  expect_equal(czml_color("white", alpha = 0), list(rgba = c(255, 255, 255, 0)))
  expect_equal(czml_color(c("white", "white")), list(rgba = c(255, 255, 255, 255)))
  expect_error(czml_color(c("white", "red")), "no time variable is present")
  timesteps <- simt(4)
  color <- rep("white", n - 1)
  expect_error(czml_color(color = color, 1, timesteps), "not match supplied timesteps")
  out <- czml_color(color, 1, timesteps[1:(n-1)])
  expect_equal(names(out[[1]]), c("interval", "rgba"))
  out <- czml_color(color, 1, timesteps[1:(n-1)], interpolation = interpolation_options())
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "rgba"))
  out <- out$rgba
  expect_equal(length(out), (n-1) * 5)
  expect_equal(sapply(out, class), rep(c("character", rep("numeric", 4)), n-1))
})

test_that("czml_string() works", {
  expect_equal(czml_string(NULL), NULL)
  expect_error(czml_string(1), "string must be of type character")
  expect_equal(czml_string("a"), "a")
  expect_equal(czml_string(c("a", "a")), "a")
  expect_error(czml_string(c("a", "b")), "More than one string specified, but no time variable is present")
  out <- czml_string(c("a", "b"), simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "string"))
  expect_equal(out$string, "b")
})

test_that("czml_cartesian3() works", {
  expect_equal(czml_cartesian3(x=1, y=2), NULL)
  expect_error(czml_cartesian3(x=1, y=2, z=1:2), "need to be supplied with equal length")
  expect_equal(czml_cartesian3(x=1,y=2,z=3), list(cartesian = list(1,2,3)))
  expect_error(czml_cartesian3(x=1:2,y=3:4,z=5:6), "no time variable is present")
  expect_error(czml_cartesian3(x=1:2,y=3:4,z=5:6, timesteps = simt(3)), "not match supplied timesteps")
  out <- czml_cartesian3(x=1:2,y=3:4,z=5:6, simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "cartesian"))
  out <- czml_cartesian3(x=1:2,y=3:4,z=5:6, simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "cartesian"))
  expect_equal(length(out$cartesian), 8)
})


test_that("czml_bounding_rectangle() works", {
  expect_equal(czml_bounding_rectangle(x=1, y=2, width=3, height=NULL), NULL)
  czml_bounding_rectangle(x=1, y=2, width=3, height=NULL)
  expect_equal(czml_bounding_rectangle(x=1, y=2, width=3, height=4), list(boundingRectangle = 1:4))
  expect_error(czml_bounding_rectangle(x=1:2, y=3:4, width=5:6, height=7:8), "no time variable")
  expect_error(czml_bounding_rectangle(x=1:2, y=3:4, width=5:6, height=7:8, simt(1)), "does not match")
  out <- czml_bounding_rectangle(x=1:2, y=3:4, width=5:6, height=7:8, simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "boundingRectangle"))
  out <- czml_bounding_rectangle(x=1:2, y=3:4, width=5:6, height=7:8, simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "boundingRectangle"))
  expect_equal(length(out$boundingRectangle), 10)
})


test_that("czml_cartesian2() works", {
  expect_equal(czml_cartesian2(x=1, y=NULL), NULL)
  expect_error(czml_cartesian2(x=1, y=1:2), "need to be supplied with equal length")
  expect_equal(czml_cartesian2(x=1,y=2), list(cartesian2 = list(1,2)))
  expect_error(czml_cartesian2(x=1:2,y=3:4), "no time variable is present")
  expect_error(czml_cartesian2(x=1:2,y=3:4, timesteps = simt(3)), "not match supplied timesteps")
  out <- czml_cartesian2(x=1:2,y=3:4, simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "cartesian2"))
  out <- czml_cartesian2(x=1:2,y=3:4, simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "cartesian2"))
  expect_equal(length(out$cartesian2), 6)
})


test_that("czml_url() works", {
  expect_equal(czml_url(NULL), NULL)
  expect_error(czml_url(1), "url must be of type character")
  expect_equal(czml_url("a"), list(uri = "a"))
  expect_equal(czml_url(c("a", "a")), list(uri = "a"))
  expect_error(czml_url(c("a", "b")), "no time variable")
  out <- czml_url(c("a", "b"), simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "uri"))
  expect_equal(out$uri, "b")
})


test_that("czml_double() works", {
  expect_equal(czml_double(NULL), NULL)
  expect_error(czml_double(1L), "numeric")
  expect_error(czml_double("A"), "numeric")
  expect_equal(czml_double(1), 1)
  expect_error(czml_double(c(1,2)), "no time variable")
  expect_error(czml_double(c(1,2), simt(1)), "does not match")
  out <- czml_double(c(1,2), simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "number"))
  expect_equal(out$number, 2)
  out <- czml_double(c(1,2), simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "number"))
  expect_equal(length(out$number), 4)
})


test_that("czml_integer() works", {
  expect_equal(czml_integer(NULL), NULL)
  expect_error(czml_integer("A"), "integer")
  expect_equal(czml_integer(1), 1)
  expect_error(czml_integer(c(1,2)), "no time variable")
  expect_error(czml_integer(c(1,2), simt(1)), "does not match")
  out <- czml_integer(c(1,2), simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "number"))
  expect_equal(out$number, 2)
  out <- czml_integer(c(1,2), simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "number"))
  expect_equal(length(out$number), 4)
})


test_that("czml_near_far_scalar() works", {
  expect_equal(czml_near_far_scalar(10, 1, 20, NULL), NULL)
  expect_error(czml_near_far_scalar(10, 1, 20, 2:3), "equal length")
  expect_equal(czml_near_far_scalar(10, 1, 20, 2), list(nearFarScalar = list(10,1,20,2)))
  expect_error(czml_near_far_scalar(10:11, 1:2, 20:21, 2:3), "no time variable")
  expect_error(czml_near_far_scalar(10:11, 1:2, 20:21, 2:3, simt(1)), "does not match")
  out <- czml_near_far_scalar(10:11, 1:2, 20:21, 2:3, simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "nearFarScalar"))
  expect_equal(out$nearFarScalar,c(11,2,21,3))
  out <- czml_near_far_scalar(10:11, 1:2, 20:21, 2:3, simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "nearFarScalar"))
  expect_equal(length(out$nearFarScalar), 10)
})


test_that("czml_dist_display_cond() works", {
  expect_equal(czml_dist_display_cond(1, NULL), NULL)
  expect_error(czml_dist_display_cond(1, 2:3), "equal length")
  expect_equal(czml_dist_display_cond(1, 2), list(distanceDisplayCondition = list(1,2)))
  expect_error(czml_dist_display_cond(1:2, 2:3), "no time variable")
  expect_error(czml_dist_display_cond(1:2, 2:3, simt(1)), "does not match")
  out <- czml_dist_display_cond(1:2, 2:3, simt(2))
  expect_equal(class(out), "list")
  out <- out[[1]]
  expect_equal(length(out), 2)
  expect_equal(names(out), c("interval", "distanceDisplayCondition"))
  expect_equal(out$distanceDisplayCondition, c(2,3))
  out <- czml_dist_display_cond(1:2, 2:3, simt(2), interpolation_options())
  expect_equal(class(out), "list")
  expect_equal(length(out), 4)
  expect_equal(names(out), c("algorithm", "forward_type", "backward_type", "distanceDisplayCondition"))
  expect_equal(length(out$distanceDisplayCondition), 6)
})
