# adopted from https://github.com/rstudio/leaflet/blob/main/R/utils.R
# Distributed under GPL-3 (GNU GENERAL PUBLIC
# LICENSE version 3).

meta_data <- function(obj) return(obj)

dispatch <- function(
    globe,
    func_name,
    cesium = stop(paste(func_name, "requires a globe proxy object")),
    cesium_proxy = stop(paste(func_name, "does not support globe proxy objects"))
) {
  if (inherits(globe, "cesium"))
    return(cesium)
  else if (inherits(globe, "cesium_proxy"))
    return(cesium_proxy)
  else
    stop("Invalid globe parameter")
}



invoke_method <- function(globe, data, method, ...) {
  if (crosstalk::is.SharedData(data)) {
    globe$dependencies <- c(globe$dependencies, crosstalk::crosstalkLibs())
    data <- data$data()
  } else {
    NULL
  }

  args <- eval_formula(list(...), data)

  dispatch(globe,
           method,
           cesium = {
             x <- globe$x$calls
             if (is.null(x)) x <- list()
             n <- length(x)
             x[[n + 1]] <- list(method = method, args = args)
             globe$x$calls <- x
             globe
           },
           cesium_proxy = {
             invoke_remote(globe, method, args)
             globe
           }
  )
}


cesium_proxy <- function(globe_id, session = shiny::getDefaultReactiveDomain(),
                         data = NULL, deferUntilFlush = TRUE) {

  if (is.null(session)) {
    stop("cesium_proxy must be called from the server function of a Shiny app")
  }

  # If this is a new enough version of Shiny that it supports modules, and
  # we're in a module (nzchar(session$ns(NULL))), and the globe_id doesn't begin
  # with the current namespace, then add the namespace.
  #
  # We could also have unconditionally done `globe_id <- session$ns(globe_id)`, but
  # older versions of cesium would have broken unless the user did session$ns
  # themselves, and we hate to break their code unnecessarily.
  #
  # This won't be necessary in future versions of Shiny, as session$ns (and
  # other forms of ns()) will be smart enough to only namespace un-namespaced
  # IDs.
  if (
    !is.null(session$ns) &&
    nzchar(session$ns(NULL)) &&
    substring(globe_id, 1, nchar(session$ns(""))) != session$ns("")
  ) {
    globe_id <- session$ns(globe_id)
  }

  structure(
    list(
      session = session,
      id = globe_id,
      x = structure(
        list(),
        cesiumData = data
      ),
      deferUntilFlush = deferUntilFlush,
      dependencies = NULL
    ),
    class = "cesium_proxy"
  )
}

# Shiny versions <= 0.12.0.9001 can't guarantee that onFlushed
# callbacks are called in the order they were registered. Rather
# than wait for this to be fixed in Shiny and released to CRAN,
# work around this for older versions by maintaining our own
# queue of work items. The names in this environment are session
# tokens, and the values are lists of invoke_remote msg objects.
# During the course of execution, cesium_proxy() should cause
# deferred messages to be appended to the appropriate value in
# session_flush_queue. It's the responsibility of invoke_remote to
# ensure that the session_flush_queue values are properly reaped
# as soon as possible, to prevent session objects from being
# leaked.
#
# When Shiny >0.12.0 goes to CRAN, we should update our version
# dependency and remove this entire mechanism.
session_flush_queue <- new.env(parent = emptyenv())

invoke_remote <- function(globe, method, args = list()) {
  if (!inherits(globe, "cesium_proxy"))
    stop("Invalid globe parameter; globe proxy object was expected")

  deps <- htmltools::resolveDependencies(globe$dependencies)

  msg <- list(
    id = globe$id,
    calls = list(
      list(
        dependencies = lapply(deps, shiny::createWebDependency),
        method = method,
        args = args,
        evals = htmlwidgets::JSEvals(args)
      )
    )
  )

  sess <- globe$session
  if (globe$deferUntilFlush) {
    if (packageVersion("shiny") < "0.12.1.9000") {

      # See comment on session_flush_queue.

      if (is.null(session_flush_queue[[sess$token]])) {
        # If the current session doesn't have an entry in the session_flush_queue,
        # initialize it with a blank list.
        session_flush_queue[[sess$token]] <- list()

        # If the session ends before the next onFlushed call, remove the entry
        # for this session from the session_flush_queue.
        endedUnreg <- sess$onSessionEnded(function() {
          rm(list = sess$token, envir = session_flush_queue)
        })

        # On the next flush, pass all the messages to the client, and remove the
        # entry from session_flush_queue.
        sess$onFlushed(function() {
          on.exit(rm(list = sess$token, envir = session_flush_queue), add = TRUE)
          endedUnreg()
          for (msg in session_flush_queue[[sess$token]]) {
            sess$sendCustomMessage("cesium-calls", msg)
          }
        }, once = TRUE) # nolint
      }

      # Append the current value to the apporpriate session_flush_queue entry,
      # which is now guaranteed to exist.
      session_flush_queue[[sess$token]] <- c(session_flush_queue[[sess$token]], list(msg))

    } else {
      sess$onFlushed(function() {
        sess$sendCustomMessage("cesium-calls", msg)
      }, once = TRUE) # nolint
    }
  } else {
    sess$sendCustomMessage("cesium-calls", msg)
  }
  globe
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Evaluate list members that are formulae, using the map data as the environment
#' (if provided, otherwise the formula environment)
#' @param list with members as formulae
#' @param data map data
#' @export
eval_formula <- function(list, data) {
  evalAll <- function(x) {
    if (is.list(x)) {
      # Use `x[] <-` so attributes on x are preserved
      x[] <- lapply(x, evalAll)
      x
    } else {
      resolve_formula(x, data)
    }
  }
  evalAll(list)
}

resolve_formula <- function(f, data) {
  if (!inherits(f, "formula")) return(f)
  if (length(f) != 2L) stop("Unexpected two-sided formula: ", deparse(f))

  eval(f[[2]], meta_data(data), environment(f))
}
