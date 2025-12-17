new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)
  arg_info <- constructor_args(parent, properties, envir, package)

  parent_args <- as_names(names(arg_info$parent), named = TRUE)
  self_args <- as_names(names(arg_info$self), named = TRUE)

  obj_call <- if (has_S7_symbols(envir, "new_object")) {
    "new_object"
  } else {
    c("S7", "new_object")
  }

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
    new_object_call <- new_call(obj_call, self_args)
    return(new_function(
      args = arg_info$self,
      body = as.call(c(quote(`{`),
        # Force all promises here so that any errors are signaled from
        # the constructor() call instead of the new_object() call.
        unname(self_args),
        new_object_call
      )),
      env = envir
    ))
  }

  if (is_class(parent)) {
    parent_name <- parent@name
    parent_fun <- parent
    formal_args <- modify_list(arg_info$parent, arg_info$self)
  } else if (is_base_class(parent)) {
    parent_name <- parent$constructor_name
    parent_fun <- parent$constructor
    formal_args <- modify_list(arg_info$parent, arg_info$self)
  } else if (is_S3_class(parent)) {
    parent_name <- paste0("new_", parent$class[[1]])
    parent_fun <- parent$constructor
    formal_args <- formals(parent$constructor)
    formal_args[names(arg_info$self)] <- arg_info$self
  } else {
    # user facing error in S7_class()
    stop("Unsupported `parent` type", call. = FALSE)
  }

  obj_call_args <- formal_args
  obj_call_args <- as_names(names(formal_args), named = TRUE)
  names(obj_call_args)[names(obj_call_args) == "..."] <- ""

  body <- new_call(obj_call, obj_call_args)
  env <- new.env(parent = envir)
  env[[parent_name]] <- parent_fun

  new_function(formal_args, body, env)
}

constructor_args <- function(parent, properties = list(),
                             envir = asNamespace("S7"), package = NULL) {
  parent_args <- formals(class_constructor(parent))

  # Remove read-only properties
  properties <- properties[!vlapply(properties, prop_is_read_only)]

  self_arg_nms <- names2(properties)
  self_args <- as.pairlist(lapply(
    setNames(, self_arg_nms),
    function(name) prop_default(properties[[name]], envir, package)
  ))

  list(
    parent = parent_args,
    self = self_args
  )
}


# helpers -----------------------------------------------------------------

is_property_dynamic <- function(x) is.function(x$getter)

missing_args <- function(names) {
  lapply(setNames(, names), function(i) quote(class_missing))
}

new_call <- function(call, args) {
  if (is.character(call)) {
    call <- switch(length(call),
                   as.name(call),
                   as.call(c(quote(`::`), lapply(call, as.name))))
  }
  as.call(c(list(call), args))
}

as_names <- function(x, named = FALSE) {
  if (named) {
    names(x) <- x
  }
  lapply(x, as.name)
}

uses_default_constructor <- function(class) {
  isTRUE(attr(class, ".constructor_is_default"))
}

has_S7_symbols <- function(env, ...) {
  env <- topenv(env)
  if (identical(env, asNamespace("S7")))
    return (TRUE)
  if (!isNamespace(env))
    return (FALSE)
  imports <- getNamespaceImports(env)[["S7"]]
  symbols <- c(...) %||% getNamespaceExports("S7")
  all(symbols %in% imports)
}
