# This adds a call (e.g. a simpr verb such as
# fit()) to the include_calls element
add_call = function(obj, obj_call, call_name, replace_arg = 2) {
  ## add the name of the function to be called
  obj_call[1] = call(call_name)

  ## replace the appropriate argument with "." for
  ## later piping. This is not necessary if this
  ## was originally generated from a pipe, but is
  ## necessary in case the function was NOT piped.
  obj_call[replace_arg] = call(".")

  obj$include_calls = c(obj$include_calls, obj_call)

  obj
}



