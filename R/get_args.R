#' Print the ... argument
#' 
#' A really simple function that explores what happens when use ...
#' @param ... The ... argument from the function to be analysed
#' @export
print_dots = function(...) {
    dots_list = eval(substitute(alist(...)))
    dots_vec = unlist(dots_list)
    for(n in names(dots_vec))
      message(n, " = ", dots_vec[n])
    invisible(dots_vec)
}
