#' Game: Calculate 4 number to 24.
#' @usage Calc24(c(a,b,c,d))
#' @param a A integer.
#' @param b A integer.
#' @param c A integer.
#' @param d A integer.
#' @return The final formula to calculate 24 using \code{a}, \code{b}, \code{c} and \code{d} by  +, -, *, and /.
#' @export
#' @examples
#' Calc24(c(4,4,4,4))
#' [1] '((4*4)+4)+4=24'
#' #This is the formula to calculate 24.#
#' 
#' Calc24(c(1,2,3,4))
#' [1] '((4*3)*2)*1=24'
#' #This is the formula to calculate 24.#



Calc24 <- function(n) {
    ops <- c("+", "-", "*", "/")
    for (i in 1:10000) {
        n1 <- sample(n, 4)
        f <- paste0("(", "(", n1[1], sample(ops, 1), n1[2], ")", sample(ops, 1), n1[3], ")", sample(ops, 1), n1[4])
        a <- eval(parse(text = f))
        if (i == 10000) {
            print("no answer")
        }
        if (a == 24) {
            print(paste0(f, "=", a))
            break
        }
    }
}
