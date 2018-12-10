#' Game: Guess which number I think!.
#' @usage 1. print guess() 
#' 2. The number you guess is:x
#' @param x which number you guess (x must be integer from 1 to 200).
#' @return If the number you input is incorrect, it will return 'Wrong answer, it ranges from a to b'. (a, b is the range of correct answer.)
#' @return If the number you input correct, it will return 'Good job, you are right!'.
#' @export
#' @examples
#' guess()
#' The number you guess is:6
#' [1] 'Wrong answer, it ranges from 6 to 200'
#' The number you guess is:45
#' [1] 'Wrong answer, it ranges from 6 to 45'
#' The number you guess is:16
#' [1] 'Good job, you are right!'
#' #Congratulation! You win this game!#

guess <- function() {
    num = round(runif(1, 1, 200))
    num
    b = 1
    c = 200
    d = 0
    g = 0
    print("This number is range from 1 to 200")
    
    while (1) {
        a = as.numeric(readline("The number you guess is:"))
        if (a > num) {
            if (d == 1) {
                b = e
            }
            print(paste("Wrong answer, it ranges from ", b, " to ", a, sep = ""))
            f = a
            g = 1
            d = 0
        } else if (a < num) {
            if (g == 1) {
                c = f
            }
            print(paste("Wrong answer, it ranges from ", a, " to ", c, sep = ""))
            e = a
            d = 1
            g = 0
        } else {
            print("Good job, you are right!")
            break
        }
    }
}
