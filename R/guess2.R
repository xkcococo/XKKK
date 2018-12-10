#' Game: Guess which four digits number I think!.
#' @usage 1. print guess2() 
#' 2. Please input a four-digits number:a
#' @param a a non-repeating four-digits for each digit from 1 to 9.
#' @return If the number you input has only one digit which position and number are both correct and none of the digit has correct number and incorrect position, it will return '1A0B'.
#' @return If the number you input is correct, it will return 'Good job!!'.
#' @return When you guess too much times, it will return 'Input answer to get the answer'. Then, if you input 'answer', it will return the correct answer of this game.
#' @export
#' @examples
#' guess2()
#' Please input a four-digits number:4567
#' [1] '1A1B'
#' #1A means one digit we guess has correct position and number#
#' #1B means one digit we guess has correct number but position is incorrect.#
#' Please input a four-digits number:4568
#' '2A1B'
#' #We are closer to the answer.#
#' Please input a four-digits number:3468
#' 'Good job!!'
#' #Congratulation! You win this game!#
#' 
#' 
#' #If we guess too much times#
#' Please input a four-digits number:4568
#' [1] 'Input answer to get the answer'
#' [1] '2A1B'
#' Please input a four-digits number:answer
#' [1] 3 4 6 8
#' #The answer is 3468.#


guess2 <- function(seed = sample(99999, 1)) {
    set.seed(seed)
    a <- sample(0:9, 4)
    a1 <- a[1]
    a2 <- a[2]
    a3 <- a[3]
    a4 <- a[4]
    t = 0
    while (1) {
        na = 0
        nb = 0
        n = readline("Please input a four-digits number:")
        if (n == "answer") {
            print(a)
            break
        } else {
            t = t + 1
            if (t >= 7) {
                print("Input answer to get the answer")
            }
            n <- as.numeric(n)
            n1 <- as.numeric(substring(n, 1, 1))
            n2 <- as.numeric(substring(n, 2, 2))
            n3 <- as.numeric(substring(n, 3, 3))
            n4 <- as.numeric(substring(n, 4, 4))
            if (n1 == a1) {
                na = na + 1
            }
            if (n2 == a2) {
                na = na + 1
            }
            if (n3 == a3) {
                na = na + 1
            }
            if (n4 == a4) {
                na = na + 1
            }
            if ((n1 %in% a) & (n1 != a1)) {
                nb = nb + 1
            }
            if ((n2 %in% a) & (n2 != a2)) {
                nb = nb + 1
            }
            if ((n3 %in% a) & (n3 != a3)) {
                nb = nb + 1
            }
            if ((n4 %in% a) & (n4 != a4)) {
                nb = nb + 1
            }
            print(paste0(na, "A", nb, "B"))
            if (na == 4) {
                print("Good job!!")
                break
            }
        }
    }
}
