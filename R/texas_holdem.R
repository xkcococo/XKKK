#' Game: Texas Hold'em Tool.
#' @usage texas_holdem(c('A','B','C','D','E'))
#' texas_holdem(c('A','B','C','D','E','F'))
#' @param A a card composed of a suit and a number or letter.
#' @param B a card composed of a suit and a number or letter.
#' @param C a card composed of a suit and a number or letter.
#' @param D a card composed of a suit and a number or letter.
#' @param E a card composed of a suit and a number or letter.
#' @param F a card composed of a suit and a number or letter.
#' @details S, H, C, and D stand for four suits: Spade, Heart, Club and Diamond.
#' @details 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, and A stand for the number of cards.
#' @details In this function, 'H2' stand for the two of hearts, 'HJ' stands for the jack of hearts.
#' @details The total number of cards should be 5 or 6.
#' @return It will return the relative probability of each type of winning game.
#' @export
#' @examples
#' #If the we only know the 2 pokers we have and 3 pokers on the table, then we input the suits and number of these five pokers.#
#' texas_holdem(c('S4','S6','H8','H9','H7'))
#'                         1 pair     2 pair 3 of a kind  straight      flush full house 4 of a kind straight flush
#' Relative probability 0.5855689 0.08325624  0.01387604 0.3145236 0.04162812          0           0     0.01572618
#' #It returned the probabilitiy of each type of winning game.#
#' 
#' 
#' #Now, we know one more piece of card, then we add it in this function.#
#' texas_holdem(c('S4','S6','H8','H9','H7','SK'))
#' Please input a four-digits number:4567
#'                        1 pair 2 pair 3 of a kind straight flush full house 4 of a kind straight flush
#' Relative probability 0.3913043      0     0      0.173913  0          0           0              0
#' #It returned the new probabilitiy of each type of winning game.#



texas_holdem <- function(input) {
    card <- c("S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "SJ", "SQ", "SK", "SA", "H2", "H3", "H4", "H5", "H6", "H7", 
        "H8", "H9", "H10", "HJ", "HQ", "HK", "HA", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "DJ", "DQ", "DK", 
        "DA", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "CJ", "CQ", "CK", "CA")
    ## select from rest pokers
    aa <- card[!(card %in% input)]
    ## select 5 pokers from all 7 pokers
    s <- data.frame()
    if (length(input) == 5) {
        n <- 0
        for (i in 1:(length(aa) - 1)) {
            for (j in (i + 1):length(aa)) {
                n = n + 1
                s[n, 1] <- aa[i]
                s[n, 2] <- aa[j]
            }
        }
    } else if (length(input) == 6) {
        for (i in 1:length(aa)) {
            s[i, 1] <- aa[i]
        }
    }
    ## the 5 or 6 pokers we choose
    if (length(input) == 5) {
        s[, 3] <- input[1]
        s[, 4] <- input[2]
        s[, 5] <- input[3]
        s[, 6] <- input[4]
        s[, 7] <- input[5]
    } else if (length(input) == 6) {
        s[, 2] <- input[1]
        s[, 3] <- input[2]
        s[, 4] <- input[3]
        s[, 5] <- input[4]
        s[, 6] <- input[5]
        s[, 7] <- input[6]
    }
    ## Combinations
    for (i in 8:15) {
        s[, i] <- 0
    }
    colnames(s)[8:15] <- c("1 pair", "2 pair", "3 of a kind", "straight", "flush", "full house", "4 of a kind", "straight flush")
    for (i in 1:nrow(s)) {
        judge <- t(s[i, 1:7])
        judge <- cbind(judge, as.data.frame(substring(judge[, 1], 1, 1)), as.data.frame(substring(judge[, 1], 2, 3)))
        colnames(judge) <- c("card", "suit", "number")
        judge <- as.data.frame(judge)
        ## 1 pair
        if (any(table(judge$number) >= 2)) {
            s[i, "1 pair"] <- 1
        }
        ## two pair
        if (length(table(judge$number)) < 7 & length(table(judge$number)) >= 2) {
            if (table(table(judge$number) >= 2)["TRUE"] >= 2) {
                s[i, "2 pair"] <- 1
            }
        }
        ## 3 of a kind
        if (any(table(judge$number) >= 3)) {
            s[i, "3 of a kind"] <- 1
        }
        ## straight
        if (all(c(2, 3, 4, 5, 6) %in% judge$number) | all(c(3, 4, 5, 6, 7) %in% judge$number) | all(c(4, 5, 6, 7, 8) %in% judge$number) | 
            all(c(5, 6, 7, 8, 9) %in% judge$number) | all(c(6, 7, 8, 9, 10) %in% judge$number) | all(c(7, 8, 9, 10, "J") %in% 
            judge$number) | all(c(8, 9, 10, "J", "Q") %in% judge$number) | all(c(9, 10, "J", "Q", "K") %in% judge$number) | 
            all(c(10, "J", "Q", "K", "A") %in% judge$number) | all(c("A", 2, 3, 4, 5) %in% judge$number)) {
            s[i, "straight"] <- 1
        }
        ## flush
        if (any(table(judge$suit) >= 5)) {
            s[i, "flush"] <- 1
        }
        ## full house
        if ((table(table(judge$number) >= 2)["TRUE"] >= 2) & (any(table(judge$number) >= 3))) {
            s[i, "full house"] <- 1
        }
        ## 4 of a kind
        if (any(table(judge$number) >= 4)) {
            s[i, "4 of a kind"] <- 1
        }
        ## straight flush
        if ((all(c(2, 3, 4, 5, 6) %in% judge$number) | all(c(3, 4, 5, 6, 7) %in% judge$number) | all(c(4, 5, 6, 7, 8) %in% judge$number) | 
            all(c(5, 6, 7, 8, 9) %in% judge$number) | all(c(6, 7, 8, 9, 10) %in% judge$number) | all(c(7, 8, 9, 10, "J") %in% 
            judge$number) | all(c(8, 9, 10, "J", "Q") %in% judge$number) | all(c(9, 10, "J", "Q", "K") %in% judge$number) | 
            all(c(10, "J", "Q", "K", "A") %in% judge$number) | all(c("A", 2, 3, 4, 5) %in% judge$number)) & (any(table(judge$suit) >= 
            5))) {
            s[i, "straight flush"] <- 1
        }
    }
    result <- s[, 8:15]
    n <- nrow(result)
    for (i in 1:ncol(result)) {
        result[n + 1, i] <- (sum(result[1:n, i])/n)
    }
    row.names(result)[n + 1] <- "Relative probability"
    print(result[n + 1, ])
}

























