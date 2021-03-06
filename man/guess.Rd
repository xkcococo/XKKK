% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess.R
\name{guess}
\alias{guess}
\title{Game: Guess which number I think!.}
\usage{
1. print guess() 
2. The number you guess is:x
}
\arguments{
\item{x}{which number you guess (x must be integer from 1 to 200).}
}
\value{
If the number you input is incorrect, it will return 'Wrong answer, it ranges from a to b'. (a, b is the range of correct answer.)

If the number you input correct, it will return 'Good job, you are right!'.
}
\description{
Game: Guess which number I think!.
}
\examples{
guess()
The number you guess is:6
[1] 'Wrong answer, it ranges from 6 to 200'
The number you guess is:45
[1] 'Wrong answer, it ranges from 6 to 45'
The number you guess is:16
[1] 'Good job, you are right!'
#Congratulation! You win this game!#
}
