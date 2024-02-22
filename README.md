# HAL

The goal of this project is to implement an interpreter for a minimalist dialect of LISP in Haskell.

Our dialect of Lisp is functionnal (almost no side effects or mutable states), and a subset of Scheme. Therefore, an expression evaluated by your interpreter must give the same result as the same expression evaluated by a Scheme interpreter (the reference implementation being Chez-Scheme).

See the subject for further details !

========================

Grade : E | Mark : 5.3

|              Category             | Percentage |   Tests   | Crash ? |
|:---------------------------------:|:----------:|:---------:|:-------:|
| Basics                            | 33.3%      | 1/3       | x       |
| Quote                             | 50%        | 1/2       | x       |
| Cons Car Cdr                      | 20%        | 1/5       | x       |
| Arithmetics                       | 33.3%      | 1/3       | x       |
| Equalities                        | 40%        | 2/5       | x       |
| Define                            | 0%         | 0/2       | x       |
| Conditionals                      | 0%         | 0/3       | x       |
| Lambda                            | 0%         | 0/3       | x       |
| Complex programs                  | 0%         | 0/5       | x       |
| Error handling                    | 75%        | 3/4       | x       |
| Coding style                      | 100%       | 3/3       | x       |
| **End score**                     | **31.6%**  | **12/38** | **NO**  |

Beware of -42 Epitech students !!!
