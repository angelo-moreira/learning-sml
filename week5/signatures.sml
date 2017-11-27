
signature MATHLIB =
sig
    val fact: int -> int
    val half_pi: real
end

structure MyMathLib :> MATHLIB =
struct
    fun fact x =
        if x = 1
        then 1
        else x * fact (x-1)

    val half_pi = Math.pi / 2.0
    fun doubler y = y + y
end