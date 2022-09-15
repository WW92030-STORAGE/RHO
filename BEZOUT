# Solves the equation ax + by = gcd(a, b) for integers x, y
bezout <- function(a, b) {
    if (a == b) return(res)
    r0 = a
    r1 = b
    s0 = t1 = 1
    s1 = t0 = 0
    r2 = s2 = t2 = 0
    
    while (r1 != 0) {
        q = floor(r0 / r1)
        r2 = r0 - q * r1
        s2 = s0 - q * s1
        t2 = t0 - q * t1
        r0 = r1; r1 = r2;
        s0 = s1; s1 = s2;
        t0 = t1; t1 = t2;
    }
    return(c(s0, t0))
}

a = 243;
b = 198;
res = bezout(a, b)
cat(res[1], res[2], "\n") # 9 -11
cat(res[1] * a + res[2] * b, gcd(a, b), "\n") 
# 9(243) - 11(198) = 9 = gcd(243, 198)
