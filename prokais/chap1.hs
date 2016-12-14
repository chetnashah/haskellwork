



-- signal is function of time/ space or any other function of paramtr
s t = 5 * t

-- analog signals

xa t = A * cos (Om * t + Th)
-- Om is in rad/sec
Om = 2 * pi * F
-- F is in cycles/sec

-- analog signal is periodic if
isPeriodic xa = (xa t == xa (t + Tp))


-- discrete signals

xd n = A * cos (smOm * n + Th) --  -Inf < n < Inf
 -- smOm is in radians per sample
smOm = 2 * pi * f -- f is in cycles per sample

isPeridicDis xd = (xd n == xd (n + N))
 -- smallest value of N is fundamental period





