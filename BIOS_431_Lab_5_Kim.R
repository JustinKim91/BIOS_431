# BIOS 431 Emerging Infectious Diseases Lab 5 ----
# Justin Kim
# 10/11/2024

## Question 1 -----------------------------------------------------------------

vaccination_and_culling = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      dSdt = (b*(1 - c*(S + I + R))) * (S + I + R) - (d + k + r)*S - B*S*I + w*R
      dIdt = B*S*I - (d + v + k)*I
      dRdt = r*S - (w + d + k)*R
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}

## Question 2 -----------------------------------------------------------------

# Parameters
initials = c(S = 99, I = 1, R = 0)
params1 = c(b = 0.5, d = 0.05, c = 0.01, B = 0.01, w = 0.05, v = 0.15, k = 0)
t = 0:100

z <- vector()
for (i in seq(from = 0, to = 1, by = 0.01)){
  z[i] <- vaccination_and_culling()
}
