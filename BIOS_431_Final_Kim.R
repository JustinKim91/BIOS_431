









SIR = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      r_adj <- r*(1-f)
      
      dSdt = (b*(1 - c*(S + I + R))) * (S + I + R) - (d + k + r)*S - B*S*I + w*R
      dIdt = B*S*I - (d + v + k)*I
      dRdt = r*S - (w + d + k)*R
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}


# Parameters
initials = c(S = 99, I = 1, R = 0)
params1 = c(b = 0.5, d = 0.05, c = 0.01, B = 0.01, 
            w = 0.05, v = 0.15, k = 0, r =0, f = 0)
t = 0:100


