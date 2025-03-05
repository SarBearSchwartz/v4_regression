compute pz=2*(1-cdf.normal(4,0,1)).
compute pt=2*(1-cdf.t(4,20)).
compute pf=1-cdf.f(4,3,30).
compute pchi=1-cdf.chi(4,2).
format pz pt pf pchi (F16.8).
execute.