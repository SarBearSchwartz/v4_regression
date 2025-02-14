data pprob;
pz=2*(1-cdf('normal',4));
pt=2*(1-cdf('t',4,20));
pf=1-cdf('F',4,3,30);
pchi=1-cdf('chisquare',4,2);
run;
proc print data=pprob;var pz pt pf pchi;run;
