data multtest;
array x {10} x1-x10;
do j = 1 to 100;do i = 1 to 10;x{i} = rand("Normal");end;output;end;
run;
proc corr data=multtest;var x1-x10;run;
proc reg data=multtest;model x10=x1-x9;run;
