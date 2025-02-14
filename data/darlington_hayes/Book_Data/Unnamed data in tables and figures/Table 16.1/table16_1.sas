data chap16;
input case x1 x2 x3 y;
datalines;
1.00	.00	1.00	3.00	8.00
2.00	.00	3.00	4.00	13.00
3.00	.00	11.00	5.00	17.00
4.00	.00	7.00	8.00	7.00
5.00	.00	9.00	8.00	10.00
6.00	.00	12.00	12.00	10.00
7.00	1.00	2.00	5.00	10.00
8.00	1.00	4.00	6.00	15.00
9.00	1.00	3.00	4.00	11.00
10.00	1.00	7.00	12.00	6.00
11.00	1.00	9.00	10.00	11.00
12.00	1.00	13.00	14.00	9.00
run;
proc reg data=chap16;
model y=x1 x2 x3/influence;
output out=ch16diag p=pred r=resid student=str rstudent=t
cookd=cook h=h;run;
proc print data=ch16diag;run;

