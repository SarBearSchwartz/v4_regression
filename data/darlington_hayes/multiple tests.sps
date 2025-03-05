new file.
input program.
loop rep=1 to 100.
end case.
end loop.
end file.
end input program.
do repeat x=x1 to x10.
compute x=rv.normal(0,1).
end repeat.
correlations variables = x1 to x10.
regression/dep=x10/method=enter x1 to x9.