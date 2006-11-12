##local search nehme 3 Orte und vertausche sie.



local_search=function(T)

{
a=T
b=sample(a,3)
a=a[-b]
c=sample(order(runif(length(a),0,1)),3)
i=1
d=a
while(i != 4)
{
d=insert(d,c[i],b[i])
i=i+1
}
names[d]
}


#oder


local_search2=function(T)
a=T
b=sample(a,3)
a=a[-b]
c=sample(order(runif(length(a),0,1)),3)
d=a
for(i in 1 : length(c))
{
d=insert(d,c[i],b[i])
}
d
}

