from math import *

prime={}
maxsieve=0
def do_sieve(n):
    global prime
    global maxsieve
    if n<maxsieve:
        return
    n=maxsieve=min(n,1000000)
    for x in range(1,n+1):
        prime[x]=True
    for x in range(2,floor(sqrt(n)+1)):
        if prime[x]:
            for y in range(x*x,n,x):
                prime[y]=False

def num2paren(n):
    k=n
    i=2
    j=0
    global prime
    fact={}
    do_sieve(n)
    while k>1:
        print(k)
        if prime[i]:
            j+=1
            fact[j]=0
            while k%i==0:
                fact[j]+=1
                k/=i
        i+=1
    s="("
    nrv=0
    for x in fact:
        if fact[x]==0:
            nrv+=1
            continue
        if nrv>0:
            s=s+num2paren(nrv)
        s=s+num2paren(fact[x])
        nrv=1
    s=s+")"
    return s

##def paren2num(s):
##    s=s[1:len(s)-1]
##    if len(s)==0:
##        return 1
##    l=[]
##    s=0
##    for x in s:
##        if x=='(':
##            s+=1
##        if x==')':
##            s-=1
