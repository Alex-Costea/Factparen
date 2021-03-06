# Factparen

Change the literal in "isItPrime = genPrimes 100000" to change the upper limit on prime numbers it recognizes

Functions:

genPrimes : Generates a list with True or False value corresponding with if the index is prime or not, up to the number n

num2paren : Takes a tuple of 2 integrals and returns the Factparen representation associated with the fraction between the 2 numbers

int2paren : Takes an integral and returns the Factparen representation associated with the number

paren2num : Takes a Factparen representation and returns the associated number as a tuple of 2 integrals representing a fraction

paren2int : Takes a Factparen representation and returns the associated number as an integral

paren2float : Takes a Factparen representation and returns the associated number as an double

Python version is unfinished

Below I will reproduce the original description for Factparen, written on January 23, 2019

#
# **Factparen representation**

I&#39;m gonna describe a representation of numbers I came up with a while ago and ask an open question about it.

49042 in Factparen

The representation, which I&#39;m gonna call &quot;Factparen&quot; (short for Factorization Parentheses), assigns every integer bigger than 0 to an unique string of correctly closing parentheses, and every string of correctly closing parentheses to such an integer. Well, almost. The one more rule is that this excepts strings such as &quot;()(())&quot;, which can be separated into multiple strings of correctly closing parentheses (&quot;()&quot; and &quot;(())&quot;) .

The algorithm works as follows:

132

(2,1,0,0,1)

(2,1,,,1)

(2 1 1 3 1)

((())()()(()())())

Step 1: Take the number and factorize it. For example, for 132, the factorization is 2²\*3\*11. We add the factors that are to the power of 0 too (up to the biggest non-zero factor), as such: 2²\*3¹\*5⁰\*7⁰\*11¹. Write down just the exponents in parentheses separated by commas, so we can get (2,1,0,0,1).

Step 2: Remove all exponents of 0, while keeping the commas. So we get (2,1,,,1).

Step 3: Replace all strings of commas with how many commas there are in the string, such as we get (2 1 1 3 1).

Step 4: Apply Step 1 to all the numbers inside recursively, until there are no more numbers left. The number 1 is written as simply &quot;()&quot;. The final result will be ((())()()(()())()).

## **Unique representation**

Obviously the factorization of a natural number is unique, and as such the representation in step 2 is unique. Converting the commas to numbers to reach step 3 might seem like it could create some confusion, as we aren&#39;t sure if the first number corresponds to a number of commas or an exponent. However, this isn&#39;t the case, as it can be deduced by simply looking at how many numbers there are. If there is an even number of numbers, then the first number must correspond to a number of commas (for example, 27 is written in step 3 as &quot;(1 3)&quot;, and corresponds to &quot;(,3)&quot;). If there is an odd number of numbers, the first number corresponds to an exponent (such as for 6, which is written in step 3 as &quot;(1 1 1)&quot; and corresponds to &quot;(1,1)&quot;).

As such, the Factparen function is reversible, and in fact bijective. For every correctly constructed string of parentheses there is a number, and for every number there is a correctly constructed string of parentheses.

## **Properties**

The first few Factparen numbers are:

1 ()

2 (())

3 (()())

4 ((()))

5 ((())())

6 (()()())

7 ((()())())

8 ((()()))

9 (()(()))

10 (()(())())

11 (((()))())

12 ((())()())

13 (((())())())

14 (()(()())())

15 (()()()())

16 (((())))

17 ((()()())())

18 (()()(()))

19 (((()())())())

20 ((())(())())

An interesting property is that, because of its recursive structure, some (but not by all means all) big numbers can be written really shortly. For example, running the algorithm on the number

115792089237316195423570985008687907853269984665640564039457584007913129639936

gives the result

((((()()))))

Why? Because the number given is equal to 2²⁵⁶, which can be written quite elegantly this way. In fact, for the number 2^(2²⁵⁶), which has around 10⁷⁶ digits and as such I will **not** be writing in full, the string is simply

(((((()())))))

In general, for a string x, (x) is equal to 2^x. As such, for 2n characters, the biggest number that can be written is (((…((()))…))), where each part has n parentheses. This is equal to 2^2^2^2^…^2, n-1 times. Needless to say, this grows pretty fast.

## **Open question**

But how about the smallest? What&#39;s the smallest number that can be written in Factparen representation in 2n characters?

For n=1, 2, 3,…, the first 20 terms are, as follows,

1, 2, 3, 5, 7, 13, 19, 38, 69, 138, 217, 395, 581, 1162, 1957, 3781, 7383, 14559, 24521, 49042

The last term is in the illustration at the beginning of the article.

I don&#39;t see any obvious patterns in this sequence. The factorization of those terms gives some quite odd results

1, 2, 3, 5, 7, 13, 19, 2\*19, 3\*23, 2\*3\*23, 7\*31, 5\*79, 7\*83, 2\*7\*83, 19\*103, 19\*199, 3\*23\*107, 3\*23\*211. 7\*31\*113, 2\*7\*31\*113

It seems to like numbers with a little amount of factors, but actual prime numbers tend to not win. The sequence seems to grow a bit slower than 2^x, at about 1.7160^n.

My questions is also whether there is an efficient algorithm to calculate those numbers. Backtracking will eventually calculate those numbers with precision, but it will take a long time for bigger numbers.

My guess is that this is really difficult to answer, as are most problems related to prime numbers and factorization in general. Either way I find this notation of numbers and related questions interesting, and I hope you do too.

## **Update: Generalized Factparen**

As I said in the beginning, Factparen works for every correctly closing string of parentheses that can&#39;t be divided in smaller parts. But how about those who _can?_

Well, it turns out Factparen can be generalized to all non-negative rational numbers, so that _all_ correct strings have a number assigned to them.

First, one edge case. We have yet to define the number which &quot;&quot;, the empty string, corresponds to. By convention, I&#39;ll define that the empty string will be equal to 0.

Now to showcase the method, let&#39;s pick a new string.

(()()())()(())

First, apply the usual Factparen to all smaller parts. We will get

{6, 1, 2}

My first instinct was to say that the first number is the numerator and to consider every other number the denominator. As such, {6, 1, 2} would be equal to 6 / (1 2), or 6 /(,2), or, 6/9, in other words 2/3. Voilá, we have solved it!

Or have we? See, the problem with this method is that (()()())()(()) is 2/3, but so is (())()(). We have stated at the beginning that every number must correspond to an unique string, so this is no good.

However, there&#39;s a way to avoid this. While writing the denominator, skip the prime factors that already appear in the numerator. So, for example, 6=2\*3, so when interpreting (1 2) it won&#39;t be equal to 3², but 7².

Note that this is done only at the first level, **not** recursively. We&#39;re gonna skip the factors 2 and 3 when interpreting (1 2), but not when interpreting &quot;1&quot; and &quot;2&quot; (or &quot;()&quot; and &quot;(())&quot;). This way, the string &quot;(()()())()(())&quot; will be equal to 6/49.

Since every irreducible fraction is distinct, this method is bijective, so every non-negative rational number has a corresponding string of correctly closing parentheses, and vice versa. Normal Factparen is a special case of generalized Factparen.
