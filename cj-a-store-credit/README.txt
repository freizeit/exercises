Solves the code jam practice problem described here:

    http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

You receive a credit C at a local store and would like to buy two items. You first walk through the store and create a list L of all available items. From this list you would like to buy two items that add up to the entire value of the credit.
The solution you provide will consist of the two integers indicating the positions of the items in your list (smaller number first).  

The test cases are contained in input files (see e.g. `A-large-practice.in`) whose path is to be specified on the command line.

The first line of input gives the number of cases, N. N test cases follow. For each test case there will be:

 * One line containing the value C, the amount of credit you have at the store.
 * One line containing the value I, the number of items in the store.
 * One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.
 * Each test case will have exactly one solution.

I have solved the problem in clojure, erlang, golang and haskell so far. My main motivation is to assess how hard it is to write parallel code in the languages of interest and to see how the latter perform.
