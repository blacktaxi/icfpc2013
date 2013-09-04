M-type vs ICFPC 2013
====================

This is our (@blacktaxi, @bazuzu, @susl) solution for the [ICFP 2013 contest](http://icfpc2013.cloudapp.net/) problem.

We scored 249 in the lightning round and 547 overall. This was a first ICFPC for all of us.

The idea of our solution was to run a search over candidate programs generated before querying the server for eval. After implementing this basic algorithm for the lightning round, we used the rest of the time to optimize it.

We tried different optimization ideas in different places, but I think our solution was fundamentally sub-optimal.

We had three mid-range laptops and one desktop computer at our disposal. The solver was ran on at most two of those at the same time.

[Here's](http://www.reversemicrowave.me/blog/2013/08/29/icfpc2013/) an elaborate report.