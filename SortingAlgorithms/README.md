# Comparison of Sorting Algorithms

The report on this project was created as a [PDF](https://github.com/SeanMeyer/UCD/blob/master/SortingAlgorithms/Report.pdf). Below is the introduction of the problem, view the PDF to get a full breakdown..

# Introduction

I implemented three different algorithms for sorting: INSERTION-SORT, MERGE-
SORT, and COUNTING-SORT. All three algorithms are specified in psuedo-
code and implemented in the C# programming language. Static analysis was
done to show correctness and the expected order of growth, and dynamic anal-
ysis of the actual number of comparisons and assignments was done against
seven different data sets in order to determine the effect of data ordering on the
expected order of growth.

From these analysis, we show that the expected order of growth of the algo-
rithms is as follows, INSERTION-SORT:O(n^2 ), MERGE-SORT:O(nlogn),
COUNTING-SORT:O(n).

I implemented all three algorithms in C# in order to do analysis.