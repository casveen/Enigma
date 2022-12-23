An important part in th algorithm of the bombe is the wiring. This is one of the bigger bottlenecks.
To efficiently model the algorithm the wiring model should quickly arrive at a transitive closure of the wiring. 
Either by closing the wiring before checking it, or by connecting wires in a way that preserves the transitive closure property.

- connecting while maintaining transitive closure
Given that the matrix is transitively closed, and two wires are to be connected, how can we modify the matrix to still have its transitive closure?
is one matrix mult suffucient?

- transitive closure algorithm
DFS from each node
say we start from the first node, 
this one reaches all wires in the first column. These should be reached.