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








If 
    assume A encrypts to B, 
        with the inner encryption (without plugboard!) being C encrypts to D, A to B
        (B, A, D, C)

        then, if A is steckered to A => B steckered to B
        A~A => B~B
        A~B => A~B
        A~C => D~B 
        A~D => C~B 

        but, with symmetry!
        A~A => B~B
        
        A~B => A~B, B~A
        B~A => A~B, B~A
        



        A C => D B
            A~C => D~B
            A~C => B~D
        
            D~B => A~C
            D~B => C~A
        
            B~D => A~C
            B~D => C~A

            C~A => D~B
            C~A => B~D





        C~A => D~B, B~D
        
        A~D => C~B, B~C
        D~A => C~B, B~C

        plain <> cipher =>
        plain ~ x => (enc e) ~ cipher
        implicantFrom = plain
        implicatTo    = x
        impliesFrom   = enc x
        impliesTo     = cipher

        
        
        
        
        
        
        B~B => A~B 
        B~C => D~B
        B~D => C~B

        C~C => D~B
        C~D => C~B

        D~D => C~B


        then this implies that 
        A is steckered to C, and B steckered to D



lets find the transitive closure of a connection matrix using the second type og configuration.
Ie double symmetry.

scan all elements. 
    if an element is 1, preserve
    if an element is 0.
        search all nodes that might connect to it, if any 1 then set 1.
        



non-trivial elemets :
    n*n - (n * n-1)(n * n-2)/2 - (n-1)(n-2)/2 *  ---------------
    (n(n+1)/2)^2 = 
    n^2(n+1)^2/4  

    n = 1 -> 1
    n = 2 -> 3
    n = 3 -> 6
    n = 4 -> 10 = 4*5/2
          -> 16*25/4 = 4*25 = 100

ellers: n^4
nå      n^4/4 + n^3/2 + n^2/4
n = 4: fra 256 til 100
n = 5: fra 625 til (5*6/2)^2 = (5 *3)^2= 15^2 ? 225
n = 26:fra 456976 til 123201