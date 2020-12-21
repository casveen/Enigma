/*
The configuration tree

From a given starting position of the rotors, there are many possible paths the next positions can take depending on what the position of the notches are
if we track all positions, drawing edges between positions that step into other ones, 
we get a sort of tree.
the branches will split and merge into several positions

when checking a position, we can check all paths from that position, and if no solutions fit we can safely assume
that the enigma configuration of the ciphertext does not start with the given starting position 

It seems likely that all configuration "trees"( or paths) are isomorphic, so we need only build the tree once,
each path corresponds to a form of stepping, either the first rortr, first and second, first second and third, first thuird and fourth,
first fourth and fifth, etc.

XXX
The problem is finding out which ring setting ws used when we actually find a valid configuration.
It is not enough to store it in the leaves.

SUGGESTIONS
    we can store the configurations that led to one position in all leaves, and cross check when traversing downward,
    in the end we have the valid configurations. 
        NO, HEAVY, NOT EXACT

    Make it a tree structure, making distinct nodes though the nodes
    have been explored befroe, then the leaves can contain the 
    ring settings.
    The structure will be much bigger
    ANALYSIS:
        THE REALISTIC CASE, no notches immedeately after each other
        not exactly a max limimt, the starting position is much
        more versatile

        let c be the crib length

        each node has:
            position:      R * small integer XXX CAN BE REMOVED!!!
            notch engages: R * (boolean + pointer to next node)
        additionally, a leaf has:
            ring settings that terminated here: n*(R*small int)
        
        for a R rotor enigma, there are AT MOST R possible branches,
        gives a max of  
        sum_i=0..c R^i =  (1-R^(c+1))(1-R) 
        total nodes

        the list of ring settings in the leaves totals letters^R
        each element is a list of R integers

        MEMORY:
            (1-R^c)/(1-R)  *  (R*int + R*bol + R*point)
            letters^R * (R*int)

            R = 3, letters = 26:
                (1-3^c)(-2) * (3*int + 3*bol + 3*point) bytes
                26^3 * (3*int) = 17576 * (3*int) bytes
                
                c = 12:
                    (1-3^12)(-2) * (3*2 + 3*1 + 3*8) bytes 
                        = 265 720 * 33 
                        = 8 768 760 bytes
                    17576 * 6 bytes 
                        = 105 456 bytes

                    total: 8 874 216 bytes = 0.00826476 Gb NICE
                
            R = 4, letters = 26
    
    max 14 letters in crib :(




IMplement as merged tree, 
    reduced memory footprint.
    at any time, the fast rotor is the same for given steps used.
    max width at any given section of tree is 2^(R-1)
    any node(except root!) points to at most R other
    ANALYSIS:
        max: 
        c * (2^(R-1) * (2 * int + R * pointer)
    Extremely small compared to non-merged solution, requires fewer searches




    REQUIRES POST PRECESSING TO OBTAIN RING SETTING





XXX wiring must be able to be undone


When the tree is made, to analayse all
    -traverse tree depth-first, make wiring in diagonal board as suggested by the enigma position 
    -eventually rech a leaf, now use the tests to check for validity, 
        -if valid:     ???? how to get ring setting?
        -if not valid, undo wiring of leaf, return
    -for each branch, eaxhaust all selections, if valid found, store in sloutions and continue, 
    -when all exhausted, undo wiring of current branch and return valid solutions.






*/