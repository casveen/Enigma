import pattern_grapher
import numpy as np
import pydot
import networkx as nx
import pygraphviz as pgv














def enigma_step(positions, notches, letters):
    new_positions = [p for p in positions]
    new_positions[0] = (positions[0]+1)%letters
    for i in range(1,len(positions)):
        #if in notch, step, but also step previous 
        #but remember to do both from original so no overstepping
        if (type(notches[i-1])==list):
            for notch in notches[i-1]:
                if (positions[i-1] == notch):
                    new_positions[i] = (positions[i]+1) % letters
                    if (i>1):
                        new_positions[i-1] = (positions[i-1]+1) % letters
        else:
            if (positions[i-1] == notches[i-1]):
                new_positions[i] = (positions[i]+1) % letters
                if (i>1):
                    new_positions[i-1] = (positions[i-1]+1) % letters
    #handle the last rotor
    if (type(notches[-1])==list):
        for notch in notches[-1]:
            if (positions[-1] == notch): 
                new_positions[-1] = (positions[-1]+1) % letters
                #new_positions[-2] = (positions[-2]+1) % letters
    else:
        if (positions[-1] == notches[-1]): 
            new_positions[-1] = (positions[-1]+1) % letters
            #new_positions[-2] = (positions[-2]+1) % letters #XXX if last roror also doublestpes, uncomment
    return new_positions


def position_string_to_string(position):
    out = '|'
    for p in eval(position):
        out+=str(p)+'|'
    return out

def position_to_string(position):
    out = ''
    for p in position:
        out+=str(p)+''
    return out


#assumes one argument function
def repeat(proc, n):
    if n > 1:
        def step(x):
            print(reverse(x))
            return repeat(proc, n-1)(proc(x))
        return step
    else: 
        return proc

def reverse(x):
    return x[::-1]

def path(starting_position, notches, letters, stopping_criterion = lambda p: False, max_steps=-1):
    #make a list, then make that into a dictionary of steps
    step    = lambda p: enigma_step(p, notches ,letters)
    current = step(starting_position)
    steps   = [starting_position]
    i       = 1
    while True:
        i+=1
        steps.append(current)
        current = step(current)
        if (current in steps or stopping_criterion(current) or (i == max_steps)):
            steps.append(current)
            break

    #make stepping dictionary
    d= []
    for previous, current in zip(steps[:-1], steps[1:]):
        d.append(tuple([position_to_string(previous), position_to_string(current)]))
    return d, steps



#repeat(enigma, 18)([1,1,1])

#okay we have it

path([2,1,0], [0,0,0], 3)[0]


import networkx as nx
import matplotlib.pyplot as plt

def permutations(selection, n):
    if ((len(selection)==0) or (n <= 0)):
        #print("empty")
        out = [[]]
    else: 
        #print("non-empty")
        out = []
        for p in permutations(selection, n-1):
            #print(p)
            for s in selection:
                out.append([s]+p)
    #print("layer %d finished " %(n))
    #print(out)
    return out























def stepping_matrix(rotors):
    def value(r,c):
        if (r == 0 or r-c == 0 or c-r == 1): 
            return 1 
        else: 
            return 0
    return np.matrix([[value(r,c) for c in range(rotors)] for r in range(rotors)])
    
def stepping_matrix_inverse(rotors):
    def value(r,c):
        if (r == 0):
            if (c == 0):
                return 1
            else:
                return -(1-(-1)**c)/2 
        elif (c>=r):
            return (-1)**(r-c)
        else:
            return 0
    return np.matrix([[value(r,c) for c in range(rotors)] for r in range(rotors)])

def is_valid_stepping(step):
    return (all(step[:-1]>=step[1:]))

v = np.matrix([[4],[2],[1],[2]])
stepping_matrix_inverse(4)*v
is_valid_stepping(stepping_matrix_inverse(4)*v)

"""
algorithms:
-check all valid paths of crib length from a given starting position
    -space efficient
    -must DETERMINE valid paths
    -keep track of starting positions checked
    -when all paths ecxhausted, step into ONE of the subpaths, the others must be forgotten to save space
    
    -COULD have stepped into all paths, but requires stepping one depth at a time for all paths. might be viable with good bookkeeping

    -when stepping into a path, 
        -we need to find the additional paths that are now valid due to starting one step later
        -might as wellMIGHT AS WELL take it from the start? hmm

-how to determine valid paths
    -if you just did a S2, you must have done a S1 before, 
    -generally, a Si is preceeded by a S(i-1)
    -if you did a S2 and there is notching distances 10 and 16 on rotor 1
     you have to wait for 10 or 16 S1s before a S2 can be done again
    -generally
     If Si, 
"""

#notches here does not care about absoute position, but RELATIVE position of notches, or more importatnly
#the distance between notches in a rotor, as these give non-isomorph paths
def valid_paths(starting_position, notches, length):
    #while trying out steps, keep track of Sis done.
    #initially, all can be done
    #but after that, if previous was a Si, you might currently do a S1, or S(i+1)
    #if 

    #if there are very few letters and many rotors with short notch distance, this might not be valid, a cascade might be so long as to
    #trigger a notch in the first rotor again.

    #IT IS ASSUMED THAT THERE ARE LESS ROTORS THAN THE SMALLEST NOTCH DISTANCE IN THE FIRST ROTOR

    #All graphs should be isomorph(CHECK) so we only need to find the nodes, which should be easy as
    #the paths are fixed by Sis

    return

#rot is an integer
def rotate_wheel(wheel, rot, mx):
    if type(wheel) == list:
        return [(w+rot)%mx for w in wheel]
    else:
        return (wheel+rot)%mx

#rot is a list
def rotate_wheels(wheels, rots, mx):        
    return [rotate_wheel(wheel, rot, mx) for wheel, rot in zip(wheels, rots)]

def valid_paths_brute(starting_position, notches, letters, length):
    #try paths of given length for all ROTATIONS of notches
    rotors = len(notches)
    notch_rotations = [rotate_wheels(notches, rots, letters) for rots in permutations(range(letters), rotors)]
    #print(notch_rotations)
    G = nx.DiGraph()
    for i_notches in notch_rotations:
        p = path(starting_position, i_notches, letters, max_steps=length)
        #print(i_notches, " -> ", p[1])
        #if (p[0][1][0]=='111' and p[0][2][0]=='222'):
        #    print(i_notches)
        #    print(p[1])
        G.add_edges_from(p[0]) 
    print(G.edges)
    
    plt.figure(figsize=(20,20))
    pos = nx.nx_agraph.graphviz_layout(G, prog="dot")
    nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_size = 1)
    nx.draw_networkx_labels(G, pos)
    nx.draw_networkx_edges(G, pos, edge_color = 'r', arrows=True, arrowsize=8)
    #nx.draw_shell(G)
    plt.show()
    
valid_paths_brute([0,0,0], [[0],[0],[0]], 6, 3)
valid_paths_brute([0,0,0], [[0],[0],[0]], 10, 10) #isopmorph to above? seems so?
valid_paths_brute([0,0,0,0,0], [[0,2],[0,3],[0,4],[0,1],[0,8]], 10, 10)
valid_paths_brute([0,0,0], [[1,2],[2],[0,2]], 10, 10)
enigma_step([1,2,0], [[2, 0], [0], [2, 1]], 3)

def check_valid_path_isomorphy():
    return


"""
note the double S3 engage for [[2, 0], [0], [2, 1]]  ->  
[0, 1, 0]
[[0, 0, 0], [1, 1, 1], [2, 2, 2], [3, 2, 2], [4, 2, 2], [5, 2, 2], [6, 2, 2], [7, 2, 2], [8, 2, 2]]
the first S3 is fake, as in, it is a S1 and S2 at the same time, which is legal only in the starting position!'
It then goes into a S3
"""

"""
for r5 there are 12 initial choices
S1,   (10000)
S2,   (11000)
S3,   (11100)
S4,   (10110)
S5,   (10011)
S2+S4,(11110)
S2+S5,(10011)
S3+S5,(10000)
S2
"""




"""
non-merged
def upper_space_usage_in_gb(r = 3, letters = 26, c = 10):
    int_bytes     = 2
    pointer_bytes = 8
    bool_bytes    = 1
    node_usage = (1-r**(c+1))/(1-r) * (r*bool_bytes+r*pointer_bytes)
    leaf_usage = letters**r * (r*int_bytes)
    
    nodes         = (1-r**(c+1))/(1-r)
    print("Nodes:       %d\nSpace usage: %.2E Gb"%(nodes, (node_usage+leaf_usage)/(10**9)))
    return (node_usage + leaf_usage)/(10**9)

upper_space_usage_in_gb(4,26,14)
"""

#merged
def upper_space_usage_in_gb(r = 3, letters = 26, c = 10):
    int_bytes     = 2
    pointer_bytes = 8
    bool_bytes    = 1
    nodes         = c * (2*(r-1))
    node_usage =  nodes * (r * bool_bytes + r*pointer_bytes)
    print("Nodes:       %d\nSpace usage: %.2E Gb"%(nodes, node_usage/(10**9)))
    return node_usage/(10**9)

upper_space_usage_in_gb(4,26,14)