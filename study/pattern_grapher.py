import numpy as np
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

def path(starting_position, notches, letters, stopping_criterion = lambda p: False, max_steps=None):
    #make a list, then make that into a dictionary of steps
    step    = lambda p: enigma_step(p, notches ,letters)
    current = step(starting_position)
    steps   = [starting_position]
    i       = 0
    while True:
        i+=1
        steps.append(current)
        current = step(current)
        if (current in steps or stopping_criterion(current) or i == max_steps):
            steps.append(current)
            break

    #make stepping dictionary
    d= []
    for previous, current in zip(steps[:-1], steps[1:]):
        d.append(tuple([str(previous), str(current)]))
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

def position_to_string(position):
    out = '|'
    for p in eval(position):
        out+=str(p)+'|'
    return out

def make_cycle_graph(notches,letters):
    rotors =len(notches)
    #for all positions, find path
    #dont start from positions already in path, as these
    #will just behave as the path
    all_starting_positions = permutations([i for i in range(letters)], rotors)
    all_paths = []
    positions_checked = []
    for starting_position in all_starting_positions:
        if (not (str(starting_position) in positions_checked)):
            #add edges, until we hit a path that is traversed from before
            def already_traversed(position):
                return str(position) in positions_checked
            edges, _ = path(starting_position, notches, letters, stopping_criterion=already_traversed)
            for edge in edges:
                all_paths+=[edge]
                positions_checked.append(edge[0])
    #remap edges to something more readable
    edges = []
    for e in all_paths:
        edges.append(tuple([position_to_string(e[0]), position_to_string(e[1])]))
    return edges


def draw_cycle_graph(notches, letters):
    rotors =len(notches)
    G = nx.DiGraph() 
    edges=make_cycle_graph(notches, letters)
    plt.figure(figsize=(20,20))
    G.add_edges_from(edges)
    black_edges = [edge for edge in G.edges()]
    cycle_edges = nx.find_cycle(G)
    cycle_nodes = [e[0] for e in cycle_edges]
    cycle_n     =len(cycle_nodes)
    r           = 50
    cycle_pos   = {n: (r*np.cos(i/cycle_n * 2* np.pi), r*np.sin(i/cycle_n * 2* np.pi))  for i,n in enumerate(cycle_nodes)}
    non_cycle_nodes = [n for n in G.nodes if (not n in cycle_nodes)]
    #pos = nx.spring_layout(G, fixed = cycle_nodes, pos = cycle_pos)
    pos = nx.nx_agraph.graphviz_layout(G, prog="circo")
    nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_size = rotors*1)
    #nx.draw_networkx_labels(G, pos)
    nx.draw_networkx_edges(G, pos, edge_color = 'r', edgelist=black_edges, arrows=True, arrowsize=8)
    #nx.draw_shell(G)
    plt.show()

def check_isomorphy(letters, rotors, notch_distance=None):
    graphs             = []
    #all_single_notches= permutations([i for i in range(letters)], notches_per_rotor)
    all_notches        = permutations([i for i in range(letters)], rotors)
    #update with more notches
    if (notch_distance!=None):
        all_notches        = [[[(notch+d)%letters for d        in [0]+notch_distance[r]] 
                                                  for r, notch in enumerate(notches)   ]
                                                  for notches  in all_notches          ]
    pG=0
    trigger=True
    for notches, n in zip(all_notches, range(len(all_notches))):
        print("\rcheck_multiple_notch_isomorphy(5,3,2)rG%d: notching "%n, notches, end='')
        G = nx.DiGraph()
        G.add_edges_from(make_cycle_graph(notches, letters)) 
        if n>0:
            print(" --- comparing graph %d and %d [%3d%%]"%(n-1,n,round(100*n/(len(all_notches)-1))), end='')
            if (not nx.faster_could_be_isomorphic(G, pG)):
                print("non-isomorphic graphs found!")
                trigger=False
        pG=G
    return trigger

#checks for multiple notches
def check_multiple_notch_isomorphy(letters, rotors, max_notches):
    possible_notch_configurations = permutations([i for i in range(round(letters/2))]  , max_notches-1)
    all_notch_distances           = permutations(possible_notch_configurations, rotors)
    trigger = True
    for i, notch_distance in enumerate(all_notch_distances):
        print("[%3d%%] --- notching distance "%(round(100*i/len(all_notch_distances))), notch_distance)
        if (not check_isomorphy(letters, rotors, notch_distance)):
            trigger = False
        print()
    return trigger

    





check_isomorphy(5,3)
check_isomorphy(5,3,[[1,2],[1],[2]])
check_multiple_notch_isomorphy(9,3,3)


draw_cycle_graph([[0,0],[0,0],[0,0]], 5)
draw_cycle_graph([[0,3],[0,1],[0,0]], 5)


#separates into two cycles!!!
#draw_cycle_graph([[0,3],[0,2],[0,0]], 5)

"""
It seems that all enigmas with equal distance between notches in corresponding rotors
have an isomorphic path

since ditanc eb between notches in a rotor is constant, all paths of an enigma when are determined ONLY
by the rotor order (paths are equal by isometry), an dnot staerting position or starting ring setting.
"""

































#dra ALL paths for all notches, but mark one notching path as red
#only uses singly notched rotors
def draw_entire_graph(marked_notches, letters):
    rotors      = len(marked_notches)
    all_notches = permutations([i for i in range(letters)], rotors)

    plt.figure(figsize=(20,20))
    G = nx.DiGraph() 
    M = nx.DiGraph()
    for notches in all_notches:
        G.add_edges_from(make_cycle_graph(notches, letters))
    M.add_edges_from(make_cycle_graph(marked_notches, letters))

    black_edges = [edge for edge in G.edges()]
    red_edges   = [edge for edge in M.edges()]
    red_nodes   = [e[0] for e in red_edges]
    cycle_edges = nx.find_cycle(M)
    cycle_nodes = [e[0] for e in cycle_edges]
    cycle_n     =len(cycle_nodes)
    red_n       = len(red_nodes)
    r           = 50
    cycle_pos   = {n: (r*np.cos(i/cycle_n * 2* np.pi), r*np.sin(i/cycle_n * 2* np.pi))  for i,n in enumerate(cycle_nodes)}
    
    non_cycle_nodes = [n for n in G.nodes if (not n in cycle_nodes)]

    #U = nx.union(G,M)

    #pos = nx.spring_layout(G, fixed = cycle_nodes, pos = cycle_pos)
    pos = nx.nx_agraph.graphviz_layout(G, prog="dot")
    nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_size = rotors*1)
    #nx.draw_networkx_labels(G, pos)
    nx.draw_networkx_edges(G, pos, edge_color = 'b', edgelist=black_edges, arrows=True, arrowsize=8)
    nx.draw_networkx_edges(G, pos, edge_color = 'r', edgelist=red_edges,   arrows=True, arrowsize=8)
    #nx.draw_shell(G)
    plt.show()

draw_entire_graph([0,0,0],3)




"""
(n+0)         (n+1)         (n+2)       (n+3)
(n+0)         (n+0)         (n+0)       (n+0)
(n+0)         (n+0)         (n+0)       (n+0)

              (n+1)         (n+2)       (n+3)
              (n+1)         (n+1)       (n+1)
              (n+0)         (n+0)       (n+0)

              (n+1)         (n+2)       (n+3)
              (n+1)         (n+1)       (n+1)
              (n+1)         (n+1)       (n+1)
                              
                            (n+2)       (n+3)
                            (n+2)       (n+2)
                            (n+0)       (n+0)

                            (n+2)       (n+3)
                            (n+2)       (n+2)
                            (n+1)       (n+1)

                            (n+2)       (n+3)
                            (n+2)       (n+2)
                            (n+2)       (n+2)
                                        
                                        (n+3)
                                        (n+3)
                                        (n+0)

                                        (n+3)
                                        (n+3)
                                        (n+1)

                                        (n+3)
                                        (n+3)
                                        (n+2)

                                        (n+3)
                                        (n+3)
                                        (n+3)
this certainly takes of, n(n+1)/2-2
"""


"""
given
    (n+a)     (n)
x = (n+b) y = (n)
    (n+c)     (n)
and basis 
     (1)      (1)       (1)
s1 = (0) s2 = (1) s3 =  (1)
     (0)      (0)       (1)
can we find l1 l2 l3 so that

y + l1s1 + l2s2 + l3s3 = x

[1 1 1] [s1]
[0 1 1] [s2] = x-y
[0 0 1] [s3]
             [1 -1  0]
L inverse is [0  1 -1] so we can find the amount of notch engages, order does not matter
             [0  0  1]
then we know that there were s1 only first rotor engages, 
                             s2 only second and first rotor engages
                             s3 all rotor (third) engages
But in the real world, order matters, as some orders are not possible
XXX What orders are valid?
two S3 in a row is not valid, S3 is always preceeded by a S2
                              S2 is always preceeded by a S1, or might in veery special cases be preceeded by a S3, but not in real life cases.
"""

"""
1 1 1 1 1 1        1 -1  0 -1  0  0
0 1 1 0 0 0        0  1 -1  1 -1  1
0 0 1 1 0 0   ->   0  0  1 -1  1 -1
0 0 0 1 1 0   ->   0  0  0  1 -1  1
0 0 0 0 1 1        0  0  0  0  1 -1
0 0 0 0 0 1        0  0  0  0  0  1
S(k+1) is always preceded by a Sk
boundaries:
   forall k
      S(k+1)<Sk

XXX for modulo rotation, use (letters-1) instead of -1, then inverse should be inverse modulo rotation?
"""

