def enigma_step(positions, notches, letters):
    new_positions = [p for p in positions]
    new_positions[0] = (positions[0]+1)%letters
    for i in range(1,len(positions)):
        #if in notch, step, but also step previous 
        #but remember to do both from original so no overstepping
        if (type(notches[i])==list):
            for notch in notches[i]:
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

def path(starting_position, notches, letters, stopping_criterion = lambda p: False):
    #make a list, then make that into a dictionary of steps
    step    = lambda p: enigma_step(p, notches ,letters)
    current = step(starting_position)
    steps   = [starting_position]
    while True:
        steps.append(current)
        current = step(current)
        if (current in steps or stopping_criterion(current)):
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
    out = ''
    for p in eval(position):
        out+=str(p)
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
    pos = nx.planar_layout(G)
    nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_size = rotors*150)
    nx.draw_networkx_labels(G, pos)
    nx.draw_networkx_edges(G, pos, edge_color = 'r', edgelist=black_edges, arrows=True, arrowsize=8)
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
        print("\rG%d: notching "%n, notches, end='')
        G = nx.DiGraph()
        G.add_edges_from(make_cycle_graph(notches, letters)) 
        if n>0:
            print(" --- comparing graph %d and %d [%3d%%]"%(n-1,n,round(100*n/(len(all_notches)-1))), end='')
            if (not nx.is_isomorphic(G, pG)):
                print("non-isomorphic graphs found!")
                trigger=False
        pG=G
    return trigger

#checks for multiple notches
def check_multiple_notch_isomorphy(letters, rotors, max_notches):
    possible_notch_configurations = permutations([i for i in range(round(letters/2))]  , max_notches-1)
    all_notch_distances           = permutations(possible_notch_configurations, rotors)
    trigger = True
    for notch_distance in all_notch_distances:
        if (not check_isomorphy(letters, rotors, notch_distance)):
            trigger = False
    return trigger

    





check_isomorphy(5,3)
check_isomorphy(5,3,[[1,2],[1],[2]])
check_multiple_notch_isomorphy(5,3,2)


draw_cycle_graph([0,0,0], 3)

"""
It seems that all enigmas with equal distance between notches in corresponding rotors
have an isomorphic path

since ditanc eb between notches in a rotor is constant, all paths of an enigma when are determined ONLY
by the rotor order (paths are equal by isometry), an dnot staerting position or starting ring setting.
"""