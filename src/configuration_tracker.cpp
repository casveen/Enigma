//#include "bombe.hpp"

#include "configuration_tracker.hpp"

using namespace std;
typedef int shint;
/*
vector<vector<shint>> permutations(int upto, int n) {
    cout<<"im in";
    vector<vector<shint>> out;
    vector<vector<shint>> result;

    //out.reserve(pow(n,upto)); XXX put back???
    cout<<"n = "<<n;
    if (n>1) {
        for (int i = 0; i<upto; i++) {
            cout<<"     i = "<<i;
            result = permutations(upto, n-1);
            //append i to END of result 
            for (vector<shint> p : result) {
                p.push_back(i);
            }
            out.insert(out.end(), result.begin(), result.end());
        }
    }
    else {
        out.reserve(n);
        for (int i = 0; i<upto; i++) {
            out[i].reserve(1);
            out[i][0] = i;
        }
    }
    return out;
}*/



//when going from previous to current, which rotors engaged?
void notch_engages(vector<shint> prev_position, vector<shint> current_position, vector<bool>& engages) {
    int n = prev_position.size(); 
    for (int i = 0; i<n; i++) {
        engages[i] = !(prev_position[i] == current_position[i]);
    }
}




vector<shint> read_positions(Enigma *enigma) {
    vector<shint> out;
    string read = enigma->get_positions_as_string();
    int    rotor_count = read.length();
    for (int i= rotor_count - 1; i >= 0; --i) {
        out.push_back((shint) (read[i]-(int)'A')); //XXX might be reverseed?
    }
    return out;
}


string vector_to_string(vector<shint> vec) {
    string out = "";
    for (shint i : vec) {
        out += to_string(i);
    }
    return out;
}



shint hash_position(vector<shint> position, set<vector<shint>> position_set) {
    shint i = 0;
    for (auto it = position_set.begin(); it!=position_set.end(); ++it) {
        if (*it == position) {
            return i;
        }
        i++;
    }
    cout<<"ERROR: "<<vector_to_string(position)<<" not found in hash set!\n"; //should be an error
    return 0;
}







//Stolen from https://www.techiedelight.com/graph-implementation-using-stl/
// data structure to store graph edges
Graph::Graph(int N) { 
    adjList.resize(N);
}
Graph::~Graph() { 
    cout<<"oh fuck, the graph died!\n";//XXX
}

void Graph::add_edges(vector<Edge> const &edges) {
    // resize the vector to N elements of type vector<int>
    // add edges to the directed graph
    vector<bool> engaged_notches;
    for (auto &edge: edges) {
        //cout<<"inserted to graph\n";
        // insert at the end
        //notch_engages(edge.src, edge.dest, engaged_notches);
        adjList[edge.src].insert(make_pair(edge.dest, vector<bool>{edge.engages}));
    }
}

shint Graph::count_edges() {
    shint sum = 0;
    for(auto s : adjList) {
        sum += s.size();
    }
    return sum;
}

void Graph::set_root(shint value) {
    root = value;
}

adjacency_list& Graph::get_adjacency_list() {
    return adjList;
}
 
/*
// print adjacency list representation of graph
void printGraph(Graph const& graph, int N)
{
    for (int i = 0; i < N; i++)
    {
        // print current vertex number
        cout << i << " --> ";
 
        // print all neighboring vertices of vertex i
        for (int v : graph.adjList[i])
            cout << v << " ";
        cout << endl;
    }
}*/

































//initializes as well as creates, since needs to initialize path_set and path graph as well.
//the set can be forgotten after this construction
// TODO make an enigma copy!
ConfigurationTracker::ConfigurationTracker(Enigma *enigma, const int length) {
    m_length                   = length;
    m_enigma                   = enigma;
    m_letters                  = enigma->get_wires();
    m_rotor_count              = enigma->get_rotors();
    //notch_engages(previous_position, current_position, )
    



    if (verbose) { cout<<"\rInitializing tracker ... "; }
    int* initial_position= new shint[m_rotor_count];
    
    //XXXsloppy solution... but copies the pointer
    for(int i =0; i<m_rotor_count; i++) {
        initial_position[i] = m_enigma->get_positions()[i];
    }

    //first, make a set of all possible positions, to use as a hash
    set<vector<shint>> position_set;
    vector<shint> position;
    //insert initial
    position = read_positions(m_enigma);
    position_set.insert(position);
    //set<vector<shint>>::iterator it;
    for (int rs = 0; rs < pow(m_letters,m_rotor_count); rs++) {
        cout<<"\rmaking position set ["<<rs/pow(m_letters, m_rotor_count)*100<<"%]";
        //make length steps and record all positions
        for (int step = 0; step<m_length; step++) {
            m_enigma->turn();
            position = read_positions(m_enigma);
            position_set.insert(position);
        }
        m_enigma->set_positions(initial_position); //safe pointer
        //advance ring_setting, odometer style
        m_enigma->next_ring_setting();
    }
    cout<<"Made a set of "<<position_set.size()<<" nodes\n";
    path_graph = new Graph(position_set.size());

    m_start_node = hash_position(vector<shint>{initial_position, initial_position+m_rotor_count}, position_set);
    


    //then make a graph
    //with initial rotor pos (any)
    //for all ring settings
    //track paths of given length
    //store enigma setting
    //notch engages
    vector<bool> engaged_notches(m_rotor_count, false);
    vector<Edge> edges;
    vector<shint> previous_position = read_positions(m_enigma);
    m_enigma->next_ring_setting();
    vector<shint> current_position  = read_positions(m_enigma);
    shint previous_hash, current_hash;




    shint prev_sz = 0;
    //vector<vector<bool>> notch_engage_path;
    for (int rs = 0; rs < pow(m_letters,m_rotor_count); rs++) {
        cout<<m_enigma->get_ring_setting();
        //cin;

        previous_position = read_positions(m_enigma);
        m_enigma->turn();
        
        //make path
        for (int p=0; p<m_length; p++) {
            cout<<"\rtracking paths ["<<rs/pow(m_letters, m_rotor_count)*100<<"%]";
            current_position   = read_positions(m_enigma);
            notch_engages(previous_position, current_position, engaged_notches);
            //notch_engage_path.push_back(engaged_notches);
            

            previous_hash = hash_position(previous_position, position_set);
            current_hash  = hash_position(current_position,  position_set);
            edges.push_back((Edge {previous_hash, current_hash, engaged_notches}));

            previous_position  = read_positions(m_enigma);
            m_enigma->turn();
        }
        prev_sz = path_graph->count_edges();
        path_graph->add_edges(edges);
        cout<< "  ---  "<<edges.size()<<" edges "<<(path_graph->count_edges()-prev_sz)<<" of which are new";
        edges.clear();
        //edges.shrink_to_fit();
        //translate to tracker structure
        //this.add_path(notch_engage_path);
        //reset enigma POSITION
        m_enigma->set_positions(initial_position); //safe pointer
        //advance ring_setting, odometer style
        m_enigma->next_ring_setting();
        //notch_engage_path.clear();
    }
    //add edges to graph
    
    //and we are ok?
    cout<<"made graph with edge total "<<edges.size();
    cout<<"\n but actually just "<<path_graph->count_edges()<<" edges";




    if (verbose) { cout<<"DONE\n"; }
    delete[] initial_position;
}

ConfigurationTracker::~ConfigurationTracker() {
    delete path_graph;
}

const Graph* ConfigurationTracker::get_graph() {
    return path_graph;
}


//the bombe_unit has found a path that is valid, 
//we need to find out what ring settings that give this particular path
//to do this, we look at the notch engages of the solution path, and
//return the valid ring settings 
//assumes the same enigma as before is used XXX maybe it should make its own???
//might be thread unsafe
/*
in:
    path:
        a valid path, represented by a vector of positions(vector of shints)
return
    valid ring settings, as a vecotr of ring settings (which is a vector of shints)
*/





/*
vector<vector<shint>> ConfigurationTracker::get_ring_setting_from_path(vector<vector<shint>> path) {
    //translate the path to a notch engage path
    vector<bool>         engaged_notches    = vector<bool>(m_rotor_count, false);
    vector<shint>        previous_position  = *path.begin();
    vector<shint>        current_position;
    vector<vector<bool>> valid_notch_engage_path;
    for (vector<shint> current_position : path) {
        notch_engages(previous_position, current_position, engaged_notches);
        //vector<bool> engaged_notches_copy(engaged_notches); 
        valid_notch_engage_path.push_back(vector<bool>{engaged_notches}); //us copy as to not just add the same object
    }
    //we have the notch engage path of the valid path.
    //now we need to find the ring settings that replicate these notch engages
    shint* initial_position= new shint[m_rotor_count];
    //XXXsloppy solution... but copies the pointer
    for(int i =0; i<m_rotor_count; i++) {
        initial_position[i] = m_enigma->get_positions()[i];
    }
    vector<shint> initial_position_vector(initial_position, m_rotor_count);
    //step through all ring settings
    //XXX does a lot of unneccesarry work, but seldom
    
    auto valid_engaged_notch_iterator = valid_notch_engage_path.begin();
    for (int rs = 0; rs < pow(m_letters,m_rotor_count); rs++) {
        previous_position = read_positions(m_enigma);
        m_enigma->turn();
        notch_engages(previous_position, read_positions(m_enigma), engaged_notches);
        //make path from given position, break if notch engages is not equal
        while(engaged_notches == *valid_engaged_notch_iterator) {    
         //ter Syn   //the ring setting is currently valid,

            //step the iterator
            valid_engaged_notch_iterator++;


            //update noth engage
            notch_engages(previous_position, read_positions(m_enigma), engaged_notches);
        }




        for (int p=0; p<m_length; p++) {
            cout<<"\rtracking paths ["<<rs/pow(m_letters, m_rotor_count)*100<<"%]";
            current_position   = read_positions(m_enigma);
            notch_engages(previous_position, current_position, engaged_notches);
            //notch_engage_path.push_back(engaged_notches);
            

            previous_hash = hash_position(previous_position, position_set);
            current_hash  = hash_position(current_position,  position_set);
            edges.push_back((Edge {previous_hash, current_hash, engaged_notches}));

            previous_position  = read_positions(m_enigma);
            m_enigma->turn();

        }
        prev_sz = path_graph->count_edges();
        path_graph->add_edges(edges);
        cout<< "  ---  "<<edges.size()<<" edges "<<(path_graph->count_edges()-prev_sz)<<" of which are new";
        edges.clear();
        //edges.shrink_to_fit();
        //translate to tracker structure
        //this.add_path(notch_engage_path);
        //reset enigma POSITION
        m_enigma->set_positions(initial_position); //safe pointer
        //advance ring_setting, odometer style
        m_enigma->next_ring_setting();
        //notch_engage_path.clear();
    }



}
*/





void append(vector<pair<Engage, Engage_direction>> &v1, const vector<pair<Engage, Engage_direction>> &v2) {
    v1.insert(v1.end(), v2.begin(), v2.end());
}

vector<pair<Engage, Engage_direction>> ConfigurationTracker :: path_iterator() {
    //from start node.
    vector<pair<Engage, Engage_direction>> out;
    append(out, path_iterator_inner(m_start_node)); //should be 0?
    return out; //XXX store in configurationtracker.
}

vector<pair<Engage, Engage_direction>> ConfigurationTracker :: path_iterator_inner(shint from) {
    //from start node.
    vector<pair<Engage, Engage_direction>> out;
    //cout<<"->initialized vector "<<"\n";
    //cout<<"adj list has size "<<path_graph->get_adjacency_list().size();
    for (pair<shint, Engage> to_and_engages : path_graph->get_adjacency_list().at(from)) {
        /*cout<<from<<"->"<<to_and_engages.first<<" ";
        for (bool b : to_and_engages.second ) {
            b?cout<<"T":cout<<"F";
        }
        cout<<"\n";*/

        //in
        out.push_back(make_pair(to_and_engages.second, Engage_direction::forward));
        append(out, path_iterator_inner(to_and_engages.first));
        //out
        /*cout<<from<<"<-"<<to_and_engages.first<<" ";
        for (bool b : to_and_engages.second ) {
            b?cout<<"T":cout<<"F";
        }
        cout<<"\n";*/
        out.push_back(make_pair(to_and_engages.second, Engage_direction::backward));
    }
    return out;
}


void ConfigurationTracker::print_path_iterator() {
    vector<pair<Engage, Engage_direction>> iterator = path_iterator();
    for (auto engage_and_direction : iterator) {
        switch(engage_and_direction.second) {
            case Engage_direction::forward:  cout<<"f - "; break;
            case Engage_direction::backward: cout<<"b - "; break;
            case Engage_direction::stop:     cout<<"s - "; break;
        }
        /*
        for (bool b : engage_and_direction.first ) {
            b?cout<<"T":cout<<"F";
        }
        cout<<"\n";*/
    }
}

/*
#include "rotors.cpp" 
int main() {
    Enigma enigma= Enigma(3, 6);
    enigma.randomize();
    cout<<"made enigma\n";
    ConfigurationTracker tracker(&enigma, 4);
    cout<<"made tracker\n";
    cout<<"initialized\n";
    cout<<"checking iterator\n";
    tracker.print_path_iterator();
}
*/














/*
int main() {
    vector<vector<shint>> result;
    cout<<"initialized result\n";
    result.reserve(3*3);
    cout<<"allocated result\n";
    result = permutations(3,2);
    cout<<"testing permutation of 2 letters from 3\n";
    for (vector<shint> p : result) {
        for (shint i : p) {
            cout<<i<<", ";
        }
        cout<<"\n";
    }

    cout<<"testing permutation of 3 letters from 5\n";
    for (vector<shint> p : permutations(5,3)) {
        for (shint i : p) {
            cout<<i<<", ";
        }
        cout<<"\n";
    }


}*/




//g++ -O3 -o ct.exe src/configuration_tracker.cpp src/enigma.cpp -I include
//christian@christian-Yoga-Slim-7-14ARE05:~/Documents/Code/Enigma$ ./ct.exe
