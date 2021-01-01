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

vector<shint> unhash_position(shint hash, set<vector<shint>> position_set) {
    shint i = 0;
    for (auto it = position_set.begin(); it!=position_set.end(); ++it) {
        if (i == hash) {
            return *it;
        }
        i++;
    }
    cout<<"ERROR: faulty hash\n";
    return *position_set.end();
}








//Stolen from https://www.techiedelight.com/graph-implementation-using-stl/
// data structure to store graph edges

 
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
























void ConfigurationTracker::initialize_position_set() {
    //if (verbose) { cout<<"\rInitializing tracker ... "; }
    int* initial_position= new shint[m_rotor_count];
    
    //XXXsloppy solution... but copies the pointer
    for(int i =0; i<m_rotor_count; i++) {
        initial_position[i] = m_enigma->get_positions()[i];
    }

    //first, make a set of all possible positions, to use as a hash
    //set<vector<shint>> position_set;
    vector<shint> position;
    //insert initial
    position = read_positions(m_enigma);
    position_set.insert(position); //init in header
    //set<vector<shint>>::iterator it;
    for (int rs = 0; rs < pow(m_letters,m_rotor_count); rs++) {
        //cout<<"\rmaking position set ["<<rs/pow(m_letters, m_rotor_count)*100<<"%]";
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
    delete[] initial_position;
}



void ConfigurationTracker::make_tight_graph() {
    int* initial_position= new shint[m_rotor_count]();
    
    //XXXsloppy solution... but copies the pointer
    for(int i =0; i<m_rotor_count; i++) {
        initial_position[i] = m_enigma->get_positions()[i];
    }
    //cout<<"Made a set of "<<position_set.size()<<" nodes\n";
    path_graph = new Graph(position_set.size(), m_length);
    //m_start_node = hash_position(vector<shint>{initial_position, initial_position+m_rotor_count}, position_set);

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
        /*edges.push_back(Edge{hash_position(current_position,  position_set), 
                             hash_position(current_position,  position_set),
                             vector<bool>{m_rotor_count, false}});*/
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
        //prev_sz = path_graph->count_edges();
        path_graph->add_edges(edges);
        //cout<< "  ---  "<<edges.size()<<" edges "<<(path_graph->count_edges()-prev_sz)<<" of which are new";
        edges.clear();
        //cout<<"cleared edges\n";
        //edges.shrink_to_fit();
        //translate to tracker structure
        //this.add_path(notch_engage_path);
        //reset enigma POSITION
        m_enigma->set_positions(initial_position); //safe pointer
        //advance ring_setting, odometer style
        m_enigma->next_ring_setting();
        //notch_engage_path.clear();

    }
    //cout<<"tight graph dealloc\n";
    delete[] initial_position;
    //cout<<"made tight graph\n";
}



















void ConfigurationTracker::make_wide_graph() {
    int* initial_position= new shint[m_rotor_count];
    const shint* ring_setting;
    
    //XXXsloppy solution... but copies the pointer
    for(int i =0; i<m_rotor_count; i++) {
        initial_position[i] = m_enigma->get_positions()[i];
    }
    //cout<<"Made a set of "<<position_set.size()<<" nodes\n";
    path_graph_wide = new PointerGraph(m_length, m_rotor_count);
    //m_start_node = hash_position(vector<shint>{initial_position, initial_position+m_rotor_count}, position_set);

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
        //cout<<m_enigma->get_ring_setting();
        //cin;

        previous_position = read_positions(m_enigma);
        m_enigma->turn();
        
        //make path
        for (int p=0; p<m_length; p++) {
            cout<<"\rtracking paths ["<<rs/pow(m_letters, m_rotor_count)*100<<"%]";
            current_position   = read_positions(m_enigma);
            notch_engages(previous_position, current_position, engaged_notches);
            //notch_engage_path.push_back(engaged_notches);
            

            //previous_hash = hash_position(previous_position, position_set);
            //current_hash  = hash_position(current_position,  position_set);
            edges.push_back((Edge {0, 0, engaged_notches}));

            previous_position  = read_positions(m_enigma);
            m_enigma->turn();
        }
        //cout<<"made a path\n";
        //prev_sz = path_graph_wide->count_edges();
        //cout<<"counted\n";
        ring_setting = m_enigma->get_ring_setting();
        path_graph_wide->add_edges(edges, vector<shint>{ring_setting, ring_setting+m_rotor_count});
        //cout<<"added edges\n";
        //cout<< "  ---  "<<edges.size()<<" edges "<<(path_graph->count_edges()-prev_sz)<<" of which are new";
        edges.clear();
        //cout<<"cleared edges\n";
        //edges.shrink_to_fit();
        //translate to tracker structure
        //this.add_path(notch_engage_path);
        //reset enigma POSITION
        m_enigma->set_positions(initial_position); //safe pointer
        //advance ring_setting, odometer style
        m_enigma->next_ring_setting();
        //notch_engage_path.clear();
    }
    //cout<<"\rmade a ptah graph of "<<path_graph_wide->count_edges()<<" edges";
    delete[] initial_position;
}














//initializes as well as creates, since needs to initialize path_set and path graph as well.
//the set can be forgotten after this construction
// TODO make an enigma copy!
ConfigurationTracker::ConfigurationTracker(Enigma *enigma, const int length) {
    m_length                   = length;
    m_enigma                   = enigma;
    m_letters                  = enigma->get_wires();
    m_rotor_count              = enigma->get_rotors();
    mode = CT_mode::none;
    
    try {
        //cout<<"making wide graph\n";
        make_wide_graph();
        mode = CT_mode::wide;
        make_path_iterator();
        make_ring_settings_iterator(); 
        //cout<<"made wide graph\n";
    } catch (bad_alloc &ba) {
        cout<<"unable to allocate to wide CT, trying to make tight CT\n";
        try {
            initialize_position_set();
            make_tight_graph();
            mode = CT_mode::tight;
            make_path_iterator();
            //cout<<"\n\n size of tight CT: "<<sizeof(path_graph->get_adjacency_list())<<"\n\n";
            
        } catch (bad_alloc &ba) {
            cout<<"unable to allocate to tight CT, CT unusable\n";
            position_set.clear();
        }
    }
    //cout<<"done init CT\n";
}

ConfigurationTracker::~ConfigurationTracker() {
    switch(mode) {
        case(CT_mode::tight) : 
            delete path_graph;
            break;
        case(CT_mode::wide) :
            delete path_graph_wide;
            break;
        case(CT_mode::none) :
            break;
    }
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





void ConfigurationTracker::make_path_iterator() {
    vector<pair<Engage, Engage_direction>> out;
    //cout<<mode<<"\n";
    if        (mode == CT_mode::wide) {
        out = path_graph_wide->path_iterator();
    } else if (mode == CT_mode::tight) {
        out = path_graph->path_iterator();
    } else {
        cout<<"ERROR requesting iterator without valid mode\n";
    }
    m_path_iterator = out;
}

const vector<pair<Engage, Engage_direction>>& ConfigurationTracker::get_path_iterator() {
    return m_path_iterator;
}

void ConfigurationTracker::make_ring_settings_iterator() {
    vector<vector<vector<shint>>> out;
    if        (mode == CT_mode::wide) {
        out = path_graph_wide->ring_settings_iterator();
    } else if (mode == CT_mode::tight) {
        cerr << "ERROR cannot get ring settings iterator while in tight mode\n";
    } else {
        cerr << "ERROR requesting iterator without valid mode\n";
    }
    m_ring_settings_iterator = out;
}

const vector<vector<vector<shint>>>& ConfigurationTracker::get_ring_settings_iterator() {
    return m_ring_settings_iterator;
}



void ConfigurationTracker::print_path_iterator() {
    vector<pair<Engage, Engage_direction>> iterator = m_path_iterator;
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
