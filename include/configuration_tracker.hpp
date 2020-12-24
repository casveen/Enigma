#ifndef CT_H   // include guard
#define CT_H

bool verbose = true;

typedef vector<set<    pair<shint,vector<bool>>      >> adjacency_list;
typedef vector<bool> Engage ;
enum Engage_direction {forward, backward, stop};

struct Edge {
    int          src, dest;
    vector<bool> engages;
};
 
// class to represent a graph object
class Graph {
    public:
    adjacency_list adjList;
    shint          root;
 
    // Graph Constructor
    Graph(int);
    void           add_edges(vector<Edge> const &);
    adjacency_list get_adjacency_list();
    void           set_root(shint);
    shint          count_edges();
};

class ConfigurationTracker {
  private:
    shint    m_length;
    Enigma*  m_enigma;
    shint    m_letters;
    shint    m_rotor_count;
    Graph*   path_graph;
    shint    m_start_node; //hash of starting node, probably 0
    shint    current_position; //XXXmight overshadow?

  public:
    ConfigurationTracker(Enigma *, const int);
    ~ConfigurationTracker();
    //vector<pair<Edge, Engage>> out;
    const Graph* get_graph();

    //interface for moving around in the graph
    
    //iterator of engaged notches, includes the whole path, ie going backwards to.
    //Edge next(); //XXX important, tell if going backwards.
    //bool is_leaf();
    //vector<vector<shint>> get_ring_setting_from_path(vector<vector<shint>> path);
    vector<pair<Engage, Engage_direction>> path_iterator();
    vector<pair<Engage, Engage_direction>> path_iterator_inner(shint from);

};
#endif