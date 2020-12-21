#ifndef CT_H   // include guard
#define CT_H

bool verbose = true;

typedef vector<set<pair<shint,vector<bool>>>> adjacency_list;

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
  public:
    ConfigurationTracker(Enigma *, const int);

    const Graph* get_graph();
};
#endif