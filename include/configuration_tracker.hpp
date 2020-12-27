#ifndef CT_H   // include guard
#define CT_H
#include "cmath"
#include <set>
#include "enigma.hpp"

//bool verbose = true;

typedef vector<set<    pair<shint,vector<bool>>      >> adjacency_list;
typedef vector<bool> Engage ;
enum Engage_direction {forward, backward, stop};
enum CT_mode {tight, wide, none};

struct Edge {
    int          src, dest;
    vector<bool> engages;
};
 
// class to represent a graph object
// However, these do not care about node values, only edge values
class Graph { 
    private:
    shint          m_depth;
    adjacency_list adjList;
    shint          root;

    public:
    // Graph Constructor
    Graph(int, shint);
    ~Graph();
    void            add_edges(vector<Edge> const &);
    adjacency_list& get_adjacency_list();
    void            set_root(shint);
    shint           count_edges();
    vector<pair<Engage, Engage_direction>> path_iterator();
    vector<pair<Engage, Engage_direction>> path_iterator_inner(shint from, shint depth);
    void print_adjacency_list();
};

class PointerGraph { 
  private:
    shint m_depth;
    class PointerGraphEdge;

    class Node {
      private:
        set<PointerGraphEdge> connected_to;

      public:
        Node();
        void connect(pair<Node*, vector<bool>>);
        set<PointerGraphEdge> get_connections();
    };

    class PointerGraphEdge {
      private:
        Node*        to;
        vector<bool> engages;
      public:
        PointerGraphEdge(Node*, vector<bool>);
        bool operator<(PointerGraphEdge const&) const;
        Node* get_node() const;
        vector<bool> get_engages() const;
    };
    
    vector<Node*> nodeList;
    Node*        root;
  public:
    // Graph Constructor
    PointerGraph(shint);
    ~PointerGraph();
    void            add_edges(vector<Edge> const &);
    shint           count_edges();
    shint           count_edges_inner(Node *);
    vector<pair<Engage, Engage_direction>> path_iterator();
    vector<pair<Engage, Engage_direction>> path_iterator_inner(Node* from, shint depth);
};

class ConfigurationTracker {
  private:
    bool verbose=true;
    shint    m_length;
    Enigma*  m_enigma;
    shint    m_letters;
    shint    m_rotor_count;
    Graph*        path_graph;
    PointerGraph* path_graph_wide;
    shint    m_start_node; //hash of starting node, probably 0
    shint    current_position; //XXXmight overshadow?
    set<vector<shint>> position_set;
    CT_mode  mode;

  public:
    ConfigurationTracker(Enigma *, const int);
    ~ConfigurationTracker();
    //vector<pair<Edge, Engage>> out;
    const Graph* get_graph();
    void initialize_position_set();
    void make_tight_graph();
    void make_wide_graph();

    //interface for moving around in the graph
    
    //iterator of engaged notches, includes the whole path, ie going backwards to.
    //Edge next(); //XXX important, tell if going backwards.
    //bool is_leaf();
    //vector<vector<shint>> get_ring_setting_from_path(vector<vector<shint>> path);
    vector<pair<Engage, Engage_direction>> path_iterator();
    void print_path_iterator();
};
#endif