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
    Graph(shint, shint);
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
    int   m_rotor_count;
    class PointerGraphEdge;

    class Node {
      private:
        set<PointerGraphEdge> connected_to;

      public:
        Node();
        virtual ~Node() {};
        void connect(pair<Node*, vector<bool>>);
        set<PointerGraphEdge> get_connections();
    };

    class Leaf : public Node {
      private:
        vector<vector<shint>> ring_settings;
      public:
        ~Leaf() {};
        vector<vector<shint>>& get_ring_settings();
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
    PointerGraph(shint, int);
    ~PointerGraph();
    void            add_edges(vector<Edge> const &, vector<shint>);
    shint           count_edges();
    shint           count_edges_inner(Node *);
    vector<pair<Engage, Engage_direction>> path_iterator();
    vector<pair<Engage, Engage_direction>> path_iterator_inner(Node* from, shint depth);
    vector<vector<vector<shint>>>          ring_settings_iterator();
    vector<vector<vector<shint>>>          ring_settings_iterator_inner(Node* from, shint depth);
};

class ConfigurationTracker {
  private:
    bool verbose=true;
    shint    m_length;
    Enigma*  m_enigma;
    shint    m_letters;
    shint    m_rotor_count;
    shint    m_start_node; //hash of starting node, probably 0
    shint    current_position; //XXXmight overshadow?
    set<vector<shint>> position_set;
    CT_mode  mode;
    //tight
    Graph*   path_graph;
    //wide
    PointerGraph*                          path_graph_wide;
    vector<pair<Engage, Engage_direction>> m_path_iterator;
    vector<vector<vector<shint>>>          m_ring_settings_iterator;
    vector<shint*>                         m_positions_iterator;
    //flags for deterining what parts were made
    bool made_path_iterator=false, 
         made_ring_settings_iterator=false, 
         made_position_iterator=false;
    

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
    void                                          make_positions_iterator();
    void                                          make_ring_settings_iterator();
    void                                          make_path_iterator();
    const vector<pair<Engage, Engage_direction>>& get_path_iterator();
    const vector<vector<vector<shint>>>&          get_ring_settings_iterator();
    const vector<shint*>&                         get_positions_iterator();

    void print_path_iterator();
};
#endif