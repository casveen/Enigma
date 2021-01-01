#include "configuration_tracker.hpp"

Graph::Graph(int N, shint t_depth) : adjList{N} {
    m_depth = t_depth;
    root  = 0;
    //set<pair<shint,vector<bool>>> s;
    //for (int i =0; i<N; i++) {
    //    adjList.push_back(set<pair<shint,vector<bool>>>(s));
    //}
}
Graph::~Graph() { 
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

template <typename T>
void append(vector<T> &v1, const vector<T> &v2) {
    v1.insert(v1.end(), v2.begin(), v2.end());
}











vector<pair<Engage, Engage_direction>> Graph :: path_iterator() {
    //from start node.
    vector<pair<Engage, Engage_direction>> out;
    //print_adjacency_list();
    out = path_iterator_inner(root, 0); //should be 0?
    return out; //XXX store in configurationtracker.
}

vector<pair<Engage, Engage_direction>> Graph :: path_iterator_inner(shint from, shint depth) {
    //from start node.
    vector<pair<Engage, Engage_direction>> out;
    //cout<<"->initialized vector "<<"\n";
    //cout<<"adj list has size "<<path_graph->get_adjacency_list().size();
    //if (!adjList.at(from).empty()) {
    for (auto to_and_engages : adjList.at(from)) {
        /*cout<<from<<"->"<<to_and_engages.first<<"      ";
        for (shint i : unhash_position(from, position_set) ) {
            cout<<i<<" ";
        }
        cout<<" -> ";
        for (shint i : unhash_position(to_and_engages.first, position_set) ) {
            cout<<i<<" ";
        }
        cout<<"     ";
        for (bool b : to_and_engages.second ) {
            b?cout<<"T":cout<<"F";
        }
        cout<<"\n";*/

        //in
        if (depth<m_depth) {
            out.push_back(make_pair(to_and_engages.second, Engage_direction::forward));
            append(out, path_iterator_inner(to_and_engages.first, depth+1));
        //out
        /*cout<<from<<"<-"<<to_and_engages.first<<" ";
        for (bool b : to_and_engages.second ) {
            b?cout<<"T":cout<<"F";
        }
        cout<<"\n";*/
            out.push_back(make_pair(to_and_engages.second, Engage_direction::backward));
        }
    }//}
    return out;
}

void Graph::print_adjacency_list() {
    int i=0;
    cout<<"\n";
    for(set<pair<shint,vector<bool>>> s : adjList) {
        cout<<i<<": ";
        for(pair<shint,vector<bool>> p : s)        {
            cout<<p.first<<" ";
        }
        cout<<"\n";
        i++;
    }
}




/*
Graph::Graph_Pointer(int N) { 
    adjList.resize(N);
}
Graph::~Graph() { 
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
}*/






















PointerGraph::PointerGraph(shint t_depth, int t_rotor_count) {
    m_depth = t_depth;
    m_rotor_count = t_rotor_count;
    root = new PointerGraph::Node();
    nodeList.push_back(root);
}

PointerGraph::~PointerGraph() {
    for (Node* n : nodeList) {
        delete n;
    }
    nodeList.clear();
}

void  PointerGraph::add_edges(vector<Edge> const& edges, vector<shint> ring_setting) {
    //for each edge, look in current node for edges, if not found make a new path
    Node* current_node = root;
    Node* temp_node;
    /*for(PointerGraphEdge pedge : current_node->get_connections()) {
        cout<<pedge.get_node()<<"\n";
    }*/
    //auto pg_edge;
    //cout<<"adding edges\n";
    int depth = 1;
    for (auto edge: edges) {
        
        //cout<<"e: ";
        /*for(bool b: edge.engages) {
            cout<<(b?"T":"F");
        }*/
        //pg_edge = current_node->get_connections().find(PointerGraphEdge{current_node, edge.engages});
        //cout<<(pg_edge)<<"\n";
        //cout<<(current_node->get_connections().end())<<"\n";
        //if (current_node->get_connections().find(PointerGraphEdge{current_node, edge.engages}) == current_node->get_connections().end()) {
        if (current_node->get_connections().count(PointerGraphEdge{current_node, edge.engages}) == 0) {
            //cout<<"new\n";
            //not found
            if (depth < m_depth) {
                temp_node = new Node();
            } else {
                temp_node = new Leaf();
            }
            current_node->connect(make_pair(temp_node, edge.engages));
            current_node=temp_node;
            nodeList.push_back(current_node);
            //cout<<"n";

        } else {
            //found
            //cout<<"o";
            current_node = current_node->get_connections().find(PointerGraphEdge{current_node, edge.engages})->get_node();
            //cout<<"got old\n";
        }

        if (depth == m_depth) {
            //add the ring setting to the leaf node
            ((PointerGraph::Leaf*) current_node)->get_ring_settings().push_back(ring_setting);
        }
        depth++;
    }
    //cout<<"\n";
    //cin.get();
}

shint PointerGraph::count_edges() {
    return count_edges_inner(root);
}

shint PointerGraph::count_edges_inner(Node* n) {
    shint sum = 0;
    for (PointerGraphEdge edge : n->get_connections()) {
        sum += 1 + count_edges_inner(edge.get_node());
    }
    return sum;
}

//PointerGraph::Node() {
//};
PointerGraph::Node::Node() {
}
void PointerGraph::Node::connect(pair<Node*, vector<bool>> node_and_engage) {
    connected_to.insert(PointerGraphEdge(node_and_engage.first, node_and_engage.second));
}

set<PointerGraph::PointerGraphEdge> PointerGraph::Node::get_connections() {
    return connected_to;
}


vector<vector<shint>>& PointerGraph::Leaf::get_ring_settings() {
    return ring_settings;
}



PointerGraph::PointerGraphEdge::PointerGraphEdge(Node* t_to, vector<bool> t_engages) {
    to = t_to;
    engages = t_engages;
}


PointerGraph::Node* PointerGraph::PointerGraphEdge::get_node() const {
    return to;
}

vector<bool> PointerGraph::PointerGraphEdge::get_engages() const {
    return engages;
}

shint hash_engages(vector<bool> in) {
    shint out = 0;
    for (bool b: in) {
        out*=2;
        out+=b?1:0;
    }
    return out;
}

bool PointerGraph::PointerGraphEdge::operator<(PointerGraphEdge const& rhs) const {
    return hash_engages(get_engages())<hash_engages(rhs.get_engages());
}













/*
void append(vector<pair<Engage, Engage_direction>> &v1, const vector<pair<Engage, Engage_direction>> &v2) {
    v1.insert(v1.end(), v2.begin(), v2.end());
}*/










vector<pair<Engage, Engage_direction>> PointerGraph :: path_iterator() {
    //from start node.
    vector<pair<Engage, Engage_direction>> out;
    out = path_iterator_inner(root, 0); //should be 0?
    return out; //XXX store in configurationtracker.
}

vector<pair<Engage, Engage_direction>> PointerGraph :: path_iterator_inner(Node* from, shint depth) {
    //from start node.
    vector<pair<Engage, Engage_direction>> out(0);
    for (PointerGraphEdge edge : from->get_connections()) {
        //in
        if (depth<m_depth) {
            out.push_back(make_pair(edge.get_engages(), Engage_direction::forward));
            append(out, path_iterator_inner(edge.get_node(), depth+1));
        //out
            out.push_back(make_pair(edge.get_engages(), Engage_direction::backward));
        }
    }
    if (depth == m_depth) {
        out.push_back(make_pair(vector<bool>(m_rotor_count, false), Engage_direction::stop));
    }
    return out;
}

vector<vector<vector<shint>>> PointerGraph :: ring_settings_iterator() {
    //from start node.
    vector<vector<vector<shint>>>  out;
    out = ring_settings_iterator_inner(root, 0); //should be 0?
    return out; //XXX store in configurationtracker.
}

vector<vector<vector<shint>>>  PointerGraph :: ring_settings_iterator_inner(Node* from, shint depth) {
    //from start node.
    vector<vector<vector<shint>>> out(0);
    for (PointerGraphEdge edge : from->get_connections()) {
        //in
        if (depth<m_depth) {
            //out.push_back(make_pair(edge.get_engages(), Engage_direction::forward));
            append(out, ring_settings_iterator_inner(edge.get_node(), depth+1));
        //out
            //out.push_back(make_pair(edge.get_engages(), Engage_direction::backward));
        }
    }
    if (depth == m_depth) {
        out.push_back(((PointerGraph::Leaf *) from)->get_ring_settings());
    }
    return out;
}


/*
class PointerGraph { 
  private:
    class Node {
      private:
        set<pair<shint, vector<bool>>> connected_to;
      public:
        void connect(pair<shint, vector<bool>>);
        set<pair<shint, vector<bool>>> get_connections();
    };

    public:
    adjacency_list adjList;
    Node           root;
 
    // Graph Constructor
    PointerGraph(int);
    ~PointerGraph();
    void            add_edges(vector<Edge> const &);
    shint           count_edges();
};





*/
































