// main() provided by Catch in file test.cpp.
#include "catch.hpp"
#include "enigma.hpp"
#include "rotors.cpp"
#include "configuration_tracker.hpp"

const int MAX_TESTS   = 100;
const int MESSAGE_SIZE= 26 * 10;
const int WIRES       = 26;
const int WHEELS      = 4;


/*
tracking from 000
a three rotor enigma with 6 letters over a length 4 gives the tree
000
 |-------+-------+--+
 |       \       \   \
101     111     110 100
 |--- ---+------ |-- |---
 |   v   |   \  \|  \|   \
201 211 222 212 221 210 200
 |-- |-- | --|  /|
 |  \|  \|/--+-- | ETC
301 311 322 312 321 310 300
*/
shint simple_hash(string position, int letters) {
    shint out = 0;
    for (char l : position) {
        out*=letters;
        out+=((shint) l - (shint) '0');
    }
    return out;
}

TEST_CASE("Testing configuration tracker: simple tracking tree") {
    //make an enigma, with position(ie not rotor position!) 000, and single notches (say 000)
    //check if the graph made correspinds with the one in
    //p000n000l6c3.png

    vector<Edge> edges {
        Edge {simple_hash("000",6), simple_hash("101",6) ,vector<bool>{true,false,true}},
        Edge {simple_hash("000",6), simple_hash("111",6) ,vector<bool>{true,true,true}},
        Edge {simple_hash("000",6), simple_hash("110",6) ,vector<bool>{true,true,false}},
        Edge {simple_hash("000",6), simple_hash("100",6) ,vector<bool>{true,false,false}},
        
        Edge {simple_hash("101",6), simple_hash("201",6) ,vector<bool>{true,false,false}},
        Edge {simple_hash("101",6), simple_hash("211",6) ,vector<bool>{true,true,false}},
        
        Edge {simple_hash("111",6), simple_hash("211",6) ,vector<bool>{true,false,false}},
        Edge {simple_hash("111",6), simple_hash("222",6) ,vector<bool>{true,true,true}},
        Edge {simple_hash("111",6), simple_hash("212",6) ,vector<bool>{true,false,true}},
        Edge {simple_hash("111",6), simple_hash("221",6) ,vector<bool>{true,true,false}},
        
        Edge {simple_hash("110",6), simple_hash("221",6) ,vector<bool>{true,true,true}},
        Edge {simple_hash("110",6), simple_hash("210",6) ,vector<bool>{true,false,false}},
        
        Edge {simple_hash("100",6), simple_hash("210",6) ,vector<bool>{true,true, false}},
        Edge {simple_hash("100",6), simple_hash("200",6) ,vector<bool>{true,false,false}},

        Edge {simple_hash("201",6), simple_hash("301",6) ,vector<bool>{true,false,false}},
        Edge {simple_hash("201",6), simple_hash("311",6) ,vector<bool>{true,true,false}},
        
        Edge {simple_hash("211",6), simple_hash("311",6) ,vector<bool>{true,false,false}},
        Edge {simple_hash("211",6), simple_hash("322",6) ,vector<bool>{true,true, true}},
        Edge {simple_hash("211",6), simple_hash("321",6) ,vector<bool>{true,true, false}},
        
        Edge {simple_hash("222",6), simple_hash("322",6) ,vector<bool>{true,false,false}},

        Edge {simple_hash("212",6), simple_hash("322",6) ,vector<bool>{true,true ,false}},
        Edge {simple_hash("212",6), simple_hash("312",6) ,vector<bool>{true,false,false}},

        Edge {simple_hash("221",6), simple_hash("322",6) ,vector<bool>{true,false,true}},
        Edge {simple_hash("221",6), simple_hash("321",6) ,vector<bool>{true,false,false}},

        Edge {simple_hash("210",6), simple_hash("321",6) ,vector<bool>{true,true,true}},
        Edge {simple_hash("210",6), simple_hash("310",6) ,vector<bool>{true,false,false}},
        
        Edge {simple_hash("200",6), simple_hash("310",6) ,vector<bool>{true,true, false}},
        Edge {simple_hash("200",6), simple_hash("300",6) ,vector<bool>{true,false,false}}};
    //make the correct graph, then make the graph with the tracker.
    Graph correct_graph(edges);
    Enigma enigma= Enigma(3, 6);
    enigma.randomize();
    enigma.set_ring_setting("AAA");
    enigma.set_rotor_position("AAA");
    //position (NOT ROTOR POSITION!) is now 000.
    //compare the graphs edge-for-edge,starting with 000
    ConfigurationTracker tracker(&enigma,3);
    Graph* tracked_graph = tracker.get_graph();
    //ok we are set up, lets compare
    for (int t= 0; t < MAX_TESTS; ++t) {




        Rotor rotor= Rotor(WIRES);
        rotor.randomize();
        REQUIRE(rotor.is_valid());
    }
}

//check wether the two path graphs from trackers are isomorphic, by checking notch engages
bool test_isomorph_tracks(Graph* g1, Graph* g2, shint rotors) {
    //both start with element 0
    vector<set<shint>> adj_list1 = g1.get_adjacency_list();
    vector<set<shint>> adj_list2 = g2.get_adjacency_list();
    shint              pos = 0;
    //set<shint>   



}