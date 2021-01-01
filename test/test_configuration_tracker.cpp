#include<catch.hpp>
#include<configuration_tracker.hpp>
#include<stdlib.h>     /* srand, rand */

vector<shint> engage_position(vector<shint> position, vector<bool> engages, Engage_direction direction, int rotor_count, int wires) {
    vector<shint> out(rotor_count, 0);
    for (int i=0; i<(int) engages.size(); i++) {
        out.at(i) = position[i];
        if (engages[i]) {
            switch(direction) {
                case Engage_direction::forward:  out.at(i) = (position[i]+1)%wires;       break;
                case Engage_direction::backward: out.at(i) = (position[i]-1+wires)%wires; break;
                case Engage_direction::stop:                                              break;
            }
        } 
    }
    return out;
}

vector<shint> follow_path_iterator(vector<pair<Engage, Engage_direction>> path_iterator, int rotor_count, int wires) {
    vector<shint> current_position(rotor_count,0);
    //cout<<"testing if path sums to zero\n";
    for (pair<Engage, Engage_direction> engage_and_direction : path_iterator) {
        //cout<<"e";
        current_position = engage_position(current_position, engage_and_direction.first, engage_and_direction.second, rotor_count, wires);
        /*for(auto p:current_position) {
            cout<<p<<" ";
        }*/
        
        /*for (bool b : engage_and_direction.first ) {
            b?cout<<"T":cout<<"F";
        }*/
        //cout<<"\n";
    }
    //cout<<"current_position: \n";
    //for(auto p:current_position) {
    //    cout<<p<<" ";
    //}
    //cout<<"initial position: \n";
    //for(auto p:initial_position) {
    //    cout<<p<<" ";
    //}
    return current_position;
}


TEST_CASE("Testing if following path iteraator of configuration tracker ends up at the initial position(small)") {
    int path_length = 4,
        wires       = 6,
        rotors      = 3;
    Enigma enigma= Enigma(rotors, wires);
    srand(334);
    enigma.randomize();
    ConfigurationTracker tracker(&enigma, path_length);

    //Very quickly exhausts new, but hard to predict
    vector<shint> initial_position(rotors,0);
    vector<shint> current_position = follow_path_iterator(tracker.get_path_iterator(), rotors, wires);
    for(int i=0; i<rotors; i++) {
        CHECK(initial_position[i] == current_position[i]);
    }
}

TEST_CASE("Testing if following path iteraator of configuration tracker ends up at the initial position(realistic)") {
    int path_length = 12,
        wires       = 26,
        rotors      = 3;
    Enigma enigma= Enigma(rotors, wires);
    srand(334);
    enigma.randomize();
    ConfigurationTracker tracker(&enigma, path_length);

    //Very quickly exhausts new, but hard to predict
    vector<shint> initial_position(rotors,0);
    vector<shint> current_position = follow_path_iterator(tracker.get_path_iterator(), rotors, wires);
    for(int i=0; i<rotors; i++) {
        CHECK(initial_position[i] == current_position[i]);
    }
}

TEST_CASE("Testing if following path iteraator of configuration tracker ends up at the initial position(small, but many rotors)") {
    int path_length = 4,
        wires       = 6,
        rotors      = 7;
    Enigma enigma= Enigma(rotors, wires);
    srand(334);
    enigma.randomize();
    ConfigurationTracker tracker(&enigma, path_length);

    //Very quickly exhausts new, but hard to predict
    vector<shint> initial_position(rotors,0);
    vector<shint> current_position = follow_path_iterator(tracker.get_path_iterator(), rotors, wires);
    for(int i=0; i<rotors; i++) {
        CHECK(initial_position[i] == current_position[i]);
    }
}

















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
*/