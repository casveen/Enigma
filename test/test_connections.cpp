#include<catch.hpp>
#include<connections.hpp>
#include<stdlib.h>     /* srand, rand */



TEST_CASE("Testing transitivity of connections(small)") {
    int wires = 3;
    Connections connections(wires);
    //add some connections
    connections.connect(0,1);
    connections.connect(1,2);
    connections.closure();
    REQUIRE(connections.is_transitive());
    REQUIRE(connections.is_symmetric());
    
}

TEST_CASE("Testing transitivity of connections(small, both directions)") {
    int wires = 3;
    Connections connections(wires);
    //add some connections
    connections.connect(1,2);
    connections.connect(0,1);
    connections.closure();

    REQUIRE(connections.is_transitive());
    REQUIRE(connections.is_symmetric());
    
}

TEST_CASE("Testing transitivity of connections(four transitives)") {
    int wires = 4;
    Connections connections(wires);
    //add some connections
    connections.closure();

    //connections.print();
    //connections.connect(4,5);
    //connections.print();
    //connections.connect(3,4);
    //connections.print();
    REQUIRE(connections.is_transitive());
    REQUIRE(connections.is_symmetric());
    
}

TEST_CASE("Testing transitivity of connections(medium)") {
    int wires = 10;
    Connections connections(wires);
    //add some connections
    connections.connect(0,1);
    connections.connect(0,2);
    connections.connect(0,3);
    connections.connect(5,0);
    connections.connect(0,6);
    connections.connect(0,7);
    connections.connect(0,9);
    connections.connect(1,3);
    connections.connect(1,4);
    connections.connect(8,1);
    connections.connect(1,9);
    connections.connect(2,5);
    connections.connect(2,6);
    connections.connect(3,5);
    connections.connect(4,9);
    connections.connect(9,5);
    connections.connect(6,7);
    connections.connect(6,8);
    connections.connect(6,9);
    connections.connect(7,8);
    connections.connect(8,9);
    connections.closure();
    REQUIRE(connections.is_transitive());
    REQUIRE(connections.is_symmetric());
}

TEST_CASE("Testing transitivity of connections(with disconnects)") {
    int wires = 8;
    Connections connections(wires);
    srand(42);
    //add some connections
    for (int i = 0; i < 20; i++) {
        connections.connect(rand()%wires, rand()%wires);
        
    }
    //make some disconnects
    for (int i = 0; i < 10; i++) {
        connections.disconnect(rand()%wires, rand()%wires);
    }
    //add some connections
    for (int i = 0; i < 5; i++) {
        connections.connect(rand()%wires, rand()%wires);
    }
    connections.closure();
    REQUIRE(connections.is_symmetric());
    REQUIRE(connections.is_transitive());
}

TEST_CASE("Testing transitivity of connections(large, with disconnects)") {
    int wires = 26;
    Connections connections(wires);
    srand(69);
    //add some connections
    for (int i = 0; i < 20; i++) {
        connections.connect(rand()%wires, rand()%wires);
    }
    //make some disconnects
    for (int i = 0; i < 20; i++) {
        connections.disconnect(rand()%wires, rand()%wires);
    }
    //add some connections
    for (int i = 0; i < 10; i++) {
        connections.connect(rand()%wires, rand()%wires);
    }
    connections.closure();
    REQUIRE(connections.is_symmetric());
    REQUIRE(connections.is_transitive());
}

TEST_CASE("Testing transitivity of connections(very large, with disconnects)") {
    int wires = 1000;
    Connections connections(wires);
    srand(123);
    //add some connections
    for (int i = 0; i < 200; i++) {
        connections.connect(rand()%wires, rand()%wires);
    }
    //make some disconnects
    for (int i = 0; i < 100; i++) {
        connections.disconnect(rand()%wires, rand()%wires);
    }
    //add some connections
    for (int i = 0; i < 100; i++) {
        connections.connect(rand()%wires, rand()%wires);
    }
    connections.closure();
    REQUIRE(connections.is_symmetric());
    REQUIRE(connections.is_transitive());
}
