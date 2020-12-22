#ifndef CON_H   // include guard
#define CON_H

#include <armadillo>
#include <iostream>


//models connections between wires.
//represented by a matrix M, where M[i,j] are how many connections there are between wire i and wire j.
//there can be more than one connection between two wires.

//by the nature of connection between wires, the matrix is
//-symmetric (if wire i is wired to wire j, j is also wired to i), M is symmetric
//-reflexive (any wire is connected to itself), the diagonal of M is all ones(or more) 
//-transitive(if i is wired to j, and j to k, then i must be wired to k), M[i,j]>0 and M[j,k]>0 => M[j,k]>0
class Connections {
    protected:
    const int       m_wires;
    arma::Mat<int>  connection_matrix;    //enforces symmetry manually, bad solution
    arma::Mat<int>  connection_closure_matrix;
    bool closed = false;

    public:
    Connections(const int wires);
    void connect(int wire_from, int wire_to);
    void disconnect(int wire_from, int wire_to);
    void closure();
    void transitive_closure();
    const void print();
    const bool is_transitive();
    const bool is_symmetric();

};




class Bundle_connections : public Connections {
    protected:
    int m_bundles;
    int m_wires_per_bundle; //not the same as connctions.m_wires, these are wires_per_bundle

    public:
    Bundle_connections(int bundles, int wires_per_bundle);
    void connect(int bundle_from, int wire_from, int bundle_to, int wire_to);
    void disconnect(int bundle_from, int wire_from, int bundle_to, int wire_to);
    void set_live(int bundle, int wire);
    void set_dead(int bundle, int wire);
    bool check_live(int bundle, int live);
};




class Enigma_connections : public Bundle_connections {
    public:
    Enigma_connections(int);
    //void connect_enigma(int *, int , int ) ;
};


#endif