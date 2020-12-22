#include "connections.hpp"

class DiagonalBoard {
    private: 
    Enigma_connections* connections;

    public:
    void  reset();
    void  connect_enigma();
    bool  bundle_contradiction(int);
    void  activate(int, int); //does nothing, as there is only one activated at a time.
    void  print();
    void  print_live();
    void  print_connections();
    shint bundle_sum(int);
    bool  get_if_live(int bundle, int wire);
    void  wipe();
};