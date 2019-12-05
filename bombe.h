#ifndef BOMBE_H   // include guard
#define BOMBE_H

using namespace std;
#include "enigma.h"
#include <chrono>

// wiring
class Wire {
  private:
    vector<Wire *> m_connections;
    int            m_live= 0;   // 0: dead, 1: live, -1: just activated
  public:
    ~Wire();
    void            flow();   // make wire live, and also wires connected to it
    int             get_live() const;   // 0 dead, -1 just set to live, 1 live
    void            set_live(int);
    void            kill();   // set to 0
    void            reset();
    void            connect(Wire *w);
    vector<Wire *> *get_connections();
};

class DiagonalBoard {
  private:
    vector<vector<Wire *>> m_bundles;

  public:
    DiagonalBoard(int);
    ~DiagonalBoard();
    Wire *get_wire(int bundle, int wire) const;
    void  activate(int, int);
    void  connect(int, int, int, int);
    void  print() const;
    void  print_live() const;
    void  connect_enigma(vector<pair<int, int>> *, int, int);
    void  wipe();
    void  reset();
    bool  bundle_contradiction(int) const;
    int   bundle_sum(int) const;
};

struct BombeSetting {
    // if true, stops as soon as a valid config is found
    bool   stop_on_first_valid  = false;
    int    max_ring_settings    = 26 * 26;
    string starting_ring_setting= "AAA";   // TODO should adapt to more rotors
    string starting_rotor_positions= "AAA";
    bool   interactive_wiring_mode = false;
    const bool time_performance    = true;   // XXX can only be set in code...
    double     performance_mean    = 0;      // performance of a rs run
    double     performance_variance= 0;
};

class Bombe {
  private:
    int                            m_letters= 26, m_rotor_count= 3;
    DiagonalBoard *                m_diagonal_board;
    Enigma *                       m_enigma;
    vector<vector<pair<int, int>>> m_enigma_encryptions;   // track encryptions
    struct BombeSetting            m_setting;

  public:
    Bombe(const std::initializer_list<Rotor> rotors, const Reflector reflector);
    ~Bombe();
    // setters
    void set_ring_setting(const string);
    void set_rotor_position(const string);
    // getters
    struct BombeSetting &get_setting();
    // other
    vector<int> probable_search(const string, const string);
    int         find_most_wired_letter(const string, const string);
    void        init_enigma_encryptions(int);
    vector<struct EnigmaSetting> analyze(const string, const string);
    void setup_diagonal_board(const string, const string);
    bool check_one_wire(int);
    bool bundle_contradiction(int);
    void print_encryptions() const;
    bool doublecheck_and_get_plugboard();
    void interactive_wirechecking();
};
#endif
