// wiring
class Wire {
  private:
    vector<Wire *> m_connections;
    bool           m_live= false;   // 0: dead, 1: live, -1: just activated
  public:
    ~Wire();
    void            flow();   // make wire live, and also wires connected to it
    bool            get_live() const;
    void            set_live(bool);
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
    Wire *get_wire(int, int) const;
    void  activate(int, int);
    void  connect(int, int, int, int);
    void  print() const;
    void  print_live() const;
    void  print_connections() const;
    void  connect_enigma(int *, int, int);
    void  wipe();
    void  reset();
    bool  bundle_contradiction(int) const;
    int   bundle_sum(int) const;
};