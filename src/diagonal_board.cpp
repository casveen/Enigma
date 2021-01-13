#include "bombe.hpp" 

DiagonalBoard::DiagonalBoard(int t_bundles) {
    // initialize, in triangle
    for (int b= 0; b < t_bundles; b++) {
        // make wires for bundle
        vector<Wire *> wires;
        for (int w= 0; w <= b; w++) { wires.push_back(new Wire()); }
        // push onto bundle
        m_bundles.push_back(wires);
    }
    // fix symmetries
    for (int b= 0; b < t_bundles; b++) {
        for (int w= b + 1; w < t_bundles; w++) { m_bundles.at(b).push_back(m_bundles.at(w).at(b)); }
    }
}
DiagonalBoard::~DiagonalBoard() {
    for (unsigned int b= 0; b < m_bundles.size(); ++b) {
        for (unsigned int w= 0; w <= b; ++w) { delete m_bundles.at(b).at(w); }
    }
    for (auto bundle : m_bundles) {
        bundle.clear();
        bundle.shrink_to_fit();
    }
    m_bundles.clear();
    m_bundles.shrink_to_fit();
}
Wire *DiagonalBoard::get_wire(int t_bundle, int t_wire) const {
    return m_bundles.at(t_bundle).at(t_wire);
}
void DiagonalBoard::activate(int t_bundle, int t_wire) { get_wire(t_bundle, t_wire)->flow(); }
void DiagonalBoard::wipe() {
    for (unsigned int bundle= 0; bundle < m_bundles.size(); ++bundle) {
        for (unsigned int wire= 0; wire <= bundle; ++wire) { get_wire(bundle, wire)->kill(); }
    }
}
void DiagonalBoard::reset() {
    for (unsigned int bundle= 0; bundle < m_bundles.size(); ++bundle) {
        for (unsigned int wire= 0; wire <= bundle; ++wire) { get_wire(bundle, wire)->reset(); }
    }
}
void DiagonalBoard::connect(int t_bundle_1, int t_wire_1, int t_bundle_2, int t_wire_2) {
    //cout<<t_bundle_1<<","<<t_wire_1<<" <-> "<<t_bundle_2<<","<<t_wire_2<<"\n";
    get_wire(t_bundle_1, t_wire_1)->connect(get_wire(t_bundle_2, t_wire_2));
    get_wire(t_bundle_2, t_wire_2)->connect(get_wire(t_bundle_1, t_wire_1));
}
void DiagonalBoard::disconnect(int t_bundle_1, int t_wire_1, int t_bundle_2, int t_wire_2) {
    //cout<<t_bundle_1<<","<<t_wire_1<<" <-> "<<t_bundle_2<<","<<t_wire_2<<"\n";
    get_wire(t_bundle_1, t_wire_1)->disconnect(get_wire(t_bundle_2, t_wire_2));
    get_wire(t_bundle_2, t_wire_2)->disconnect(get_wire(t_bundle_1, t_wire_1));
}
void DiagonalBoard::connect_enigma(const shint *encryption, shint t_from, shint t_to) {
    int m_letters= m_bundles.size();
    for (int i= 0; i < m_letters; i++) { connect(t_from, i, t_to, encryption[i]); }
}
void DiagonalBoard::disconnect_enigma(const shint *encryption, shint t_from, shint t_to) {
    int m_letters= m_bundles.size();
    for (int i= 0; i < m_letters; i++) { disconnect(t_from, i, t_to, encryption[i]); }
}

int DiagonalBoard::bundle_sum(int bundle) const {
    int sum= 0;
    for (unsigned int i= 0; i < m_bundles.size(); ++i) { sum+= get_wire(bundle, i)->get_live(); }
    return sum;
}
void DiagonalBoard::print() const {
    unsigned int bundle_size= m_bundles.size();
    // cout<<"bundle size "<<bundle_size<<"\n";
    cout << "  ";
    for (unsigned int b= 0; b < bundle_size; b++) { cout << (char)(b + (int)'a') << " "; }
    cout << "\n";
    for (unsigned int b= 0; b < bundle_size; b++) {
        // cout<<"wires size "<<m_bundles.at(b).size()<<"\n";
        cout << (char)(b + (int)'A') << " ";
        for (unsigned int w= 0; w < m_bundles.at(b).size(); w++) {
            Wire *wire= m_bundles.at(b).at(w);
            cout << ((wire->get_live()) ? '1' : ' ') << " ";
        }
        cout << "\n";
    }
}
void DiagonalBoard::print_connections() const {
    unsigned int bundle_size= m_bundles.size();
    cout << "   ";
    for (unsigned int b= 0; b < bundle_size; b++) { cout << (char)(b + (int)'a') << "  "; }
    cout << "\n";
    for (unsigned int b= 0; b < bundle_size; b++) {
        // cout<<"wires size "<<m_bundles.at(b).size()<<"\n";
        cout << (char)(b + (int)'A') << " ";
        for (unsigned int w= 0; w < m_bundles.at(b).size(); w++) {
            Wire *wire= m_bundles.at(b).at(w);
            printf("%2ld ", wire->get_connections()->size());
        }
        cout << "\n";
    }
}
void DiagonalBoard::print_live() const {
    int bundle_size= m_bundles.size();
    for (int b= 0; b < bundle_size; b++) { cout << " | "; }
    cout << "\n";
    for (int b= 0; b < bundle_size; b++) { cout << " " << (char)(b + (int)'A') << " "; }
    cout << "\n";
    // cout<<"bundle size "<<bundle_size<<"\n";
    for (int b= 0; b < bundle_size; b++) {
        // cout<<"wires size "<<m_bundles.at(b).size()<<"\n";
        printf("%2d ", bundle_sum(b));
    }
    cout << "\n";
    cout << "\n";
}
bool DiagonalBoard::bundle_contradiction(int bundle) const {
    int ones= 0, zeros= 0;
    // we have a contradiction if there is not exactly 1 or exactly 25 live
    // wires
    for (unsigned int i= 0; i < m_bundles.size(); ++i) {
        get_wire(bundle, i)->get_live() == 0 ? ++zeros : ++ones;
        //  1 impossible, 25 impossible
        if (ones >= 2 && zeros >= 2) return true;
    }
    // if only ones or only zeros, we have a contradiciton
    if (ones == 0 || zeros == 0) return true;
    // otherwise, the bundle wiring is valid(no contradiction)
    return false;
}
