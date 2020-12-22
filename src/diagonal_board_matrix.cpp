#include "bombe.hpp" 
#include "diagonalBoardMatrix.hpp"

DiagonalBoard::DiagonalBoard(int t_bundles) : connections{new Enigma_connections(t_bundles)} {
}

DiagonalBoard::~DiagonalBoard() {
    delete connections;
}

Wire *DiagonalBoard::get_if_live(int t_bundle, int t_wire) const {
    return m_bundles.at(t_bundle).at(t_wire).islive; //XXX
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
    get_wire(t_bundle_1, t_wire_1)->connect(get_wire(t_bundle_2, t_wire_2));
    // get_wire(t_bundle_1, t_wire_2)->connect(get_wire(t_bundle_2, t_wire_1));
    get_wire(t_bundle_2, t_wire_2)->connect(get_wire(t_bundle_1, t_wire_1));
    // XXX this is the correct secoind one!!!
    // get_wire(t_bundle_2,
    // t_wire_2)->connect(get_wire(t_bundle_1, t_wire_1));
}
void DiagonalBoard::connect_enigma(int *encryption, int t_from, int t_to) {
    int m_letters= m_bundles.size();
    // vector<Wire *> bundle_1 = m_bundles[t_from];
    // vector<Wire *> bundle_2 = m_bundles[t_to];
    /*for (int i= 0; i < m_letters; i++) {
        bundle_1[i]->connect(bundle_2[encryption[i]]);
        bundle_1[encryption[i]]->connect(bundle_2[i]);
    }*/

    for (int i= 0; i < m_letters; i++) { connect(t_from, i, t_to, encryption[i]); }
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
