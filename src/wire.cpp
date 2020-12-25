#include "bombe.hpp" 

Wire::~Wire() {
    // do not deallocate aythng, handled by diag board
    m_connections.clear();
    m_connections.shrink_to_fit();
}

void Wire::flow() {
    m_live= true;
    // activate connected dead wires
    for (Wire *wire : m_connections) {
        if (wire->get_live() == false) { wire->flow(); }
    }
}

bool Wire::get_live() const { return m_live; }

void Wire::kill() { m_live= false; }

void Wire::set_live(bool t_set) { m_live= t_set; }

void Wire::reset() {
    kill();
    m_connections.clear();
}

vector<Wire *> *Wire::get_connections() { return &m_connections; }

void            Wire::connect(Wire *w) { m_connections.push_back(w); }
void            Wire::disconnect(Wire *w) { 
    //cout<<"disconnecting a wire\n";
    m_connections.pop_back(); } //TODO, due to way used, can merely discard last