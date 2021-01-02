

//LIBS += -llapack -lblas -larmadillo
#include <connections.hpp>




//models connections between wires.
//represented by a matrix M, where M[i,j] are how many connections there are between wire i and wire j.
//there can be more than one connection between two wires.

//by the nature of connection between wires, the matrix is
//-symmetric (if wire i is wired to wire j, j is also wired to i), M is symmetric
//-reflexive (any wire is connected to itself), the diagonal of M is all ones(or more) 
//-transitive(if i is wired to j, and j to k, then i must be wired to k), M[i,j]>0 and M[j,k]>0 => M[j,k]>0
    Connections::Connections(const int wires) : m_wires{wires}, connection_matrix{wires, wires}{
        connection_matrix.eye(wires, wires);
        connection_matrix = symmatu(connection_matrix); //hope it gets some flags that optimizes multiplication...
    }

    //enforces symmetry manually, bad solution    //tHIS DOES NOT WORK!!
    void Connections::connect(int wire_from, int wire_to) {
        //std::cout<<"connecting "<<wire_from<<" to "<<wire_to<<"\n";
        connection_matrix(std::min(wire_from, wire_to), std::max(wire_from, wire_to))++;
    }

    //enforces symmetry manually, bad solution
    void Connections::disconnect(int wire_from, int wire_to) {
        if (connection_matrix(std::min(wire_from, wire_to), std::max(wire_from, wire_to))>0) {
            connection_matrix(std::min(wire_from, wire_to), std::max(wire_from, wire_to))--;
        }
    }

    void Connections::closure() {
        //enforce transitivity, reflexivity and symmetry. Reflexivity is assumed
        //symmetry, upper
        //transitivity
        //for (int i = 0;         i < m_wires; i++) {
        //    for (int j = 0;     j < m_wires; j++) {
        //        for (int k = 0; k < m_wires; k++) {
        //            if (connection_matrix(i,j)>0 and connection_matrix(j,k)>0 and connection_matrix(i,k)<=0)
        //        }
        //    }
        //}
        
        connection_matrix = symmatu(connection_matrix);
        transitive_closure();
    }

    //only works on upper half
    void Connections::transitive_closure() {
        for (int k = 0;         k < m_wires; k++) {
            for (int i = 0;     i < m_wires; i++) {
                for (int j = 0; j < m_wires; j++) {
                    connection_matrix(i,j) = std::min(connection_matrix(i,k), connection_matrix(k,j));  
                }
            }
        }
    }


    const void Connections::print() {
        connection_matrix.print(std::cout);
    }

    const bool Connections::is_transitive() {
        for (int i = 0;         i < m_wires; i++) {
            for (int j = 0;     j < m_wires; j++) {
                for (int k = 0; k < m_wires; k++) {
                    //check that if M[i,j]>0, M[j,k]>0, then M[i,k]>0 (but what should the exact values be?)
                    if (connection_matrix(i,j)>0 and connection_matrix(j,k)>0 and connection_matrix(i,k)<=0) {
                        print();
                        std::cout<<i<<"<->"  <<j<<"("<<connection_matrix(i,j)<<") and "
                                 <<j<<"<->"  <<k<<"("<<connection_matrix(j,k)<<") but "
                                 <<i<<"<-/->"<<k<<"("<<connection_matrix(i,k)<<")\n";
                        return false;
                    }
                }
            }
        }
        return true;
    }

    const bool Connections::is_symmetric() {
        for (int i = 0;         i < m_wires; i++) {
            for (int j = 0;     j < m_wires; j++) {
                if (connection_matrix(i,j) != connection_matrix(j,i)) {
                    return false;
                }
            }
        }
        return true;
    }

/*
void set_live(int wire) {
    m_live_wires.at(wire)=true;
}

void set_dead(int wire) {

}

bool check_live(int wire) {
    
}*/

Bundle_connections::Bundle_connections(int bundles, int wires_per_bundle) : Connections(bundles * wires_per_bundle){
    m_bundles          = bundles;
    m_wires_per_bundle = wires_per_bundle;
}

void Bundle_connections::connect(int bundle_from, int wire_from, int bundle_to, int wire_to) {
    Connections::connect(bundle_from*m_wires_per_bundle+wire_from, bundle_to*m_wires_per_bundle+wire_to);
}

void Bundle_connections::disconnect(int bundle_from, int wire_from, int bundle_to, int wire_to) {
    Connections::disconnect(bundle_from*m_wires_per_bundle+wire_from, bundle_to*m_wires_per_bundle+wire_to);
}





    Enigma_connections::Enigma_connections(int wires) : Bundle_connections(wires, wires) {
        for (int bundle_from=0;   bundle_from<wires; bundle_from++) {
            for (int bundle_to=0; bundle_to  <wires; bundle_to++)   {
                if (bundle_from != bundle_to)                     {             
                    Bundle_connections::connect(bundle_from, bundle_to, bundle_to, bundle_from);
                }
            }
        }
    }
/*
void Bombe_connections::connect_enigma(int *encryption, int t_from, int t_to) {
    int m_letters= m_bundles;
    // vector<Wire *> bundle_1 = m_bundles[t_from];
    // vector<Wire *> bundle_2 = m_bundles[t_to];
    for (int i= 0; i < m_letters; i++) {
        bundle_1[i]->connect(bundle_2[encryption[i]]);
        bundle_1[encryption[i]]->connect(bundle_2[i]);
    }

    for (int i= 0; i < m_letters; i++) { Bundle_connections::connect(t_from, i, t_to, encryption[i]); }
}*/