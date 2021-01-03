#include "bombe.hpp"
const int PROGRESS_BAR_WIDTH= 50;
#include <cmath>


//HELPER FUNCTIONS
void update_performance(double &mean, double &var, std::chrono::duration<double> measurement,
                        int &records) {
    auto   t     = measurement.count();
    double mean_p= mean;
    mean         = mean_p + (t - mean_p) / (records + 1);
    var          = (records * var + (t - mean_p) * (t - mean)) / (records + 1);
    records++;
}

void vector_from_array_inplace(vector<shint> &vec, const shint *array, int length) {
    for (int i= 0; i < length; ++i) { vec[i]= (shint)array[i]; }
}

vector<shint> vector_from_array(const shint *array, int length, bool debug= false) {
    vector<shint> vec;
    // if (debug) { cout << "READING: "; }
    for (int i= 0; i < length; ++i) {
        vec.push_back((shint)array[i]);
        // if (debug) { cout << (shint)array[i] << "-"; }
    }
    // if (debug) { cout << "\n"; }
    return vec;
}

vector<shint> vector_from_string(const string &str) {
    vector<shint> vec;
    for (unsigned int i= 0; i < str.length(); ++i) {
        vec.push_back((shint)((shint)str[i] - (shint)'A'));
    }
    return vec;
}









//the UNITS of the bombe, does the hard work
BombeUnit::BombeUnit(const vector<Rotor> rotors, const Reflector reflector,
                     const bool use_configuration_tracker /*true*/) :
    m_use_configuration_tracker{use_configuration_tracker} {
    // Top constructor
    m_letters       = (rotors.begin())->get_wires();
    m_rotor_count   = rotors.size();
    m_diagonal_board= new DiagonalBoard(m_letters); //diagonalboard
    m_enigma        = new Enigma(rotors, reflector);
    m_identifier    = "";
    for (unsigned int r= 0; r < rotors.size(); ++r) {
        m_identifier+= rotors[r].get_name();
        m_identifier+= "-";
    }
    m_identifier+= reflector.get_name();
}

BombeUnit::BombeUnit(const std::initializer_list<Rotor> rotors, const Reflector reflector,
                     const bool use_configuration_grid /*true*/) :
    BombeUnit(vector<Rotor>(rotors), reflector, use_configuration_grid) {}

BombeUnit::BombeUnit(struct EnigmaSetting enigma_setting,
                     const bool           use_configuration_grid /*true*/) :
    BombeUnit(enigma_setting.rotors, enigma_setting.reflector, use_configuration_grid) {
    // plugboard, rotor position are ignored
    m_setting.starting_ring_setting   = enigma_setting.ring_setting;
    m_setting.starting_rotor_positions= enigma_setting.rotor_position;
}

BombeUnit::~BombeUnit() {
    delete m_diagonal_board;
    delete m_enigma;
    for (shint *encryption : m_enigma_encryptions) { delete[] encryption; }
    m_enigma_encryptions.clear();
    m_enigma_encryptions.shrink_to_fit();
}



// setters
void BombeUnit::set_ring_setting(const string setting) { m_enigma->set_ring_setting(setting); }
void BombeUnit::set_rotor_position(const string setting) { m_enigma->set_rotor_position(setting); }
void BombeUnit::set_identifier(string identifier) { m_identifier = identifier; }



// getters
struct BombeUnitSetting &BombeUnit::get_setting() {
    return m_setting;
}
string BombeUnit::get_identifier() const { return m_identifier; }



// other
//clear all tracks, and setup a new tracking of a set of enigmas from given starting position
void BombeUnit::init_enigma_encryptions(int encryptions, vector<string> &rotor_positions,
                                        vector<vector<shint>> &positions) {
    m_enigma->set_rotor_position(m_setting.starting_rotor_positions);
    for (shint *encryption : m_enigma_encryptions) { delete[] encryption; }
    m_enigma_encryptions.clear();
    rotor_positions.clear();
    rotor_positions.push_back(m_enigma->get_rotor_position_as_string());
    positions.clear();
    positions.push_back(vector_from_array(m_enigma->get_positions(), m_rotor_count));
    for (int i= 0; i < encryptions; ++i) {
        m_enigma->turn();   // enigma turns before encryption
        m_enigma_encryptions.push_back(m_enigma->get_encryption());
        rotor_positions.push_back(m_enigma->get_rotor_position_as_string());
        positions.push_back(vector_from_array(m_enigma->get_positions(), m_rotor_count, true));
    }
}
void BombeUnit::reset_diagonal_board() { m_diagonal_board->reset(); }
void BombeUnit::setup_diagonal_board(const string &ciphertext, const string &crib) {
    for (unsigned int j= 0; j < crib.length(); j++) {
        shint *encryption= m_enigma_encryptions.at(j);
        m_diagonal_board->connect_enigma(encryption, (int)crib[j] - (int)'A',
                                         (int)ciphertext[j] - (int)'A');
    }
}


//the main function, searches for valid configurations
/*
ciphertext:        ciphertext to crack
crib:              corresponding crib, a plaintext that encrypts to a substring of ciphertext
most_wired_letter: when finding a potential crib, this is the letter that is most "wired to"
position:          XXX not used? in triplecheck, but not there either
*/
vector<struct EnigmaSetting> BombeUnit::analyze(const string &ciphertext, const string &crib,
                                                shint most_wired_letter, int position) {
    //general setup
    vector<struct EnigmaSetting> solutions;
    int                          crib_n= crib.length(),
        total_permutations= m_enigma->compute_total_permutations_brute_force() + crib_n - 1,
        ring_settings     = min((int)pow(m_letters, m_rotor_count), m_setting.max_ring_settings);
    ring_settings+= 1;   // maybe only if using CG
    vector<string> rotor_positions;
    vector<vector<shint>> positions;
    m_enigma->set_ring_setting(m_setting.starting_ring_setting);
    
    // for each ring setting
    auto start_ring_setting= std::chrono::system_clock::now();
    for (int rs= 0; rs < ring_settings; ++rs) {
        print_progress(rs, ring_settings, (int)solutions.size());
        init_enigma_encryptions(crib_n, rotor_positions, positions);
        if (m_setting.time_performance) { start_ring_setting= std::chrono::system_clock::now(); }
        // for each rotor position
        for (int j= 0; j < total_permutations - 1; j++) {
            reset_diagonal_board();
            setup_diagonal_board(ciphertext, crib);
            // BEGIN TESTS
            if (check_one_wire(most_wired_letter)) {     // first test
                if (doublecheck_and_get_plugboard()) {   // second test
                    if (tripplecheck(crib, ciphertext, position,
                                     rotor_positions)) {   // final test
                            /*cout << "\nRS:" << m_enigma->get_ring_setting_as_string() << "   RP:"
                                 << rotor_positions[rotor_positions.size() - crib.length() - 1]
                                 << "   P:" << m_enigma->get_cartridge()->get_positions_as_string()
                                 << "\n";*/
                        m_enigma->set_rotor_position(rotor_positions[0]);
                        solutions.push_back(m_enigma->get_setting());
                        m_enigma->set_rotor_position(rotor_positions.back());
                        if (m_setting.stop_on_first_valid) { return solutions; }
                    }
                    m_enigma->set_plugboard("");   // reset plugboard
                }
            }
            

            // END TESTS
            // shuffle arrays TODO make own function
            m_enigma->turn();
            shint *encryption= m_enigma_encryptions.front();
            m_enigma->get_encryption_inplace(encryption);
            m_enigma_encryptions.erase(m_enigma_encryptions.begin());
            m_enigma_encryptions.push_back(encryption);
            if ((int)rotor_positions.size() > crib_n + position) {
                rotor_positions.erase(rotor_positions.begin());
                positions.erase(positions.begin());
            }
            rotor_positions.push_back(m_enigma->get_rotor_position_as_string());
            positions.push_back(vector_from_array(m_enigma->get_positions(), m_rotor_count));
        }   // end for rotor position

        m_enigma->next_ring_setting();

        if (m_setting.time_performance) {
            auto stop_ring_setting= std::chrono::system_clock::now();
            update_performance(
                m_setting.performance_ring_setting_mean, m_setting.performance_ring_setting_var,
                stop_ring_setting - start_ring_setting, m_setting.records_ring_setting);
        }

    }   //end for ring setting
    /*cout<<"                                                                                      "
        << "      \r";*/
    cout<<"\r                                                             "
        <<"                                                             \r";
    if (m_setting.time_performance && m_verbose) { print_performance(); }
    //if (m_use_configuration_tracker) { m_configuration_tracker->find_unchecked(); }
    return solutions;
}

vector<struct EnigmaSetting> BombeUnit::analyze_with_configuration_tracker(const string &ciphertext, const string &crib,
                                                                           shint most_wired_letter, int position) {
    //general setup
    vector<struct EnigmaSetting> solutions;
    int                          crib_n= crib.length(),
                                 //total_permutations = m_enigma->compute_total_permutations_brute_force() + crib_n - 1,
                                 positions_count = min((int)pow(m_letters, m_rotor_count), m_setting.max_ring_settings),
                                 path_i = 0;
    //vector<vector<shint>> positions;
    //vector<shint> initial_positions(m_rotor_count, 0);
    shint *encryption         = new shint[m_letters]; //used in place by enigma
    shint *initial_positions  = new shint[m_letters];
    shint *current_positions  = new shint[m_letters];
    for(int i=0; i<m_rotor_count; i++) {initial_positions[i]=0;}
    m_enigma->set_positions(initial_positions);
    //get engagae_path, TODO move to bombe
    ConfigurationTracker tracker = ConfigurationTracker(m_enigma, crib_n);
    //cout<<"made tracker\n";
    const vector<pair<vector<bool>, Engage_direction>>& path_iterator          = tracker.get_path_iterator();
    //cout<<"\npath iterator of size "<<path_iterator.size()<<"\n";
    //vector<vector<vector<shint>>>::iterator      
    auto                                         ring_settings_iterator_begin = tracker.get_ring_settings_iterator().begin();
    auto                                         ring_settings_iterator = ring_settings_iterator_begin;
    //int  stop_counts = 0;
    //cout<<"\nring_settings iterator of size "<<ring_settings_iterator.size()<<"\n";
    //cout<<"got iterator\n";
    

    // for each rotor_position
    auto start_ring_setting= std::chrono::system_clock::now();
    for (int rp= 0; rp < positions_count; ++rp) {
        print_progress(rp, positions_count, (int) solutions.size());
        //init_enigma_encryptions(crib_n, rotor_positions, positions);
        if (m_setting.time_performance) { start_ring_setting= std::chrono::system_clock::now(); }
        //reset ring settings iterator
        ring_settings_iterator = ring_settings_iterator_begin;
        // for each edge in the engage path
        for (pair<vector<bool>, Engage_direction> engage_and_direction : path_iterator) {
                //engage, connect the enigma if forward, disconnect if backward, test if stop
                //setup_diagonal_board(ciphertext, crib);
                switch (engage_and_direction.second) {
                case Engage_direction::forward :
                    //connect
                    //cout<<"\n f:turnning manually\n";
                    m_enigma->turn_manually(engage_and_direction.first, true);
                    //cout<<"\n f:getting encryption\n";
                    m_enigma->get_encryption_inplace(encryption);
                    //cout<<"\n f:connectiong\n";
                    m_diagonal_board->connect_enigma(encryption, 
                                                     (int)crib[path_i] - (int)'A',
                                                     (int)ciphertext[path_i] - (int)'A');
                    path_i++;
                    break;
                case Engage_direction::backward :
                    //disconnect
                    path_i--;
                    //cout<<"\n b:turnning manually\n";
                    m_enigma->turn_manually(engage_and_direction.first, false);
                    //cout<<"\n b:getting encryption\n";
                    m_enigma->get_encryption_inplace(encryption);
                    //cout<<"\n b:connectiong\n";
                    //cout<<crib[path_i]<<" <-("<<path_i<<")-> "<<ciphertext[path_i]<<"\n";
                    m_diagonal_board->disconnect_enigma(encryption, 
                                                        (int)crib[path_i] - (int)'A',
                                                        (int)ciphertext[path_i] - (int)'A');
                    
                    break;
                case Engage_direction::stop :

                    //each time a stop is met, consume a ring_settings
                    //cout<<"stopped "<< ++stop_counts <<"\n";
                    vector<vector<shint>> ring_settings = *ring_settings_iterator;

                    
                    /*
                    interactive_wirechecking();
                    cin.get();*/
                    m_diagonal_board->wipe();
                    if (check_one_wire(most_wired_letter)) {     // first test
                        if (doublecheck_and_get_plugboard()) {   // second test
                            if (tripplecheck_with_configuration_tracker(crib, ciphertext, ring_settings[0], initial_positions)) {   // final test
                                //add all solutions
                                for (vector<shint> ring_setting : ring_settings) {
                                    //get some aspects of the solution, RS and RP are wrong, though
                                    EnigmaSetting solution = m_enigma->get_setting();
                                    //read ring setting properly
                                    string proper_ring_setting   = "";
                                    string proper_rotor_position = "";
                                    //const shint* positions = m_enigma->get_positions();
                                    for (int i = 0; i<(int) ring_setting.size(); i++) {
                                        current_positions [i] = m_enigma->get_positions()[i];
                                        proper_ring_setting = (char) ((ring_setting[i]-initial_positions[i]+m_letters)%m_letters +(shint) 'A') + proper_ring_setting;
                                    }
                                    m_enigma->set_ring_setting(proper_ring_setting); //analyze disregards ring setting, so this is safe
                                    m_enigma->set_positions(initial_positions);      //reset later
                                    solution.ring_setting   = proper_ring_setting;
                                    solution.rotor_position = m_enigma->get_rotor_position_as_string();
                                    solutions.push_back(solution);
                                    m_enigma->set_positions(current_positions);
                                }
                                if (m_setting.stop_on_first_valid) { return solutions; }
                            }
                            m_enigma->set_plugboard("");   // reset plugboard
                        }
                    } // END TESTS
                    ring_settings_iterator++;
                    break;
                } //END SWITCH
        }//END FOR PATH

        m_enigma->set_positions(initial_positions);
        m_enigma->turn_positions_odometer();
        for(int i=0; i<m_rotor_count; i++) {initial_positions[i] = m_enigma->get_positions()[i];}

        if (m_setting.time_performance) {
            auto stop_ring_setting= std::chrono::system_clock::now();
            update_performance(
                m_setting.performance_ring_setting_mean, m_setting.performance_ring_setting_var,
                stop_ring_setting - start_ring_setting, m_setting.records_ring_setting);
        }
    }   // end for rotor position
    
    //cout<<"almost done\n";
    if (m_setting.time_performance && m_verbose) { print_performance(); }
    delete[] encryption;
    delete[] initial_positions;
    delete[] current_positions;
    cout<<"\r                                                             "
        <<"                                                             \r";
    return solutions;
}

bool BombeUnit::bundle_contradiction(shint bundle) {
    return m_diagonal_board->bundle_contradiction(bundle);
}
bool BombeUnit::check_one_wire(shint most_wired_letter) {
    m_diagonal_board->activate(most_wired_letter, min(4, m_letters - 1));
    return (!bundle_contradiction(most_wired_letter));
}
bool BombeUnit::doublecheck_and_get_plugboard() {
    // we have a valid configuration od the enigma, the most occurring letter is
    // activated in some wire.
    // for all bundles, the sum is either
    // 1 : the steckered letter is live
    // 25: all but the steckered letter is live
    // other: unable to find, probably self-steckered
    Plugboard *plugboard= m_enigma->get_cartridge()->get_plugboard();

    if (m_setting.debug_doublecheck_show_wiring) {
        cout << "DEBUG: RS" << m_enigma->get_ring_setting_as_string() << ", RP "
             << m_enigma->get_rotor_position_as_string();
        m_diagonal_board->print();
        m_diagonal_board->print_live();
        m_diagonal_board->print_connections();
        cin.get();
    }
    for (shint bundle= 0; bundle < m_letters; ++bundle) {
        int sum= m_diagonal_board->bundle_sum(bundle);
        if (sum == 1) {   // steckered is live
            // all other bundles should have 1 or less live wires
            for (shint bundle_2= 0; bundle_2 < m_letters; ++bundle_2) {
                if (m_diagonal_board->bundle_sum(bundle_2) > 1) {
                    plugboard->reset();
                    return false;
                }
            }
            // find live wire
            for (shint wire= 0; wire < m_letters; ++wire) {
                if (m_diagonal_board->get_wire(bundle, wire)->get_live()) {
                    plugboard->set_wiring(bundle, wire);
                    plugboard->set_wiring(wire, bundle);
                    break;
                }
            }
        } else if (sum == m_letters - 1) {   // steckered is dead
            // find dead wire
            for (shint wire= 0; wire < m_letters; ++wire) {
                if (!m_diagonal_board->get_wire(bundle, wire)->get_live()) {
                    plugboard->set_wiring(bundle, wire);
                    plugboard->set_wiring(wire, bundle);
                    break;
                }
            }
        } else {   // undeterminable... try some more! TODO
            return doublecheck_thoroughly_and_get_plugboard();
        }
    }
    return true;
}
bool BombeUnit::doublecheck_thoroughly_and_get_plugboard() {
    //cout<<"\nTHOROUGH DOUBLECHECK\n";
    //same as doublecheck, but do for all wires activated, can be optimized if it i a bottleneck.
    Plugboard *plugboard= m_enigma->get_cartridge()->get_plugboard();
    int sum;
    bool all_determined;
    for (shint b = 0; b < m_letters; b++) {
        for (shint w = 0; w < m_letters; w++) {
            all_determined = true;
            m_diagonal_board->wipe();
            m_diagonal_board->activate(b, w);
            for (shint bundle= 0; bundle < m_letters; ++bundle) {
                sum = m_diagonal_board->bundle_sum(bundle);
                //cout<<"s: "<<sum<<"\n";
                if (sum == 1) {   // steckered is live
                    // all other bundles should have 1 or less live wires
                    for (shint bundle_2= 0; bundle_2 < m_letters; ++bundle_2) {
                        if (m_diagonal_board->bundle_sum(bundle_2) > 1) {
                            plugboard->reset();
                            return false;
                        }
                    }
                
                    // find live wire
                    for (shint wire= 0; wire < m_letters; ++wire) {
                        if (m_diagonal_board->get_wire(bundle, wire)->get_live()) {
                            plugboard->set_wiring(bundle, wire);
                            plugboard->set_wiring(wire, bundle);
                            break;
                        }
                    }
                } else if (sum == m_letters - 1) {   // steckered is dead
                    // find dead wire
                    for (shint wire= 0; wire < m_letters; ++wire) {
                        if (!m_diagonal_board->get_wire(bundle, wire)->get_live()) {
                            plugboard->set_wiring(bundle, wire);
                            plugboard->set_wiring(wire, bundle);
                            break;
                        }
                    }
                } else {
                    all_determined = false;
                }
            }
            if (all_determined) {
                return true;
            }
        } 
    }
    //cout<<"\nWARNING: unable to determine validity of wiring\n";
    return true;
}

bool BombeUnit::tripplecheck(const string &crib, const string &ciphertext, int candidate,
                             vector<string> &rotor_positions) {
    // test if the given configuration encrypts the crib to plaintext
    // turn back
    m_enigma->set_rotor_position(rotor_positions[rotor_positions.size() - crib.length() - 1]);
    string recrypt= m_enigma->encrypt(ciphertext);
    if (m_setting.interactive_wiring_mode) { interactive_wirechecking(); }
    return (recrypt == crib);
}
bool BombeUnit::tripplecheck_with_configuration_tracker(const string &crib, const string &ciphertext, const vector<shint>& shifted_ring_setting, const shint* initial_positions) {
    //store positions to reset at end
    shint* current_positions = new shint[m_letters];
    for(int i=0; i<m_rotor_count; i++) {
        current_positions[i] = m_enigma->get_positions()[i];
        //cout<<(char) (current_positions[i] + (int) 'A');
    }
    m_enigma->set_positions(initial_positions);

    // test if the given configuration encrypts the crib to plaintext
    // get proper configuration
    // get proper ring setting, and set position.
    // both can be set at the same time, XXX source of error?
    // the positions are already correct.

    string proper_ring_setting  = "";
    //const shint* positions = m_enigma->get_positions();
    //cout<<"PROPER RING SETTING:\n";

    //XXX REMEBER TO REVERSE!!!
    for (int i = 0; i<(shint) shifted_ring_setting.size(); i++) {
        proper_ring_setting = (char) ((shifted_ring_setting[i]-initial_positions[i]+m_letters)%m_letters +(shint) 'A') + proper_ring_setting;
    }
    //cout<<proper_ring_setting<<"\n";
    m_enigma->set_ring_setting(proper_ring_setting);
    //cout<<"PROPER ROTOR POSITION: \n";
    //cout<<m_enigma->get_rotor_position_as_string()<<"\n";

    string recrypt= m_enigma->encrypt(ciphertext);
    
    if (m_setting.interactive_wiring_mode) { interactive_wirechecking(); }

    //reset positions, this mode does not care about ring settings
    m_enigma->set_positions(current_positions);
    
    delete[] current_positions; //XXX wastefull solution
    return (recrypt == crib);
}

void BombeUnit::interactive_wirechecking() {
    cout << "--------------------INTERACTIVE WIRING MODE------------------\n";
    string input;
    shint    bundle, wire;
    while (true) {
        cout << "    INPUT A WIRE TO ACTIVATE(q to EXIT): ";
        cin >> input;
        if (input == "q") { break; }
        bundle= (int)input[0] - (int)'A';
        wire  = (int)input[1] - (int)'a';
        if (bundle > m_letters || bundle < 0 || wire > m_letters || wire < 0) {
            cout << "WRONG INPUT FORMAT. USAGE: Aa, Ab, Zk\n";
            continue;
        }
        cout << "activating " << (char)(bundle + (int)'A') << (char)(wire + (int)'a') << "\n";
        m_diagonal_board->wipe();
        m_diagonal_board->activate(bundle, wire);
        m_diagonal_board->print();
    }
}

void BombeUnit::print_progress(int ring_setting, int max_ring_settings, int solutions_n) {
    cout << "\r";
    string progress_bar= "[";
    for (int i= 0; i < PROGRESS_BAR_WIDTH; ++i) {
        progress_bar+=
            i <= (PROGRESS_BAR_WIDTH * (ring_setting + 1)) / max_ring_settings ? "#" : " ";
    }
    progress_bar+= "]";
    cout << progress_bar;
    if (m_use_configuration_tracker) {
        cout << " --- using CT ";
             /*<< (100 * (float)m_configuration_grid->get_checked_configurations()) /
                    (float)m_configuration_grid->get_total_configurations()*/
        cout << " [" << m_identifier << "] ";
        cout << " --- PO: " << m_enigma->get_positions_as_string() << ", solutions: " << solutions_n
             << flush;
        
    } else {
        cout << " [" << m_identifier << "] ";
        cout << " --- RS: " << m_enigma->get_ring_setting_as_string() << ", solutions: " << solutions_n
             << flush;
    }

    
}












void BombeUnit::print_encryptions() const {
    cout << "size of encryptions: " << m_enigma_encryptions.size() << "\n";
    for (shint wire= 0; wire < m_letters; ++wire) {
        cout << (char)(wire + (int)'A') << ": ";
        for (unsigned int e= 0; e < m_enigma_encryptions.size(); ++e) {
            cout << (char)(m_enigma_encryptions.at(e)[wire] + (int)'A') << " ";
        }
        cout << "\n";
    }
}
void BombeUnit::print_performance() const {
    // ring_setting
    cout<<"printingggg performance\n";
    printf("1---------------------------------------------------\n");
    printf("2|                   MEAN       VAR        RECORDS |\n");
    printf("3| RING-SETTING      %6.2E   %6.2E   %7d |\n", m_setting.performance_ring_setting_mean,
           m_setting.performance_ring_setting_var, m_setting.records_ring_setting);
    printf("4---------------------------------------------------\n");
    cout<<"printed performance\n";
}