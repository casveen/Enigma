#include "bombe.hpp"
const int PROGRESS_BAR_WIDTH= 50;
#include <cmath>


//HELPER FUNCTIONS
void update_performance(double &mean, double &var, std::chrono::duration<double> measurement,
                        int &records) {
    auto   t     = measurement.count();
    double mean_p= mean;auto start_ring_setting= std::chrono::system_clock::now();
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
    //m_diagonal_board= new DiagonalBoard(m_letters); //diagonalboard
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
    //delete m_diagonal_board;
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
void BombeUnit::reset_diagonal_board(DiagonalBoard& diagonal_board) { diagonal_board.reset(); }
void BombeUnit::setup_diagonal_board(DiagonalBoard& diagonal_board, const string &ciphertext, const string &crib) {
    for (unsigned int j= 0; j < crib.length(); j++) {
        shint *encryption= m_enigma_encryptions.at(j);
        diagonal_board.connect_enigma(encryption, (int)crib[j] - (int)'A',
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
    DiagonalBoard diagonal_board(m_letters);
    
    // for each ring setting
    auto start_ring_setting= std::chrono::system_clock::now(); 
    for (int rs= 0; rs < ring_settings; ++rs) {
        print_progress(rs, ring_settings, (int)solutions.size());
        init_enigma_encryptions(crib_n, rotor_positions, positions);
        if (m_setting.time_performance) { start_ring_setting= std::chrono::system_clock::now(); }
        // for each rotor position
        for (int j= 0; j < total_permutations - 1; j++) {
            reset_diagonal_board(diagonal_board);
            setup_diagonal_board(diagonal_board, ciphertext, crib);
            // BEGIN TESTS
            if (check_one_wire(diagonal_board, most_wired_letter)) {     // first test
                if (doublecheck_and_get_plugboard(diagonal_board, *m_enigma)) {   // second test, XXX dereference source of error
                    if (tripplecheck(*m_enigma, crib, ciphertext, position,
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
                                 positions_count = min((int)pow(m_letters, m_rotor_count), m_setting.max_ring_settings);

    //get engagae_path, TODO move to bombe
    ConfigurationTracker tracker      = ConfigurationTracker(m_enigma, crib_n);
    

    //copy the enigma pointer, so as to reset later.
    //Enigma* m_enigma_original = m_enigma;
    //priavvte enigma wont work, as it just copies the pointer, not the structure





    double total_time = 0, mean_time = 0;
     #ifdef _OPENMP
    double total_time_start = omp_get_wtime();
    #endif
    #pragma omp parallel reduction(+: mean_time) firstprivate(ciphertext, crib, most_wired_letter)
    {
    #ifdef _OPENMP
    //printf("\n%2d/%2d GO\n", omp_get_thread_num(), omp_get_num_threads());
    #pragma omp barrier 
    #endif
    double start_ring_setting;

    //each thread makes its own copy of m_enigma, and stores it in the original pointer
    //use the indirect copy constructor
    //#pragma omp critical (make_enigma_copies)
    //{
    Enigma enigma(m_enigma->get_setting());
    //}
    #ifdef _OPENMP
    //printf("\n%2d/%2d made copy\n", omp_get_thread_num(), omp_get_num_threads());
    //m_enigma->print();

    //m_enigma->get_cartridge()->get_plugboard()->print();
    #pragma omp barrier 
    #endif

    DiagonalBoard diagonal_board(m_letters);


    //private thread iterators
    //begin could be shared, but made local to avoid cache-sloshing.
    vector<vector<vector<shint>>>                 ring_settings_vector_copy(tracker.get_ring_settings_iterator()); //XXX COPY
    auto                                          ring_settings_iterator_begin = ring_settings_vector_copy.begin();
    auto                                                ring_settings_iterator = ring_settings_iterator_begin;
    

    const vector<pair<vector<bool>, Engage_direction>>                           path_iterator(tracker.get_path_iterator()); //XXX copy
    //cout<<&(path_iterator[0].first)<<"\n";
    //const vector<shint*>&                                   positions_iterator = tracker.get_positions_iterator();

    //other
    shint *encryption         = new shint[m_letters]; //used in place by enigma
    //shint *initial_positions  = new shint[m_rotor_count]; //keep track of positions in start of a run
    shint current_positions;





    





    int path_i = 0, position_count=0;
    
    #ifdef _OPENMP
    if (m_setting.time_performance) { start_ring_setting= omp_get_wtime(); }
    #endif

    #pragma omp for schedule(guided)
    for (int initial_positions= 0; initial_positions < positions_count; ++initial_positions) {
        //printf("%1d/%1d:%3d/%3d -> ", omp_get_thread_num(), omp_get_num_threads(), initial_positions, positions_count);
        //cout<<&(path_iterator[0])<<"\n";
        position_count++;
        enigma.set_positions(initial_positions);
        print_progress(initial_positions, positions_count, (int) solutions.size());
        //reset ring settings iterator
        ring_settings_iterator = ring_settings_iterator_begin;
        // for each edge in the engage path
        for (pair<vector<bool>, Engage_direction> engage_and_direction : path_iterator) {
                //engage, connect the enigma if forward, disconnect if backward, test if stop
                //setup_diagonal_board(ciphertext, crib);
                switch (engage_and_direction.second) {
                case Engage_direction::forward :
                    //connect

                    //printf("%1dF", omp_get_thread_num());
                    
                    enigma.turn_manually(engage_and_direction.first, true);
                    enigma.get_encryption_inplace(encryption);
                    /*#pragma omp critical (db1)
                    {
                    cout<<"connecting ";
                    for(int i = 0; i<m_letters; i++) {
                        cout<<encryption[i]<<"|";
                    }
                    cout<<" at ";
                    cout<<(int)crib[path_i] - (int)'A'<<", "<<(int)ciphertext[path_i] - (int)'A')<<"\n";
                    }*/
                    diagonal_board.connect_enigma(encryption, 
                                                     (int)crib[path_i] - (int)'A',
                                                     (int)ciphertext[path_i] - (int)'A');
                    path_i++;
                    break;
                case Engage_direction::backward :
                    //disconnect
                    //printf("%1dB", omp_get_thread_num());
                    path_i--;
                    enigma.turn_manually(engage_and_direction.first, false);
                    enigma.get_encryption_inplace(encryption);
                    diagonal_board.disconnect_enigma(encryption, 
                                                        (int)crib[path_i] - (int)'A',
                                                        (int)ciphertext[path_i] - (int)'A');
                    
                    break;
                case Engage_direction::stop :
                    #ifdef _OPENMP
                    //printf("%1dS", omp_get_thread_num());
                    //printf("\n%2d/%2d stopped\n", omp_get_thread_num(), omp_get_num_threads()); 
                    #endif
                    //each time a stop is met, consume a ring_setting
                    vector<vector<shint>> ring_settings = *ring_settings_iterator;

                    diagonal_board.wipe();
                    if (check_one_wire(diagonal_board, most_wired_letter)) {     // first test
                        if (doublecheck_and_get_plugboard(diagonal_board, enigma)) {   // second test
                            if (tripplecheck_with_configuration_tracker(enigma, crib, ciphertext, ring_settings[0], initial_positions)) {   // final test
                                //cout<<"got solution from positions "<<initial_positions<<" ";
                                //hash the rotor positions, so as to return to this poition after adding all the solutions
                                //XXX error source
                                current_positions = 0;
                                for(int i =m_rotor_count-1; i>=0; i--) {
                                    //cout<<enigma.get_positions()[i]<<"|";
                                    current_positions *= m_letters;
                                    current_positions += enigma.get_positions()[i]; 
                                }
                                //cout<<" ending in positions "<<current_positions<<"\n";
                                //add all solutions
                                for (vector<shint> ring_setting : ring_settings) {
                                    //get some aspects of the solution, RS and RP are wrong, though
                                    EnigmaSetting solution = enigma.get_setting();
                                    //read ring setting properly
                                    string proper_ring_setting   = "";
                                    string proper_rotor_position = "";
                                    //const shint* positions = m_enigma->get_positions();
                                    shint initial_positions_i = 999;
                                    shint initial_positions_rest = initial_positions;
                                    for (int i = 0; i<(int) ring_setting.size(); i++) {
                                        initial_positions_i    = initial_positions_rest%m_letters;
                                        initial_positions_rest = initial_positions_rest / (int) m_letters;
                                        //current_positions [i]  = enigma.get_positions()[i];
                                        proper_ring_setting    = (char) ((ring_setting[i]-initial_positions_i+m_letters)%m_letters +(shint) 'A') + proper_ring_setting;
                                    }
                                    enigma.set_ring_setting(proper_ring_setting); //analyze disregards ring setting, so this is safe
                                    enigma.set_positions(initial_positions);      //reset later
                                    solution.ring_setting   = proper_ring_setting;
                                    solution.rotor_position = enigma.get_rotor_position_as_string();

                                    //TODO, reduce later, not here.
                                    #pragma omp critical (add_solution) 
                                    {
                                    solutions.push_back(solution);
                                    }
                                }//all solutions added
                                enigma.set_positions(current_positions);
                                #ifndef _OPENMP
                                if (m_setting.stop_on_first_valid) { return solutions; }
                                #endif
                            }
                            enigma.set_plugboard("");   // reset plugboard
                        }
                    } // END TESTS
                    ring_settings_iterator++;
                    break;
                } //END SWITCH
        }//END FOR PATH

        //take the setting from the original enigma, then turn it
        /*#pragma omp critical (turn_original_enigma)
        {
        for(int i=0; i<m_rotor_count; i++) {initial_positions[i] = m_enigma_original->get_positions()[i];}
        m_enigma_original->turn_positions_odometer();
        }*/
        //enigma.set_positions(initial_positions);
    } //END FOR POSITION
    //each thread frees its enigma copy, and resets to the original one, which is still in its initial setting
    //delete m_enigma;
    //delete m_diagonal_board;
    


    
    if (m_setting.time_performance && m_verbose) { print_performance(); }
    delete[] encryption;
    //delete[] initial_positions;
    //delete[] current_positions;
    if (m_setting.time_performance) {
        #ifdef _OPENMP
        double stop_ring_setting= omp_get_wtime();
        mean_time += (stop_ring_setting-start_ring_setting);
        #else
        mean_time = 0;
        #endif
        

            /*update_performance(
                m_setting.performance_ring_setting_mean, m_setting.performance_ring_setting_var,
                stop_ring_setting - start_ring_setting, m_setting.records_ring_setting);*/
            
    }


    }//END PARALLEL REGION

   
    mean_time = mean_time/positions_count;
    #ifdef _OPENMP
    double total_time_end = omp_get_wtime();
    m_setting.performance_ring_setting_var = total_time_end-total_time_start;

    #endif
    m_setting.performance_ring_setting_mean = mean_time;
    //m_enigma = m_enigma_original;
    cout<<"\r                                                             "
        <<"                                                             \r";
    return solutions;
}

bool BombeUnit::check_one_wire(DiagonalBoard& diagonal_board, shint most_wired_letter) {
    diagonal_board.activate(most_wired_letter, min(4, m_letters - 1));
    return (!diagonal_board.bundle_contradiction(most_wired_letter));
}
bool BombeUnit::doublecheck_and_get_plugboard(DiagonalBoard& diagonal_board, Enigma& enigma) {
    // we have a valid configuration od the enigma, the most occurring letter is
    // activated in some wire.
    // for all bundles, the sum is either
    // 1 : the steckered letter is live
    // 25: all but the steckered letter is live
    // other: unable to find, probably self-steckered
    Plugboard *plugboard= enigma.get_cartridge()->get_plugboard();

    if (m_setting.debug_doublecheck_show_wiring) {
        cout << "DEBUG: RS" << enigma.get_ring_setting_as_string() << ", RP "
             << enigma.get_rotor_position_as_string();
        diagonal_board.print();
        diagonal_board.print_live();
        diagonal_board.print_connections();
        cin.get();
    }
    for (shint bundle= 0; bundle < m_letters; ++bundle) {
        int sum= diagonal_board.bundle_sum(bundle);
        if (sum == 1) {   // steckered is live
            // all other bundles should have 1 or less live wires
            for (shint bundle_2= 0; bundle_2 < m_letters; ++bundle_2) {
                if (diagonal_board.bundle_sum(bundle_2) > 1) {
                    plugboard->reset();
                    return false;
                }
            }
            // find live wire
            for (shint wire= 0; wire < m_letters; ++wire) {
                if (diagonal_board.get_wire(bundle, wire)->get_live()) {
                    plugboard->set_wiring(bundle, wire);
                    plugboard->set_wiring(wire, bundle);
                    break;
                }
            }
        } else if (sum == m_letters - 1) {   // steckered is dead
            // find dead wire
            for (shint wire= 0; wire < m_letters; ++wire) {
                if (!diagonal_board.get_wire(bundle, wire)->get_live()) {
                    plugboard->set_wiring(bundle, wire);
                    plugboard->set_wiring(wire, bundle);
                    break;
                }
            }
        } else {   // undeterminable... try some more! TODO
            //return doublecheck_thoroughly_and_get_plugboard();
        }
    }
    return true;
}
bool BombeUnit::doublecheck_thoroughly_and_get_plugboard(DiagonalBoard& diagonal_board, Enigma& enigma) {
    //cout<<"\nTHOROUGH DOUBLECHECK\n";
    //same as doublecheck, but do for all wires activated, can be optimized if it i a bottleneck.
    Plugboard *plugboard= enigma.get_cartridge()->get_plugboard();
    int sum;
    bool all_determined;
    for (shint b = 0; b < m_letters; b++) {
        for (shint w = 0; w < m_letters; w++) {
            all_determined = true;
            diagonal_board.wipe();
            diagonal_board.activate(b, w);
            for (shint bundle= 0; bundle < m_letters; ++bundle) {
                sum = diagonal_board.bundle_sum(bundle);
                //cout<<"s: "<<sum<<"\n";
                if (sum == 1) {   // steckered is live
                    // all other bundles should have 1 or less live wires
                    for (shint bundle_2= 0; bundle_2 < m_letters; ++bundle_2) {
                        if (diagonal_board.bundle_sum(bundle_2) > 1) {
                            plugboard->reset();
                            return false;
                        }
                    }
                
                    // find live wire
                    for (shint wire= 0; wire < m_letters; ++wire) {
                        if (diagonal_board.get_wire(bundle, wire)->get_live()) {
                            plugboard->set_wiring(bundle, wire);
                            plugboard->set_wiring(wire, bundle);
                            break;
                        }
                    }
                } else if (sum == m_letters - 1) {   // steckered is dead
                    // find dead wire
                    for (shint wire= 0; wire < m_letters; ++wire) {
                        if (!diagonal_board.get_wire(bundle, wire)->get_live()) {
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

bool BombeUnit::tripplecheck(Enigma& enigma, const string &crib, const string &ciphertext, int candidate,
                             vector<string> &rotor_positions) {
    // test if the given configuration encrypts the crib to plaintext
    // turn back
    enigma.set_rotor_position(rotor_positions[rotor_positions.size() - crib.length() - 1]);
    string recrypt= enigma.encrypt(ciphertext);
    if (m_setting.interactive_wiring_mode) { interactive_wirechecking(); }
    return (recrypt == crib);
}
bool BombeUnit::tripplecheck_with_configuration_tracker(Enigma& enigma, const string &crib, const string &ciphertext, const vector<shint>& shifted_ring_setting, int initial_positions) {
    //store positions to reset at end
    /*shint* current_positions = new shint[m_letters];
    for(int i=0; i<m_rotor_count; i++) {
        current_positions[i] = m_enigma->get_positions()[i];
        //cout<<(char) (current_positions[i] + (int) 'A');
    }*/
    enigma.set_positions(initial_positions);

    // test if the given configuration encrypts the crib to plaintext
    // get proper configuration
    // get proper ring setting, and set position.
    // both can be set at the same time, XXX source of error?
    // the positions are already correct.

    string proper_ring_setting  = "";
    //const shint* positions = m_enigma->get_positions();
    //cout<<"PROPER RING SETTING:\n";

    //XXX REMEBER TO REVERSE!!!
    shint initial_positions_i = 0;
    shint initial_positions_rest = initial_positions; 
    for (int i = 0; i<(shint) shifted_ring_setting.size(); i++) {
        initial_positions_i    = initial_positions_rest % m_letters;   
        initial_positions_rest = initial_positions_rest / (int) m_letters;
        proper_ring_setting = (char) ((shifted_ring_setting[i]-initial_positions_i+m_letters)%m_letters +(shint) 'A') + proper_ring_setting;
    }
    //cout<<proper_ring_setting<<"\n";
    enigma.set_ring_setting(proper_ring_setting);
    //cout<<"PROPER ROTOR POSITION: \n";
    //cout<<m_enigma->get_rotor_position_as_string()<<"\n";

    string recrypt= enigma.encrypt(ciphertext);
    
    if (m_setting.interactive_wiring_mode) { interactive_wirechecking(); }

    //reset positions, this mode does not care about ring settings
    //enigma.set_positions(current_positions);
    
    //delete[] current_positions; //XXX wastefull solution
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



//if parallel, this function is called in a parallel region
void BombeUnit::print_progress(int ring_setting, int max_ring_settings, int solutions_n) {
    cout << "\r";
    /*
    #ifdef _OPENMP
    const int THREAD_PROGRESS_WIDTH = 12;
    for (int i=0; i<omp_get_thread_num(); i++) {
        cout<<string(THREAD_PROGRESS_WIDTH+1, ' ');
    }
    printf("%1d/%1d:%3d/%3d", omp_get_thread_num(), omp_get_num_threads(), ring_setting, max_ring_settings);
    //cout<<"["<<omp_get_thread_num()<<"/"<<NUM_THREADS<<": "<<(ring_setting/max_ring_settings)*100  <<"\%]"
    //cout<<"\n";
    #else
    */
    #pragma omp master
    {
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
    //#endif    
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