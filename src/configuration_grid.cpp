#include "bombe.hpp"
#include <cmath>



/*If a message of length m is encrypted with a particular ring setting and rotor posiiton,
then encrypting the same message with rotor position turned one step forward and ring setting
one step "backward"(compensating for turn of rotor) should give the exact same message unless
there was a notch turnover when encrypting. That is, there are a lot of settings of the enigma
that would encrypt to the exact same message.*/
ConfigurationGrid::ConfigurationGrid(Enigma &enigma) {
    m_letters                  = enigma.get_wires();
    m_rotor_count              = enigma.get_rotors();
    m_all_rotor_positions      = enigma.get_all_rotor_positions();
    m_rotor_configuration_count= m_all_rotor_positions.size();
    try {
        cout << "\rAllocating to inverse ";
        // larger than it needs to be...only m_all_rotor_poositions.size()<<letter^rotors
        m_all_rotor_positions_inverse= vector<unsigned int>(pow(m_letters, m_rotor_count), 0);
    } catch (bad_alloc &ba) {
        cerr << "ERROR:Failed to allocate inverse of all rotor positions, disable configuration "
                "grid";
    }
    cout << "DONE\n";
    try {
        cout << "\rAllocating checked, "
             << m_rotor_configuration_count * pow(m_letters, m_rotor_count) << " bits="
             << ((m_rotor_configuration_count * pow(m_letters, m_rotor_count)) / (8 * 125000000))
             << " Gb\n";
        m_checked= vector<bool>(m_rotor_configuration_count * pow(m_letters, m_rotor_count), false);
        cout << "got " << m_checked.size() << " asked for "
             << m_all_rotor_positions.size() * pow(m_letters, m_rotor_count) << "\n";
    } catch (bad_alloc &ba) {
        cerr << "ERROR:Failed to allocate checklist in configuration grid, disable "
                "configuration "
                "grid";
        exit(EXIT_FAILURE);
    }
    cout << "DONE\n";
    for (unsigned int rp= 0; rp < m_all_rotor_positions.size(); ++rp) {
        cout << "\rSetting to inverse, " << rp << "/" << m_rotor_configuration_count << "     "
             << flush;
        vector<shint> rotor_position= m_all_rotor_positions[rp];
        m_all_rotor_positions_inverse[vector_to_int_hash(rotor_position)]= rp;
    }
    cout << "DONE\n";
    cout << "SUCCESFULLY MADE CONFIGURATION GRID\n";
    m_total_configurations= pow(m_letters, m_rotor_count) * m_rotor_configuration_count;
}



void ConfigurationGrid::reset_checked() {
    for (int rs= 0; rs < pow(m_letters, m_rotor_count); ++rs) {
        for (unsigned int rp= 0; rp < m_rotor_configuration_count; ++rp) {
            m_checked[rs * m_rotor_configuration_count + rp]= false;
        }
    }
}

bool ConfigurationGrid::get_checked(const int *ring_setting, const vector<int> &rotor_position) {
    // translate to index
    int rs  = ring_setting_array_to_int(ring_setting),
        rp  = rotor_position_vector_to_int(rotor_position);
    bool out= m_checked[rs * m_rotor_configuration_count + rp];
    return out;
}

const std::string wc("\033[0;31m");
const std::string rc("\033[0;33m");
const std::string gc("\033[1;30m");
const std::string bgc("\033[0;40m");
const std::string bgr("\033[0;40m");

void ConfigurationGrid::set_checked(vector<vector<shint>>::const_iterator positions_original) {
    unsigned int set_count= 0;
    unsigned int rss      = 0;
    int          position_p_i;
    bool         equal;
    for (unsigned int rpp= 0; rpp < m_rotor_configuration_count; ++rpp) {
        // find the ring setting corresponding to this rotor position(rs so that
        // rp-rs=r_original[0])
        for (int i= 0; i < m_rotor_count; ++i) {
            current_ring_setting[i]= ((m_all_rotor_positions[rpp][i] -
                                       positions_original[0][m_rotor_count - 1 - i] + m_letters) %
                                      m_letters);
        }
        rss= vector_to_int_hash(current_ring_setting);

        if (!m_checked[rss * m_rotor_configuration_count + rpp]) {   // if not checked
            // now we can search from rss, rpp, along rpp axis and compare to original
            // p=0 isalready good, and also the fast wheel
            // search through all configurations starting with the same position as
            // positions_origina return if the configuration we are looking at equals the original
            equal= [&]() {   // lambda, so as to break out of both loops quickly with return
                for (int i= 1; i < m_rotor_count; ++i) {   // note i=1, fast rotors are always equal
                    for (int p= 1; p < m_crib_length; ++p) {   // from p=1; first pos equal per def
                        position_p_i=
                            (m_all_rotor_positions[(rpp + p) % m_rotor_configuration_count][i] -
                             current_ring_setting[i] + m_letters) %
                            m_letters;

                        if (position_p_i != positions_original[p][m_rotor_count - i - 1]) {
                            return false;
                        }
                    }
                }
                return true;
            }();
            if (equal) {
                m_checked[rss * m_rotor_configuration_count + rpp]= true;
                set_count++;
            }
        }
    }
    m_checked_configurations+= set_count;
    return;
}

void ConfigurationGrid::set_crib_length(int crib_length) {
    m_crib_length       = crib_length;
    current_ring_setting= vector<shint>(m_rotor_count, 0);
}

void ConfigurationGrid::find_unchecked() const {
    // after an entire run there should not be any unchecked configurations. If Ther
    // are any, print them and issue a warning
    if (m_total_configurations > m_checked_configurations) {
        int missing_counter= 0;
        for (int rs= 0; rs < pow(m_letters, m_rotor_count); ++rs) {
            for (unsigned int rp= 0; rp < m_rotor_configuration_count; ++rp) {
                if (!m_checked[rs * m_rotor_configuration_count + rp]) {
                    missing_counter++;
                    cout << "WARNING: unchecked configuration! ";
                    // hash to rs:
                    cout << "[RS:";
                    int rss= rs;
                    for (int i= 0; i < m_rotor_count; ++i) {
                        cout << (char)(rss % m_letters + (int)'A');
                        rss-= rss % m_letters;
                        rss/= m_letters;
                    }
                    cout << " RP:";
                    for (int i= 0; i < m_rotor_count; ++i) {
                        cout << (char)(m_all_rotor_positions[rp][i] + (int)'A');
                    }
                    cout << " P:";
                    rss= rs;
                    for (int i= 0; i < m_rotor_count; ++i) {
                        cout << (m_all_rotor_positions[rp][i] - rss % m_letters + m_letters) %
                                    m_letters;
                        if (i < m_rotor_count - 1) { cout << "-"; }
                        rss-= rss % m_letters;
                        rss/= m_letters;
                    }
                    cout << "]\n";
                }
            }
        }
        if (missing_counter > 0) {
            cout << "In total, missing " << m_total_configurations - m_checked_configurations
                 << " configurations";
        }
    }
    return;
}

unsigned long int ConfigurationGrid::get_total_configurations() const {
    return m_total_configurations;
}
unsigned long int ConfigurationGrid::get_checked_configurations() const {
    return m_checked_configurations;
}
unsigned int ConfigurationGrid::get_rotor_position_count() const {
    return m_all_rotor_positions.size();
}

unsigned int ConfigurationGrid::rotor_position_string_to_int(const string &rotor_position) {
    // a injective mapping from ring setting strings to int. Essentially a hash
    // must map correctly to m_all_rotor_positions
    // that is,
    // m_all_rotor_positions[rotor_position_string_to_int[rotor_position]]=rotor_position
    // here we use the array m_all_rotor_positions_inverse, but first we have make an index
    // from the string, to look up in the inverse array
    return m_all_rotor_positions_inverse[string_to_int_hash(rotor_position)];
}
unsigned int ConfigurationGrid::ring_setting_array_to_int(const int *ring_setting) {
    // a injective mapping from ring setting strings to int. Essentially a hash
    // when rs is an array, it is given in to opposite order of all other states, so use reverse
    // hash
    return array_to_int_reverse_hash(ring_setting);
}
unsigned int ConfigurationGrid::rotor_position_array_to_int(const int *rotor_position) {
    // a injective mapping from ring setting strings to int. Essentially a hash
    // must map correctly to m_all_rotor_positions
    // that is,
    // m_all_rotor_positions[rotor_position_string_to_int[rotor_position]]=rotor_position
    // here we use the array m_all_rotor_positions_inverse, but first we have make an index
    // from the string, to look up in the inverse array
    return m_all_rotor_positions_inverse[array_to_int_reverse_hash(rotor_position)];
}
unsigned int ConfigurationGrid::rotor_position_vector_to_int(const vector<int> &rotor_position) {
    // a injective mapping from ring setting strings to int. Essentially a hash
    // must map correctly to m_all_rotor_positions
    // that is,
    // m_all_rotor_positions[rotor_position_string_to_int[rotor_position]]=rotor_position
    // here we use the array m_all_rotor_positions_inverse, but first we have make an index
    // from the string, to look up in the inverse array
    return m_all_rotor_positions_inverse[vector_to_int_hash(rotor_position)];
}
unsigned int ConfigurationGrid::ring_setting_string_to_int(const string &ring_setting) {
    // a injective mapping from ring setting strings to int. Essentially a hash
    return string_to_int_hash(ring_setting);
}
unsigned int ConfigurationGrid::string_to_int_hash(const string &str) {
    unsigned int as_int= 0;
    for (int i= 0; i < m_rotor_count; ++i) {
        as_int*= m_letters;
        as_int+= (char)(str[i] - (int)'A');
    }
    return as_int;
}
unsigned int ConfigurationGrid::vector_to_int_hash(const vector<shint> &vec) {
    unsigned int as_int= 0;
    for (int i= 0; i < m_rotor_count; ++i) {
        as_int*= m_letters;
        as_int+= vec[i];
    }
    return as_int;
}
unsigned int ConfigurationGrid::array_to_int_hash(const int *array) {
    unsigned int as_int= 0;
    for (int i= 0; i < m_rotor_count; ++i) {
        as_int*= m_letters;
        as_int+= array[i];
    }
    return as_int;
}
unsigned int ConfigurationGrid::array_to_int_reverse_hash(const int *array) {
    unsigned int as_int= 0;
    for (int i= m_rotor_count - 1; i >= 0; --i) {   // TODO reverse iterator
        as_int*= m_letters;
        as_int+= array[i];
    }
    return as_int;
}