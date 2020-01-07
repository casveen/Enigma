// main() provided by Catch in file 020-TestCase-1.cpp.
#include "bombe.hpp"
#include "catch.hpp"
#include "enigma.hpp"
#include "rotors.cpp"   //all rotors,

SCENARIO("bombeunit finds the configuration of an enimga", "[bombeunit]") {
    GIVEN("Enigma with rotors I, II, III, reflector UKW and a long plaintext") {
        Bombe bombe({I, II, III}, UKWR);
        // struct EnigmaSetting enigma_setting;
        bombe.get_setting().stop_on_first_valid   = true;
        bombe.get_setting().only_one_configuration= true;
        Enigma enigma({I, II, III}, UKWR);
        // enigma.set_verbose(true);
        string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDWITHTHEENIGMAANDISVERYV"
                          "ERYLONGITSSUPPOSEDTOBESOLONGASTOENGAGEBOTHTHEFASTAND"
                          "MIDDLEROTOROFATHREEROTORENIGMA";

        WHEN("Given ciphertext encrypted with ring-setting AAA, "
             "ring-position AAA and no steckering") {
            enigma.set_ring_setting("AAA");
            enigma.set_rotor_position("AAA");
            enigma.set_plugboard("");
            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AAA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                enigma.set_setting(solutions[0]);
                CHECK(enigma.encrypt(ciphertext) == plaintext);
                CHECK(solutions[0].rotor_position == "AAA");
            }
        }

        WHEN("Given ciphertext encrypted with ring-position CAA and no "
             "steckering") {
            // there was a bug with the enigma doing two turns for the first
            // check this test would not pass with this bug
            enigma.set_ring_setting("AAA");
            enigma.set_rotor_position("CAA");
            enigma.set_plugboard("");
            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AAA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                // enigma.set_setting(solutions[0]);
                // enigma.reset();
                enigma.set_setting(solutions[0]);
                CHECK(enigma.encrypt(ciphertext) == plaintext);
                CHECK(solutions[0].rotor_position == "CAA");
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting FGH, ring-position "
             "CDE, no steckering and bombe at same ring set") {
            enigma.set_ring_setting("FGH");
            enigma.set_rotor_position("CDE");

            string ciphertext                           = enigma.encrypt(plaintext);
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "FGH";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                // enigma.reset();
                // enigma.set_setting(solutions[0]);
                // cout<<"\n\nTEST: "<<enigma.encrypt(ciphertext)<<"\n";
                REQUIRE(solutions[0].rotor_position == "CDE");
                REQUIRE(solutions[0].ring_setting == "FGH");
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting RFW, ring-position "
             "BCD, bombe in same config and steckering") {
            // there was a bug with the enigma doing two turns for the first
            // check this test would not pass with this bug
            enigma.set_ring_setting("BBA");
            enigma.set_rotor_position("MCW");
            enigma.set_plugboard("AC. BD. EG. IR. JT. QZ");
            string ciphertext= enigma.encrypt(plaintext);
            // bombe.set_rotor_position("RFW");
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "BBA";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                enigma.set_setting(solutions[0]);
                CHECK(enigma.encrypt(ciphertext) == plaintext);
                CHECK(solutions[0].rotor_position == "MCW");
                CHECK(solutions[0].ring_setting == "BBA");
                // compare plugboards
                Plugboard correct_plugboard("AC. BD. EG. IR. JT. QZ", 26);
                for (int i= 0; i < 26; ++i) {
                    CHECK(solutions[0].plugboard.get_wiring(i) == correct_plugboard.get_wiring(i));
                }
            }
        }

        WHEN("Given ciphertext encrypted with ring-setting AFG, rotor-position "
             "HAP, steckering and bombe starting from ring-setting AEZ") {
            enigma.set_ring_setting("AFG");
            enigma.set_rotor_position("HAP");
            enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
            string ciphertext= enigma.encrypt(plaintext);
            enigma.reset();
            bombe.get_setting().starting_rotor_positions= "AAA";
            bombe.get_setting().starting_ring_setting   = "AEZ";
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                enigma.set_setting(solutions[0]);
                CHECK(enigma.encrypt(ciphertext) == plaintext);
                CHECK(solutions[0].rotor_position == "HAP");
                CHECK(solutions[0].ring_setting == "AFG");
                // compare plugboards
                Plugboard correct_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX", 26);
                for (int i= 0; i < 26; ++i) {
                    CHECK(solutions[0].plugboard.get_wiring(i) == correct_plugboard.get_wiring(i));
                }
            }
        }
    }
}   // SCENARIO

SCENARIO("bombe finds the configuration of an enimga", "[bombe]") {
    GIVEN("Enigma: VI, I, VI, UKWR. Steckered") {
        Bombe bombe({I, II, III, IV}, UKWR);
        bombe.get_setting().stop_on_first_valid  = true;
        bombe.get_setting().max_ring_settings    = 3;
        bombe.get_setting().rotor_count          = 3;
        bombe.get_setting().starting_ring_setting= "GHJ";
        bombe.get_setting().only_one_candidate   = false;
        // struct EnigmaSetting enigma_setting;

        Enigma enigma({II, I, IV}, UKWR);
        enigma.set_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX");
        // enigma.set_verbose(true);
        // TODO withoun andisveryverylong behaves weird, cannot find
        string plaintext= "THISISAPLAINTEXTTOBEENCRYPTEDANDISVERYVERYLONG";
        WHEN("ciphertext encrypted with RS:GHK, RP:BLO") {
            enigma.set_ring_setting("GHK");
            enigma.set_rotor_position("BLO");
            // enigma.set_plugboard("");
            string ciphertext= enigma.encrypt(plaintext);
            THEN("Running bombe with a complete crib should return the above "
                 "setting") {
                vector<struct EnigmaSetting> solutions= bombe.analyze(ciphertext, plaintext);
                enigma.set_setting(solutions[0]);
                CHECK(enigma.encrypt(ciphertext) == plaintext);
                CHECK(solutions[0].rotor_position == "BLO");
                CHECK(solutions[0].ring_setting == "GHK");
                Plugboard correct_plugboard("AK. IE. DV. CQ. BN, MO, PJ. WR. UX", 26);
                for (int i= 0; i < 26; ++i) {
                    CHECK(solutions[0].plugboard.get_wiring(i) == correct_plugboard.get_wiring(i));
                }
            }
        }
    }
    cout << "\r";
}
