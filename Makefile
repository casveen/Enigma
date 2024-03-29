CC:= g++
FLAGS= -Wall -pedantic -Ofast #-fopenmp #openmp only needed in linking
SRC=src
BIN=bin
BUILD=build
INCLUDE=include
##NAMES
SOURCE_NAME    =enigma.cpp bombe.cpp enigma_main.cpp rotors.cpp
OBJECT_NAME    = $(SOURCE_NAME:%.cpp=%.o)
EXECUTABLE_NAME=enigma.exe bombe.exe test.exe performance.exe benchmarker.exe
#TEST_NAME      =test.cpp test_bombe.cpp test_enigma.cpp
##FILES
SOURCE_FILES     = $(SOURCE_NAME:%=$(SRC)/%)
EXECUTABLE_FILES = $(EXECUTABLE_NAME:%=$(BIN)/%)
OBJECT_FILES     = $(SOURCE_NAME:%.cpp=$(BUILD)/%.o)
##DEPENDENCIES
ENIGMA_DEP_NAMES = enigma.o rotors.o
ENIGMA_DEP       = $(ENIGMA_DEP_NAMES:%=$(BUILD)/%)
BOMBE_DEP_NAMES = $(ENIGMA_DEP_NAMES) bombe.o diagonal_board.o wire.o configuration_tracker.o bombe_unit.o graph.o
BOMBE_DEP       = $(BOMBE_DEP_NAMES:%=$(BUILD)/%)
TEST_DEP_NAMES   = $(BOMBE_DEP_NAMES) test.o test_configuration_tracker.o test_enigma.o test_connections.o connections.o #test_bombe.o 
TEST_DEP         = $(TEST_DEP_NAMES:%=$(BUILD)/%)
BENCHMARKER_DEP_NAMES   = benchmarker.o $(BOMBE_DEP_NAMES)
BENCHMARKER_DEP         = $(BENCHMARKER_DEP_NAMES:%=$(BUILD)/%)
PERFORMANCE_DEP_NAMES   = performance.o $(BOMBE_DEP_NAMES)
PERFORMANCE_DEP         = $(PERFORMANCE_DEP_NAMES:%=$(BUILD)/%)
##MAIN DEPENDENCIES
ENIGMA_MAIN_DEP_NAMES = enigma_main.o $(ENIGMA_DEP_NAMES)
ENIGMA_MAIN_DEP       = $(ENIGMA_MAIN_DEP_NAMES:%=$(BUILD)/%)
BOMBE_MAIN_DEP_NAMES  = bombe_main.o $(BOMBE_DEP_NAMES)
BOMBE_MAIN_DEP        = $(BOMBE_MAIN_DEP_NAMES:%=$(BUILD)/%)


LIB := -L lib -lboost_program_options 
INC := -I $(INCLUDE) -I $(SRC)

.PHONY : all clean test valgrind benchmark remake

#VPATH, so that make looks in correct directories
vpath %.cpp src test
#vpath %.hpp include
#vpath %.o   build
#vpath %.exe bin


#check:
#	@echo $(SOURCE_NAME:%=$(BUILD)/%)

##EXECUTABLES
all : $(EXECUTABLE_NAME)

enigma.exe : $(ENIGMA_DEP)
	$(CC) -o bin/enigma.exe $(FLAGS) $^ $(LIB)

bombe.exe : $(BOMBE_DEP)
	$(CC) -o bin/bombe.exe $(FLAGS) $^ $(LIB)

benchmarker.exe : $(BENCHMARKER_DEP)
	$(CC) -o bin/benchmarker.exe $(FLAGS) $^ $(INC)

performance.exe : $(PERFORMANCE_DEP)
	$(CC) -o bin/performance.exe $(FLAGS) $^ $(INC)

test.exe : $(TEST_DEP)
	$(CC) -o bin/test.exe $(FLAGS) $^ $(INC)

#COMMANDS
profile : performance.exe
	#gprof ./bin/performance.exe gmon.out
	valgrind --tool=callgrind ./bin/performance.exe
	#valgrind --tool=massif --stacks=yes ./bin/performance.exe
	kcachegrind
	#operf ./bin/performance.exe

valgrind : performance.exe #enigma.exe
	valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes -s ./bin/performance.exe 
	#valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes ./bin/enigma.exe --rotors I,II,III --reflector UKWK --plaintext ARTADOZSDUXDHCAMMRTCBVBLUYTOKGGEWZFYUICNNVPBRNYBRSCTSNUMLAYVAW

test : test.exe
	./bin/test.exe

benchmark : benchmarker.exe
	./bin/benchmarker.exe

clean:
	@echo " Cleaning...";
	@echo " rm -r $(BUILDDIR) $(EXECUTABLE_FILES)"; rm -r -f $(BUILD) $(EXECUTABLE_FILES)

remake:
	make clean
	make all

#object files
$(BUILD)/%.o: %.cpp
	@mkdir -p $(BUILD)
	$(CC) $(FLAGS) -c $< -o $@ $(INC)
