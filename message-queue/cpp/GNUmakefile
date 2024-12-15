include ../mqp-base.mk

CXX=gcc
CPPFLAGS=-g -std=c++23 -Wall
LDFLAGS=-g -std=c++23 -Wall
LDLIBS=
LDLIBS+=-lpqxx -lcurl
# debian: prometheus-cpp-dev
LDLIBS+=-lprometheus-cpp-push -lprometheus-cpp-core
LDLIBS+=-lz -lstdc++ -lm

# debian: libgtest-dev
test_LDLIBS=$(LDLIBS) -lgtest -lgtest_main

all_srcs := $(shell find ./src/*.cpp -type f)

SRCS := $(filter-out %_unittest.cpp,$(all_srcs))
OBJS = $(subst .cpp,.o,$(SRCS))

test_srcs := $(filter %_unittest.cpp,$(all_srcs))
test_objs = $(subst .cpp,.o,$(test_srcs)) $(filter-out ./src/main.o,$(OBJS))

.PHONY: all
all: build test

.PHONY: build
build: main ## Build

main: $(OBJS)

	$(CXX) $(LDFLAGS) -o main $^ $(LDLIBS)

test: $(test_objs)

	$(CXX) $(LDFLAGS) -o test $^ $(test_LDLIBS)

%.o: %.cpp %.hpp

	$(CXX) $(CPPFLAGS) -c $< -o $@

.PHONY: check
check: test

	./test

.PHONY: clean
clean:

	$(RM) src/*.o main test
