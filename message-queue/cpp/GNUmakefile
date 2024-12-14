include ../mqp-base.mk

CXX=gcc
CPPFLAGS=-g -std=c++23 -Wall
LDFLAGS=-g -std=c++23 -Wall
LDLIBS=
LDLIBS+=-lpqxx -lcurl
# prometheus-cpp-dev on debian
LDLIBS+=-lprometheus-cpp-push -lprometheus-cpp-core
LDLIBS+=-lz -lstdc++ -lm

SRCS := $(shell find ./src/*.cpp -type f )
OBJS = $(subst .cpp,.o,$(SRCS))

.PHONY: build
build: main ## Build

main: $(OBJS)

	$(CXX) $(LDFLAGS) -o main $(OBJS) $(LDLIBS)

%.o: %.cpp %.hpp

	$(CXX) $(CPPFLAGS) -c $< -o $@

clean:

	$(RM) src/*.o
