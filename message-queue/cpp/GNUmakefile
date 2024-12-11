include ../mqp-base.mk

CXXOPTS =-g -std=c++23
CXXOPTS+=-lpqxx
# prometheus-cpp-dev on debian
CXXOPTS+=-lprometheus-cpp-push -lprometheus-cpp-core -lcurl -lz
CXXOPTS+=-Wall
CXXOPTS+=-o main

sources := $(wildcard src/*)

.PHONY: build
build: main ## Build

main: $(sources)
	g++ src/main.cpp $(CXXOPTS)
