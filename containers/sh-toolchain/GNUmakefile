ROOT = ../..

# NAME: path to the code sscce relative to ROOT
NAME = $(shell realpath --relative-to $(ROOT)/containers $$PWD)

image:

	buildah bud --layers -t $(NAME)
