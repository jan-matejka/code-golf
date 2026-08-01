include ../../common.mk
exe = $$PWD/main
build_image_name=hello-world-haskell

build:

	ghc main.hs

build_image: build_leaf_image
check: dram
