BIN_DIR=bin/

object_files := $(patsubst %.hs,%,$(wildcard *.hs))

all: $(object_files)

$(object_files):
	ghc --make -outputdir bin $@.hs

clean:
	rm -f $(object_files)
	rm -rf bin

${BIN_DIR}:
	mkdir -p ${BIN_DIR}

install-deps:
	cabal install json2 leveldb-haskell