BIN_DIR=bin/

default: compile

compile: ${BIN_DIR}
	ghc --make -outputdir bin Main.hs

${BIN_DIR}:
	mkdir -p ${BIN_DIR}

install-deps:
	cabal install json2 leveldb-haskell