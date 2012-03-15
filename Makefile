BIN_DIR=bin/

default: compile

compile: ${BIN_DIR}
	ghc --make -outputdir bin -o bin/ParseChunks ParseChunks.hs

${BIN_DIR}:
	mkdir -p ${BIN_DIR}
