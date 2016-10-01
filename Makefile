.PHONY : all build run

all : tags.json build

run : 
	stack exec jasenrekisteri-server data.json tags.json

build : tags.json
	stack build --pedantic

tags.json : tags.yaml
	yaml2json tags.yaml > tags.json
