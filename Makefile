.PHONY : all build run

all : tags.json build

run : 
	POSTGRES_URL=postgres://$(USER)@localhost:5432/$(USER) stack exec jasenrekisteri-server data.json tags.json

build : tags.json
	stack build --pedantic
