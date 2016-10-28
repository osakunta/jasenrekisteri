.PHONY : all build run

all : build

run : 
	POSTGRES_URL=postgres://$(USER)@localhost:5432/$(USER) stack exec jasenrekisteri-server data.json

build :
	stack build --pedantic
