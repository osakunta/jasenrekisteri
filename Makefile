.PHONY : all build run

all : build

run : 
	PORT=8000 POSTGRES_URL=postgres://$(USER)@localhost:5432/$(USER) stack exec jasenrekisteri-server data.json

build :
	stack build --pedantic
