.PHONY : all build run

all : build

run : 
	dotenv -f .env cabal new-run jasenrekisteri-server

build :
	cabal new-build
