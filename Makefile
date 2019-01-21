.PHONY : all build run

all : build

run : 
	PORT=8000 GOOGLE_CLIENT_ID=198725857640-tl7c0h3o7mgon7h901rocnm4jfe3nlak.apps.googleusercontent.com POSTGRES_URL=postgres://$(USER)@/$(USER) cabal new-run jasenrekisteri-server data.json

build :
	cabal new-build
