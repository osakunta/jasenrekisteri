NAME   := osakunta/jasenrekisteri
TAG    := $$(git log -1 --pretty=%h)
IMAGE  := ${NAME}:${TAG}
LATEST := ${NAME}:latest
EXEC   := jasenrekisteri-server

docker-build:
	sudo docker build --build-arg EXECUTABLE=${EXEC} -t ${IMAGE} .
	sudo docker tag ${IMAGE} ${LATEST}
 
docker-run:
	sudo docker run -it --name ${EXEC} --publish 8080:8080 ${LATEST}

docker-rm:
	sudo docker rm ${EXEC}

docker-push:
	sudo docker push ${NAME}

docker-bash:
	sudo docker exec -it ${EXEC} /bin/bash

.PHONY : all build run

all : build

run : 
	dotenv -f .env cabal new-run jasenrekisteri-server

build :
	cabal new-build
