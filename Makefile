REPO   := docker-registry.itransition.corp
NAME   := ldabot
TAG    := $$(git log -1 --pretty=%H)
IMG    := ${NAME}:${TAG}
LATEST := ${NAME}:latest

all: build push

build:
	@stack test --docker && stack install --docker && docker build -t ${NAME} .
	@docker tag ${NAME} ${IMG}
	@docker tag ${IMG} ${REPO}/${IMG}
	@docker tag ${IMG} ${REPO}/${LATEST}

push:
	@docker push ${REPO}/${NAME}