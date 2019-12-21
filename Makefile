INSCONFDIR = /usr/local/calc-tweet
INSTALL_PATH=/usr/local/bin
UINSDIR=$(HOME)/.local/bin
LOGDIR=/var/log/calc-tweet
.PHONY: build install clean test foruser
build :
	@stack build

install:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}
	@sudo mkdir -p ${INSCONFDIR}
	@sudo mkdir -p ${INSCONFDIR}/helps
	@sudo mkdir -p ${LOGDIR}
	@sudo cp conf/helps/* ${INSCONFDIR}/helps/ 
	@sudo cp conf/recept.log  ${LOGDIR}
	@echo "...done"

clean:
	@stack clean
	@sudo rm ${INSTALL_PATH}/calc-tweet
	@sudo rm -rf ${INSCONFDIR}

reinstall:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}

test:
	@stack test
