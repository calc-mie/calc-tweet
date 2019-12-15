INSCONFDIR = /usr/local/calc-tweet
INSTALL_PATH=/usr/local/bin
UINSDIR=$(HOME)/.local/bin
.PHONY: build install clean test foruser
build :
	@stack build

install:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}
	@sudo mkdir -p ${INSCONFDIR}
	@sudo mkdir -p ${INSCONFDIR}/helps
	@sudo cp conf/helps/* ${INSCONFDIR}/helps/ 
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
