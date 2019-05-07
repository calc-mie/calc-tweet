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
	@sudo cp conf/template.conf ${INSCONFDIR}/template.conf 
	@read -p "APIKey?:" APIKey; sudo sh -c "echo  '$${APIKey}' > ${INSCONFDIR}/bot.conf"
	@read -p "API secret key?:" APISecretKey; sudo sh -c "echo '$${APISecretKey}' >> ${INSCONFDIR}/bot.conf"
	@read -p "Access token?:" AccessToken; sudo sh -c "echo '$${AccessToken}' >> ${INSCONFDIR}/bot.conf"
	@read -p "Access token secret?:" AccessTokenSecret; sudo sh -c "echo '$${AccessTokenSecret}' >> ${INSCONFDIR}/bot.conf"
	@sudo touch ${INSCONFDIR}/permissionuser.conf
	@echo "...done"

clean:
	@stack clean
	@sudo rm ${INSTALL_PATH}/calc-tweet
	@sudo rm -rf ${INSCONFDIR}

test:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}

