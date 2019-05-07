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
	@sudo mkdir -p ${INSCONFDIR}/temp
	@sudo mkdir -p ${INSCONFDIR}/bot
	@sudo cp conf/temp/* ${INSCONFDIR}/temp/ 
	@read -p "APIKey?:" APIKey; sudo sh -c "echo  '$${APIKey}' > ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "API secret key?:" APISecretKey; sudo sh -c "echo '$${APISecretKey}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Access token?:" AccessToken; sudo sh -c "echo '$${AccessToken}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Access token secret?:" AccessTokenSecret; sudo sh -c "echo '$${AccessTokenSecret}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@sudo touch ${INSCONFDIR}/permissionuser.conf
	@echo "...done"

clean:
	@stack clean
	@sudo rm ${INSTALL_PATH}/calc-tweet
	@sudo rm -rf ${INSCONFDIR}

test:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}

