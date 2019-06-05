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
	@sudo mkdir -p ${INSCONFDIR}/reminder
	@sudo cp conf/temp/* ${INSCONFDIR}/temp/ 
	@read -p "Twitter:APIKey?:" APIKey; sudo sh -c "echo  '$${APIKey}' > ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Twitter:API secret key?:" APISecretKey; sudo sh -c "echo '$${APISecretKey}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Twitter:Access token?:" AccessToken; sudo sh -c "echo '$${AccessToken}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Twitter:Access token secret?:" AccessTokenSecret; sudo sh -c "echo '$${AccessTokenSecret}' >> ${INSCONFDIR}/bot/twitterbot.conf"
	@read -p "Slack:APIToken?:" APIToken; sudo sh -c "echo '$${APIToken}' > ${INSCONFDIR}/bot/slackbot.conf"
	@read -p "Slack:Channel?:" Channel; sudo sh -c "echo '$${Channel}' >> ${INSCONFDIR}/bot/slackbot.conf"
	@sudo touch ${INSCONFDIR}/permissionuser.conf
	@echo "...done"

clean:
	@stack clean
	@sudo rm ${INSTALL_PATH}/calc-tweet
	@sudo rm -rf ${INSCONFDIR}

reinstall:
	@stack build
	@sudo install $(shell stack exec which calc-tweet) ${INSTALL_PATH}

