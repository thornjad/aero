CONF_FILE = config/.spacemacs

setup:
	@cp $(CONF_FILE) $(HOME)
	@echo "SETUP CONFIGURATION FILE. YOU SHOULD NOW START EMACS TO COMPLETE SETUP"

grabconf:
	@cp $(HOME)/.spacemacs $(CONF_FILE)
	@echo "GRABBED CONFIGURATION FILE"

rmFiles = .circleci/ .travis.yml .travisci/ CHANGELOG.* CONTRIBUTING.org EXPERIMENTAL.org doc/ news/

upstream-staging:
	@echo "This target will help prepare upstream staging"
	git stash
	git checkout upstream-staging
	git pull origin
	git rebase master
	git fetch upstream
	git merge upstream/develop
	rm -rf $(rmFiles)
	git status
	@echo "Preparation complete for upstream-staging"
