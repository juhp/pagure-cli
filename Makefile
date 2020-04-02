stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts.yaml build
