all:
	@stack build

docker:
	@docker build .
