##
## EPITECH PROJECT, 2024
## mypandoc
## File description:
## Makefile
##

NAME	=	glados

STACK	=	stack

DOCS_DIR := $(shell $(STACK) path --local-doc-root)

all:
	$(STACK) build --copy-bins --local-bin-path .

clean:
	$(STACK) clean
	rm -rf test/coverage/*
	rm -rf docs

fclean: clean
	$(STACK) clean --full
	rm -f $(NAME)

tests_run :
	$(STACK) test --coverage
	$(STACK) hpc report --all --destdir test/coverage/

docs:	fclean
	$(STACK) build --haddock
	mkdir -p docs
	cp -r $(DOCS_DIR)/* docs

re : fclean all
