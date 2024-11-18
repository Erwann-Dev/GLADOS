##
## EPITECH PROJECT, 2024
## mypandoc
## File description:
## Makefile
##

NAME	=	glados

STACK	=	stack

BIN_DIR := $(shell $(STACK) path --local-install-root)

all:
	$(STACK) build
	cp $(BIN_DIR)/bin/$(NAME) ./

clean:
	$(STACK) clean
	rm -rf test/coverage/*

fclean: clean
	$(STACK) clean --full
	rm -f $(NAME)

tests_run :
	$(STACK) test --coverage
	$(STACK) hpc report --all --destdir test/coverage/

re : fclean all
