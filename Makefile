.PHONY: help
help:
	@echo "make go  - build Langsam (Go version)"
	@echo "make tgo - test Langsam (Go version)"
	@echo
	@echo "make c  - build Langsam (C version)"
	@echo "make tc - test Langsam (C version)"

.PHONY: go
go:
	$(MAKE) -C cmd/go

.PHONY: tgo
tgo: go
	./cmd/go/langsam tests/*.l

.PHONY: c
c:
	$(MAKE) -C c
	$(MAKE) -C cmd/c

.PHONY: tc
tc: c
	valgrind --leak-check=full --show-leak-kinds=all ./cmd/c/langsam tests/*.l

.PHONY: gdb
gdb: c
	gdb -x langsam.gdb --args ./cmd/c/langsam tests/*.l

.PHONY: clean
clean:
	$(MAKE) -C c clean
	$(MAKE) -C cmd/c clean
	$(MAKE) -C cmd/go clean
