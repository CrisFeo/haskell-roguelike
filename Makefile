DIR_DIST=$(shell stack path | sed -En 's/dist-dir: (.+)/\1/p')
EXECUTABLE = $(DIR_DIST)/build/haskell-roguelike-exe/haskell-roguelike-exe
APP_HS=$(shell find ./app -type f -name '*.hs')
SRC_HS=$(shell find ./src -type f -name '*.hs')


.PHONY: build
build: $(EXECUTABLE)

.PHONY: clean
clean:
	stack clean

.PHONY: exec
exec: build
	stack exec haskell-roguelike-exe

.PHONY: setup
setup:
	stack setup

tags: $(APP_HS) $(SRC_HS)
	echo ':ctags' | stack repl

$(EXECUTABLE): $(APP_HS) $(SRC_HS)
	stack build

