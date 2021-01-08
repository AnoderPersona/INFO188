all:
	ghc -dynamic -threaded -rtsopts -XStrict main.hs -o prog