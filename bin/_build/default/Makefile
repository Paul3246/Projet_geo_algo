.PHONY:	build run clean

build:
	dune build

run: build
	dune exec ./_build/default/spring_opt.exe
	neato -Tpng data/graph.dot -o graph.png
	neato -Tpng data/leaf_6.dot -o leaf_6.png
	neato -Tpng data/graph_rebuilt.dot -o graph_rebuilt.png

clean:
	dune clean
	rm -f *.dot *.png
	rm -f data/*.dot data/*.png