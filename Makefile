.PHONY:	build run clean

build:
	dune build

run: build
	clear
	dune exec ./_build/default/bin/main.exe
	# comment the following line to avoid generating the graph images
	neato -Tpng src/data/graph.dot -o src/data/graph.png
	neato -Tpng src/data/graph_partition.dot -o src/data/graph_partition.png
	neato -Tpng src/data/graph_marked.dot -o src/data/graph_marked.png
	neato -Tpng src/data/graph_rebuilt.dot -o src/data/graph_rebuilt.png

clean:
	clear
	dune clean
	rm -f src/data/*.dot src/data/*.png