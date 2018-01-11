if [ ! -z "$1" ]; then
	echo "No file given."
	exit 1
fi

./suffix_automata < "$1"
