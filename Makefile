.PHONY: clean clean-js install

clean-js:
	rm -rf js/dist

clean: clean-js
	rm -rf dist
	stack clean

js/dist/main.js:
	mkdir -p js/dist
	(cd js && yarn build)

dist/grasana: js/dist/main.js
	mkdir -p dist
	stack build grasana
	cp "`stack path --dist-dir`/build/grasana/grasana" dist/grasana

install: clean js/dist/main.js
	stack build --copy-bins grasana
