verifyPackages:
	elm make src/Main.elm --output parse.js
	node package-tests.js
