Karkeze is a project for me to experiment and learn both haskell and full text search.

You'll need to install ghc and cabal, then install dependencies:
	
	rake install_deps

TODO:

  * ✔ Limit search by field name
  * ✔ Delete Action
  * ✔ Safer CTRL-c quiting with thread
  * Update Action
  * More advanced field querying (e.g. field=value) Maybe additional AND query1
  * Limit search by collection (gmail tag-like
  * Parse query string
  * Score results
  * Removing stop words
  * Stemming
  * Document parsing? e.g. accepting .pdf files