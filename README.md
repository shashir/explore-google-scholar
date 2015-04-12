Google Scholar Extended Extracts
===

Introduction
---

This project is an exploration in attempting to extract the most salient chunk of sentences to describe research papers queried from Google Scholar. There are two major components:
 * Scala code containing the primitive Google Scholar API as well as proof of concept methods for parsing PDFs of research papers for salient chunks (e.g. the abstract)
 * Web page containing some atrocious JQuery used to query the Scala web service which fetch research papers and their extracts from Google Scholar.

Extract generation strategies
---

Available strategies for generating extracts include:
 * SpecialSauce -- a weighted combination of packing title words, removing captions, reducing numerics, and maximizing density of the sentences (number of characters as well as number of words).
 * TitlePacked -- find the sentences which use title words the most.
 * Dense -- find the sentences with the most characters.
 * Wordy -- find the sentences with the most words.
 * Abstract -- attempt to locate the abstract using queues.

The web page shows these strategies as radio button choices.

Build and launch
---

In order to compile the Scala code do.
```
mvn clean install
```

This creates the necessary classes and bundled JAR in the `/target` directory.

Launch the web service with:
```
java -cp target/scholar-0.0.1-shaded.jar service.Boot 8080
```

Now open the HTML page in your favorite browser with the following query param.
```
google-chrome "file:///path/to/project/html/index.html?addr=http://localhost:8080"
```

Use the search bar to make queries.

Dependencies
---
 * Scala Google Scholar API, extract generation, and web service
   * Spray for web service.
   * JSoup for querying HTML Google Scholar pages for article results (since Google Scholar lacks a public API).
   * ScalaNLP for processing article PDFs.
   * PDFBox for parsing PDFs of the papers.
 * Web page
   * Bootstrap for pretty displaying
   * JQuery for launching RESTful queries from the web service.
