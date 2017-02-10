
## Parsing WikiMedia for Points of Interest

The objective of this project is to parse the information that is part of the public domain, in wikimedia format, to obtain points of interest to entice people to travel to those places around the world.

It was written in Scala, the xml parsing is done using the normal Scala API, making use of it's powerful parser combinators to structure out a grammar for the points of interest. The rest of the XML was parsed in a hackish way using regular expressions and making some assumptions that may be perilously wrong about the wikimedia format. There's a lot of room for improvement, I hope to make use of the parsing combinator functionality to describe a full grammar for the wikimedia format in the future. The Json encoding is done using Circe. Processing is done via streaming to keep memory footprint as low as possible. 

I started out with a big XML file of [wikivoyage](https://en.wikivoyage.org/)
