GraphVizify
===========

A pandoc-based tool allowing one to use inline GraphViz dot/neato inside Markdown

To process this file, use pipes, for example:

     cat README.md | graphvizify -Tpng -oimages | pandoc > output.html

## Example inline GraphViz pictures below

Note that the captions are optional.

~~~ {.dot caption="a directed graph"}
digraph G1 {
    node[shape=box]
    a -> b;
    b -> c;
    a -> c;
}
~~~

~~~ {.neato caption="a simple graph"}
graph G1 {
    a -- b;
    b -- c;
    a -- c;
}
~~~

