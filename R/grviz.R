#' viz.js dubious console
#'
#' @param expr viz.js code as a string or as an expression between `{}`
#'
#' @export
#' @examples
#' # all 3 same output!
#'
#' DiagrammeR::grViz("graph {
#'   a -- b -- c;
#'   b -- d;}")
#'
#' ?grViz> "
#'   a -- b -- c;
#'   b -- d;"
#'
#' ?grViz> {
#'   a -- b -- c;
#'   b -- d;
#' }
#'
#' # more complex
#'
#' ?grViz> {
#'   # add node statement
#'   node[shape = circle,
#'        fontname = Helvetica,
#'        penwidth = 2.0]
#'   A; B; C; D; E; F
#'
#'   node[shape = box]
#'   1; 2; 3; 4; 5; 6; 7; 8
#'
#'   # add edge statement
#'   edge[arrowhead = diamond]
#'   A->1; B->2; B->3; B->4; C->A;
#'   1->D; E->A; 2->4; 1->5; 1->F;
#'   E->6; 4->6; 5->7; 6->7;
#'   3->8 [label = " label!",
#'         fontname = Helvetica]
#'   # add a graph statement
#'   graph[nodesep = 0.1]
#'
#'   # Enjoy using DiagrammeR... It's fun!
#' }
#'
#' ?grViz> {
#'   # This attribute applies to the graph itself
#'   size="1,1";
#'   # The label attribute can be used to change the label of a node
#'   a [label="Foo"];
#'   # Here, the node shape is changed.
#'   b [shape=box];
#'   # These edges both have different line properties
#'   a -- b -- c [color=blue];
#'   b -- d [style=dotted];
#'   # [style=invis] hides a node.
#' }
#'
#'
#' ?grViz> {
#'   C_0 -- H_0 [type=s];
#'   C_0 -- H_1 [type=s];
#'   C_0 -- H_2 [type=s];
#'   C_0 -- C_1 [type=s];
#'   C_1 -- H_3 [type=s];
#'   C_1 -- H_4 [type=s];
#'   C_1 -- H_5 [type=s];
#' }
#'
#' # example for wikipedia, needed to add ++ to simulate space
#' # and to remove simple quote from label but that's a grViz() issue
#' ?grViz> {
#'   node [shape=plaintext];
#'   A1 -> B1;
#'   A2 -> B2;
#'   A3 -> B3;
#'
#'   A1 -> A2 [label=f];
#'   A2 -> A3 [label=g];
#'   B2 -> B3 [label="g"];
#'   B1 -> B3 [label="(g o f)", tailport=s, headport=s];
#'
#'   { rank=same; A1 ++ A2 ++ A3 }
#'   { rank=same; B1 ++ B2 ++ B3 }
#' }




`?grViz>` <- function (expr) {
  if (!requireNamespace("DiagrammeR"))
    stop("Install package DiagrammeR to use `?grViz>` function")

  expr <- substitute(expr)
  if (is.character(expr)) {
    code <- expr
  } else {
    if(expr[[1]] != quote(`{`)) stop("The expression must be wrapped in {braces}")
    expr[[1]] <- NULL
    code <- as.character(expr)
    code <- sapply(code, function(x) paste(rev(strsplit(x,"<-")[[1]]), collapse = " -> "))
    # code <- gsub("^(.*?)<-(.*?)$", "\\2 -> \\1", code)
    code <- gsub(" \\+ \\+", " ", code)
    code <- gsub(" - -", " -- ", code)
    code <- paste(code, collapse = "\n")
  }
  if(grepl("-\\s?-",code)){
    if(grepl("<-",code))
      stop("You can't use both `--` (graph) and `->` (digraph)")
    g <- "graph"
  } else {
    g <- "digraph"
  }

  code <- paste0(g, "{", code, "}")
  DiagrammeR::grViz(code)
}

