////////////////////////////////////////////////////////////////////////
// Graph

var width  = 600,
    height = 650;

var radius = 15,
    colors = d3.scale.category10(),
    lineColor = "#e8e8d3",
    circleWidth = "4px",
    fuseWidth = "4px",
    noFuseWidth = "2px";

var darker = function(c, n) {
  var rgb = d3.rgb(c);
  n = n || 1;
  while (n--) {
    rgb = rgb.darker();
  }
  return rgb.toString();
}

var svg = d3.select('svg.graph')
    .attr('width', width)
    .attr('height', height);

var createGraph = function(graph) {
  svg.selectAll("*").remove();

  svg.append("svg:defs")
      .append("svg:marker")
        .attr("id", "arrow")
        .attr("viewBox", "0 -5 10 10")
        .attr("refX", 7)
        .attr("refY", 0)
        .attr("markerWidth", 3)
        .attr("markerHeight", 3)
        .attr("orient", "auto")
        .append("svg:path")
          .attr("d", "M0,-5L10,0L0,5")
          .style("fill", lineColor);

  var force = d3.layout.force()
      .nodes(graph.nodes)
      .links(graph.edges)
      .size([width, height])
      .linkDistance(50)
      .charge([-1500])
      .start();

  var edges = svg.selectAll(".edge")
      .data(graph.edges)
      .enter()
      .append("path")
        .classed("edge", true)
        .style("fill", "transparent")
        .style("stroke", lineColor)
        .style("stroke-width", function(d) { return d.canFuse ? fuseWidth : noFuseWidth; })
        .style("stroke-dasharray", function(d) { return d.canFuse ? null : "10,2"; })
        .style("marker-end", "url(#arrow)");

  var nodes = svg.selectAll(".node")
      .data(graph.nodes)
      .enter()
      .append("g")
        .classed("node", true)
        .call(force.drag);

  nodes.append("circle")
      .attr("r", radius)
      .style("fill",   function(d) { return colors(d.cluster); })
      .style("stroke", function(d) { return darker(colors(d.cluster)); })
      .style("stroke-width", circleWidth);

  nodes.append("text")
      .attr("x", 0)
      .attr("y", 4)
      .style("text-anchor", "middle")
      .style("font-size", "12px")
      .style("font-family", "sans-serif")
      .style("font-weight", "bold")
      .style("fill", function(d) { return darker(colors(d.cluster), 2); })
      .text(function(d) { return d.name; });

  force.on("tick", function() {
    edges.attr('d', function(d) {
      var dx = d.target.x - d.source.x,
          dy = d.target.y - d.source.y,
          length = Math.sqrt(dx * dx + dy * dy),
          nx = dx / length,
          ny = dy / length,
          soffset = radius;
          toffset = radius + 5;
          sx = d.source.x + (soffset * nx),
          sy = d.source.y + (soffset * ny),
          tx = d.target.x - (toffset * nx),
          ty = d.target.y - (toffset * ny);
      return 'M' + sx + ',' + sy + 'L' + tx + ',' + ty;
    });

    nodes.attr("transform", function(d) {
      return 'translate(' + [d.x, d.y] + ')';
    });
  });
}

var receiveGraph = function(json) {
  var graphs = JSON.parse(json);

  var gs = graphs.filter(function(x) {
    return x.nodes && x.nodes.length != 0;
  });

  var g = gs.shift();
  if (!g) return;

  console.log(g);

  var table = {};
  var lookup = function(name) {
    var node = table[name];
    if (node) return node;

    node = { name: name, cluster: "unknown" }
    g.nodes.push(node);
    table[name] = node

    return node;
  };

  g.nodes.forEach(function(n) {
    n.cluster = "unknown";
    table[n.name] = n;
  });

  g.edges.forEach(function(e) {
    e.source = lookup(e.source);
    e.target = lookup(e.target);
  });

  g.clusters.forEach(function(ns, i) {
    ns.forEach(function(n) {
      table[n].cluster = i;
    });
  });

  createGraph(g);
}

////////////////////////////////////////////////////////////////////////
// Editor

function betterTab(cm) {
  if (cm.somethingSelected()) {
    cm.indentSelection("add");
  } else {
    cm.replaceSelection(
      cm.getOption("indentWithTabs") ? "\t" : Array(cm.getOption("indentUnit") + 1).join(" "), "end", "+input");
  }
}

var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
  mode: "text/x-ddc-core",
  lineNumbers: true,
  matchBrackets: true,
  styleActiveLine: true,
  showCursorWhenSelecting: true,
  tabSize: 8,
  indentUnit: 4,
  indentWithTabs: false,
  extraKeys: { Tab: betterTab }
});

editor.setOption("theme", "jellybeans");
editor.setOption("vimMode", true);

var check = function() {
  var code    = editor.getValue();
  var results = document.getElementById("ddc-results");

  //var checkReq = new XMLHttpRequest();
  //checkReq.open('POST', '/check', true);
  //checkReq.onload  = function() { results.textContent = this.response; };
  //checkReq.onerror = function() { results.textContent = "an unknown error occurred" };
  //checkReq.setRequestHeader('Content-Type', 'text/x-ddc-core; charset=UTF-8');
  //checkReq.send(code);

  var graphReq = new XMLHttpRequest();
  graphReq.open('POST', '/graph', true);
  graphReq.onload  = function() { receiveGraph(this.response); };
  graphReq.setRequestHeader('Content-Type', 'text/x-ddc-core; charset=UTF-8');
  graphReq.send(code);
};

CodeMirror.on(editor, "changes", function(args) {
  check();
});

check();


