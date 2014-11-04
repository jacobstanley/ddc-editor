////////////////////////////////////////////////////////////////////////
// Graph

// set up SVG for D3
var width  = 600,
    height = 600,
    circleRadius = 15,
    colors = d3.scale.category10();

var svg = d3.select('svg.graph')
  .attr('width', width)
  .attr('height', height);

// set up initial nodes and edges
var nodes = [];
var edges = [];

// init D3 force layout
var force = d3.layout.force()
    .nodes(nodes)
    .links(edges)
    .size([width, height])
    .linkDistance(50)
    .charge([-100])
    .on('tick', tick);

// define arrow markers for graph edges
svg.append('svg:defs').append('svg:marker')
    .attr('id', 'arrow')
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 6)
    .attr('markerWidth', 3)
    .attr('markerHeight', 3)
    .attr('orient', 'auto')
  .append('svg:path')
    .attr('d', 'M0,-5L10,0L0,5')
    .attr('fill', '#e8e8d3');

// handles to edge and node element groups
var path = svg.append('svg:g').selectAll('path'),
    circle = svg.append('svg:g').selectAll('g');

// mouse event vars
var selected_node = null,
    selected_edge = null,
    mousedown_edge = null,
    mousedown_node = null;

// update force layout (called automatically each iteration)
function tick() {
  // draw directed edges with proper padding from node centers
  path.attr('d', function(d) {
    var deltaX = d.target.x - d.source.x,
        deltaY = d.target.y - d.source.y,
        dist = Math.sqrt(deltaX * deltaX + deltaY * deltaY),
        normX = deltaX / dist,
        normY = deltaY / dist,
        sourcePadding = circleRadius;
        targetPadding = circleRadius + 5;
        sourceX = d.source.x + (sourcePadding * normX),
        sourceY = d.source.y + (sourcePadding * normY),
        targetX = d.target.x - (targetPadding * normX),
        targetY = d.target.y - (targetPadding * normY);
    return 'M' + sourceX + ',' + sourceY + 'L' + targetX + ',' + targetY;
  });

  circle.attr('transform', function(d) {
    return 'translate(' + d.x + ',' + d.y + ')';
  });
}

// update graph (called when needed)
function restart() {
  // path (edge) group
  path = path.data(edges);

  // update existing edges
  path.classed('canFuse', function(d) { return d.canFuse; })
    .style('marker-end', 'url(#arrow)');

  // add new edges
  path.enter().append('svg:path')
    .attr('class', 'edge')
    .classed('canFuse', function(d) { return d.canFuse; })
    .style('marker-end', 'url(#arrow)')
    .on('mousedown', function(d) {
      // select edge
      mousedown_edge = d;
      if(mousedown_edge === selected_edge) selected_edge = null;
      else selected_edge = mousedown_edge;
      selected_node = null;
      restart();
    });

  // remove old edges
  path.exit().remove();

  // circle (node) group
  // NB: the function arg is crucial here! nodes are known by name, not by index!
  circle = circle.data(nodes, function(d) { return d.name; });

  // update existing nodes (reflexive & selected visual states)
  circle.selectAll('circle')
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.name)).brighter().toString() : colors(d.name); })
    .classed('type-input', function(d) { return d.type == "(input)"; });

  // add new nodes
  var g = circle.enter().append('svg:g');

  g.append('svg:circle')
    .attr('class', 'node')
    .attr('r', circleRadius)
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.name)).brighter().toString() : colors(d.name); })
    .style('stroke', function(d) { return d3.rgb(colors(d.name)).darker().toString(); })
    .classed('type-input', function(d) { return d.type == "(input)"; })
    .call(force.drag);

    //.on('mouseover', function(d) {
    //  // enlarge target node
    //  d3.select(this).attr('transform', 'scale(1.1)');
    //})
    //.on('mouseout', function(d) {
    //  // unenlarge target node
    //  d3.select(this).attr('transform', '');
    //})
    //.on('mousedown', function(d) {
    //  // select node
    //  mousedown_node = d;
    //  if(mousedown_node === selected_node) selected_node = null;
    //  else selected_node = mousedown_node;
    //  selected_edge = null;

    //  restart();
    //});

  // show node IDs
  g.append('svg:text')
      .attr('x', 0)
      .attr('y', 4)
      .attr('class', 'name')
      .text(function(d) { return d.name; });

  // remove old nodes
  circle.exit().remove();

  // set the graph in motion
  force.start();
}

restart();

function receiveGraph(json) {
  var graphs = JSON.parse(json);
  console.log(graphs);

  var gs = graphs.filter(function(x) {
    return x.nodes && x.nodes.length != 0;
  });

  var g = gs.shift();
  if (!g) return;

  console.log(g);

  nodes.length = 0;
  edges.length = 0;

  var table = {};
  var lookup = function(name) {
    var node = table[name];
    if (node) return node;

    node = { name: name, type: "(input)" }
    nodes.push(node);
    table[name] = node

    return node;
  };

  g.nodes.forEach(function(n) {
    nodes.push(n);
    table[n.name] = n;
  });

  g.edges.forEach(function(e) {
    e.source = lookup(e.source);
    e.target = lookup(e.target);
    edges.push(e);
  });

  restart();

  //console.log(nodes);
  //console.log(edges);
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

  var checkReq = new XMLHttpRequest();
  checkReq.open('POST', '/check', true);
  checkReq.onload  = function() { results.textContent = this.response; };
  checkReq.onerror = function() { results.textContent = "an unknown error occurred" };
  checkReq.setRequestHeader('Content-Type', 'text/x-ddc-core; charset=UTF-8');
  checkReq.send(code);

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


