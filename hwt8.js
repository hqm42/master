actions = {} // global FIXME
models = {}  // global FIXME

function mkContainer(/*children*/) {
  var d = document.createElement('div');
  for (var i=0, len=arguments.length; i<len; i++) {
    d.appendChild(arguments[i]);
  }
  return d;
} 

function mkLabel(text) {
  var l = document.createElement('span');
  return l;
}

function mkButton(text,action) {
  var b = document.createElement('button');
  b.setAttribute('onclick', 'actions[\'' + action + '\']()');
  return b;
}

function mkAction(target,actionName) {
   var a = function() {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.open("POST","action/" + actionName,true);
      xmlhttp.onreadystatechange=function() {
        if (xmlhttp.readyState!=4) {
          setModel(target,xmlhttp.responseText)
        };
     }
     xmlhttp.setRequestHeader('Content-Type','text/plain');
     xmlhttp.send(getModel(target));
  }
  actions[actionName] = a;
  return actionName
}

// helper begin

function getModel(elem) {
  return elem.firstChild.textContent
}

function setModel(elem,model) {
  var newContent = document.createTextNode(model);
  if (elem.childNodes && elem.childNodes.length > 0) {
    elem.replaceChild(newContent, elem.firstChild);
  } else {
    elem.appendChild(newContent);
  }
  models[elem] = model;
}

// helper end
